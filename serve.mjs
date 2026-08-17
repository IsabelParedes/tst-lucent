#!/usr/bin/env node
/**
 * Local dev server for the site harness.
 *
 * - Serves this directory (packages/, runtime/, webApp/, lucent/dist/, …).
 * - Aliases /httpuv-sw.js to lucent/dist/httpuv-sw.js (GitHub Pages layout).
 * - Runs `npm run dev` in lucent/ so tsup rebuilds on source changes.
 * - Repacks webApp/ into packages/webApp.tar.gz when the app sources change.
 *
 * Usage:
 *   node serve.mjs [port]          default port 9008
 *   npm run serve [-- port]
 */
import { spawn } from "node:child_process";
import { watch } from "node:fs";
import { copyFile, readFile, stat } from "node:fs/promises";
import { createServer } from "node:http";
import { dirname, extname, join, normalize } from "node:path";
import { fileURLToPath } from "node:url";

const SITE_DIR = dirname(fileURLToPath(import.meta.url));
const LUCENT_DIR = join(SITE_DIR, "lucent");
const LUCENT_DIST = join(LUCENT_DIR, "dist");
const WEBAPP_DIR = join(SITE_DIR, "webApp");
const PACKAGES_DIR = join(SITE_DIR, "packages");
const PORT = Number.parseInt(process.argv[2] ?? "9008", 10);

const MIME = {
  ".css": "text/css; charset=utf-8",
  ".gif": "image/gif",
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".map": "application/json; charset=utf-8",
  ".png": "image/png",
  ".r": "text/plain; charset=utf-8",
  ".svg": "image/svg+xml",
  ".tar.gz": "application/gzip",
  ".wasm": "application/wasm",
};

/** Root-scoped service worker alias (see prepare-pages.sh). */
const SW_ALIAS = {
  "/httpuv-sw.js": "lucent/dist/httpuv-sw.js",
  "/httpuv-sw.js.map": "lucent/dist/httpuv-sw.js.map",
};

function log(message) {
  console.log(`[serve] ${message}`);
}

function die(message) {
  console.error(`[serve] ERROR: ${message}`);
  process.exit(1);
}

function resolveSitePath(relativePath) {
  const abs = normalize(join(SITE_DIR, relativePath));
  if (abs !== SITE_DIR && !abs.startsWith(`${SITE_DIR}/`)) {
    throw new Error(`path escapes site root: ${relativePath}`);
  }
  return abs;
}

async function servePath(relativePath, res) {
  let abs;
  try {
    abs = resolveSitePath(relativePath);
  } catch {
    res.writeHead(403).end("Forbidden");
    return;
  }

  let info;
  try {
    info = await stat(abs);
  } catch {
    res.writeHead(404).end("Not found");
    return;
  }

  if (!info.isFile()) {
    res.writeHead(404).end("Not found");
    return;
  }

  const body = await readFile(abs);
  const type = MIME[extname(abs)] ?? "application/octet-stream";
  res.writeHead(200, { "Content-Type": type }).end(body);
}

async function syncServiceWorkerCopy() {
  const src = join(LUCENT_DIST, "httpuv-sw.js");
  const dst = join(SITE_DIR, "httpuv-sw.js");
  try {
    await copyFile(src, dst);
  } catch {
    return;
  }
  log("synced httpuv-sw.js to site root");
  try {
    await copyFile(`${src}.map`, `${dst}.map`);
  } catch {
    // source map is optional during early builds
  }
}

function runCommand(command, args, { cwd = SITE_DIR } = {}) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      cwd,
      stdio: "inherit",
      shell: process.platform === "win32",
    });
    child.on("error", reject);
    child.on("exit", (code) => {
      if (code === 0) {
        resolve();
        return;
      }
      reject(new Error(`${command} exited with code ${code}`));
    });
  });
}

let webAppRepackRunning = false;
let webAppRepackQueued = false;

async function webAppMountRegistered() {
  const metaPath = join(PACKAGES_DIR, "empack_env_meta.json");
  const meta = JSON.parse(await readFile(metaPath, "utf8"));
  const mounts = meta.mounts ?? [];
  return mounts.some((mount) => mount.filename === "webApp.tar.gz");
}

async function repackWebApp() {
  if (webAppRepackRunning) {
    webAppRepackQueued = true;
    return;
  }

  webAppRepackRunning = true;
  try {
    log("repacking webApp → packages/webApp.tar.gz");
    await runCommand("empack", [
      "pack",
      "dir",
      "--host-dir",
      WEBAPP_DIR,
      "--mount-dir",
      "/webApp",
      "--outname",
      "webApp.tar.gz",
      "--outdir",
      PACKAGES_DIR,
    ]);
    if (await webAppMountRegistered()) {
      log("webApp.tar.gz updated (mount already in empack_env_meta.json)");
    } else {
      await runCommand("empack", [
        "pack",
        "append",
        "--env-meta",
        join(PACKAGES_DIR, "empack_env_meta.json"),
        "--tarfile",
        join(PACKAGES_DIR, "webApp.tar.gz"),
      ]);
    }
    log("webApp repack done — reload the browser to apply VFS changes");
  } catch (err) {
    log(`webApp repack failed: ${err.message}`);
  } finally {
    webAppRepackRunning = false;
    if (webAppRepackQueued) {
      webAppRepackQueued = false;
      repackWebApp().catch((err) => log(`webApp repack failed: ${err.message}`));
    }
  }
}

function startLucentWatch() {
  log("starting lucent watch (npm run dev)");
  const child = spawn("npm", ["run", "dev"], {
    cwd: LUCENT_DIR,
    stdio: "inherit",
    shell: process.platform === "win32",
  });
  child.on("exit", (code, signal) => {
    if (signal) {
      log(`lucent watch stopped (${signal})`);
    } else if (code !== 0) {
      log(`lucent watch exited with code ${code}`);
    }
  });
  return child;
}

function watchDebounced(dir, { delayMs, label }, onChange) {
  let timer = null;
  const schedule = () => {
    clearTimeout(timer);
    timer = setTimeout(() => {
      onChange().catch((err) => log(`${label} failed: ${err.message}`));
    }, delayMs);
  };

  try {
    watch(dir, { recursive: true }, schedule);
    log(`watching ${dir}`);
  } catch (err) {
    log(`could not watch ${dir} (${err.message})`);
  }
}

async function assertPrerequisites() {
  const checks = [
    ["packages/empack_env_meta.json", "run ./pack-empack.sh"],
    ["runtime/bin/Rmain.js", "run ./pack-empack.sh"],
    ["runtime/bin/Rmain.wasm", "run ./pack-empack.sh"],
    ["index.html", "missing site/index.html"],
    ["webApp", "missing site/webApp"],
    ["lucent/package.json", "missing lucent submodule (git submodule update --init lucent)"],
  ];

  for (const [path, hint] of checks) {
    try {
      await stat(resolveSitePath(path));
    } catch {
      die(`missing ${path} (${hint})`);
    }
  }
}

function startHttpServer() {
  const server = createServer(async (req, res) => {
    const url = new URL(req.url ?? "/", `http://${req.headers.host ?? "localhost"}`);
    let pathname = decodeURIComponent(url.pathname);

    const alias = SW_ALIAS[pathname];
    if (alias) {
      await servePath(alias, res);
      return;
    }

    if (pathname.endsWith("/")) {
      pathname += "index.html";
    } else if (!extname(pathname)) {
      pathname += "/index.html";
    }

    const relative = pathname.replace(/^\//, "");
    await servePath(relative, res);
  });

  server.listen(PORT, () => {
    log(`http://localhost:${PORT}/`);
  });

  return server;
}

let lucentWatch = null;
let httpServer = null;

function shutdown() {
  log("shutting down");
  lucentWatch?.kill("SIGTERM");
  httpServer?.close();
  process.exit(0);
}

process.on("SIGINT", shutdown);
process.on("SIGTERM", shutdown);

await assertPrerequisites();

if (!Number.isFinite(PORT) || PORT <= 0 || PORT > 65535) {
  die(`invalid port: ${process.argv[2] ?? ""}`);
}

try {
  await stat(join(LUCENT_DIST, "runApp.js"));
} catch {
  log("lucent/dist missing; running one-off build first");
  await new Promise((resolve, reject) => {
    const build = spawn("npm", ["run", "build"], {
      cwd: LUCENT_DIR,
      stdio: "inherit",
      shell: process.platform === "win32",
    });
    build.on("exit", (code) => {
      if (code === 0) resolve();
      else reject(new Error(`lucent build failed with code ${code}`));
    });
  });
}

await syncServiceWorkerCopy();
lucentWatch = startLucentWatch();
watchDebounced(LUCENT_DIST, { delayMs: 80, label: "lucent sync" }, syncServiceWorkerCopy);
watchDebounced(WEBAPP_DIR, { delayMs: 400, label: "webApp repack" }, repackWebApp);
httpServer = startHttpServer();
