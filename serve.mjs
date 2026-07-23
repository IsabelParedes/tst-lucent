#!/usr/bin/env node
// Static file server for the site/ harness.
//
// - Cache-Control: no-store so Lucent / transport rebuilds show up immediately.
// - On startup, copies httpuv-sw.js(.map) from _env-wasm/.../httpuv/www/ to the
//   site root so Lucent's root-scoped SW registration stays current.
// - Auto-generate directory manifest.json when absent (e.g. webApp/).
//
// Usage: node serve.mjs [port]   (defaults to 9008, or $PORT)

import { createReadStream, promises as fs } from "node:fs";
import { createServer } from "node:http";
import { dirname, extname, join, normalize, relative, sep } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = fileURLToPath(new URL(".", import.meta.url));
const PORT = Number(process.argv[2] ?? process.env.PORT ?? 9008);
const DEEP_SW = join(ROOT, "_env-wasm/lib/R/library/httpuv/www/httpuv-sw.js");
const ROOT_SW = join(ROOT, "httpuv-sw.js");

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".map": "application/json; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".svg": "image/svg+xml",
  ".wasm": "application/wasm",
  ".ico": "image/x-icon",
  ".png": "image/png",
  ".R": "text/plain; charset=utf-8",
  ".r": "text/plain; charset=utf-8",
  ".ts": "text/plain; charset=utf-8",
  ".txt": "text/plain; charset=utf-8",
};

function contentType(path) {
  if (path.endsWith(".d.ts")) return "text/plain; charset=utf-8";
  return MIME[extname(path)] ?? "application/octet-stream";
}

function headers(extra = {}) {
  return { "Cache-Control": "no-store", ...extra };
}

/** Resolve a request path to an absolute file path inside ROOT (or null). */
function resolvePath(urlPath) {
  const decoded = decodeURIComponent(urlPath.split("?")[0]);
  const rel = normalize(decoded).replace(/^(\.\.[/\\])+/, "");
  const abs = join(ROOT, rel);
  if (abs !== ROOT.replace(/[/\\]$/, "") && !abs.startsWith(ROOT)) {
    return null;
  }
  return abs;
}

async function listFilesRecursive(dir, base = dir) {
  const out = [];
  for (const entry of await fs.readdir(dir, { withFileTypes: true })) {
    if (entry.name.startsWith(".")) continue;
    const abs = join(dir, entry.name);
    if (entry.isDirectory()) {
      out.push(...(await listFilesRecursive(abs, base)));
    } else if (entry.isFile()) {
      out.push(relative(base, abs).split(sep).join("/"));
    }
  }
  return out;
}

async function statFile(path) {
  try {
    const stat = await fs.stat(path);
    return stat.isFile() ? stat : null;
  } catch {
    return null;
  }
}

/** Refresh site-root httpuv-sw.js from the wasm prefix transport build. */
async function syncRootServiceWorker() {
  if (!(await statFile(DEEP_SW))) {
    console.warn(`[serve] missing ${DEEP_SW}; /httpuv-sw.js not updated`);
    return;
  }
  await fs.copyFile(DEEP_SW, ROOT_SW);
  const deepMap = `${DEEP_SW}.map`;
  if (await statFile(deepMap)) {
    await fs.copyFile(deepMap, `${ROOT_SW}.map`);
  }
  console.log("[serve] synced httpuv-sw.js from _env-wasm/.../httpuv/www/");
}

const server = createServer(async (req, res) => {
  const method = req.method ?? "GET";
  if (method !== "GET" && method !== "HEAD") {
    res.writeHead(405, headers({ Allow: "GET, HEAD" }));
    res.end("Method Not Allowed");
    return;
  }

  const urlPath = req.url ?? "/";
  const pathname = decodeURIComponent(urlPath.split("?")[0]);
  let filePath = resolvePath(urlPath);
  if (!filePath) {
    res.writeHead(403, headers({ "Content-Type": "text/plain; charset=utf-8" }));
    res.end("Forbidden");
    return;
  }

  // Synthesize manifest.json for a directory when no static one exists.
  if (pathname.endsWith("/manifest.json") && !(await statFile(filePath))) {
    const dirAbs = dirname(filePath);
    const dirStat = await fs.stat(dirAbs).catch(() => null);
    if (dirStat?.isDirectory()) {
      const files = (await listFilesRecursive(dirAbs)).filter((f) => f !== "manifest.json");
      const body = JSON.stringify({ files });
      res.writeHead(
        200,
        headers({
          "Content-Type": "application/json; charset=utf-8",
          "Content-Length": String(Buffer.byteLength(body)),
        }),
      );
      res.end(method === "HEAD" ? undefined : body);
      return;
    }
  }

  try {
    let stat = await fs.stat(filePath);
    if (stat.isDirectory()) {
      filePath = join(filePath, "index.html");
      stat = await fs.stat(filePath);
    }

    res.writeHead(
      200,
      headers({
        "Content-Type": contentType(filePath),
        "Content-Length": String(stat.size),
      }),
    );
    if (method === "HEAD") {
      res.end();
      return;
    }
    createReadStream(filePath).pipe(res);
  } catch {
    res.writeHead(404, headers({ "Content-Type": "text/plain; charset=utf-8" }));
    res.end(`Not Found: ${urlPath}`);
  }
});

await syncRootServiceWorker();
server.listen(PORT, () => {
  console.log(`[serve] site root: ${ROOT.replace(new RegExp(`${sep}$`), "")}`);
  console.log(`[serve] open http://localhost:${PORT}/`);
});
