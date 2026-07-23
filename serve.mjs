#!/usr/bin/env node
// Tiny static dev server for the site/ test harness.
//
// Beyond plain file serving it:
// 1. Aliases `/httpuv-sw.js` → the canonical copy under
//    `/_env-wasm/lib/R/library/httpuv/www/` so Lucent can register a root-scoped
//    service worker (required on hosts like GitHub Pages that cannot send
//    `Service-Worker-Allowed`).
// 2. Still sends `Service-Worker-Allowed: /` so a deep-path SW registration
//    keeps working during local debugging.
//
// Usage: node serve.mjs [port]   (defaults to 9008, or $PORT)

import { createReadStream, promises as fs } from "node:fs";
import { createServer } from "node:http";
import { dirname, extname, join, normalize, relative, sep } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = fileURLToPath(new URL(".", import.meta.url));
const PORT = Number(process.argv[2] ?? process.env.PORT ?? 9008);

const DEEP_SW_REL = join("_env-wasm", "lib", "R", "library", "httpuv", "www", "httpuv-sw.js");

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

/**
 * Map public URL paths onto files. Root SW aliases fall through to the
 * canonical copy under the wasm prefix when no physical root file exists.
 */
function resolveFile(urlPath) {
  const pathname = decodeURIComponent(urlPath.split("?")[0]);
  if (pathname === "/httpuv-sw.js" || pathname === "/httpuv-sw.js.map") {
    const rootCopy = resolvePath(pathname);
    return { preferred: rootCopy, fallback: join(ROOT, DEEP_SW_REL + (pathname.endsWith(".map") ? ".map" : "")) };
  }
  return { preferred: resolvePath(urlPath), fallback: null };
}

async function firstExisting(paths) {
  for (const p of paths) {
    if (!p) continue;
    try {
      const stat = await fs.stat(p);
      if (stat.isFile()) return { path: p, stat };
    } catch {
      // try next
    }
  }
  return null;
}

/** Recursively list files under `dir`, as forward-slash paths relative to `dir`. */
async function listFilesRecursive(dir, base = dir) {
  const out = [];
  const entries = await fs.readdir(dir, { withFileTypes: true });
  for (const entry of entries) {
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

function baseHeaders(extra = {}) {
  return {
    // Let a deep-path service worker control the whole origin (local fallback).
    "Service-Worker-Allowed": "/",
    // Dev server: never cache, so rebuilds are picked up immediately.
    "Cache-Control": "no-store",
    ...extra,
  };
}

const server = createServer(async (req, res) => {
  const method = req.method ?? "GET";
  if (method !== "GET" && method !== "HEAD") {
    res.writeHead(405, baseHeaders({ Allow: "GET, HEAD" }));
    res.end("Method Not Allowed");
    return;
  }

  const urlPath = req.url ?? "/";

  const { preferred, fallback } = resolveFile(urlPath);
  if (!preferred && !fallback) {
    res.writeHead(403, baseHeaders({ "Content-Type": "text/plain; charset=utf-8" }));
    res.end("Forbidden");
    return;
  }

  // Auto-generate a manifest.json for a directory (the app file list) when no
  // static one is present. Lets the browser enumerate app files (e.g. webApp/).
  if (decodeURIComponent(urlPath.split("?")[0]).endsWith("/manifest.json") && preferred) {
    const exists = await fs.stat(preferred).then(() => true).catch(() => false);
    if (!exists) {
      const dirAbs = dirname(preferred);
      const dirStat = await fs.stat(dirAbs).catch(() => null);
      if (dirStat?.isDirectory()) {
        const files = (await listFilesRecursive(dirAbs)).filter((f) => f !== "manifest.json");
        const body = JSON.stringify({ files });
        const headers = baseHeaders({
          "Content-Type": "application/json; charset=utf-8",
          "Content-Length": String(Buffer.byteLength(body)),
        });
        res.writeHead(200, headers);
        res.end(method === "HEAD" ? undefined : body);
        return;
      }
    }
  }

  try {
    let hit = await firstExisting([preferred, fallback]);
    if (!hit && preferred) {
      // Directory → index.html (only for preferred path; aliases are files).
      const dirStat = await fs.stat(preferred).catch(() => null);
      if (dirStat?.isDirectory()) {
        const indexPath = join(preferred, "index.html");
        const indexStat = await fs.stat(indexPath);
        hit = { path: indexPath, stat: indexStat };
      }
    }
    if (!hit) {
      throw new Error("not found");
    }

    const headers = baseHeaders({
      "Content-Type": contentType(hit.path),
      "Content-Length": String(hit.stat.size),
    });

    res.writeHead(200, headers);
    if (method === "HEAD") {
      res.end();
      return;
    }
    createReadStream(hit.path).pipe(res);
  } catch {
    res.writeHead(404, baseHeaders({ "Content-Type": "text/plain; charset=utf-8" }));
    res.end(`Not Found: ${urlPath}`);
  }
});

server.listen(PORT, () => {
  const root = ROOT.replace(new RegExp(`${sep}$`), "");
  console.log(`[serve] site root: ${root}`);
  console.log(`[serve] /httpuv-sw.js → ${DEEP_SW_REL} (if no root copy)`);
  console.log(`[serve] Service-Worker-Allowed: / on every response`);
  console.log(`[serve] open http://localhost:${PORT}/`);
});
