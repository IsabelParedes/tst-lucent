#!/usr/bin/env node
// Tiny static dev server for the site/ test harness.
//
// Its one job beyond plain file serving is to send `Service-Worker-Allowed: /`
// so the transport service worker installed deep under
// /R_HOME/library/httpuv/www/httpuv-sw.js can claim the site root (and thus the
// app shell at /contents/ and the virtual Shiny app URLs). `python3 -m
// http.server` cannot set that header, which is why this exists.
//
// Usage: node serve.mjs [port]   (defaults to 9008, or $PORT)

import { createReadStream, promises as fs } from "node:fs";
import { createServer } from "node:http";
import { extname, join, normalize, sep } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = fileURLToPath(new URL(".", import.meta.url));
const PORT = Number(process.argv[2] ?? process.env.PORT ?? 9008);

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

function baseHeaders(extra = {}) {
  return {
    // Let the deep-path service worker control the whole origin.
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

  // Land on the app shell instead of a bare directory listing.
  if (urlPath === "/" || urlPath === "") {
    res.writeHead(302, baseHeaders({ Location: "/contents/" }));
    res.end();
    return;
  }

  let filePath = resolvePath(urlPath);
  if (!filePath) {
    res.writeHead(403, baseHeaders({ "Content-Type": "text/plain; charset=utf-8" }));
    res.end("Forbidden");
    return;
  }

  try {
    let stat = await fs.stat(filePath);
    if (stat.isDirectory()) {
      filePath = join(filePath, "index.html");
      stat = await fs.stat(filePath);
    }

    const headers = baseHeaders({
      "Content-Type": contentType(filePath),
      "Content-Length": String(stat.size),
    });

    res.writeHead(200, headers);
    if (method === "HEAD") {
      res.end();
      return;
    }
    createReadStream(filePath).pipe(res);
  } catch {
    res.writeHead(404, baseHeaders({ "Content-Type": "text/plain; charset=utf-8" }));
    res.end(`Not Found: ${urlPath}`);
  }
});

server.listen(PORT, () => {
  const root = ROOT.replace(new RegExp(`${sep}$`), "");
  console.log(`[serve] site root: ${root}`);
  console.log(`[serve] Service-Worker-Allowed: / on every response`);
  console.log(`[serve] open http://localhost:${PORT}/contents/`);
});
