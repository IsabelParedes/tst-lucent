# Browser harness for Lucent + R.wasm + Shiny

- **App shell** — `index.html`, `style.css`, `favicon.svg`, and the Shiny app sources at `webApp/` (packed into the VFS via empack).
- **Lucent runtime** — built to `lucent/dist/{runApp.js,rWasmWorker.js,httpuv-web.js,httpuv-sw.js,shiny-socket.js}`.
- **httpuv transport** — Lucent-built assets under `lucent/dist/` (root `httpuv-sw.js` is a scope alias).
- **R runtime** — `runtime/bin/Rmain.{js,wasm}` plus `packages/` (`empack_env_meta.json` and archives, including appended `webApp.tar.gz`).

Clone with submodules:

```bash
git clone --recurse-submodules <this-repo-url>
# or after a plain clone:
git submodule update --init --recursive
```

## Dev server

Requirements:
- `empack`
- `nodejs`

```bash
# Create WASM environment
micromamba create -p ./_prefix-wasm --platform=emscripten-wasm32 -f env_wasm_run.yaml
./pack-empack.sh                 # once / when env or webApp changes
( cd lucent && npm run build )
npm run serve                    # port 9008 (override: npm run serve -- 8080)
```

For debugging: `http://localhost:9008/?httpuvDebug=1`

## Testing with native R

Create environment (`env_native_lagun.yaml`)

```R
library(shiny)
options(browser="chromium")
runApp("./webApp")
```

## Serving layout

```
/index.html                                       app shell (page you open)
/webApp/                                          Shiny app sources (also empack-appended into VFS)
/lucent/dist/runApp.js                            Lucent host → Worker(rWasmWorker.js)
/lucent/dist/httpuv-*.js                          transport + SW (canonical)
/httpuv-sw.js                                     root-scoped SW alias
/runtime/bin/Rmain.{js,wasm}                      embed front-end (MAIN_MODULE)
/packages/empack_env_meta.json                    empack package + webApp list
/packages/*.tar.gz                                conda + webApp archives
```

Lucent registers the service worker from **`/httpuv-sw.js`** (site root) so the
default scope covers the app shell and virtual Shiny URLs. That matters on
hosts like **GitHub Pages**, which cannot send `Service-Worker-Allowed: /`.
Locally, `serve.mjs` copies `lucent/dist/httpuv-sw.js` to the site root;
`prepare-pages.sh` writes the same physical root copy for static hosting.

## Transport (thin service worker)

The Shiny app runs in an iframe at `/shiny/`. The R worker holds httpuv and
the Emscripten VFS. The SW is only a router:

```
GET /lib/R/**              → VFS read (package htmlwidget / Shiny assets)
GET /shiny/{prefix}/**     → VFS read via shiny::resourcePaths() (runtime
                             dirs such as /tmp sass cache and plot PNGs)
GET /shiny/ , sessions     → R worker HTTP / virtual WebSocket
/runtime/**  /lucent/**    → network (not intercepted)
```

On wasm, `createWebDependency()` sets `href` to `/lib/R/library/...` for files
under `R_HOME` and skips `addResourcePath()`. Paths outside `R_HOME` still use
`/shiny/{name-version}/` and are synced to the SW at warmup, then incrementally
after reactive work.


## Config overrides

```html
<script>
  const siteRoot = new URL("./", window.location.href);
  globalThis.__LUCENT__ = {
    transportBaseUrl: new URL("lucent/dist/", siteRoot).href,
    serviceWorkerUrl: new URL("httpuv-sw.js", siteRoot).href,
    rRuntimeBaseUrl: new URL("runtime/", siteRoot).href,
    empackMetaUrl: new URL("packages/empack_env_meta.json", siteRoot).href,
    empackPackagesBaseUrl: new URL("packages/", siteRoot).href,
    shinyBaseUrl: siteRoot.href,
  };
</script>
```
