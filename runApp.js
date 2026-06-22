import { MSG } from "./httpuv-constants.js";
import { installHttpuvBridge } from "./httpuv-bridge.js";
import { resolveShinyPrefix, setShinyPrefix, shinyAppUrl } from "./httpuv-prefix.js";

const WASM_R_HOME = "/R_HOME";

// Build-time-only files; set LD_LIBRARY_PATH via ENV instead of mounting ldpaths.
const VFS_SKIP = new Set([`${WASM_R_HOME}/etc/ldpaths`, `${WASM_R_HOME}/etc/Makeconf`]);

const glueUrl = new URL("R", import.meta.url);
const wasmUrl = new URL("R.wasm", import.meta.url);
const rHomeUrl = new URL("R_HOME/", import.meta.url);
const rLibUrl = new URL("R_HOME/lib/", import.meta.url);
const manifestUrl = new URL("R_HOME-manifest.json", import.meta.url);

/** @type {Map<string, Uint8Array>} */
const fileCache = new Map();
/** @type {Promise<any> | null} */
let rModulePromise = null;
/** @type {Promise<ServiceWorkerRegistration | null> | null} */
let swRegistrationPromise = null;
/** @type {Promise<void> | null} */
let httpuvReadyPromise = null;

const HTTPUV_SW_RELOAD_KEY = "httpuv-sw-reload";

function announceHostToServiceWorker() {
  const prefix = resolveShinyPrefix(import.meta.url);
  const msg = { type: MSG.REGISTER_HOST, shinyPrefix: prefix };
  const controller = navigator.serviceWorker.controller;
  if (controller) {
    controller.postMessage(msg);
    console.info("[httpuv] Announced host to service worker");
    return true;
  }
  return false;
}

/**
 * Wait until navigator.serviceWorker.controller is set (optional, non-fatal).
 * @param {number} timeoutMs
 */
async function waitForServiceWorkerController(timeoutMs = 3_000) {
  if (navigator.serviceWorker.controller) {
    return navigator.serviceWorker.controller;
  }

  await navigator.serviceWorker.ready;
  if (navigator.serviceWorker.controller) {
    return navigator.serviceWorker.controller;
  }

  return new Promise((resolve, reject) => {
    /** @type {ReturnType<typeof setInterval> | undefined} */
    let poll;

    const deadline = setTimeout(() => {
      if (poll) clearInterval(poll);
      reject(new Error("timeout"));
    }, timeoutMs);

    const onController = () => {
      if (navigator.serviceWorker.controller) {
        clearTimeout(deadline);
        if (poll) clearInterval(poll);
        navigator.serviceWorker.removeEventListener("controllerchange", onController);
        resolve(navigator.serviceWorker.controller);
      }
    };

    navigator.serviceWorker.addEventListener("controllerchange", onController);
    poll = setInterval(onController, 100);
  });
}

/**
 * @param {ServiceWorkerRegistration} reg
 */
async function waitForWorkerActivated(reg) {
  const worker = reg.installing ?? reg.waiting ?? reg.active;
  if (!worker) {
    await navigator.serviceWorker.ready;
    return;
  }
  if (worker.state === "activated") {
    return;
  }
  await new Promise((resolve) => {
    worker.addEventListener("statechange", () => {
      if (worker.state === "activated") {
        resolve();
      }
    });
  });
}

/**
 * Register the httpuv service worker and announce this page as the WASM host.
 * @returns {Promise<ServiceWorkerRegistration | null>}
 */
async function registerHttpuvServiceWorker() {
  if (!("serviceWorker" in navigator)) {
    console.warn("[httpuv] Service workers are not supported in this browser");
    return null;
  }

  try {
    const reg = await navigator.serviceWorker.register(new URL("./httpuv-sw.js", import.meta.url), {
      type: "module",
      updateViaCache: "none",
    });
    await waitForWorkerActivated(reg);

    if (!navigator.serviceWorker.controller) {
      const reloaded = sessionStorage.getItem(HTTPUV_SW_RELOAD_KEY);
      if (!reloaded) {
        sessionStorage.setItem(HTTPUV_SW_RELOAD_KEY, "1");
        console.info("[httpuv] Service worker installed — reloading once to activate");
        window.location.reload();
        await new Promise(() => {});
      }
      console.warn(
        "[httpuv] Page still not controlled after reload; check Application → Service Workers for httpuv-sw.js errors",
      );
    } else {
      sessionStorage.removeItem(HTTPUV_SW_RELOAD_KEY);
    }

    await waitForServiceWorkerController().catch(() => undefined);
    announceHostToServiceWorker();
    console.info("[httpuv] Service worker registered", {
      scope: reg.scope,
      shinyPrefix: resolveShinyPrefix(import.meta.url),
      controller: Boolean(navigator.serviceWorker.controller),
    });
    return reg;
  } catch (err) {
    console.error("[httpuv] Service worker registration failed:", err);
    throw err;
  }
}

function ensureHttpuvServiceWorker() {
  if (!swRegistrationPromise) {
    swRegistrationPromise = registerHttpuvServiceWorker();
  }
  return swRegistrationPromise;
}

/**
 * Wait until the httpuv bridge and service worker are ready.
 * @returns {Promise<void>}
 */
export async function ensureHttpuvReady() {
  if (!httpuvReadyPromise) {
    setShinyPrefix(resolveShinyPrefix(import.meta.url));
    installHttpuvBridge();
    httpuvReadyPromise = ensureHttpuvServiceWorker().then(() => undefined);
  }
  return httpuvReadyPromise;
}

void ensureHttpuvReady().catch((err) => {
  console.error("[httpuv] Failed to initialize:", err);
});

navigator.serviceWorker.addEventListener("controllerchange", () => {
  announceHostToServiceWorker();
});

globalThis.__shinyForge = {
  shinyUrl: (subpath = "") => shinyAppUrl(subpath),
  ensureHttpuvReady,
  /** Open a virtual socket and send one message (tests session fetch API). */
  async testVirtualSocket(message = '{"method":"ping"}') {
    await ensureHttpuvReady();
    if (!navigator.serviceWorker.controller) {
      console.warn(
        "[shiny-forge] No service worker controller — fetch may not be intercepted; unregister old workers and hard-refresh",
      );
    }

    const openUrl = new URL("__session__/open", shinyAppUrl());
    console.info("[shiny-forge] testVirtualSocket: open", openUrl.href);
    const openRes = await fetch(openUrl, { method: "POST" });
    if (!openRes.ok) {
      throw new Error(`session open failed: HTTP ${openRes.status} ${await openRes.text()}`);
    }
    const { handle } = await openRes.json();
    console.info("[shiny-forge] testVirtualSocket: handle", handle);

    const recvUrl = new URL(`__session__/recv?handle=${encodeURIComponent(handle)}`, shinyAppUrl());
    const sendUrl = new URL(`__session__/send?handle=${encodeURIComponent(handle)}`, shinyAppUrl());
    const recvPromise = fetch(recvUrl);
    const sendRes = await fetch(sendUrl, {
      method: "POST",
      headers: { "Content-Type": "text/plain" },
      body: message,
    });
    if (!sendRes.ok && sendRes.status !== 204) {
      throw new Error(`session send failed: HTTP ${sendRes.status}`);
    }

    const recvRes = await recvPromise;
    const body = await recvRes.text();
    const result = { handle, status: recvRes.status, body };
    console.info("[shiny-forge] testVirtualSocket: result", result);
    return result;
  },
};

function rEnv() {
  return {
    R_HOME: WASM_R_HOME,
    R_DEFAULT_PACKAGES: "NULL",
    R_LIBS: `${WASM_R_HOME}/library`,
    R_LIBS_USER: "NULL",
    R_LIBS_SITE: "NULL",
    LD_LIBRARY_PATH: `${WASM_R_HOME}/lib:/lib`,
  };
}

function dstToFetchUrl(dst) {
  const prefix = `${WASM_R_HOME}/`;
  if (dst.startsWith(prefix)) {
    return new URL(dst.slice(prefix.length), rHomeUrl).href;
  }
  if (dst.startsWith("/lib/")) {
    return new URL(dst.slice(5), rLibUrl).href;
  }
  throw new Error(`Unknown mount path: ${dst}`);
}

async function mountRHome() {
  if (fileCache.size > 0) {
    return;
  }

  const res = await fetch(manifestUrl);
  if (!res.ok) {
    throw new Error(`Failed to fetch ${manifestUrl.href}: HTTP ${res.status}`);
  }
  const { files } = await res.json();
  console.info("[runApp] Mounting", files.length, "files from manifest");

  let next = 0;
  await Promise.all(
    Array.from({ length: 32 }, async () => {
      while (next < files.length) {
        const dst = files[next++];
        if (VFS_SKIP.has(dst)) {
          continue;
        }
        const fileRes = await fetch(dstToFetchUrl(dst));
        if (!fileRes.ok) {
          throw new Error(`Failed to fetch ${dstToFetchUrl(dst)}: HTTP ${fileRes.status}`);
        }
        fileCache.set(dst, new Uint8Array(await fileRes.arrayBuffer()));
      }
    })
  );

  console.info("[runApp] Cached", fileCache.size, "files");
}

function writeCachedTree(module) {
  for (const path of fileCache.keys()) {
    const parent = path.substring(0, path.lastIndexOf("/"));
    if (parent) {
      module.FS.mkdirTree(parent);
    }
  }
  for (const [path, data] of fileCache) {
    module.FS.writeFile(path, data);
  }
}

function verifyMountedTree(module) {
  const methodsSo = `${WASM_R_HOME}/library/methods/libs/methods.so`;
  const info = module.FS.analyzePath(methodsSo);
  if (!info.exists) {
    throw new Error(`Mounted FS is missing ${methodsSo}`);
  }
  const data = module.FS.readFile(methodsSo, { encoding: "binary" });
  if (!(data[0] === 0 && data[1] === 97 && data[2] === 115 && data[3] === 109)) {
    throw new Error(`${methodsSo} is not a wasm module (bad magic bytes)`);
  }
}

function locateFile(file) {
  const base = file.split("/").pop();
  if (base.endsWith(".wasm")) {
    return new URL("R.wasm", import.meta.url).href;
  }
  // Package dynlib path, e.g. /R_HOME/library/methods/libs/methods.so
  const pkgMatch = file.match(/\/library\/([^/]+)\/libs\/([^/]+)$/);
  if (pkgMatch) {
    return new URL(`R_HOME/library/${pkgMatch[1]}/libs/${pkgMatch[2]}`, import.meta.url).href;
  }
  // Core R shared libs (libR.so, libRblas.so, …)
  return new URL(`R_HOME/lib/${base}`, import.meta.url).href;
}

async function loadGlue() {
  let glue = await fetch(glueUrl).then((res) => {
    if (!res.ok) {
      throw new Error(`Failed to fetch ${glueUrl.href}: HTTP ${res.status}`);
    }
    return res.text();
  });

  const env = rEnv();
  const envLiteral = JSON.stringify(env);
  glue = glue.replace("var ENV={};", `var ENV=${envLiteral};`);
  glue = glue.replace(
    "var Module=typeof Module!=\"undefined\"?Module:{};",
    "var Module=globalThis.Module;"
  );
  glue = glue.replace(
    'env={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};',
    `env={R_HOME:"${env.R_HOME}",R_LIBS:"${env.R_LIBS}",R_LIBS_USER:"${env.R_LIBS_USER}",R_LIBS_SITE:"${env.R_LIBS_SITE}",LD_LIBRARY_PATH:"${env.LD_LIBRARY_PATH}",USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:lang,_:getExecutableName()};`
  );
  glue = glue.replace(
    'Module["callMain"]=callMain;',
    `Module["loadDynamicLibraryAsync"]=(name)=>loadDynamicLibrary(name,{loadAsync:true,global:true,nodelete:true,allowUndefined:true});
Module["callMain"]=callMain;`
  );
  return glue;
}

async function preloadWasmSideModules(module) {
  const libR = `${WASM_R_HOME}/lib/libR.so`;
  const paths = [...fileCache.keys()].filter((p) => p.endsWith(".so"));
  const ordered = [
    ...paths.filter((p) => p === libR),
    ...paths.filter((p) => p !== libR).sort(),
  ];
  console.info("[runApp] Preloading", ordered.length, "WASM side modules");
  for (const path of ordered) {
    await module.loadDynamicLibraryAsync(path);
  }
}

async function initRModule() {
  await mountRHome();

  const [glue, wasmBinary] = await Promise.all([
    loadGlue(),
    fetch(wasmUrl).then((res) => {
      if (!res.ok) {
        throw new Error(`Failed to fetch ${wasmUrl.href}: HTTP ${res.status}`);
      }
      return res.arrayBuffer();
    }),
  ]);

  return new Promise((resolve, reject) => {
    globalThis.Module = {
      noInitialRun: true,
      wasmBinary,
      locateFile,
      ENV: rEnv(),
      httpuv: globalThis.Module?.httpuv,
      preRun: [() => writeCachedTree(globalThis.Module)],
      onRuntimeInitialized() {
        // Re-mount after FS.init() (runs in initRuntime between preRun and here).
        writeCachedTree(globalThis.Module);
        try {
          verifyMountedTree(globalThis.Module);
        } catch (err) {
          reject(err);
          return;
        }
        // dlopen() sync-compiles side modules; libR.so is >8MB and fails on the
        // main thread unless we async-instantiate everything first.
        preloadWasmSideModules(globalThis.Module)
          .then(() => resolve(globalThis.Module))
          .catch(reject);
      },
      onAbort(reason) {
        reject(new Error(`R.wasm aborted: ${reason}`));
      },
      print(text) {
        console.log(String(text));
      },
      printErr(text) {
        console.error(String(text));
      },
    };

    try {
      globalThis.eval(glue);
    } catch (err) {
      reject(err);
    }
  });
}

async function ensureRModule() {
  if (!rModulePromise) {
    rModulePromise = initRModule();
  }
  return rModulePromise;
}

export async function runApp(code) {
  const trimmed = code.trim();
  if (!trimmed) {
    console.warn("[runApp] No R code to run");
    return;
  }

  const Module = await ensureRModule();
  // Avoid --vanilla so /R_HOME/etc/Rprofile.site runs before default packages
  // (needed to bind methods.so native symbols such as C_R_initMethodDispatch).
  const rArgs = ["--no-restore", "--no-save", "-e", trimmed];
  console.info("[runApp] callMain", rArgs.slice(0, 3).concat(["-e", code]));
  const status = Module.callMain(rArgs);
  console.info("[runApp] callMain finished with status", status);
  return status;
}

document.getElementById("run-button")?.addEventListener("click", async () => {
  const button = document.getElementById("run-button");
  const code = document.getElementById("app-code")?.value ?? "";

  button.disabled = true;
  try {
    await runApp(code);
  } catch (err) {
    console.error("[runApp] Failed:", err);
  } finally {
    button.disabled = false;
  }
});
