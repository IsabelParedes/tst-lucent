import { MSG, REQUEST_TIMEOUT_MS, WARMUP_REQUEST_HEADER } from "./httpuv-constants.js";
import { connectHttpuvComlink } from "./httpuv-comlink-setup.js";
import { enableHttpuvDebug, isHttpuvDebug } from "./httpuv-debug.js";
import { resolveShinyPrefix, setShinyPrefix, shinyAppUrl } from "./httpuv-prefix.js";
import { RWASM } from "./rwasm-constants.js";

const RUN_WEB_APP_R = `shiny::startApp(appDir = "webApp", port = 3838L, host = "127.0.0.1", launch.browser = FALSE, quiet = TRUE)`;

/** @type {Worker | null} */
let rWorker = null;
/** @type {Promise<Worker> | null} */
let rWorkerPromise = null;
/** @type {boolean} */
let comlinkConnected = false;
/** @type {Promise<void> | null} */
let comlinkPromise = null;
/** @type {Promise<ServiceWorkerRegistration | null> | null} */
let swRegistrationPromise = null;
/** @type {Promise<void> | null} */
let httpuvReadyPromise = null;
/** @type {number} */
let evalSeq = 0;

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
 * @param {number} [timeoutMs]
 */
async function waitForWorkerActivated(reg, timeoutMs = 15_000) {
  await navigator.serviceWorker.ready;

  const worker = reg.installing ?? reg.waiting ?? reg.active;
  if (!worker) {
    return;
  }
  if (worker.state === "activated") {
    return;
  }

  await new Promise((resolve, reject) => {
    const onStateChange = () => {
      if (worker.state === "activated") {
        cleanup();
        resolve();
      }
    };

    const deadline = setTimeout(() => {
      cleanup();
      reject(
        new Error(
          `Service worker activation timed out (state: ${worker.state})`,
        ),
      );
    }, timeoutMs);

    const cleanup = () => {
      clearTimeout(deadline);
      worker.removeEventListener("statechange", onStateChange);
    };

    worker.addEventListener("statechange", onStateChange);

    // skipWaiting() may have activated before we subscribed.
    if (worker.state === "activated") {
      cleanup();
      resolve();
    }
  });
}

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
 * @param {Worker} worker
 * @param {object} msg
 * @param {Transferable[]} [transfer]
 * @returns {Promise<object>}
 */
function postToRWorker(worker, msg, transfer = []) {
  const id = msg.id ?? `m${++evalSeq}`;
  const payload = { ...msg, id };

  return new Promise((resolve, reject) => {
    /** @param {MessageEvent} event */
    const onMessage = (event) => {
      const data = event.data;
      if (!data || data.id !== id) {
        if (data?.type === RWASM.LOG) {
          const fn = data.level === "error" ? console.error : console.log;
          fn(`[rWasmWorker] ${data.text}`);
        }
        if (data?.type === RWASM.ERROR && !msg.id) {
          worker.removeEventListener("message", onMessage);
          reject(new Error(data.message ?? "R worker failed"));
        }
        return;
      }

      if (
        data.type === RWASM.EVAL_RESULT ||
        data.type === RWASM.STOPPED ||
        data.type === RWASM.RESOURCE_PATHS
      ) {
        worker.removeEventListener("message", onMessage);
        if (data.type === RWASM.STOPPED || data.type === RWASM.RESOURCE_PATHS || data.ok) {
          resolve(data);
        } else {
          reject(new Error(data.error ?? "eval failed"));
        }
      }
    };

    worker.addEventListener("message", onMessage);
    worker.postMessage(payload, transfer);
  });
}

/**
 * @returns {Promise<Worker>}
 */
function createRWorker() {
  const workerUrl = new URL("./rWasmWorker.js", import.meta.url);
  if (isHttpuvDebug()) {
    workerUrl.searchParams.set("httpuvDebug", "1");
  }
  const worker = new Worker(workerUrl, { type: "module" });

  return new Promise((resolve, reject) => {
    /** @param {MessageEvent} event */
    const onBoot = (event) => {
      const data = event.data;
      if (data?.type === RWASM.LOG) {
        const fn = data.level === "error" ? console.error : console.log;
        fn(`[rWasmWorker] ${data.text}`);
        return;
      }
      if (data?.type === RWASM.READY) {
        worker.removeEventListener("message", onBoot);
        console.info("[runApp] R.wasm worker ready");
        resolve(worker);
        return;
      }
      if (data?.type === RWASM.ERROR) {
        worker.removeEventListener("message", onBoot);
        reject(new Error(data.message ?? "R worker bootstrap failed"));
      }
    };

    worker.addEventListener("message", onBoot);
    worker.addEventListener("error", (event) => {
      worker.removeEventListener("message", onBoot);
      const detail = [event.message, event.filename, event.lineno].filter(Boolean).join(" ");
      reject(new Error(detail ? `R worker failed to load: ${detail}` : "R worker failed to load"));
    });
  });
}

async function ensureRWorker() {
  if (rWorker) {
    return rWorker;
  }
  if (!rWorkerPromise) {
    rWorkerPromise = createRWorker().then((worker) => {
      rWorker = worker;
      return worker;
    });
  }
  return rWorkerPromise;
}

async function ensureComlinkConnected() {
  if (comlinkConnected && comlinkPromise) {
    return comlinkPromise;
  }

  comlinkPromise = (async () => {
    console.info("[runApp] Waiting for R worker and service worker…");
    const [worker] = await Promise.all([ensureRWorker(), ensureHttpuvServiceWorker()]);
    if (!navigator.serviceWorker.controller) {
      throw new Error("Service worker controller is not available");
    }
    console.info("[runApp] Connecting Comlink…");
    await connectHttpuvComlink(worker);
    comlinkConnected = true;
  })();

  return comlinkPromise;
}

/**
 * @returns {Promise<void>}
 */
export async function ensureHttpuvReady() {
  if (!httpuvReadyPromise) {
    setShinyPrefix(resolveShinyPrefix(import.meta.url));
    httpuvReadyPromise = ensureComlinkConnected();
  }
  return httpuvReadyPromise;
}

navigator.serviceWorker.addEventListener("controllerchange", () => {
  // Re-announce the host client; do not reconnect Comlink. MessagePorts between
  // the R worker and service worker survive SW activation, and reconnecting here
  // races with in-flight HTTP (COMLINK_READY timeout).
  announceHostToServiceWorker();
});

globalThis.__shinyForge = {
  shinyUrl: (subpath = "") => shinyAppUrl(subpath),
  ensureHttpuvReady,
  stopRunningApp,
  enableHttpuvDebug,
  async debugHttpuvState(label = "manual") {
    const worker = await ensureRWorker();
    await postToRWorker(worker, {
      type: RWASM.EVAL,
      code: `source("/httpuvDebug.R"); forge_httpuv_debug_state(${JSON.stringify(label)})`,
    });
  },
  async debugHttpuvPump(label = "manual") {
    const worker = await ensureRWorker();
    await postToRWorker(worker, {
      type: RWASM.EVAL,
      code: `source("/httpuvDebug.R"); forge_httpuv_debug_pump(${JSON.stringify(label)})`,
    });
  },
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

function stopRunningApp() {
  navigator.serviceWorker.controller?.postMessage({ type: MSG.STOP });

  if (rWorker) {
    rWorker.postMessage({ type: RWASM.STOP_APP });
  }
  console.info("[runApp] App stopped");
}

function loadViewerFrame() {
  const frame = document.getElementById("app-frame");
  const url = shinyAppUrl();
  if (frame) {
    frame.src = url;
  }
  console.info("[runApp] Viewer iframe →", url);
}

/**
 * First WASM Shiny page render can take tens of seconds; complete one GET
 * through the service worker before pointing the iframe at the app.
 */
function clearAppDocumentCache() {
  navigator.serviceWorker.controller?.postMessage({ type: MSG.CLEAR_APP_CACHE });
}

/**
 * After warmup, copy shiny::resourcePaths() into the SW for direct R_HOME static serving.
 * @param {Worker} worker
 * @returns {Promise<void>}
 */
async function syncResourcePathsToServiceWorker(worker) {
  const controller = navigator.serviceWorker.controller;
  if (!controller) {
    return;
  }

  try {
    const data = await postToRWorker(worker, { type: RWASM.GET_RESOURCE_PATHS });
    const paths = data.paths ?? {};
    controller.postMessage({ type: MSG.REGISTER_RESOURCE_PATHS, paths });
    if (Object.keys(paths).length > 0) {
      console.info("[runApp] synced", Object.keys(paths).length, "resource path(s) to SW");
    }
  } catch (err) {
    console.warn("[runApp] resource path sync failed; SW will use static fallbacks", err);
  }
}

async function waitForShinyHttpReady(worker) {
  const url = shinyAppUrl();
  console.info("[runApp] Warming up Shiny (may take a minute on first load)…", url);
  const res = await fetch(url, {
    cache: "no-store",
    headers: { [WARMUP_REQUEST_HEADER]: "1" },
    signal: AbortSignal.timeout(REQUEST_TIMEOUT_MS),
  });
  if (!res.ok) {
    throw new Error(`Shiny warmup GET ${url} failed: HTTP ${res.status}`);
  }
  console.info("[runApp] Shiny warmup OK (HTTP", res.status + ")");
  await syncResourcePathsToServiceWorker(worker);
}

export async function runApp(code) {
  const trimmed = code.trim();
  if (!trimmed) {
    console.warn("[runApp] No R code to run");
    return 1;
  }

  const worker = await ensureRWorker();

  clearAppDocumentCache();
  await postToRWorker(worker, { type: RWASM.STOP_APP });

  await postToRWorker(worker, {
    type: RWASM.WRITE_WEB_APP,
    source: trimmed,
  });

  console.info("[runApp] worker eval", RUN_WEB_APP_R);
  await postToRWorker(worker, {
    type: RWASM.EVAL,
    code: RUN_WEB_APP_R,
  });

  if (isHttpuvDebug()) {
    await postToRWorker(worker, {
      type: RWASM.EVAL,
      code: `tryCatch({
  source("/httpuvDebug.R")
  forge_httpuv_install_traces()
  forge_httpuv_debug_state("post-start")
}, error = function(e) message("[httpuv-debug] setup failed: ", conditionMessage(e)))`,
    });
  }

  return 0;
}

function getEditorCode() {
  return document.getElementById("app-code")?.value ?? "";
}

/**
 * @param {{ stopFirst?: boolean }} [options]
 */
async function runEditorApp(options = {}) {
  await ensureHttpuvReady();
  const worker = await ensureRWorker();
  if (options.stopFirst) {
    stopRunningApp();
  }
  await runApp(getEditorCode());
  await waitForShinyHttpReady(worker);
  loadViewerFrame();
}

document.getElementById("run-button")?.addEventListener("click", async () => {
  const button = document.getElementById("run-button");
  button.disabled = true;
  try {
    await runEditorApp({ stopFirst: true });
  } catch (err) {
    console.error("[runApp] Failed:", err);
  } finally {
    button.disabled = false;
  }
});

if (isHttpuvDebug()) {
  console.info("[runApp] httpuv debug tracing enabled (?httpuvDebug=1)");
}

void runEditorApp().catch((err) => {
  console.error("[runApp] Failed to start:", err);
});

// Register the service worker while R.wasm boots (do not block on the worker).
void ensureHttpuvServiceWorker().catch((err) => {
  console.error("[httpuv] Service worker setup failed:", err);
});
