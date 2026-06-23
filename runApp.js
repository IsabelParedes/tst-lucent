import { MSG } from "./httpuv-constants.js";
import { connectHttpuvComlink } from "./httpuv-comlink-setup.js";
import { resolveShinyPrefix, setShinyPrefix, shinyAppUrl } from "./httpuv-prefix.js";
import { RWASM } from "./rwasm-constants.js";

const RUN_WEB_APP_R = `shiny_forge_start_app("webApp")`;

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

      if (data.type === RWASM.EVAL_RESULT) {
        worker.removeEventListener("message", onMessage);
        if (data.ok) {
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
  const worker = new Worker(new URL("./rWasmWorker.js", import.meta.url), { type: "module" });

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
    const worker = await ensureRWorker();
    await ensureHttpuvServiceWorker();
    if (!navigator.serviceWorker.controller) {
      throw new Error("Service worker controller is not available");
    }
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
  announceHostToServiceWorker();
  if (!comlinkConnected) {
    return;
  }
  comlinkConnected = false;
  comlinkPromise = null;
  void ensureComlinkConnected().catch((err) => {
    console.error("[httpuv] Comlink reconnect failed:", err);
  });
});

globalThis.__shinyForge = {
  shinyUrl: (subpath = "") => shinyAppUrl(subpath),
  ensureHttpuvReady,
  stopRunningApp,
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

function stopWasmPump(worker) {
  worker.postMessage({ type: RWASM.PUMP_STOP });
}

function startWasmPump(worker) {
  worker.postMessage({ type: RWASM.PUMP_START });
  console.info("[runApp] WASM event pump started (worker)");
}

export function stopRunningApp() {
  navigator.serviceWorker.controller?.postMessage({ type: MSG.STOP });

  if (rWorker) {
    stopWasmPump(rWorker);
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

export async function runApp(code) {
  const trimmed = code.trim();
  if (!trimmed) {
    console.warn("[runApp] No R code to run");
    return 1;
  }

  const worker = await ensureRWorker();

  worker.postMessage({ type: RWASM.STOP_APP });

  await postToRWorker(worker, {
    type: RWASM.WRITE_WEB_APP,
    source: trimmed,
  });

  console.info("[runApp] worker eval", RUN_WEB_APP_R);
  await postToRWorker(worker, {
    type: RWASM.EVAL,
    code: RUN_WEB_APP_R,
  });
  startWasmPump(worker);

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
  if (options.stopFirst) {
    stopRunningApp();
  }
  await runApp(getEditorCode());
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

void runEditorApp().catch((err) => {
  console.error("[runApp] Failed to start:", err);
});
