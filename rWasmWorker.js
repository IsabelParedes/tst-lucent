import { flushDeferredOutbound, installHttpuvBridge, pushInboundHostMessage } from "./httpuv-bridge.js";
import { httpuvDebugLog, isHttpuvDebug } from "./httpuv-debug.js";
import { MSG, REQUEST_TIMEOUT_MS } from "./httpuv-constants.js";
import { COMLINK } from "./httpuv-comlink.js";
import { Comlink, createRHostApi } from "./httpuv-comlink-setup.js";
import { resolveShinyPrefix, setShinyPrefix } from "./httpuv-prefix.js";
import { channelMessageToRExpr, isLikelyStaticAsset } from "./httpuv-r-push.js";
import { RWASM } from "./rwasm-constants.js";
import { evalR, initRModule, setEvalRPostFlush, writeWebAppToVfs } from "./rWasmBootstrap.js";

/** @typedef {{ work: () => void, resolve: () => void, reject: (err: unknown) => void }} RTask */

/** @type {RTask[]} */
const rTaskQueue = [];

/** @type {boolean} */
let rDrainScheduled = false;

/** @type {boolean} */
let rLocked = false;

const WASM_STOP_EXPR = `tryCatch({
  if (requireNamespace("shiny", quietly=TRUE) && shiny::isRunning()) {
    shiny::stopApp()
  }
}, error=function(e) NULL)`;

// Suspend Shiny's serviceNonBlocking loop without patching the installed package:
// bump serviceGeneration so in-flight serviceLoop callbacks exit (see
// shiny/tests/testthat/test-non-blocking.R). Resume via serviceNonBlocking().
const WASM_SUSPEND_SHINY_LOOP = `tryCatch({
  if (requireNamespace("shiny", quietly=TRUE) && shiny::isRunning()) {
    shiny:::.globals$serviceGeneration <- shiny:::.globals$serviceGeneration + 1L
  }
}, error=function(e) NULL)`;

const WASM_RESUME_SHINY_LOOP = `tryCatch({
  if (requireNamespace("shiny", quietly=TRUE) && shiny::isRunning()) {
    h <- shiny:::.globals$runningHandle
    if (!is.null(h)) {
      shiny:::serviceNonBlocking(h, shiny:::.globals$serviceGeneration)
    }
  }
}, error=function(e) NULL)`;

/** Background pump when the R task queue is idle. */
const WASM_SINGLE_SERVICE_TICK = `tryCatch({
  has_srv <- requireNamespace("shiny", quietly=TRUE) &&
    !is.null(shiny::getShinyOption("server", default=NULL))
  if (has_srv) {
    shiny::serviceApp(NA)
  } else if (!later::loop_empty()) {
    later::run_now(0, all=FALSE)
  }
}, error=function(e) NULL)`;

/** Host-controlled service tick while Shiny's serviceLoop is suspended. */
const WASM_HTTP_DRAIN_TICK = WASM_SINGLE_SERVICE_TICK;

/** Service rounds after push idle wait (promise resolution only). */
const HTTP_PUSH_DRAIN_ROUNDS = 64;

/** Poll interval while waiting for emscripten later timers (no evalR). */
const HTTP_IDLE_POLL_MS = 16;

/** Max time to wait for timer-fired handler before evalR drain. */
const HTTP_IDLE_MAX_MS = REQUEST_TIMEOUT_MS;

/** @type {boolean} */
let httpDeliveryActive = false;

/** @type {boolean} */
let httpDeliveryDraining = false;

/** @type {boolean} */
let shinyLoopSuspended = false;

/** @type {Array<{ req: object, resolve: () => void, reject: (err: unknown) => void }>} */
const httpDeliveryQueue = [];

/** @type {string | null} */
let activeHttpDrainUuid = null;

/** @type {Map<string, { resolved: boolean }>} */
const httpDrainByUuid = new Map();

/** @type {number} */
let httpDeliveryInflight = 0;

const R_LATER_PUMP_MS = 16;

/** @type {boolean} */
let rLaterPumpActive = false;

/** @type {object | null} */
let rModule = null;
/** @type {import('comlink').Remote<{ deliverHttpResponse: Function, deliverWsPush: Function }> | null} */
let swDelivery = null;
/** @type {boolean} */
let rHostPortReady = false;
/** @type {boolean} */
let swDeliveryPortReady = false;

function postToHost(payload, transfer = []) {
  self.postMessage(payload, transfer);
}

function log(level, text) {
  const msg = String(text);
  if (level === "error" && msg.startsWith("Error")) {
    postToHost({ type: RWASM.LOG, level: "error", text: msg });
    return;
  }
  postToHost({ type: RWASM.LOG, level: "log", text: msg });
}

function maybeAnnounceComlinkReady() {
  if (rHostPortReady && swDeliveryPortReady) {
    postToHost({ type: RWASM.COMLINK_READY });
  }
}

function deliverToServiceWorker(outbound, transfer = []) {
  if (!swDelivery) {
    console.warn("[rWasmWorker] Service worker delivery API not connected");
    return;
  }

  if (outbound.type === MSG.HTTP_RESPONSE) {
    httpuvDebugLog("comlink-deliver-http", {
      uuid: outbound.uuid,
      status: outbound.status,
    });
    const drainState = httpDrainByUuid.get(outbound.uuid);
    if (drainState) {
      drainState.resolved = true;
    }
    const resp = {
      uuid: outbound.uuid,
      status: outbound.status,
      headers: outbound.headers,
      body: outbound.body,
    };
    void swDelivery
      .deliverHttpResponse(
        transfer.length > 0 ? Comlink.transfer(resp, transfer) : resp,
      )
      .catch((err) => {
        console.error("[rWasmWorker] deliverHttpResponse failed:", formatRWasmError(err), err);
      });
    return;
  }

  if (outbound.type === MSG.WS_PUSH) {
    const msg = {
      handle: outbound.handle,
      binary: outbound.binary,
      wsType: outbound.wsType,
      message: outbound.message,
    };
    void swDelivery.deliverWsPush(
      transfer.length > 0 ? Comlink.transfer(msg, transfer) : msg,
    );
  }
}

function formatRWasmError(err) {
  if (err instanceof Error) {
    return err.message || err.name;
  }
  if (typeof WebAssembly !== "undefined" && typeof WebAssembly.Exception !== "undefined" && err instanceof WebAssembly.Exception) {
    if (typeof err.message === "string" && err.message) {
      return err.message;
    }
    return "WebAssembly.Exception (likely WASM trap during R eval)";
  }
  if (typeof err === "object" && err !== null) {
    if (typeof err.message === "string" && err.message) {
      return err.message;
    }
    if (typeof err.toString === "function") {
      const text = String(err);
      if (text !== "[object Object]" && text !== "[object WebAssembly.Exception]") {
        return text;
      }
    }
  }
  return String(err);
}

function scheduleRDrain() {
  if (rDrainScheduled) {
    return;
  }
  rDrainScheduled = true;
  setTimeout(drainRTaskQueue, 0);
}

function drainRTaskQueue() {
  rDrainScheduled = false;
  if (!rModule || rTaskQueue.length === 0) {
    return;
  }

  const task = rTaskQueue.shift();
  rLocked = true;
  try {
    task.work();
    task.resolve();
  } catch (err) {
    task.reject(err);
  } finally {
    rLocked = false;
    flushDeferredOutbound();
  }

  if (rTaskQueue.length > 0) {
    scheduleRDrain();
  }
}

/**
 * Queue work that calls evalR. Tasks run one at a time on setTimeout(0) turns
 * so later's emscripten timers can run while the worker is idle.
 * @param {() => void} work
 * @returns {Promise<void>}
 */
function enqueueRTask(work) {
  return new Promise((resolve, reject) => {
    rTaskQueue.push({ work, resolve, reject });
    scheduleRDrain();
  });
}

function pushToR(msg) {
  if (msg?.uuid) {
    httpuvDebugLog("worker-push-evalR-begin", { uuid: msg.uuid });
  }
  evalR(
    rModule,
    `tryCatch({
  ${channelMessageToRExpr(msg)}
}, error=function(e) {
  message("[httpuv] push failed: ", conditionMessage(e))
  stop(conditionMessage(e))
})`,
  );
  if (msg?.uuid) {
    httpuvDebugLog("worker-push-evalR-finish", { uuid: msg.uuid });
  }
}

/** Yield between drain rounds so emscripten later timers can fire. */
const HTTP_DRAIN_YIELD_MS = 4;

/** Yield for a fixed duration. */
function yieldMs(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

/**
 * Wait for the HTTP response with no evalR — emscripten later timers need a
 * quiet worker. Background pump is paused via httpDeliveryActive.
 * @param {{ resolved: boolean }} state
 * @param {string} uuid
 * @param {number} [maxMs]
 */
async function idleWaitForHttpResponse(state, uuid, maxMs = HTTP_IDLE_MAX_MS) {
  const start = Date.now();
  while (!state.resolved && Date.now() - start < maxMs) {
    await yieldMs(HTTP_IDLE_POLL_MS);
  }
  httpuvDebugLog("worker-push-idle-done", {
    uuid,
    resolved: state.resolved,
    waitedMs: Date.now() - start,
  });
}

/**
 * @param {string} url
 * @returns {boolean}
 */
function isStaticAssetUrl(url) {
  return isLikelyStaticAsset(url);
}

async function ensureShinyLoopSuspended() {
  if (shinyLoopSuspended || !rModule) {
    return;
  }
  await enqueueRTask(() => {
    evalR(rModule, WASM_SUSPEND_SHINY_LOOP);
  });
  shinyLoopSuspended = true;
  httpuvDebugLog("worker-shiny-loop-suspend", {});
}

async function ensureShinyLoopResumed() {
  if (!shinyLoopSuspended || !rModule) {
    return;
  }
  await enqueueRTask(() => {
    evalR(rModule, WASM_RESUME_SHINY_LOOP);
  });
  shinyLoopSuspended = false;
  httpuvDebugLog("worker-shiny-loop-resume", {});
}

async function maybeFinishHttpDeliveryBatch() {
  if (httpDeliveryQueue.length > 0 || httpDeliveryInflight > 0) {
    return;
  }
  await ensureShinyLoopResumed();
  httpDeliveryActive = false;
  httpDeliveryDraining = false;
}

/**
 * @param {object} req
 */
async function deliverOneHttpRequest(req) {
  const state = { resolved: false };
  httpDrainByUuid.set(req.uuid, state);
  httpDeliveryInflight++;
  activeHttpDrainUuid = req.uuid;

  const staticAsset = isStaticAssetUrl(req.url);

  try {
    await enqueueRTask(() => {
      pushInboundHostMessage(req);
    });

    if (!staticAsset) {
      await idleWaitForHttpResponse(state, req.uuid, HTTP_IDLE_MAX_MS);

      if (!state.resolved) {
        await drainAfterHttpPush(HTTP_PUSH_DRAIN_ROUNDS, req.uuid, state);
      }
    } else if (!state.resolved) {
      await yieldMs(HTTP_DRAIN_YIELD_MS);
    }
  } finally {
    httpDrainByUuid.delete(req.uuid);
    if (activeHttpDrainUuid === req.uuid) {
      activeHttpDrainUuid = null;
    }
    httpDeliveryInflight--;
  }
}

async function drainHttpDeliveryQueue() {
  while (httpDeliveryQueue.length > 0) {
    const item = httpDeliveryQueue.shift();
    if (!item) {
      break;
    }
    const needsSuspend = !isStaticAssetUrl(item.req.url);
    if (needsSuspend && !shinyLoopSuspended) {
      await ensureShinyLoopSuspended();
    }
    if (needsSuspend) {
      httpDeliveryActive = true;
    }
    try {
      await deliverOneHttpRequest(item.req);
      item.resolve();
    } catch (err) {
      item.reject(err);
    } finally {
      if (needsSuspend) {
        httpDeliveryActive = false;
      }
    }
    const next = httpDeliveryQueue[0];
    const nextNeedsSuspend = Boolean(next && !isStaticAssetUrl(next.req.url));
    if (shinyLoopSuspended && !nextNeedsSuspend) {
      await ensureShinyLoopResumed();
    }
  }

  await maybeFinishHttpDeliveryBatch();

  if (httpDeliveryQueue.length > 0 && !httpDeliveryDraining) {
    httpDeliveryDraining = true;
    void drainHttpDeliveryQueue();
  }
}

/**
 * @param {number} roundsLeft
 * @param {string} uuid
 * @param {{ resolved: boolean }} state
 * @returns {Promise<void>}
 */
function drainAfterHttpPush(roundsLeft, uuid, state) {
  if (roundsLeft <= 0 || !rModule || state.resolved) {
    if (!state.resolved) {
      httpuvDebugLog("worker-push-drain-exhausted", { uuid, roundsLeft });
    }
    return Promise.resolve();
  }
  return new Promise((resolve) => {
    setTimeout(() => {
      void enqueueRTask(() => {
        evalR(rModule, WASM_HTTP_DRAIN_TICK);
      })
        .then(() => drainAfterHttpPush(roundsLeft - 1, uuid, state))
        .then(resolve);
    }, HTTP_DRAIN_YIELD_MS);
  });
}

function scheduleRLaterPump() {
  if (rLaterPumpActive) {
    return;
  }
  rLaterPumpActive = true;
  setTimeout(() => {
    rLaterPumpActive = false;
    if (!rModule) {
      return;
    }
    if (!rLocked && rTaskQueue.length === 0 && !httpDeliveryActive) {
      void enqueueRTask(() => {
        evalR(rModule, WASM_SINGLE_SERVICE_TICK);
      }).catch((err) => {
        console.warn("[rWasmWorker] later pump failed:", formatRWasmError(err));
      });
    }
    scheduleRLaterPump();
  }, R_LATER_PUMP_MS);
}

function ensureRLaterPump() {
  scheduleRLaterPump();
  httpuvDebugLog("later-pump", { intervalMs: R_LATER_PUMP_MS });
}

function logHttpDeliveryError(err) {
  console.error("[rWasmWorker] deliverHttpRequest failed:", formatRWasmError(err), err);
}

/**
 * @param {string} uuid
 */
function scheduleHttpuvDebugProbes(uuid) {
  if (!isHttpuvDebug() || !rModule) {
    return;
  }

  for (const ms of [10, 50, 200, 1000, 5000]) {
    setTimeout(() => {
      if (!rModule) {
        return;
      }
      void enqueueRTask(() => {
        evalR(
          rModule,
          `if (file.exists("/httpuvDebug.R")) {
            source("/httpuvDebug.R")
            forge_httpuv_debug_state("${ms}ms post-push ${uuid}")
          }`,
        );
      }).catch((err) => {
        httpuvDebugLog("probe-failed", { ms, uuid, err });
      });
    }, ms);
  }
}

/**
 * Queue one HTTP request. Requests run strictly one-at-a-time; Shiny's
 * service loop is suspended once for the whole batch.
 * @param {object} req
 * @returns {Promise<void>}
 */
function enqueueHttpDelivery(req) {
  httpuvDebugLog("worker-push", {
    uuid: req.uuid,
    method: req.method,
    url: req.url,
  });

  return new Promise((resolve, reject) => {
    httpDeliveryQueue.push({ req, resolve, reject });
    if (!httpDeliveryDraining) {
      httpDeliveryDraining = true;
      void drainHttpDeliveryQueue().catch((err) => {
        httpDeliveryDraining = false;
        httpDeliveryActive = false;
        logHttpDeliveryError(err);
      });
    }
  });
}

function installBridge() {
  setShinyPrefix(resolveShinyPrefix(import.meta.url));
  setEvalRPostFlush(flushDeferredOutbound);
  installHttpuvBridge({
    installSwListener: false,
    postOutbound: deliverToServiceWorker,
    pushToR: (msg) => {
      pushToR(msg);
    },
  });
}

async function ensureRModule() {
  if (rModule) {
    return rModule;
  }

  installBridge();
  rModule = await initRModule({
    moduleUrl: import.meta.url,
    httpuv: globalThis.Module?.httpuv,
    print: (text) => log("log", text),
    printErr: (text) => log("error", text),
  });
  ensureRLaterPump();
  return rModule;
}

function readShinyResourcePathsFromR() {
  evalR(
    rModule,
    `tryCatch({
  paths <- if (requireNamespace("shiny", quietly=TRUE)) as.list(shiny::resourcePaths()) else list()
  jsonlite::write_json(paths, "/resourcePaths.json", auto_unbox=TRUE)
}, error=function(e) {
  jsonlite::write_json(list(), "/resourcePaths.json", auto_unbox=TRUE)
})`,
  );
  const text = rModule.FS.readFile("/resourcePaths.json", { encoding: "utf8" });
  try {
    rModule.FS.unlink("/resourcePaths.json");
  } catch {
    // ignore
  }
  const parsed = JSON.parse(text);
  return parsed && typeof parsed === "object" && !Array.isArray(parsed) ? parsed : {};
}

function getShinyResourcePaths() {
  return ensureRModule().then(() =>
    enqueueRTask(() => readShinyResourcePathsFromR()),
  );
}

function exposeRHost(port) {
  const api = createRHostApi(
    (req) =>
      ensureRModule()
        .then(() => enqueueHttpDelivery(req))
        .catch((err) => {
          logHttpDeliveryError(err);
          throw err;
        }),
    () => {
      void ensureRModule()
        .then(() => {
          void enqueueRTask(() => {
            pushInboundHostMessage({ type: MSG.STOP });
            evalR(rModule, WASM_STOP_EXPR);
          });
        })
        .catch((err) => {
          log("error", `[rWasmWorker] stop failed: ${formatRWasmError(err)}`);
        });
    },
    () => getShinyResourcePaths(),
  );
  Comlink.expose(api, port);
  rHostPortReady = true;
  console.info("[rWasmWorker] Comlink: exposing R host API");
  maybeAnnounceComlinkReady();
}

function connectSwDelivery(port) {
  swDelivery = Comlink.wrap(port);
  swDeliveryPortReady = true;
  console.info("[rWasmWorker] Comlink: connected to SW delivery API");
  maybeAnnounceComlinkReady();
}

function stopRunningApp() {
  if (!rModule) {
    return Promise.resolve();
  }
  return enqueueRTask(() => {
    evalR(rModule, WASM_STOP_EXPR);
  }).catch((err) => {
    log("error", `[rWasmWorker] stopApp failed: ${formatRWasmError(err)}`);
  });
}

/**
 * @param {MessageEvent} event
 */
async function onMessage(event) {
  const data = event.data;

  if (data?.type === RWASM.COMLINK_PORT && event.ports[0]) {
    const port = event.ports[0];
    port.start();
    if (data.role === COMLINK.ROLE.R_HOST) {
      // New handoff: wait for the matching delivery port before announcing ready.
      rHostPortReady = false;
      swDeliveryPortReady = false;
      swDelivery = null;
      exposeRHost(port);
      return;
    }
    if (data.role === COMLINK.ROLE.SW_DELIVERY) {
      connectSwDelivery(port);
      return;
    }
  }

  if (!data || typeof data.type !== "string") {
    return;
  }

  switch (data.type) {
    case RWASM.INIT: {
      try {
        await ensureRModule();
        postToHost({ type: RWASM.READY });
      } catch (err) {
        postToHost({
          type: RWASM.ERROR,
          message: err instanceof Error ? err.message : String(err),
        });
      }
      break;
    }

    case RWASM.WRITE_WEB_APP: {
      try {
        const Module = await ensureRModule();
        writeWebAppToVfs(Module, String(data.source ?? ""));
        postToHost({ type: RWASM.EVAL_RESULT, id: data.id, ok: true });
      } catch (err) {
        postToHost({
          type: RWASM.EVAL_RESULT,
          id: data.id,
          ok: false,
          error: err instanceof Error ? err.message : String(err),
        });
      }
      break;
    }

    case RWASM.EVAL: {
      try {
        const Module = await ensureRModule();
        await enqueueRTask(() => {
          evalR(Module, String(data.code ?? ""));
        });
        if (data.id != null) {
          postToHost({ type: RWASM.EVAL_RESULT, id: data.id, ok: true });
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        if (data.id != null) {
          postToHost({
            type: RWASM.EVAL_RESULT,
            id: data.id,
            ok: false,
            error: message,
          });
        } else {
          log("error", `[rWasmWorker] eval failed: ${message}`);
        }
      }
      break;
    }

    case RWASM.STOP_APP: {
      try {
        await stopRunningApp();
        if (data.id != null) {
          postToHost({ type: RWASM.STOPPED, id: data.id });
        }
      } catch (err) {
        log("error", `[rWasmWorker] stop failed: ${formatRWasmError(err)}`);
      }
      break;
    }

    case RWASM.GET_RESOURCE_PATHS: {
      try {
        const paths = await getShinyResourcePaths();
        postToHost({
          type: RWASM.RESOURCE_PATHS,
          id: data.id,
          paths,
        });
      } catch (err) {
        postToHost({
          type: RWASM.RESOURCE_PATHS,
          id: data.id,
          paths: {},
          error: formatRWasmError(err),
        });
      }
      break;
    }

    default:
      break;
  }
}

self.addEventListener("message", (event) => {
  void onMessage(event);
});

void ensureRModule()
  .then(() => postToHost({ type: RWASM.READY }))
  .catch((err) => {
    postToHost({
      type: RWASM.ERROR,
      message: err instanceof Error ? err.message : String(err),
    });
  });
