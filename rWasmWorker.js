import { installHttpuvBridge, handleInboundHostMessage } from "./httpuv-bridge.js";
import { MSG } from "./httpuv-constants.js";
import { COMLINK } from "./httpuv-comlink.js";
import { Comlink, createRHostApi } from "./httpuv-comlink-setup.js";
import { resolveShinyPrefix, setShinyPrefix } from "./httpuv-prefix.js";
import { RWASM } from "./rwasm-constants.js";
import { evalR, initRModule, writeWebAppToVfs } from "./rWasmBootstrap.js";

const WASM_HTTPUV_PUMP = `tryCatch({
  httpuv::httpuv_pump()
}, error=function(e) NULL)`;

const WASM_SHINY_SERVICE = `tryCatch({
  if (requireNamespace("shiny", quietly=TRUE) && shiny::isRunning()) {
    if (exists("serviceApp", envir=asNamespace("shiny"), inherits=FALSE)) {
      get("serviceApp", envir=asNamespace("shiny"))()
    }
  }
}, error=function(e) NULL)`;

/** @type {number} */
const REQUEST_PUMP_ROUNDS = 24;

const WASM_STOP_EXPR = `tryCatch({
  if (exists("shiny_forge_stop_app", mode="function")) {
    shiny_forge_stop_app()
  } else if (requireNamespace("shiny", quietly=TRUE) && shiny::isRunning()) {
    shiny::stopApp()
  }
}, error=function(e) NULL)`;

/** @type {object | null} */
let rModule = null;
/** @type {ReturnType<typeof setInterval> | null} */
let pumpTimer = null;
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
    const resp = {
      uuid: outbound.uuid,
      status: outbound.status,
      headers: outbound.headers,
      body: outbound.body,
    };
    void swDelivery.deliverHttpResponse(
      transfer.length > 0 ? Comlink.transfer(resp, transfer) : resp,
    );
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

function installBridge() {
  setShinyPrefix(resolveShinyPrefix(import.meta.url));
  installHttpuvBridge({
    installSwListener: false,
    postOutbound: deliverToServiceWorker,
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
  return rModule;
}

function pumpRModule(module, rounds = 1) {
  for (let i = 0; i < rounds; i += 1) {
    evalR(module, WASM_HTTPUV_PUMP);
    evalR(module, WASM_SHINY_SERVICE);
  }
}

function exposeRHost(port) {
  const api = createRHostApi(
    async (req) => {
      const module = await ensureRModule();
      handleInboundHostMessage(req);
      pumpRModule(module, REQUEST_PUMP_ROUNDS);
    },
    async () => {
      handleInboundHostMessage({ type: MSG.STOP });
      stopRunningApp();
    },
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

function stopPump() {
  if (pumpTimer !== null) {
    clearInterval(pumpTimer);
    pumpTimer = null;
  }
}

function startPump() {
  stopPump();
  pumpTimer = setInterval(() => {
    if (!rModule) {
      return;
    }
    try {
      pumpRModule(rModule, 1);
    } catch (err) {
      console.error("[rWasmWorker] pump error:", err);
      stopPump();
    }
  }, 16);
  log("log", "[rWasmWorker] event pump started");
}

function stopRunningApp() {
  stopPump();
  if (!rModule) {
    return;
  }
  try {
    evalR(rModule, WASM_STOP_EXPR);
  } catch (err) {
    log("error", `[rWasmWorker] stopApp failed: ${err}`);
  }
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
        evalR(Module, String(data.code ?? ""));
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

    case RWASM.PUMP_START: {
      await ensureRModule();
      startPump();
      break;
    }

    case RWASM.PUMP_STOP: {
      stopPump();
      break;
    }

    case RWASM.STOP_APP: {
      stopRunningApp();
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
