import {
  CHANNEL,
  HTTPUV_OPTIONS,
  MSG,
  WS_FRAME,
} from "./httpuv-constants.js";
import { httpuvDebugLog } from "./httpuv-debug.js";
import { getShinyPrefix, parseSessionAction, shinyAppUrl } from "./httpuv-prefix.js";

/** @typedef {Record<string, string>} HeaderMap */

/**
 * @typedef {object} ChannelMessage
 * @property {string} type
 */

/** @type {((optionName: string, ...args: unknown[]) => boolean) | null} */
let invokeROption = null;

/** @type {((msg: ChannelMessage) => void) | null} */
let pushToR = null;

/**
 * @param {unknown} body
 * @returns {string | ArrayBuffer | null}
 */
function decodeBase64Body(data) {
  const text = String(data ?? "");
  const binary = atob(text);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes.buffer;
}

function encodeResponseBody(body) {
  if (body == null) {
    return null;
  }
  if (typeof body === "object" && !Array.isArray(body) && body.httpuvRaw === "base64") {
    return decodeBase64Body(body.data);
  }
  if (typeof body === "string") {
    return body;
  }
  if (body instanceof ArrayBuffer) {
    return body;
  }
  if (ArrayBuffer.isView(body)) {
    return body.buffer.slice(body.byteOffset, body.byteOffset + body.byteLength);
  }
  if (Array.isArray(body)) {
    return new Uint8Array(body).buffer;
  }
  return String(body);
}

/**
 * @param {unknown} headers
 * @returns {HeaderMap}
 */
function normalizeHeaders(headers) {
  if (!headers || typeof headers !== "object") {
    return {};
  }
  /** @type {HeaderMap} */
  const out = {};
  for (const [key, value] of Object.entries(headers)) {
    if (value != null) {
      out[key] = String(value);
    }
  }
  return out;
}

/**
 * @param {string} pathname
 * @returns {string}
 */
function shinyPathInfo(pathname) {
  const shinyPrefix = getShinyPrefix();
  if (!pathname.startsWith(shinyPrefix)) {
    return pathname || "/";
  }
  const rest = pathname.slice(shinyPrefix.length).replace(/^\/+/, "");
  return rest ? `/${rest}` : "/";
}

/**
 * Build a rook-like request env object for httpuv handlers.
 * @param {ChannelMessage & { uuid: string, method: string, url: string, headers?: HeaderMap, body?: ArrayBuffer | null }} msg
 * @returns {Record<string, unknown>}
 */
export function buildReq(msg) {
  const url = new URL(msg.url);
  const pathInfo = shinyPathInfo(url.pathname);
  const queryString = url.search.length > 1 ? url.search.slice(1) : "";
  const shinyPrefix = getShinyPrefix();

  /** @type {Record<string, unknown>} */
  const req = {
    UUID: msg.uuid,
    REQUEST_METHOD: msg.method,
    SCRIPT_NAME: shinyPrefix.replace(/\/$/, ""),
    PATH_INFO: pathInfo,
    QUERY_STRING: queryString,
    "rook.version": "1.1-0",
    "rook.url_scheme": url.protocol === "https:" ? "https" : "http",
    SERVER_NAME: url.hostname,
    SERVER_PORT: url.port || (url.protocol === "https:" ? "443" : "80"),
    REMOTE_ADDR: "127.0.0.1",
    REMOTE_PORT: "0",
    HEADERS: { ...(msg.headers ?? {}) },
  };

  for (const [key, value] of Object.entries(msg.headers ?? {})) {
    req[`HTTP_${key.toUpperCase().replace(/-/g, "_")}`] = value;
  }

  if (msg.body) {
    req.body = msg.body;
    req.CONTENT_LENGTH = String(msg.body.byteLength);
  }

  return req;
}

/**
 * Absolute pathname for shiny-socket.js (served next to runApp.js).
 * @returns {string}
 */
export function shinySocketScriptUrl() {
  return new URL("./shiny-socket.js", import.meta.url).pathname;
}

/**
 * Inject the virtual Shiny socket bootstrap into an HTML document.
 * @param {string} html
 * @returns {string}
 */
export function injectShinySocketBootstrap(html) {
  const tag = `<script type="module" src="${shinySocketScriptUrl()}"></script>`;
  if (html.includes("<head")) {
    return html.replace(/<head([^>]*)>/, `<head$1>\n  ${tag}`);
  }
  if (html.includes("<body")) {
    return html.replace(/<body([^>]*)>/, `<body$1>\n  ${tag}`);
  }
  return `${tag}\n${html}`;
}

/**
 * @param {unknown} body
 * @returns {string | null}
 */
function bodyToText(body) {
  if (body == null) {
    return null;
  }
  if (typeof body === "string") {
    return body;
  }
  if (body instanceof ArrayBuffer) {
    return new TextDecoder().decode(body);
  }
  if (ArrayBuffer.isView(body)) {
    return new TextDecoder().decode(body);
  }
  if (Array.isArray(body)) {
    return new TextDecoder().decode(new Uint8Array(body));
  }
  return String(body);
}

/**
 * @param {unknown} body
 * @param {HeaderMap} headers
 * @returns {string | ArrayBuffer | null}
 */
function maybeInjectShinySocketBootstrap(body, headers) {
  const contentType =
    Object.entries(headers).find(([key]) => key.toLowerCase() === "content-type")?.[1] ?? "";
  if (!contentType.includes("text/html")) {
    return encodeResponseBody(body);
  }
  const html = bodyToText(body);
  if (!html) {
    return encodeResponseBody(body);
  }
  return injectShinySocketBootstrap(html);
}

/**
 * @param {ArrayBuffer | null | undefined} body
 * @returns {{ binary: boolean, message: string | ArrayBuffer }}
 */
function sessionMessageFromBody(body) {
  if (!body || body.byteLength === 0) {
    return { binary: false, message: "" };
  }
  const bytes = new Uint8Array(body);
  const isText = bytes.every((b) => b === 9 || b === 10 || b === 13 || (b >= 32 && b <= 126));
  if (isText) {
    return { binary: false, message: new TextDecoder().decode(body) };
  }
  return { binary: true, message: body };
}

/**
 * @param {ChannelMessage & { uuid: string, method: string, url: string, headers?: HeaderMap, body?: ArrayBuffer | null }} msg
 * @param {{ action: string, handle: string | null }} session
 */
function handleSessionHttp(msg, session) {
  switch (session.action) {
    case "open": {
      const handle = crypto.randomUUID();
      const req = buildReq(msg);
      pushInboundChannelMessage({ type: CHANNEL.WS_OPEN, handle, req });
      sendTcpResponse(
        msg.uuid,
        200,
        { "Content-Type": "application/json" },
        JSON.stringify({ handle }),
      );
      break;
    }

    case "send": {
      if (!session.handle) {
        sendTcpResponse(msg.uuid, 400, { "Content-Type": "text/plain" }, "missing handle");
        return;
      }
      const { binary, message } = sessionMessageFromBody(msg.body);
      pushInboundChannelMessage({
        type: CHANNEL.WS_MESSAGE,
        handle: session.handle,
        binary,
        message,
      });
      sendTcpResponse(msg.uuid, 204, {}, null);
      break;
    }

    case "close": {
      if (!session.handle) {
        sendTcpResponse(msg.uuid, 400, { "Content-Type": "text/plain" }, "missing handle");
        return;
      }
      pushInboundChannelMessage({
        type: CHANNEL.WS_CLOSE,
        handle: session.handle,
      });
      sendTcpResponse(msg.uuid, 204, {}, null);
      break;
    }

    default:
      sendTcpResponse(msg.uuid, 404, { "Content-Type": "text/plain" }, "unknown session action");
  }
}

/**
 * @param {string} uuid
 * @param {number} status
 * @param {HeaderMap} headers
 * @param {unknown} body
 */
function sendTcpResponse(uuid, status, headers, body) {
  getChannel().write({
    type: CHANNEL.TCP_RESPONSE,
    uuid,
    data: {
      status,
      headers,
      body,
    },
  });
}

function getServiceWorkerController() {
  return navigator.serviceWorker.controller;
}

/**
 * @param {ArrayBuffer | null | undefined} body
 * @returns {number[] | null}
 */
function serializeBodyForChannel(body) {
  if (!body) {
    return null;
  }
  return Array.from(new Uint8Array(body));
}

/**
 * @param {object} msg
 * @param {(outbound: object, transfer?: Transferable[]) => void} deliver
 */
function formatOutboundForHost(msg, deliver) {
  if (msg.type === CHANNEL.TCP_RESPONSE) {
    const headers = normalizeHeaders(msg.data?.headers);
    let body;
    try {
      body = maybeInjectShinySocketBootstrap(msg.data?.body, headers);
    } catch (err) {
      console.warn("[httpuv-bridge] shiny socket bootstrap injection failed", err);
      body = encodeResponseBody(msg.data?.body);
    }
    const transfer = body instanceof ArrayBuffer ? [body] : [];
    deliver(
      {
        type: MSG.HTTP_RESPONSE,
        uuid: msg.uuid,
        status: msg.data?.status ?? 500,
        headers,
        body,
      },
      transfer,
    );
    return;
  }

  const body = encodeResponseBody(msg.data?.message);
  const transfer = body instanceof ArrayBuffer ? [body] : [];

  if (msg.type === CHANNEL.WS_RESPONSE) {
    deliver(
      {
        type: MSG.WS_PUSH,
        handle: msg.data?.handle,
        binary: msg.data?.binary ?? false,
        wsType: msg.data?.type ?? WS_FRAME.SEND,
        message: body,
      },
      transfer,
    );
  }
}

/**
 * @param {object} msg
 */
async function postToServiceWorker(msg) {
  const controller = getServiceWorkerController();
  formatOutboundForHost(msg, (outbound, transfer = []) => {
    if (msg.type === CHANNEL.TCP_RESPONSE) {
      if (controller) {
        controller.postMessage(outbound, transfer);
        return;
      }
      void fetch(shinyAppUrl("__host__/push"), {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(outbound),
      });
      return;
    }

    if (msg.type === CHANNEL.WS_RESPONSE) {
      if (controller) {
        controller.postMessage(outbound, transfer);
        return;
      }
      void fetch(shinyAppUrl("__host__/push"), {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          ...outbound,
          message: typeof outbound.message === "string" ? outbound.message : undefined,
        }),
      });
    }
  });
}

/** @type {((msg: object, transfer?: Transferable[]) => void) | null} */
let postOutbound = null;

/** @type {ChannelMessage[]} */
const deferredOutbound = [];

/**
 * Deliver responses queued while evalR is active (Comlink must not run inside WASM eval).
 */
export function flushDeferredOutbound() {
  if (deferredOutbound.length === 0) {
    return;
  }
  const batch = deferredOutbound.splice(0, deferredOutbound.length);
  for (const msg of batch) {
    postOutboundSync(msg);
  }
}

/**
 * @param {ChannelMessage} msg
 */
function postOutboundMaybeDefer(msg) {
  const depth = globalThis.Module?._rWasmEvalDepth ?? 0;
  if (depth > 0) {
    deferredOutbound.push(msg);
    if (msg.type === CHANNEL.TCP_RESPONSE) {
      httpuvDebugLog("channel-tcp-response-deferred", {
        uuid: msg.uuid,
        status: msg.data?.status,
        depth,
      });
    }
    return;
  }
  if (msg.type === CHANNEL.TCP_RESPONSE) {
    httpuvDebugLog("channel-tcp-response", {
      uuid: msg.uuid,
      status: msg.data?.status,
    });
  }
  postOutboundSync(msg);
}

/**
 * @param {ChannelMessage} msg
 */
function postOutboundSync(msg) {
  if (postOutbound) {
    formatOutboundForHost(msg, (outbound, transfer = []) => {
      postOutbound(outbound, transfer);
    });
    return;
  }
  void postToServiceWorker(msg);
}

function createChannel() {
  /** @type {ChannelMessage[]} */
  const inbox = [];

  return {
    inbox,

    hasMessage() {
      return inbox.length > 0;
    },

    read() {
      return inbox.shift() ?? { type: CHANNEL.STDIN };
    },

    /** @param {ChannelMessage} msg */
    write(msg) {
      if (
        msg.type === CHANNEL.TCP_RESPONSE ||
        msg.type === CHANNEL.WS_RESPONSE
      ) {
        postOutboundMaybeDefer(msg);
        return;
      }
      console.warn("[httpuv-bridge] inbound channel.write is deprecated; use pushInboundChannelMessage", msg.type);
      pushInboundChannelMessage(msg);
    },
  };
}

/**
 * @param {ChannelMessage} msg
 */
export function dispatch(msg) {
  if (!msg || typeof msg !== "object" || typeof msg.type !== "string") {
    return;
  }

  switch (msg.type) {
    case CHANNEL.HTTP_REQUEST: {
      const req = buildReq(msg);
      const handled = invokeROption?.(HTTPUV_OPTIONS.ON_REQUEST, req) ?? false;
      if (!handled) {
        sendTcpResponse(
          msg.uuid,
          503,
          { "Content-Type": "text/plain" },
          "httpuv: no R handler registered",
        );
      }
      break;
    }

    case CHANNEL.WS_OPEN: {
      const handled =
        invokeROption?.(HTTPUV_OPTIONS.ON_WS_OPEN, msg.handle, msg.req) ?? false;
      if (!handled) {
        console.info("[httpuv-bridge] ws open (no R handler yet)", msg.handle);
      }
      break;
    }

    case CHANNEL.WS_MESSAGE: {
      const handled =
        invokeROption?.(
          HTTPUV_OPTIONS.ON_WS_MESSAGE,
          msg.handle,
          msg.binary,
          msg.message,
        ) ?? false;
      if (!handled) {
        console.info("[httpuv-bridge] ws message (no R handler yet)", msg.handle);
        getChannel().write({
          type: CHANNEL.WS_RESPONSE,
          data: {
            handle: msg.handle,
            binary: msg.binary,
            type: WS_FRAME.SEND,
            message: msg.message,
          },
        });
      }
      break;
    }

    case CHANNEL.WS_CLOSE: {
      invokeROption?.(HTTPUV_OPTIONS.ON_WS_CLOSE, msg.handle);
      break;
    }

    default:
      console.warn("[httpuv-bridge] unhandled channel message", msg.type);
  }
}

/**
 * Drain inbound channel messages until empty or only stdin placeholders remain.
 */
export function drainInboundChannel() {
  const channel = getChannel();
  while (channel.hasMessage()) {
    const msg = channel.read();
    if (msg.type === CHANNEL.STDIN) {
      continue;
    }
    dispatch(msg);
  }
}

function getChannel() {
  const httpuv = ensureModuleHttpuv();
  if (!httpuv.channel) {
    httpuv.channel = createChannel();
  }
  return httpuv.channel;
}

function ensureModuleHttpuv() {
  globalThis.Module = globalThis.Module ?? {};
  globalThis.Module.httpuv = globalThis.Module.httpuv ?? {};
  return globalThis.Module.httpuv;
}

/**
 * Push a channel message into R immediately (worker push path).
 * @param {ChannelMessage} msg
 * @returns {boolean}
 */
export function pushInboundChannelMessage(msg) {
  if (!pushToR) {
    console.warn("[httpuv-bridge] pushToR not configured");
    return false;
  }
  pushToR(msg);
  return true;
}

/**
 * Push an inbound host message from the service worker into R.
 * @param {object} msg
 */
export function pushInboundHostMessage(msg) {
  if (!msg || typeof msg !== "object") {
    return;
  }

  switch (msg.type) {
    case MSG.HTTP_REQUEST: {
      const session = parseSessionAction(msg.url, getShinyPrefix());
      if (session && session.action !== "recv") {
        console.info("[httpuv-bridge] inbound session request", {
          uuid: msg.uuid,
          action: session.action,
          handle: session.handle,
        });
        handleSessionHttp(msg, session);
        break;
      }

      console.info("[httpuv-bridge] push http request", {
        uuid: msg.uuid,
        method: msg.method,
        url: msg.url,
      });
      pushInboundChannelMessage({
        type: CHANNEL.HTTP_REQUEST,
        uuid: msg.uuid,
        method: msg.method,
        url: msg.url,
        headers: msg.headers ?? {},
        body: serializeBodyForChannel(msg.body),
        clientId: msg.clientId,
      });
      break;
    }

    case MSG.STOP:
      break;

    default:
      break;
  }
}

/**
 * Handle an inbound message from the service worker (or a host proxy).
 * @param {object} msg
 */
export function handleInboundHostMessage(msg) {
  pushInboundHostMessage(msg);
}

function installServiceWorkerListener() {
  navigator.serviceWorker.addEventListener("message", (event) => {
    handleInboundHostMessage(event.data);
  });
}

/**
 * @typedef {object} HttpuvBridgeOptions
 * @property {((outbound: object, transfer?: Transferable[]) => void)} [postOutbound]
 *   Deliver HTTP/WS responses to the host (main page → service worker).
 * @property {boolean} [installSwListener=true]
 *   Listen for service worker messages in this context (false in the R worker).
 * @property {((msg: ChannelMessage) => void)} [pushToR]
 *   Push inbound HTTP/WebSocket channel messages into R (R worker only).
 */

/**
 * Install Module.httpuv (channel + dispatch) and optionally wire the service worker listener.
 * Call before WASM R starts.
 * @param {HttpuvBridgeOptions} [options]
 */
export function installHttpuvBridge(options = {}) {
  if (options.postOutbound) {
    postOutbound = options.postOutbound;
  }
  if (options.pushToR) {
    pushToR = options.pushToR;
  }

  const httpuv = ensureModuleHttpuv();

  if (!httpuv.channel) {
    httpuv.channel = createChannel();
  }

  httpuv.dispatch = dispatch;
  httpuv.drainInboundChannel = drainInboundChannel;
  httpuv.pushInboundChannelMessage = pushInboundChannelMessage;
  httpuv.pushInboundHostMessage = pushInboundHostMessage;
  httpuv.buildReq = buildReq;
  httpuv.injectShinySocketBootstrap = injectShinySocketBootstrap;
  httpuv.shinySocketScriptUrl = shinySocketScriptUrl;
  httpuv.shinyPrefix = getShinyPrefix();

  /**
   * Push a message to a virtual socket recv waiter (used by R via channel.write).
   * @param {string} handle
   * @param {unknown} message
   * @param {{ binary?: boolean, wsType?: string }} [opts]
   */
  httpuv.pushWsMessage = (handle, message, opts = {}) => {
    getChannel().write({
      type: CHANNEL.WS_RESPONSE,
      data: {
        handle,
        binary: opts.binary ?? false,
        type: opts.wsType ?? WS_FRAME.SEND,
        message,
      },
    });
  };

  /**
   * Called from R (via emscripten) once httpuv registers option handlers.
   * @param {(optionName: string, ...args: unknown[]) => boolean} fn
   */
  httpuv.bindInvokeROption = (fn) => {
    invokeROption = fn;
  };

  const installSwListener = options.installSwListener ?? true;
  if (installSwListener && !httpuv._swListenerInstalled) {
    installServiceWorkerListener();
    httpuv._swListenerInstalled = true;
  }

  console.info("[httpuv-bridge] installed");
  return httpuv;
}

/**
 * @param {((optionName: string, ...args: unknown[]) => boolean) | null} fn
 */
export function setInvokeROption(fn) {
  invokeROption = fn;
}

/**
 * @param {((msg: ChannelMessage) => void) | null} fn
 */
export function setPushToR(fn) {
  pushToR = fn;
}
