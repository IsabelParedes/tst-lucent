import {
  CHANNEL,
  HTTPUV_OPTIONS,
  MSG,
  WS_FRAME,
} from "./httpuv-constants.js";
import { getShinyPrefix, parseSessionAction, shinyAppUrl } from "./httpuv-prefix.js";

/** @typedef {Record<string, string>} HeaderMap */

/**
 * @typedef {object} ChannelMessage
 * @property {string} type
 */

/** @type {((optionName: string, ...args: unknown[]) => boolean) | null} */
let invokeROption = null;

/**
 * @param {unknown} body
 * @returns {string | ArrayBuffer | null}
 */
function encodeResponseBody(body) {
  if (body == null) {
    return null;
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
  if (html.includes("</head>")) {
    return html.replace("</head>", `  ${tag}\n</head>`);
  }
  if (html.includes("<body")) {
    return html.replace(/<body([^>]*)>/, `<body$1>\n  ${tag}`);
  }
  return `${tag}\n${html}`;
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
  const channel = getChannel();

  switch (session.action) {
    case "open": {
      const handle = crypto.randomUUID();
      const req = buildReq(msg);
      channel.write({ type: CHANNEL.WS_OPEN, handle, req });
      drainInboundChannel();
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
      channel.write({
        type: CHANNEL.WS_MESSAGE,
        handle: session.handle,
        binary,
        message,
      });
      drainInboundChannel();
      sendTcpResponse(msg.uuid, 204, {}, null);
      break;
    }

    case "close": {
      if (!session.handle) {
        sendTcpResponse(msg.uuid, 400, { "Content-Type": "text/plain" }, "missing handle");
        return;
      }
      channel.write({
        type: CHANNEL.WS_CLOSE,
        handle: session.handle,
        body: msg.body,
      });
      drainInboundChannel();
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
 * @param {object} msg
 */
async function postToServiceWorker(msg) {
  const controller = getServiceWorkerController();
  const body = encodeResponseBody(
    msg.type === CHANNEL.TCP_RESPONSE ? msg.data?.body : msg.data?.message,
  );
  const transfer = body instanceof ArrayBuffer ? [body] : [];

  if (msg.type === CHANNEL.TCP_RESPONSE) {
    const outbound = {
      type: MSG.HTTP_RESPONSE,
      uuid: msg.uuid,
      status: msg.data?.status ?? 500,
      headers: normalizeHeaders(msg.data?.headers),
      body,
    };
    if (controller) {
      controller.postMessage(outbound, transfer);
      return;
    }
    await fetch(shinyAppUrl("__host__/push"), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(outbound),
    });
    return;
  }

  if (msg.type === CHANNEL.WS_RESPONSE) {
    const outbound = {
      type: MSG.WS_PUSH,
      handle: msg.data?.handle,
      binary: msg.data?.binary ?? false,
      wsType: msg.data?.type ?? WS_FRAME.SEND,
      message: body,
    };
    if (controller) {
      controller.postMessage(outbound, transfer);
      return;
    }
    await fetch(shinyAppUrl("__host__/push"), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        ...outbound,
        message: typeof body === "string" ? body : undefined,
      }),
    });
  }
}

/**
 * @param {ChannelMessage} msg
 */
function postToServiceWorkerSync(msg) {
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
        postToServiceWorkerSync(msg);
        return;
      }
      inbox.push(msg);
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

function installServiceWorkerListener() {
  navigator.serviceWorker.addEventListener("message", (event) => {
    const msg = event.data;
    if (!msg || typeof msg !== "object") {
      return;
    }

    const channel = getChannel();

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

        console.info("[httpuv-bridge] inbound http request", {
          uuid: msg.uuid,
          method: msg.method,
          url: msg.url,
        });
        channel.write({
          type: CHANNEL.HTTP_REQUEST,
          uuid: msg.uuid,
          method: msg.method,
          url: msg.url,
          headers: msg.headers ?? {},
          body: msg.body ?? null,
          clientId: msg.clientId,
        });
        drainInboundChannel();
        break;
      }

      case MSG.STOP: {
        channel.inbox.length = 0;
        break;
      }

      default:
        break;
    }
  });
}

/**
 * Install Module.httpuv (channel + dispatch) and wire the service worker listener.
 * Call before WASM R starts.
 */
export function installHttpuvBridge() {
  const httpuv = ensureModuleHttpuv();

  if (!httpuv.channel) {
    httpuv.channel = createChannel();
  }

  httpuv.dispatch = dispatch;
  httpuv.drainInboundChannel = drainInboundChannel;
  httpuv.buildReq = buildReq;
  httpuv.injectShinySocketBootstrap = injectShinySocketBootstrap;
  httpuv.shinySocketScriptUrl = shinySocketScriptUrl;

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

  if (!httpuv._swListenerInstalled) {
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
