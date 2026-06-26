import {
  MSG,
  REQUEST_TIMEOUT_MS,
  SESSION_RECV_TIMEOUT_MS,
  WARMUP_REQUEST_HEADER,
  WS_FRAME,
} from "./httpuv-constants.js";
import { httpuvDebugLog } from "./httpuv-debug.js";
import { COMLINK } from "./httpuv-comlink.js";
import { Comlink, createSwDeliveryApi } from "./httpuv-comlink-setup.js";
import { parseSessionAction, resolveSessionPrefix, resolveShinyPrefix, isHostPushUrl, normalizeSessionHandle } from "./httpuv-prefix.js";
import { resolveShinyStaticRHomePath, rHomePathFromVfsDir } from "./httpuv-static-resolve.js";

const SHINY_PREFIX = resolveShinyPrefix(import.meta.url);
const SESSION_PREFIX = resolveSessionPrefix(import.meta.url);

/** Host-announced prefix (defaults to SW script path; updated via REGISTER_HOST). */
/** @type {string} */
let shinyAppPrefix = SHINY_PREFIX;

/** @type {string | null} */
let hostClientId = null;

/** @type {import('comlink').Remote<{ deliverHttpRequest: Function, stop: Function }> | null} */
let rwasmHost = null;

/** @type {(() => void) | null} */
let rwasmHostReadyResolve = null;

/** @type {Promise<void>} */
let rwasmHostReady = new Promise((resolve) => {
  rwasmHostReadyResolve = resolve;
});

/**
 * @param {MessagePort} port
 */
async function connectSwToWorker(port) {
  const workerHost = Comlink.wrap(port);
  const deliveryChannel = new MessageChannel();
  Comlink.expose(createSwDeliveryApi(handleHostOutboundMessage), deliveryChannel.port1);
  try {
    await workerHost.registerSwDelivery(
      Comlink.transfer(deliveryChannel.port2, [deliveryChannel.port2]),
    );
    rwasmHost = workerHost;
    markRwasmHostReady();
    console.info("[httpuv-sw] Comlink: unified session connected");
  } catch (err) {
    console.error("[httpuv-sw] Comlink unified setup failed", err);
    resetRwasmHostWaiter();
  }
}

function markRwasmHostReady() {
  if (rwasmHostReadyResolve) {
    rwasmHostReadyResolve();
    rwasmHostReadyResolve = null;
  }
}

function resetRwasmHostWaiter() {
  rwasmHost = null;
  rwasmHostReady = new Promise((resolve) => {
    rwasmHostReadyResolve = resolve;
  });
}

/**
 * @param {number} [timeoutMs]
 * @returns {Promise<NonNullable<typeof rwasmHost>>}
 */
async function waitForRwasmHost(timeoutMs = REQUEST_TIMEOUT_MS) {
  if (rwasmHost) {
    return rwasmHost;
  }

  let timer;
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(() => reject(new Error("R worker Comlink not ready")), timeoutMs);
  });

  try {
    await Promise.race([rwasmHostReady, timeout]);
  } finally {
    clearTimeout(timer);
  }

  if (!rwasmHost) {
    throw new Error("R worker Comlink not ready");
  }
  return rwasmHost;
}

/**
 * Ask the host page to re-handshake Comlink MessagePorts.
 */
async function requestComlinkFromHost() {
  const client = await getHostClient();
  client?.postMessage({ type: MSG.REQUEST_COMLINK });
}

/** @type {Map<string, { resolve: (resp: PendingResponse) => void, reject: (err: Error) => void, timer: ReturnType<typeof setTimeout> }>} */
const pendingHttp = new Map();

/**
 * @typedef {object} RecvWaiter
 * @property {(response: Response) => void} resolve
 * @property {ReturnType<typeof setTimeout>} timer
 */

/** @type {Map<string, RecvWaiter[]>} */
const pendingRecv = new Map();

/** @type {Map<string, object[]>} */
const queuedWsPush = new Map();

/** Cached GET /shiny/ document so warmup and iframe do not each trigger a full R render. */
/** @type {PendingResponse | null} */
let cachedAppDocument = null;

/** addResourcePath prefix → VFS directory (e.g. jquery-3.7.1 → /R_HOME/library/shiny/www/shared). */
/** @type {Map<string, string>} */
let shinyResourcePaths = new Map();

/**
 * @param {string} urlString
 * @returns {boolean}
 */
function isAppDocumentRequest(urlString) {
  const url = new URL(urlString);
  if (!url.pathname.startsWith(shinyAppPrefix)) {
    return false;
  }
  const rest = url.pathname.slice(shinyAppPrefix.length).replace(/\/$/, "");
  return rest === "" || rest === "index.html";
}

/**
 * @param {string} pathname
 * @returns {boolean}
 */
function pathUnderShinyPrefix(pathname) {
  return pathname.startsWith(shinyAppPrefix) || pathname.startsWith(SHINY_PREFIX);
}

/**
 * @param {PendingResponse} resp
 * @returns {PendingResponse}
 */
function clonePendingResponse(resp) {
  let body = resp.body;
  if (body instanceof ArrayBuffer) {
    body = body.slice(0);
  } else if (body instanceof Uint8Array) {
    body = body.slice();
  }
  return {
    status: resp.status,
    headers: { ...(resp.headers ?? {}) },
    body,
  };
}

function clearCachedAppDocument() {
  cachedAppDocument = null;
}

function clearShinyResourcePaths() {
  shinyResourcePaths = new Map();
}

/**
 * @param {Record<string, string>} paths
 */
function setShinyResourcePaths(paths) {
  shinyResourcePaths = new Map();
  for (const [prefix, dir] of Object.entries(paths ?? {})) {
    if (prefix && dir) {
      shinyResourcePaths.set(prefix, dir);
    }
  }
  if (shinyResourcePaths.size > 0) {
    console.info(
      "[httpuv-sw] registered",
      shinyResourcePaths.size,
      "Shiny resource path(s):",
      [...shinyResourcePaths.keys()].join(", "),
    );
  }
}

/**
 * @param {string} suffix
 * @returns {string}
 */
function mimeForAssetSuffix(suffix) {
  if (suffix.endsWith(".js") || suffix.endsWith(".mjs")) {
    return "application/javascript";
  }
  if (suffix.endsWith(".css")) {
    return "text/css";
  }
  if (suffix.endsWith(".svg")) {
    return "image/svg+xml";
  }
  if (suffix.endsWith(".png")) {
    return "image/png";
  }
  if (suffix.endsWith(".woff2")) {
    return "font/woff2";
  }
  if (suffix.endsWith(".woff")) {
    return "font/woff";
  }
  return "application/octet-stream";
}

/**
 * @param {string} rHomeRelative path under R_HOME/ without leading slash
 * @param {URL} originUrl
 * @returns {Promise<Response | null>}
 */
async function fetchRHomeAsset(rHomeRelative, originUrl) {
  const assetUrl = new URL(`R_HOME/${rHomeRelative}`, originUrl.origin);
  const assetRes = await fetch(assetUrl, { cache: "force-cache" });
  if (!assetRes.ok) {
    httpuvDebugLog("sw-static-miss", { path: rHomeRelative, status: assetRes.status, url: assetUrl.href });
    return null;
  }
  return assetRes;
}

/**
 * Serve Shiny web dependencies from the preloaded R_HOME tree (no R eval).
 * @param {Request} request
 * @returns {Promise<Response | null>}
 */
async function tryServeShinyStaticAsset(request) {
  if (request.method !== "GET" && request.method !== "HEAD") {
    return null;
  }

  const url = new URL(request.url);
  if (!url.pathname.startsWith(shinyAppPrefix)) {
    return null;
  }

  const rest = url.pathname.slice(shinyAppPrefix.length);
  const slash = rest.indexOf("/");
  if (slash <= 0) {
    return null;
  }

  const prefix = rest.slice(0, slash);
  const suffix = rest.slice(slash + 1);
  if (!suffix || suffix.includes("..")) {
    return null;
  }

  const localDir = shinyResourcePaths.get(prefix);
  const rHomeRelative =
    (localDir ? rHomePathFromVfsDir(localDir, suffix) : null) ??
    resolveShinyStaticRHomePath(prefix, suffix);
  if (!rHomeRelative) {
    return null;
  }

  const assetRes = await fetchRHomeAsset(rHomeRelative, url);
  if (!assetRes) {
    return null;
  }

  httpuvDebugLog("sw-static-hit", {
    prefix,
    suffix,
    source: localDir ? "resourcePaths" : "fallback",
    path: rHomeRelative,
  });

  const headers = new Headers(assetRes.headers);
  if (!headers.has("Content-Type")) {
    headers.set("Content-Type", mimeForAssetSuffix(suffix));
  }
  headers.set("X-Httpuv-Static", localDir ? "rhome" : "rhome-fallback");

  if (request.method === "HEAD") {
    return new Response(null, { status: 200, headers });
  }

  return new Response(assetRes.body, { status: 200, headers });
}

/**
 * @typedef {object} PendingResponse
 * @property {number} status
 * @property {Record<string, string>} headers
 * @property {ArrayBuffer | Uint8Array | string | null} body
 */

self.addEventListener("install", (event) => {
  console.info("[httpuv-sw] installing, shiny prefix:", SHINY_PREFIX);
  event.waitUntil(self.skipWaiting());
});

self.addEventListener("activate", (event) => {
  console.info("[httpuv-sw] activated, shiny prefix:", SHINY_PREFIX);
  resetRwasmHostWaiter();
  event.waitUntil(self.clients.claim());
});

/**
 * @param {string} uuid
 * @param {string} url
 * @param {string} method
 * @returns {Promise<PendingResponse>}
 */
function waitForHttpResponse(uuid, url, method) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      pendingHttp.delete(uuid);
      httpuvDebugLog("sw-timeout", { uuid, timeoutMs: REQUEST_TIMEOUT_MS });
      reject(new Error(`httpuv request ${uuid} timed out after ${REQUEST_TIMEOUT_MS}ms`));
    }, REQUEST_TIMEOUT_MS);

    pendingHttp.set(uuid, { resolve, reject, timer, url, method });
  });
}

/**
 * @param {PendingResponse} resp
 * @param {string} url
 * @param {string} method
 */
function maybeCacheAppDocument(resp, url, method) {
  if (
    url &&
    method === "GET" &&
    isAppDocumentRequest(url) &&
    resp.status === 200
  ) {
    cachedAppDocument = clonePendingResponse(resp);
    console.info("[httpuv-sw] cached app document", url);
  }
}

/**
 * @param {PendingResponse} resp
 * @returns {Response}
 */
function toFetchResponse(resp) {
  const headers = new Headers(resp.headers ?? {});
  if (resp.body == null) {
    return new Response(null, { status: resp.status, headers });
  }
  return new Response(resp.body, { status: resp.status, headers });
}

/**
 * @param {Request} request
 * @returns {Promise<Record<string, string>>}
 */
async function headersToObject(request) {
  /** @type {Record<string, string>} */
  const headers = {};
  request.headers.forEach((value, key) => {
    headers[key] = value;
  });
  return headers;
}

/**
 * @param {string} handle
 * @param {object} msg
 */
function deliverWsPush(handle, msg) {
  const key = normalizeSessionHandle(handle);
  const queue = pendingRecv.get(key);
  const messageLen =
    typeof msg.message === "string"
      ? msg.message.length
      : msg.message?.byteLength ?? msg.message?.length ?? 0;
  httpuvDebugLog("sw-ws-push", {
    handle: key,
    wsType: msg.wsType,
    messageLen,
    recvWaiters: queue?.length ?? 0,
    queuedBefore: queuedWsPush.get(key)?.length ?? 0,
  });
  if (queue && queue.length > 0) {
    const waiter = queue.shift();
    clearTimeout(waiter.timer);
    if (queue.length === 0) {
      pendingRecv.delete(key);
    }
    const headers = new Headers();
    headers.set("X-Httpuv-WS-Type", msg.wsType ?? WS_FRAME.SEND);
    headers.set("X-Httpuv-WS-Binary", msg.binary ? "1" : "0");
    if (!msg.binary) {
      headers.set("Content-Type", "text/plain; charset=UTF-8");
    }
    waiter.resolve(
      new Response(msg.message ?? null, {
        status: 200,
        headers,
      }),
    );
    return;
  }

  if (!queuedWsPush.has(key)) {
    queuedWsPush.set(key, []);
  }
  queuedWsPush.get(key).push(msg);
}

/**
 * @param {FetchEvent} event
 * @returns {Promise<Response>}
 */
async function handleSessionRecv(event) {
  const url = new URL(event.request.url);
  const handle = normalizeSessionHandle(url.searchParams.get("handle"));
  httpuvDebugLog("sw-recv", { handle, url: url.href });
  if (!handle) {
    return new Response("missing handle query parameter", {
      status: 400,
      headers: { "Content-Type": "text/plain" },
    });
  }

  const queued = queuedWsPush.get(handle);
  if (queued && queued.length > 0) {
    const msg = queued.shift();
    if (queued.length === 0) {
      queuedWsPush.delete(handle);
    }
    const headers = new Headers();
    headers.set("X-Httpuv-WS-Type", msg.wsType ?? WS_FRAME.SEND);
    headers.set("X-Httpuv-WS-Binary", msg.binary ? "1" : "0");
    if (!msg.binary) {
      headers.set("Content-Type", "text/plain; charset=UTF-8");
    }
    return new Response(msg.message ?? null, { status: 200, headers });
  }

  return new Promise((resolve) => {
    const timer = setTimeout(() => {
      const waiters = pendingRecv.get(handle);
      if (!waiters) {
        return;
      }
      const idx = waiters.findIndex((w) => w.timer === timer);
      if (idx !== -1) {
        waiters.splice(idx, 1);
      }
      if (waiters.length === 0) {
        pendingRecv.delete(handle);
      }
      resolve(new Response(null, { status: 204 }));
    }, SESSION_RECV_TIMEOUT_MS);

    if (!pendingRecv.has(handle)) {
      pendingRecv.set(handle, []);
    }
    pendingRecv.get(handle).push({ resolve, timer });
  });
}

/**
 * @returns {Promise<Client | undefined>}
 */
async function getHostClient() {
  if (hostClientId) {
    const client = await self.clients.get(hostClientId);
    if (client) {
      return client;
    }
  }
  const clients = await self.clients.matchAll({
    type: "window",
    includeUncontrolled: true,
  });
  return clients[0];
}

/**
 * @param {object} msg
 */
function handleHostOutboundMessage(msg) {
  switch (msg.type) {
    case MSG.HTTP_RESPONSE: {
      httpuvDebugLog("sw-response", { uuid: msg.uuid, status: msg.status });
      const pending = pendingHttp.get(msg.uuid);
      if (!pending) {
        console.warn("[httpuv-sw] No pending request for", msg.uuid);
        return;
      }
      clearTimeout(pending.timer);
      pendingHttp.delete(msg.uuid);
      const resp = {
        status: msg.status ?? 500,
        headers: msg.headers ?? {},
        body: msg.body ?? null,
      };
      maybeCacheAppDocument(resp, pending.url, pending.method);
      pending.resolve(resp);
      break;
    }
    case MSG.WS_PUSH: {
      if (!msg.handle) {
        console.warn("[httpuv-sw] WS_PUSH missing handle");
        return;
      }
      httpuvDebugLog("sw-ws-push-inbound", {
        handle: normalizeSessionHandle(msg.handle),
        wsType: msg.wsType,
        messageLen:
          typeof msg.message === "string"
            ? msg.message.length
            : msg.message?.byteLength ?? msg.message?.length ?? 0,
      });
      deliverWsPush(normalizeSessionHandle(msg.handle), msg);
      break;
    }
    default:
      console.warn("[httpuv-sw] Ignoring unknown host push message", msg.type);
  }
}

/**
 * @param {FetchEvent} event
 * @returns {Promise<Response>}
 */
async function handleHostPush(event) {
  try {
    const msg = await event.request.json();
    handleHostOutboundMessage(msg);
    return new Response(null, { status: 204 });
  } catch (err) {
    console.error("[httpuv-sw] host push failed", err);
    return new Response("bad host push payload", { status: 400 });
  }
}

/**
 * @param {FetchEvent} event
 * @returns {Promise<Response>}
 */
async function handleShinyFetch(event) {
  const request = event.request;
  const uuid = crypto.randomUUID();
  httpuvDebugLog("sw-request", { uuid, method: request.method, url: request.url });

  const bypassAppCache = request.headers.get(WARMUP_REQUEST_HEADER) === "1";
  if (
    request.method === "GET" &&
    isAppDocumentRequest(request.url) &&
    cachedAppDocument &&
    !bypassAppCache
  ) {
    console.info("[httpuv-sw] app document cache hit", request.url);
    httpuvDebugLog("sw-app-cache-hit", { uuid, url: request.url });
    return toFetchResponse(clonePendingResponse(cachedAppDocument));
  }

  const staticRes = await tryServeShinyStaticAsset(request);
  if (staticRes) {
    return staticRes;
  }

  if (!rwasmHost) {
    void requestComlinkFromHost();
    try {
      await waitForRwasmHost(60_000);
    } catch (err) {
      console.error("[httpuv-sw] R worker not ready for", request.url, err);
      return new Response("Shiny R worker is not ready", {
        status: 503,
        headers: { "Content-Type": "text/plain" },
      });
    }
  }

  const host = rwasmHost;
  if (!host) {
    return new Response("Shiny R worker is not ready", {
      status: 503,
      headers: { "Content-Type": "text/plain" },
    });
  }

  const body =
    request.method === "GET" || request.method === "HEAD"
      ? null
      : await request.arrayBuffer();

  const responsePromise = waitForHttpResponse(uuid, request.url, request.method);

  const payload = {
    uuid,
    method: request.method,
    url: request.url,
    headers: await headersToObject(request),
    body,
    clientId: event.clientId,
  };

  const delivery = host
    .deliverHttpRequest(body ? Comlink.transfer(payload, [body]) : payload)
    .catch((err) => {
      console.error("[httpuv-sw] R worker request failed", err);
      throw err;
    });

  try {
    const resp = await responsePromise;
    await delivery;
    return toFetchResponse(resp);
  } catch (err) {
    if (pendingHttp.has(uuid)) {
      const pending = pendingHttp.get(uuid);
      if (pending) {
        clearTimeout(pending.timer);
        pendingHttp.delete(uuid);
      }
    }
    console.error("[httpuv-sw]", err);
    return new Response(
      err instanceof Error && err.message.includes("request failed") ? "Bad Gateway" : "Gateway Timeout",
      {
        status: err instanceof Error && err.message.includes("request failed") ? 502 : 504,
        headers: { "Content-Type": "text/plain" },
      },
    );
  }
}

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (!pathUnderShinyPrefix(url.pathname)) {
    return;
  }

  if (isHostPushUrl(event.request.url, shinyAppPrefix) || isHostPushUrl(event.request.url, SHINY_PREFIX)) {
    event.respondWith(handleHostPush(event));
    return;
  }

  const session = parseSessionAction(event.request.url, shinyAppPrefix) ?? parseSessionAction(event.request.url, SHINY_PREFIX);
  if (session?.action === "recv") {
    event.respondWith(handleSessionRecv(event));
    return;
  }

  event.respondWith(handleShinyFetch(event));
});

self.addEventListener("message", (event) => {
  const msg = event.data;
  if (!msg || typeof msg !== "object") {
    return;
  }

  if (msg.type === COMLINK.PORT_HANDOFF && event.ports[0]) {
    const port = event.ports[0];
    port.start();
    resetRwasmHostWaiter();
    void connectSwToWorker(port);
    return;
  }

  if (typeof msg.type !== "string") {
    return;
  }

  switch (msg.type) {
    case MSG.REGISTER_HOST: {
      if (typeof msg.shinyPrefix === "string" && msg.shinyPrefix) {
        shinyAppPrefix = msg.shinyPrefix.endsWith("/") ? msg.shinyPrefix : `${msg.shinyPrefix}/`;
      }
      if (event.source && "id" in event.source) {
        hostClientId = event.source.id;
        console.info("[httpuv-sw] Registered host client", hostClientId);
      }
      break;
    }

    case MSG.HTTP_RESPONSE: {
      handleHostOutboundMessage(msg);
      break;
    }

    case MSG.WS_PUSH: {
      handleHostOutboundMessage(msg);
      break;
    }

    case MSG.CLEAR_APP_CACHE: {
      clearCachedAppDocument();
      clearShinyResourcePaths();
      break;
    }

    case MSG.SYNC_RESOURCE_PATHS: {
      const replyPort = event.ports?.[0] ?? null;
      const finish = () => {
        replyPort?.postMessage({ ok: true });
      };
      if (!rwasmHost) {
        console.warn("[httpuv-sw] SYNC_RESOURCE_PATHS: R worker not connected");
        finish();
        break;
      }
      void rwasmHost
        .getShinyResourcePaths()
        .then((paths) => {
          setShinyResourcePaths(paths);
        })
        .catch((err) => {
          console.warn("[httpuv-sw] failed to sync resource paths", err);
        })
        .finally(finish);
      break;
    }

    case MSG.REGISTER_RESOURCE_PATHS: {
      setShinyResourcePaths(msg.paths);
      break;
    }

    case MSG.STOP: {
      clearCachedAppDocument();
      clearShinyResourcePaths();
      for (const [uuid, pending] of pendingHttp) {
        clearTimeout(pending.timer);
        pending.reject(new Error("httpuv stopped"));
        pendingHttp.delete(uuid);
      }
      for (const [, waiters] of pendingRecv) {
        for (const waiter of waiters) {
          clearTimeout(waiter.timer);
          waiter.resolve(new Response(null, { status: 204 }));
        }
      }
      pendingRecv.clear();
      queuedWsPush.clear();
      hostClientId = null;
      if (rwasmHost) {
        void rwasmHost.stop().catch((err) => {
          console.warn("[httpuv-sw] R worker stop failed", err);
        });
      }
      break;
    }

    default:
      break;
  }
});
