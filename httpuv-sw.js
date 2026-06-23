import {
  MSG,
  REQUEST_TIMEOUT_MS,
  SESSION_RECV_TIMEOUT_MS,
  WS_FRAME,
} from "./httpuv-constants.js";
import { COMLINK } from "./httpuv-comlink.js";
import { Comlink, createSwDeliveryApi } from "./httpuv-comlink-setup.js";
import { parseSessionAction, resolveSessionPrefix, resolveShinyPrefix, isHostPushUrl } from "./httpuv-prefix.js";

const SHINY_PREFIX = resolveShinyPrefix(import.meta.url);
const SESSION_PREFIX = resolveSessionPrefix(import.meta.url);

/** @type {string | null} */
let hostClientId = null;

/** @type {import('comlink').Remote<{ deliverHttpRequest: Function, stop: Function }> | null} */
let rwasmHost = null;

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
  event.waitUntil(self.clients.claim());
});

/**
 * @param {string} uuid
 * @returns {Promise<PendingResponse>}
 */
function waitForHttpResponse(uuid) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      pendingHttp.delete(uuid);
      reject(new Error(`httpuv request ${uuid} timed out after ${REQUEST_TIMEOUT_MS}ms`));
    }, REQUEST_TIMEOUT_MS);

    pendingHttp.set(uuid, { resolve, reject, timer });
  });
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
  const queue = pendingRecv.get(handle);
  if (queue && queue.length > 0) {
    const waiter = queue.shift();
    clearTimeout(waiter.timer);
    if (queue.length === 0) {
      pendingRecv.delete(handle);
    }
    const headers = new Headers();
    headers.set("X-Httpuv-WS-Type", msg.wsType ?? WS_FRAME.SEND);
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

  if (!queuedWsPush.has(handle)) {
    queuedWsPush.set(handle, []);
  }
  queuedWsPush.get(handle).push(msg);
}

/**
 * @param {FetchEvent} event
 * @returns {Promise<Response>}
 */
async function handleSessionRecv(event) {
  const url = new URL(event.request.url);
  const handle = url.searchParams.get("handle");
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
      const pending = pendingHttp.get(msg.uuid);
      if (!pending) {
        console.warn("[httpuv-sw] No pending request for", msg.uuid);
        return;
      }
      clearTimeout(pending.timer);
      pendingHttp.delete(msg.uuid);
      pending.resolve({
        status: msg.status ?? 500,
        headers: msg.headers ?? {},
        body: msg.body ?? null,
      });
      break;
    }
    case MSG.WS_PUSH: {
      if (!msg.handle) {
        console.warn("[httpuv-sw] WS_PUSH missing handle");
        return;
      }
      deliverWsPush(String(msg.handle), msg);
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

  if (!rwasmHost) {
    return new Response("Shiny R worker is not ready", {
      status: 503,
      headers: { "Content-Type": "text/plain" },
    });
  }

  const body =
    request.method === "GET" || request.method === "HEAD"
      ? null
      : await request.arrayBuffer();

  const responsePromise = waitForHttpResponse(uuid);

  const payload = {
    uuid,
    method: request.method,
    url: request.url,
    headers: await headersToObject(request),
    body,
    clientId: event.clientId,
  };

  try {
    await rwasmHost.deliverHttpRequest(
      body ? Comlink.transfer(payload, [body]) : payload,
    );
  } catch (err) {
    console.error("[httpuv-sw] R worker request failed", err);
    return new Response("Bad Gateway", {
      status: 502,
      headers: { "Content-Type": "text/plain" },
    });
  }

  try {
    const resp = await responsePromise;
    return toFetchResponse(resp);
  } catch (err) {
    console.error("[httpuv-sw]", err);
    return new Response("Gateway Timeout", {
      status: 504,
      headers: { "Content-Type": "text/plain" },
    });
  }
}

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  if (!url.pathname.startsWith(SHINY_PREFIX)) {
    return;
  }

  if (isHostPushUrl(event.request.url, SHINY_PREFIX)) {
    event.respondWith(handleHostPush(event));
    return;
  }

  const session = parseSessionAction(event.request.url, SHINY_PREFIX);
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
    if (msg.role === COMLINK.ROLE.R_HOST) {
      rwasmHost = Comlink.wrap(port);
      console.info("[httpuv-sw] Comlink: connected to R host");
      return;
    }
    if (msg.role === COMLINK.ROLE.SW_DELIVERY) {
      Comlink.expose(createSwDeliveryApi(handleHostOutboundMessage), port);
      console.info("[httpuv-sw] Comlink: exposing delivery API");
      return;
    }
  }

  if (typeof msg.type !== "string") {
    return;
  }

  switch (msg.type) {
    case MSG.REGISTER_HOST: {
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

    case MSG.STOP: {
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
