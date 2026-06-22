import {
  MSG,
  REQUEST_TIMEOUT_MS,
} from "./httpuv-constants.js";
import { resolveShinyPrefix } from "./httpuv-prefix.js";

const SHINY_PREFIX = resolveShinyPrefix(import.meta.url);

/** @type {string | null} */
let hostClientId = null;

/** @type {Map<string, { resolve: (resp: PendingResponse) => void, reject: (err: Error) => void, timer: ReturnType<typeof setTimeout> }>} */
const pendingHttp = new Map();

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
 * @param {FetchEvent} event
 * @returns {Promise<Response>}
 */
async function handleShinyFetch(event) {
  const request = event.request;
  const uuid = crypto.randomUUID();
  const hostClient = hostClientId ? await self.clients.get(hostClientId) : null;

  if (!hostClient) {
    return new Response("Shiny host page is not ready", {
      status: 503,
      headers: { "Content-Type": "text/plain" },
    });
  }

  const body =
    request.method === "GET" || request.method === "HEAD"
      ? null
      : await request.arrayBuffer();

  const responsePromise = waitForHttpResponse(uuid);

  hostClient.postMessage(
    {
      type: MSG.HTTP_REQUEST,
      uuid,
      method: request.method,
      url: request.url,
      headers: await headersToObject(request),
      body,
      clientId: event.clientId,
    },
    body ? [body] : [],
  );

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

  event.respondWith(handleShinyFetch(event));
});

self.addEventListener("message", (event) => {
  const msg = event.data;
  if (!msg || typeof msg !== "object" || typeof msg.type !== "string") {
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
      // Outbound Shiny session messages are delivered to long-poll waiters in a
      // later step; for now acknowledge receipt so the main page can log flow.
      break;
    }

    case MSG.STOP: {
      for (const [uuid, pending] of pendingHttp) {
        clearTimeout(pending.timer);
        pending.reject(new Error("httpuv stopped"));
        pendingHttp.delete(uuid);
      }
      hostClientId = null;
      break;
    }

    default:
      break;
  }
});
