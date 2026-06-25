import * as Comlink from "./comlink.mjs";
import { MSG } from "./httpuv-constants.js";
import { COMLINK } from "./httpuv-comlink.js";
import { RWASM } from "./rwasm-constants.js";

/**
 * Broker two Comlink MessagePorts between the service worker and R worker.
 * @param {Worker} rWorker
 * @returns {Promise<void>}
 */
export async function connectHttpuvComlink(rWorker) {
  const controller = navigator.serviceWorker.controller;
  if (!controller) {
    throw new Error("Service worker controller is not available for Comlink setup");
  }

  const readyPromise = waitForComlinkReady(rWorker);

  const rHostChannel = new MessageChannel();
  controller.postMessage(
    { type: COMLINK.PORT_HANDOFF, role: COMLINK.ROLE.R_HOST },
    [rHostChannel.port1],
  );
  rWorker.postMessage(
    { type: RWASM.COMLINK_PORT, role: COMLINK.ROLE.R_HOST },
    [rHostChannel.port2],
  );

  const swDeliveryChannel = new MessageChannel();
  controller.postMessage(
    { type: COMLINK.PORT_HANDOFF, role: COMLINK.ROLE.SW_DELIVERY },
    [swDeliveryChannel.port1],
  );
  rWorker.postMessage(
    { type: RWASM.COMLINK_PORT, role: COMLINK.ROLE.SW_DELIVERY },
    [swDeliveryChannel.port2],
  );

  await readyPromise;
  console.info("[httpuv-comlink] Service worker ↔ R worker connected");
}

/**
 * @param {Worker} rWorker
 * @returns {Promise<void>}
 */
function waitForComlinkReady(rWorker) {
  return new Promise((resolve, reject) => {
    const deadline = setTimeout(() => {
      rWorker.removeEventListener("message", onMessage);
      reject(new Error("Comlink setup timed out"));
    }, 30_000);

    /** @param {MessageEvent} event */
    const onMessage = (event) => {
      if (event.data?.type === RWASM.COMLINK_READY) {
        clearTimeout(deadline);
        rWorker.removeEventListener("message", onMessage);
        resolve();
      }
      if (event.data?.type === RWASM.ERROR) {
        clearTimeout(deadline);
        rWorker.removeEventListener("message", onMessage);
        reject(new Error(event.data.message ?? "R worker Comlink setup failed"));
      }
    };

    rWorker.addEventListener("message", onMessage);
  });
}

/**
 * Build the API exposed by the service worker for outbound httpuv traffic.
 * @param {(msg: object) => void} deliverOutbound
 */
export function createSwDeliveryApi(deliverOutbound) {
  return {
    deliverHttpResponse(resp) {
      deliverOutbound({
        type: MSG.HTTP_RESPONSE,
        uuid: resp.uuid,
        status: resp.status ?? 500,
        headers: resp.headers ?? {},
        body: resp.body ?? null,
      });
    },
    deliverWsPush(msg) {
      deliverOutbound({
        type: MSG.WS_PUSH,
        handle: msg.handle,
        binary: msg.binary ?? false,
        wsType: msg.wsType,
        message: msg.message ?? null,
      });
    },
  };
}

/**
 * Build the API exposed by the R worker for inbound httpuv traffic.
 * @param {(req: object) => void | Promise<void>} onHttpRequest
 * @param {() => void} onStop
 */
export function createRHostApi(onHttpRequest, onStop, getResourcePaths) {
  return {
    deliverHttpRequest(req) {
      return onHttpRequest({
        type: MSG.HTTP_REQUEST,
        uuid: req.uuid,
        method: req.method,
        url: req.url,
        headers: req.headers ?? {},
        body: req.body ?? null,
        clientId: req.clientId ?? null,
      });
    },
    getShinyResourcePaths() {
      return getResourcePaths();
    },
    stop() {
      onStop();
    },
  };
}

export { Comlink };
