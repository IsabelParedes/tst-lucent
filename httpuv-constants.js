/** Virtual URL prefix for Shiny apps served through the httpuv service worker. */
export const SHINY_PREFIX = "/shiny/";

/** Session endpoints for the virtual Shiny socket (fetch-based, no WebSocket). */
export const SESSION_PREFIX = `${SHINY_PREFIX}__session__/`;

/** Max time the service worker waits for the main page to answer a request. */
export const REQUEST_TIMEOUT_MS = 30_000;

/** Message types exchanged between the service worker and the main page. */
export const MSG = {
  REGISTER_HOST: "httpuv_register_host",
  HTTP_REQUEST: "httpuv_http_request",
  HTTP_RESPONSE: "httpuv_http_response",
  WS_PUSH: "httpuv_ws_push",
  STOP: "httpuv_stop",
};
