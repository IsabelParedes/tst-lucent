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

/** Message types on Module.httpuv.channel (R ↔ JS bridge). */
export const CHANNEL = {
  HTTP_REQUEST: "httpuv_http_request",
  TCP_RESPONSE: "httpuv_tcp_response",
  WS_OPEN: "httpuv_ws_open",
  WS_MESSAGE: "httpuv_ws_message",
  WS_CLOSE: "httpuv_ws_close",
  WS_RESPONSE: "httpuv_ws_response",
  STDIN: "stdin",
};

/** R option names registered by httpuv::startServer(). */
export const HTTPUV_OPTIONS = {
  ON_REQUEST: "httpuv_onRequest",
  ON_WS_OPEN: "httpuv_onWSOpen",
  ON_WS_MESSAGE: "httpuv_onWSMessage",
  ON_WS_CLOSE: "httpuv_onWSClose",
};
