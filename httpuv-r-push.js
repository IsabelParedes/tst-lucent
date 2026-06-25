import { CHANNEL } from "./httpuv-constants.js";

/**
 * @param {unknown} value
 * @returns {string}
 */
export function jsonForR(value) {
  return JSON.stringify(JSON.stringify(value));
}

/**
 * @param {Record<string, unknown>} req
 * @returns {Record<string, unknown>}
 */
export function serializeReqForR(req) {
  const out = { ...req };
  const body = out.body;
  if (body instanceof ArrayBuffer) {
    out.body = Array.from(new Uint8Array(body));
  } else if (ArrayBuffer.isView(body)) {
    out.body = Array.from(new Uint8Array(body.buffer, body.byteOffset, body.byteLength));
  }
  return out;
}

/**
 * @param {unknown} message
 * @returns {number[] | string}
 */
function serializeWsMessageForR(message) {
  if (message instanceof ArrayBuffer) {
    return Array.from(new Uint8Array(message));
  }
  if (ArrayBuffer.isView(message)) {
    return Array.from(new Uint8Array(message.buffer, message.byteOffset, message.byteLength));
  }
  if (Array.isArray(message)) {
    return message;
  }
  return String(message ?? "");
}

/**
 * @param {string} url
 * @returns {boolean}
 */
export function isLikelyStaticAsset(url) {
  return /\.(js|css|png|jpe?g|gif|svg|woff2?|ico|map)(\?|$)/i.test(url) ||
    /\/shiny\/(shared|jquery|bootstrap|htmltools|shiny-)/i.test(url);
}

/**
 * Build an R expression that pushes a channel message into httpuv handlers.
 * @param {{ type: string }} msg
 * @returns {string}
 */
export function channelMessageToRExpr(msg) {
  switch (msg.type) {
    case CHANNEL.HTTP_REQUEST: {
      const payload = {
        uuid: msg.uuid,
        method: msg.method,
        url: msg.url,
        headers: msg.headers ?? {},
        body: msg.body ?? null,
      };
      const msgJson = jsonForR(payload);

      // Static assets: handle synchronously in evalR. Reading from the VFS is
      // fast; scheduling on later competes with serviceNonBlocking and often
      // never fires before the SW timeout.
      if (isLikelyStaticAsset(msg.url)) {
        return `local({
  msg <- jsonlite::fromJSON(${msgJson}, simplifyVector=FALSE)
  wrapper <- get("active_app_wrapper", envir=httpuv:::.globals)
  if (is.null(wrapper)) {
    if (!is.null(msg$uuid)) {
      httpuv:::httpuv_write_tcp_response(
        msg$uuid,
        list(
          status = 503L,
          headers = list(\`Content-Type\` = "text/plain"),
          body = "httpuv: no server running"
        )
      )
    }
  } else {
    httpuv:::httpuv_handle_http_request(wrapper, msg)
  }
  invisible(TRUE)
})`;
      }

      // Dynamic HTML: schedule on later (never block evalR on a full render).
      return `local({
  msg <- jsonlite::fromJSON(${msgJson}, simplifyVector=FALSE)
  wrapper <- get("active_app_wrapper", envir=httpuv:::.globals)
  later::later(function() {
    if (is.null(wrapper)) {
      if (!is.null(msg$uuid)) {
        httpuv:::httpuv_write_tcp_response(
          msg$uuid,
          list(
            status = 503L,
            headers = list(\`Content-Type\` = "text/plain"),
            body = "httpuv: no server running"
          )
        )
      }
    } else {
      httpuv:::httpuv_handle_http_request(wrapper, msg)
    }
  }, delay = 0)
  invisible(TRUE)
})`;
    }

    case CHANNEL.WS_OPEN: {
      const reqPart = msg.req
        ? `jsonlite::fromJSON(${jsonForR(serializeReqForR(msg.req))}, simplifyVector=FALSE)`
        : "NULL";
      return `httpuv::httpuv_push_ws_open(${jsonForR(msg.handle)}, ${reqPart})`;
    }

    case CHANNEL.WS_MESSAGE: {
      const binary = Boolean(msg.binary);
      const messagePart = binary
        ? `jsonlite::fromJSON(${jsonForR(serializeWsMessageForR(msg.message))}, simplifyVector=FALSE)`
        : jsonForR(String(msg.message ?? ""));
      return `httpuv::httpuv_push_ws_message(${jsonForR(msg.handle)}, ${binary ? "TRUE" : "FALSE"}, ${messagePart})`;
    }

    case CHANNEL.WS_CLOSE:
      return `httpuv::httpuv_push_ws_close(${jsonForR(msg.handle)})`;

    default:
      throw new Error(`unsupported inbound channel message type: ${msg.type}`);
  }
}
