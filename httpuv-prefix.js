/** @type {string | null} */
let shinyPrefix = null;

/**
 * Resolve the virtual Shiny app prefix from a module script URL.
 * e.g. /site/runApp.js → /site/shiny/
 * @param {string | URL} fromUrl
 * @returns {string}
 */
export function resolveShinyPrefix(fromUrl) {
  const prefix = new URL("shiny/", fromUrl).pathname;
  return prefix.endsWith("/") ? prefix : `${prefix}/`;
}

/**
 * @param {string} prefix
 */
export function setShinyPrefix(prefix) {
  shinyPrefix = prefix.endsWith("/") ? prefix : `${prefix}/`;
}

/**
 * @returns {string}
 */
export function getShinyPrefix() {
  if (!shinyPrefix) {
    throw new Error("Shiny prefix not initialized");
  }
  return shinyPrefix;
}

/** Session directory name under the Shiny virtual app prefix. */
export const SESSION_DIR = "__session__";

/** Host → SW outbound path (fetch fallback when controller.postMessage unavailable). */
export const HOST_DIR = "__host__";

/**
 * @param {string | URL} fromUrl
 * @returns {string}
 */
export function resolveSessionPrefix(fromUrl) {
  return `${resolveShinyPrefix(fromUrl)}${SESSION_DIR}/`;
}

/**
 * @returns {string}
 */
export function getSessionPrefix() {
  return `${getShinyPrefix()}${SESSION_DIR}/`;
}

/**
 * @param {string} urlString
 * @param {string} shinyPrefix
 * @returns {{ action: string, handle: string | null } | null}
 */
export function parseSessionAction(urlString, shinyPrefix) {
  const url = new URL(urlString);
  const sessionPrefix = `${shinyPrefix}${SESSION_DIR}/`;
  if (!url.pathname.startsWith(sessionPrefix)) {
    return null;
  }
  const action = url.pathname.slice(sessionPrefix.length).replace(/\/$/, "");
  if (!["open", "send", "recv", "close"].includes(action)) {
    return null;
  }
  return {
    action,
    handle: url.searchParams.get("handle"),
  };
}

/**
 * @param {string} urlString
 * @param {string} shinyPrefix
 * @returns {boolean}
 */
export function isHostPushUrl(urlString, shinyPrefix) {
  const url = new URL(urlString);
  return url.pathname === `${shinyPrefix}${HOST_DIR}/push`;
}

/**
 * Build a same-origin URL under the Shiny virtual app prefix.
 * @param {string} [subpath]
 * @param {string | URL} [fromUrl] defaults to runApp.js location when called from main page
 * @returns {string}
 */
export function shinyAppUrl(subpath = "", fromUrl = import.meta.url) {
  const base = new URL("shiny/", fromUrl);
  return new URL(subpath.replace(/^\//, ""), base).href;
}
