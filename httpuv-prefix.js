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

/**
 * @returns {string}
 */
export function getSessionPrefix() {
  return `${getShinyPrefix()}__session__/`;
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
