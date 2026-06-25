import { evalR } from "./rWasmBootstrap.js";

export const LATER_WASM_TEST_R = "/laterWasmTest.R";

const ASYNC_IDLE_MS = 25;
const CROSS_IDLE_MS = 5;

/**
 * @param {string} moduleUrl
 * @param {object} Module
 */
export async function mountLaterWasmTest(moduleUrl, Module) {
  const url = new URL("laterWasmTest.R", new URL(".", moduleUrl));
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error(`Failed to fetch ${url.href}: HTTP ${res.status}`);
  }
  Module.FS.writeFile(LATER_WASM_TEST_R, await res.text());
}

function yieldMs(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

/**
 * Run later WASM smoke tests. Must run while the worker is otherwise idle so
 * emscripten_set_timeout callbacks can fire between evalR sessions.
 *
 * @param {string} moduleUrl
 * @param {object} Module
 * @param {(msg: string) => void} [log]
 */
export async function runLaterWasmTests(moduleUrl, Module, log = console.info) {
  await mountLaterWasmTest(moduleUrl, Module);

  log("[later-wasm] === later WASM smoke tests ===");

  evalR(Module, `source("${LATER_WASM_TEST_R}")`);
  evalR(Module, "forge_later_bootstrap_tests_load()");

  log("[later-wasm] sync tests (later + run_now in one evalR)…");
  evalR(Module, "forge_later_bootstrap_tests_sync()");

  log(`[later-wasm] async timer test (${ASYNC_IDLE_MS}ms JS idle, no run_now)…`);
  evalR(Module, "forge_later_bootstrap_tests_async_begin()");
  await yieldMs(ASYNC_IDLE_MS);
  evalR(Module, "forge_later_bootstrap_tests_async_finish_timer()");

  log(`[later-wasm] cross-eval test (${CROSS_IDLE_MS}ms idle, then run_now)…`);
  evalR(Module, "forge_later_bootstrap_tests_cross_begin()");
  await yieldMs(CROSS_IDLE_MS);
  evalR(Module, "forge_later_bootstrap_tests_cross_finish()");

  log("[later-wasm] === all later WASM smoke tests passed ===");
}
