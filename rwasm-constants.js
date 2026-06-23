/** Message types between the main page and the R.wasm dedicated worker. */
export const RWASM = {
  INIT: "rwasm_init",
  READY: "rwasm_ready",
  EVAL: "rwasm_eval",
  EVAL_RESULT: "rwasm_eval_result",
  WRITE_WEB_APP: "rwasm_write_web_app",
  PUMP_START: "rwasm_pump_start",
  PUMP_STOP: "rwasm_pump_stop",
  STOP_APP: "rwasm_stop_app",
  COMLINK_PORT: "rwasm_comlink_port",
  COMLINK_READY: "rwasm_comlink_ready",
  LOG: "rwasm_log",
  ERROR: "rwasm_error",
};
