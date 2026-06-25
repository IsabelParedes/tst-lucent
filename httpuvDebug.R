#' httpuv / Shiny-Forge debug helpers (sourced when ?httpuvDebug=1).

.forge_httpuv_debug_msg <- function(...) {
  message("[httpuv-debug] ", paste0(..., collapse = ""))
}

.forge_httpuv_has_server <- function() {
  requireNamespace("shiny", quietly = TRUE) &&
    !is.null(shiny::getShinyOption("server", default = NULL))
}

#' No-op placeholder (function wrapping breaks on WASM; use JS httpuv-debug logs).
forge_httpuv_install_traces <- function() {
  .forge_httpuv_debug_msg("using JS httpuv-debug logs (no R trace hooks)")
  invisible(TRUE)
}

#' Log later loop + Shiny state (call from worker after a pushed request).
forge_httpuv_debug_state <- function(label = "") {
  prefix <- if (nzchar(label)) paste0("[", label, "] ") else ""
  .forge_httpuv_debug_msg(prefix, "loop_empty=", later::loop_empty())
  .forge_httpuv_debug_msg(prefix, "next_op_secs=", later::next_op_secs())
  if (requireNamespace("shiny", quietly = TRUE)) {
    .forge_httpuv_debug_msg(prefix, "shiny::isRunning()=", shiny::isRunning())
    .forge_httpuv_debug_msg(prefix, "has_server=", .forge_httpuv_has_server())
    paths <- tryCatch(shiny::resourcePaths(), error = function(e) character())
    .forge_httpuv_debug_msg(prefix, "resourcePaths=", paste(names(paths), collapse = ", "))
  }
  wrapper <- get0("active_app_wrapper", envir = httpuv:::.globals, ifnotfound = NULL)
  if (!is.null(wrapper)) {
    .forge_httpuv_debug_msg(prefix, "staticPaths=", paste(names(wrapper$staticPaths), collapse = ", "))
  }
  invisible(NULL)
}

#' Drain the later loop once (diagnostic; simulates one serviceApp(NA) tick).
forge_httpuv_debug_pump <- function(label = "") {
  prefix <- if (nzchar(label)) paste0("[", label, "] ") else ""
  .forge_httpuv_debug_msg(prefix, "run_now before loop_empty=", later::loop_empty())
  if (.forge_httpuv_has_server()) {
    shiny::serviceApp(NA)
  } else {
    later::run_now(0, all = FALSE)
  }
  .forge_httpuv_debug_msg(prefix, "run_now after loop_empty=", later::loop_empty())
  invisible(NULL)
}
