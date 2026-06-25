#' later WASM smoke tests for Shiny-Forge (run before Shiny/httpuv startup).
.forge_later_test_env <- new.env(parent = emptyenv())

.forge_later_reset <- function() {
  rm(list = ls(envir = .forge_later_test_env), envir = .forge_later_test_env)
  invisible(NULL)
}

.forge_later_pass <- function(name) {
  message("[later-wasm] PASS: ", name)
  invisible(TRUE)
}

.forge_later_fail <- function(name, detail = "") {
  msg <- paste0("[later-wasm] FAIL: ", name, if (nzchar(detail)) paste0(" (", detail, ")") else "")
  message(msg)
  stop(msg)
}

#' Load later and verify the package initialised.
forge_later_bootstrap_tests_load <- function() {
  suppressPackageStartupMessages(library(later))
  if (!exists("run_now", envir = asNamespace("later"), inherits = FALSE)) {
    .forge_later_fail("library", "run_now missing")
  }
  .forge_later_pass("library")
}

#' Tests that run entirely inside one evalR session.
forge_later_bootstrap_tests_sync <- function() {
  .forge_later_reset()

  later::later(function() {
    .forge_later_test_env$sync_flag <- TRUE
  }, delay = 0)

  if (isTRUE(later::loop_empty())) {
    .forge_later_fail("sync_loop_empty", "callback not queued before run_now")
  }

  later::run_now(0, all = TRUE)

  if (!isTRUE(.forge_later_test_env$sync_flag)) {
    .forge_later_fail("sync_run_now", "callback did not run")
  }
  .forge_later_pass("sync_run_now")

  .forge_later_reset()
  later::later(function() {
    .forge_later_test_env$one_flag <- TRUE
  }, delay = 0)
  later::later(function() {
    .forge_later_test_env$two_flag <- TRUE
  }, delay = 0)

  later::run_now(0, all = FALSE)
  if (!isTRUE(.forge_later_test_env$one_flag) || isTRUE(.forge_later_test_env$two_flag)) {
    .forge_later_fail("run_now_all_false", "expected exactly one callback")
  }
  later::run_now(0, all = FALSE)
  if (!isTRUE(.forge_later_test_env$two_flag)) {
    .forge_later_fail("run_now_all_false", "second callback did not run")
  }
  .forge_later_pass("run_now_all_false")

  invisible(TRUE)
}

#' Schedule a callback; JS must idle before forge_later_bootstrap_tests_async_finish_timer().
forge_later_bootstrap_tests_async_begin <- function() {
  .forge_later_reset()
  later::later(function() {
    .forge_later_test_env$timer_flag <- TRUE
    message("[later-wasm] timer callback executed")
  }, delay = 0)
  if (isTRUE(later::loop_empty())) {
    .forge_later_fail("async_begin", "expected pending callback after later()")
  }
  .forge_later_pass("async_begin")
  invisible(TRUE)
}

#' Check whether the emscripten timer fired without an explicit run_now().
forge_later_bootstrap_tests_async_finish_timer <- function() {
  if (!isTRUE(.forge_later_test_env$timer_flag)) {
    .forge_later_fail(
      "async_emscripten_timer",
      "callback did not run after JS idle (timer or run_now path broken)"
    )
  }
  .forge_later_pass("async_emscripten_timer")
  invisible(TRUE)
}

#' Schedule like httpuv_push_on_later; JS idles; finish with run_now in a new evalR.
forge_later_bootstrap_tests_cross_begin <- function() {
  .forge_later_reset()
  later::later(function() {
    .forge_later_test_env$cross_flag <- TRUE
  }, delay = 0)
  invisible(TRUE)
}

forge_later_bootstrap_tests_cross_finish <- function() {
  later::run_now(0, all = TRUE)
  if (!isTRUE(.forge_later_test_env$cross_flag)) {
    .forge_later_fail("cross_eval_run_now", "callback did not run after idle + run_now")
  }
  .forge_later_pass("cross_eval_run_now")
  invisible(TRUE)
}
