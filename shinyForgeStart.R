#' Package-local state for Shiny-Forge restarts (not shiny:::.globals).
.forge_env <- new.env(parent = emptyenv())

.shiny_globals <- function() {
  get(".globals", envir = asNamespace("shiny"))
}

#' Non-blocking Shiny startup for Shiny-Forge (JS pump drives serviceApp).
#'
#' @param appDir Application directory or app object passed to as.shiny.appobj().
#' @param port Virtual listen port (browser routing uses /shiny/, not this port).
#' @param host Virtual listen host.
shiny_forge_start_app <- function(
  appDir = "webApp",
  port = 3838L,
  host = "127.0.0.1"
) {
  if (shiny::isRunning()) {
    shiny_forge_stop_app()
  }

  # Newer shiny exports startApp(appDir, ...) for non-blocking mode.
  if (exists("startApp", envir = asNamespace("shiny"), inherits = FALSE)) {
    fn <- get("startApp", envir = asNamespace("shiny"))
    if ("appDir" %in% names(formals(fn))) {
      return(invisible(fn(
        appDir,
        launch.browser = FALSE,
        port = port,
        host = host,
        quiet = TRUE
      )))
    }
  }

  suppressPackageStartupMessages(library(shiny))

  options(
    warn = max(1, getOption("warn", default = 1)),
    pool.scheduler = shiny:::scheduleTask,
    shiny.launch.browser = FALSE
  )

  appParts <- shiny::as.shiny.appobj(appDir)
  shiny:::initCurrentAppState(appParts)
  shiny::shinyOptions(appToken = shiny:::createUniqueId(8))

  if (is.null(shiny::getShinyOption("cache", default = NULL))) {
    shiny::shinyOptions(cache = cachem::cache_mem(max_size = 200 * 1024^2))
  }

  shiny:::applyCapturedAppOptions(appParts$appOptions)

  shiny:::workerId("")
  shiny::shinyOptions(testmode = FALSE)
  shiny:::setShowcaseDefault(0)

  if (!is.null(appParts$onStart)) {
    appParts$onStart()
  }

  server <- shiny:::startApp(appParts, port, host, quiet = TRUE)
  shiny::shinyOptions(server = server)

  globals <- .shiny_globals()
  globals$reterror <- NULL
  globals$retval <- NULL
  globals$stopped <- FALSE

  .forge_env$server <- server
  .forge_env$onStop <- appParts$onStop

  invisible(server)
}

shiny_forge_stop_app <- function() {
  if (!shiny::isRunning()) {
    return(invisible(NULL))
  }

  server <- shiny::getShinyOption("server", default = NULL)
  if (is.null(server)) {
    server <- .forge_env$server
  }

  shiny::stopApp()

  if (!is.null(server)) {
    httpuv::stopServer(server)
  }

  if (!is.null(.forge_env$onStop)) {
    .forge_env$onStop()
  }

  shiny:::clearCurrentAppState()
  shiny:::handlerManager$clear()

  .forge_env$server <- NULL
  .forge_env$onStop <- NULL
  invisible(NULL)
}
