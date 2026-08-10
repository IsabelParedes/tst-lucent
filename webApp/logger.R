#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

logger <- list()

logger$MAX_LOG_DIR_SIZE <- 50*1024*1024
logger$SAVED_LOG_DIR <- if (nchar(Sys.getenv("APPDATA")) == 0) "saved_logs" else file.path(Sys.getenv("APPDATA"), "LAGUN", "saved_logs")
logger$LOG_FILE_PREFIX <- "log"
logger$oSinkNumber <- NULL

logger$buildCurFileDay <- function() {
  format(Sys.time(), "%Y-%m-%d")
}

logger$lastFileDay <- logger$buildCurFileDay()

if (!file.exists(logger$SAVED_LOG_DIR)) {
  dir.create(logger$SAVED_LOG_DIR, recursive = T)
}

logger$setupSink <- function() {
  logger$unsetSink()

  logger$oSinkNumber <<- sink.number(type = "output")
  logFileName <- paste0(logger$SAVED_LOG_DIR, "/", logger$LOG_FILE_PREFIX, format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"))
  logger$logFile <<- file(logFileName, open = "wt")

  # Divert R 'output' stream to the log file AND to the current output stream (like the Unix program `tee`)
  sink(file = logger$logFile, type = "output", split = T)
  # Divert R 'message' stream to the log file ('split' is not available)
  sink(file = logger$logFile, type = "message")
}

logger$unsetSink <- function() {
  if (!is.null(logger$oSinkNumber)) {
    oSinkNumber <- logger$oSinkNumber
    if (sink.number(type = "output") == oSinkNumber + 1) {
      sink(NULL, type = "output")
      sink(NULL, type = "message")
      close(logger$logFile)
    }
    else {
      logger$print(paste("'logger$unsetSink failed for 'output'',", sink.number(type = "output"),  "!=", oSinkNumber + 1))
    }
    logger$oSinkNumber <<- sink.number(type = "output")
  }
}

logger$orderedInfos <- function() {
  infos <- file.info(list.files(path = logger$SAVED_LOG_DIR, pattern = "log*", full.names = TRUE))
  dateTimeOrder <- order(timeDate::as.timeDate(infos$ctime), decreasing = TRUE)
  return(infos[dateTimeOrder, ])
}

logger$dailyRolling <- function() {
  # Check if log files comply with the limit 'MAX_LOG_DIR_SIZE'
  orderedInfos <- logger$orderedInfos()
  sizeCumsum <- cumsum(orderedInfos[["size"]])
  toRemove <- rownames(orderedInfos)[sizeCumsum > logger$MAX_LOG_DIR_SIZE]
  if (length(toRemove) != 0) {
    logger$www()
    print(paste(length(toRemove), "log file(s) to remove:", toString(toRemove)))
    unlink(toRemove)
  }

  # Check that the log file name corresponds to the today's day, and if not, create a new one
  curFileDay <- logger$buildCurFileDay()
  if (logger$lastFileDay != curFileDay) {
    logger$lastFileDay <<- curFileDay
    logger$setupSink()
  }
}

logger$callerInfo <- function(level = 1) # https://stackoverflow.com/q/59537482/684229
{
  x <- .traceback(x = level + 1)
  i <- 1
  repeat { # loop for subexpressions case; find the first one with source reference
    srcref <- getSrcref(x[[i]])
    if (is.null(srcref)) {
      if (i < length(x)) {
        i <- i + 1
        next;
      } else {
        return(NULL)
      }
    }
    srcloc <- list(fun = getSrcref(x[[i+1]]), file = getSrcFilename(x[[i]]), line = getSrcLocation(x[[i]]))
    break;
  }
  return(srcloc)
}

logger$www <- function(level = 1) {
  callerInfo <- logger$callerInfo(level)
  session <- shiny::getDefaultReactiveDomain()
  print(paste("<Who:", session$token, "Where:", ifelse(is.null(callerInfo), "-", paste0(callerInfo$file, "#", callerInfo$line)), "When:", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss>")))
}

logger$print <- function(...) {
  if (!isTRUE(getOption("shiny.testmode"))) {
    logger$dailyRolling()
  }
  logger$www(2)
  print(...)
}