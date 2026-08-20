library(R6)
library(whisker)
source("modules/menuImport/importDOE/launcher/launcherConst.R", local = TRUE)
source("modules/menuImport/importDOE/launcher/shiny2LauncherHandler.R", local = TRUE)
source("modules/menuImport/importDOE/launcher/simulatorsConnection.R", local = TRUE)
source("modules/menuImport/importDOE/launcher/simulationsConnection.R", local = TRUE)
source("modules/menuImport/importDOE/launcher/runsSet.R", local = TRUE)
source("modules/menuImport/importDOE/launcher/run.R", local = TRUE)
source("modules/menuImport/importDOE/launcher/localRun.R", local = TRUE)

launcherServer <- list(
        LAUNCHER_SERVER_JSON = if (nchar(Sys.getenv("COMSPEC")) == 0) "modules/menuImport/importDOE/launcher/launcherServer_linux.json" else "modules/menuImport/importDOE/launcher/launcherServer.json",
        runsSets = list()
)

tryCatch({
    launcherServer$launcherConfig <- jsonlite::read_json(launcherServer$LAUNCHER_SERVER_JSON)
},
error = function(err) {
    logger$print("launcherConfig init failed")
    logger$print(err)
})

launcherServer$searchMustacheInputs <- function(simulatorIdsOrEmpty) {
    foundInputs <- list()
    simulatorIds <- simulatorIdsOrEmpty
    if (length(simulatorIdsOrEmpty) == 0) {
        simulatorIds <- unlist(lapply(launcherServer$launcherConfig$simulator_configs, function(config) config$id))
        simulatorIds <- if (is.null(simulatorIds)) list() else simulatorIds
    }

    for (simulatorId in simulatorIds) {
        simulatorConfig <- purrr::detect(launcherServer$launcherConfig$simulator_configs, function(config) config$id == simulatorId)
        if (!is.null(simulatorConfig)) {
            foundInputs[[toString(simulatorId)]] <- launcherServer$mustachesInParamFiles(simulatorConfig)
        }
        else {
            logger$print(paste("'searchMustacheInputs' received for simulator ", simulatorId, " but is unknown"));
        }
    }
    return(foundInputs)
}

launcherServer$mustachesInParamFiles <- function(simulatorConfig) {
    mustacheSet <- list()
    for (paramFileName in simulatorConfig$simulation_param_files) {
        tryCatch({
            mustacheSet <- launcherServer$mustachesInParamFile(simulatorConfig, paramFileName)
        },
        error = function(err) {
            logger$print(paste("error processing:", paramFileName))
            logger$print(paste("'searchMustacheInputs' received for simulator ", simulatorConfig$id, " but failed."))
            logger$print(err)
        })
    }
    return(unique(mustacheSet))
}

launcherServer$mustachesInParamFile <- function(simulatorConfig, paramFileName) {
    fullParamFileName <- file.path(simulatorConfig$simulation_dir, paramFileName)
    fileContent <- readChar(fullParamFileName, file.info(fullParamFileName)$size)
    mustacheSet <- unlist(lapply(unique(stringr::str_extract_all(fileContent, "\\{\\{([[:alnum:]_]+)\\}\\}")[[1]]), function(e) substr(e, 3, nchar(e) - 2)))
    mustacheSet <- if (is.null(mustacheSet)) list() else mustacheSet
    return(mustacheSet)
}

launcherServer$loginPasswordNeeded <- function(logPwdNeededArgs) {
    tryCatch({
        if (logPwdNeededArgs$host != "localhost") {
            success <- FALSE
            needed <- "distant host not yet implemented"
        }
        else {
            # loginPassword is not needed
            success <- TRUE
            needed <- FALSE
        }
    },
    error = function(error) {
        success <- FALSE
        needed <- error
    })
    return(list(success = success, answer = needed))
}

launcherServer$checkSimulatorConfig <- function(simulatorConfId) {
    simulatorConfig <- purrr::detect(launcherServer$launcherConfig$simulator_configs, function(config) config$id == simulatorConfId)
    if (is.null(simulatorConfig)) {
        stop(paste(simulatorConfId, "is an unknown simulator configuration"))
    }
    return(simulatorConfig)
}

