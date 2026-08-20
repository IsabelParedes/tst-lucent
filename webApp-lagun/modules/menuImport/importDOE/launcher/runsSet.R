QUEUE_STATE <- list(
    waiting = "waiting", 
    inProgress = "inProgress", 
    out = "out"
)

RunsSet <- R6Class("RunsSet",
  public = list(

    simulationsConnection = NULL,

    runs = NULL,

    cachedOrderedRuns = NULL,

    nextRunId = 0,

    initialize = function(simulationsConnection = NULL) {
      self$simulationsConnection <- simulationsConnection
      self$runs <- list()
    },

    getOrderedRuns = function() {
        if (is.null(self$cachedOrderedRuns)) {
            runIds <- names(self$runs)
            runIds <- if (is.null(runIds)) list() else runIds
            self$cachedOrderedRuns <- self$runs[order(runIds)]
        }
        return(self$cachedOrderedRuns)
    },

    parseSimuInfoJson = function(runDirName) {
        tryCatch({
            fullSimuInfoJson <- file.path(runDirName, launcherConst$SIMU_INFO_JSON)
            if (!file.exists(fullSimuInfoJson)) {
                return(NULL)
            }
            simuInfo <- jsonlite::read_json(fullSimuInfoJson)
            if (!is.null(simuInfo$simulatorConfig)) {
                simulatorConfig <- Filter(function(conf) { conf$config_name == simuInfo$simulatorConfig }, launcherServer$launcherConfig$simulator_configs)
                if (length(simulatorConfig) != 0) {
                    return(list(
                        paramNames = names(simuInfo$xValues), 
                        paramValues = as.vector(unlist(simuInfo$xValues)),
                        simulatorConfigId = simulatorConfig$id,
                        resultsFileName = simulatorConfig$result_file_name
                    ))
                }
            }
            return(list(
                paramNames = names(simuInfo$xValues), 
                paramValues = as.vector(unlist(simuInfo$xValues)),
                simulatorConfigId: -1,
                resultsFileName: NULL
            ))
        },
        error = function(err) {
            logger$print(err)
        })
    },

    initFromStoringDir = function() {
        self$runs <- list()
        self$nextRunId <- 0
        self$cachedOrderedRuns <- NULL

        tryCatch({
            storingDir <- self$getStoringDir()
            if (file.exists(storingDir)) {
                fileNames <- list.files(storingDir)
                runDirNames <- Filter(function(f) { stringr::str_detect("^run\\d+$", f) }, fileNames)
                for (runDirName in runDirNames) {
                    initFromRunXDir(storingDir, runDirName)
                }
                if (self$nextRunId > 0) {
                    logger$print(paste("Number of run directories used in", storingDir, "to populate runSet:", length(self$runs)))
                }
            }
        },
        error = function(err) {
            logger$print("initFromStoringDir failed")
            logger$print(err)
        })
    },

    initFromRunXDir = function(storingDir, runDirName) {
        tryCatch({
            fullRunDirName <- file.path(storingDir, runDirName)
            # Deal with 'SIMU_INFO_JSON'
            simuInfo <- self$parseSimuInfoJson(fullRunDirName)
            # If 'simuInfo' is not null, process 'runX' file
            if (!is.null(simuInfo)) {
                runId <- as.numeric(substr(runDirName, nchar("run") + 1, nchar(runDirName))) - 1
                run <- Run$new(
                    id = runId,
                    status = launcherConst$STATUS$ready,
                    simulatorId = simuInfo$simulatorConfigId,
                    paramNames = simuInfo$paramNames,
                    paramValues = simuInfo$paramValues,
                    result = ""
                )

                run$status <- launcherConst$STATUS$configured

                # Deal with result file
                if (!is.null(simuInfo$resultsFileName)) {
                    fullResultFileName <- file.path(fullRunDirName, simuInfo$resultsFileName)
                    if (file.exists(fullResultFileName)) {
                        run$result <- readChar(fullResultFileName, file.info(fullResultFileName)$size)
                        if (!is.null(run$result)) {
                            run$status <- launcherConst$STATUS$ended
                        }
                    }
                }

                self$runs[[toString(run$id)]] <- run
                if (self$nextRunId <= run$id) {
                    self$nextRunId <- run$id + 1
                }
            }
        },
        error = function(err) {
            logger$print(paste0("initFromStoringDir '", runDirName, "' raises an exception"))
            logger$print(err)
        })
    },

    addRuns = function(doe) {
        self$cachedOrderedRuns <- NULL
        addedRuns <- list()
        for(i in seq_along(doe$mat)){
            run <- Run$new(
                id = self$nextRunId,
                status = launcherConst$STATUS$ready,
                simulatorId = -1,
                paramNames = doe$paramNames,
                paramValues = doe$mat[[i]],
                result = ""
            )
            self$runs[[toString(run$id)]] <- run
            addedRuns <- append(addedRuns, list(run))
            self$nextRunId <- self$nextRunId + 1
        }
        return(addedRuns)
    },

    setSimulator = function(runIds, simulatorId) {
        for (runId in runIds) {
            run <- self$runs[[toString(runId)]]
            if (!is.null(run)) {
                run$simulatorId = simulatorId
            }
            else {
                logger$print(paste("setSimulator, runId:", runId, "not in", names(self$runs)))
            }
        }
    },

    execute = function(runActionArg) {
        if (runActionArg$actionId == launcherConst$RUN_ACTION$cancel) {
            self$cancel(runActionArg$runIds)
        }
        else if (runActionArg$actionId == launcherConst$RUN_ACTION$enDisable) {
            self$enDisable(runActionArg$runIds)
        }
        else {
            # Determine which runs are to execute
            runsToExecute <- self$checkRunToExecute(runActionArg$runIds)

            # Set 'Waiting' status to runs which are to execute
            for (r in runsToExecute) {
                r$actionId <- runActionArg$actionId
                r$queueState <- QUEUE_STATE$waiting
                r$status <- launcherConst$STATUS$waiting
            }
            runToExecuteIds <- unlist(lapply(runsToExecute, function(r) r$id))
            runToExecuteIds <- if (is.null(runToExecuteIds)) list() else runToExecuteIds
            self$emitSimulationsEvent(list(
                ids = runToExecuteIds,
                type = launcherConst$STATUS$waiting
            ))

            self$processRunsQueues()
        }
    },

    emitSimulationsEvent = function(launcherEvent) {
        self$simulationsConnection$emitLauncherEvent(launcherEvent)
        # for (optimizer in self$optimizers) {
        #     optimizer$checkIfTodoCompleted()
        # }
    },

    cancel = function(runIds) {
        for (runId in runIds) {
            run <- self$runs[[toString(runId)]]
            if (run && run$status == launcherConst$STATUS$waiting) {
                tryCatch({
                    self$unqueue(run$id)
                    # Send a 'ready' event
                    self$setReady(run$id)
                },
                error = function(error) {
                    # Send a 'onerror' event containing an error description
                    self$setOnError(run$id, error)
                })
            }
        }
    },

    enDisable = function(runIds) {
        # Determine which runs are to invalidate
        runsToEnDisable <- lapply(runIds, function(runId) self$runs[[toString(runId)]])
        runsToEnDisable <- if (is.null(runsToEnDisable)) list() else runsToEnDisable

        runsToDisable <- Filter(function(r) { !is.null(r) && r$status %in% c(launcherConst$STATUS$ready, launcherConst$STATUS$ended, launcherConst$STATUS$onerror) }, runsToEnDisable)
        runsToEnable <- Filter(function(r) { !is.null(r) && r$status == launcherConst$STATUS$disabled }, runsToEnDisable)

        for (r in runsToDisable) {
            self$setDisabled(r$id)
        }
        for (r in runsToEnable) {
            self$setReady(r$id)
        }
    },

    setReady = function(runId) {
        self$setStatus(runId, launcherConst$STATUS$ready)
        # Emit an 'ready launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$ready
        ))
    },

    setConfiguring = function(runId) {
        self$setStatus(runId, launcherConst$STATUS$configuring)
        # Emit an 'configuring launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$configuring
        ))
    },

    setConfigured = function(runId) {
        self$setStatus(runId, launcherConst$STATUS$configured)
        # Emit an 'configured launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$configured
        ))
    },

    setRunning = function(runId) {
        self$setStatus(runId, launcherConst$STATUS$running)
        # Emit a 'running launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$running
        ))
    },

    setLoading = function(runId) {
        self$setStatus(runId, launcherConst$STATUS$loading)
        # Emit a 'loading launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$loading
        ))
    },

    setEnded = function(runId, result) {
        self$setResult(runId, result)
        self$setStatus(runId, launcherConst$STATUS$ended)
        # Emit an 'ended launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$ended,
            data = result
        ))
    },

    setOnError = function(runId, error) {
        logger$print(paste0("Simulation ", runId, ": ", error))
        self$setResult(runId, error)
        self$setStatus(runId, launcherConst$STATUS$onerror)
        # Emit an 'onerror launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$onerror,
            data = error
        ))
    },

     setDisabled = function(runId) {
        self$setStatus(runId, launcherConst$STATUS$disabled)
        # Emit an 'disabled launcher event' to all connected sockets
        self$emitSimulationsEvent(list(
            id = runId,
            type = launcherConst$STATUS$disabled
        ))
    },

    tryRun = function(run) {
        simulatorConfig <- self$checkSimulatorConfig(run$simulatorId)
        clusterSize <- if(simulatorConfig$cluster_size > 0 ) simulatorConfig$cluster_size else 1

        inProgressRuns <- Filter(function(r) { !is.na(r$queueState) && r$queueState == QUEUE_STATE$inProgress && r$simulatorId == run$simulatorId && r$actionId == run$actionId }, self$runs)
        if (length(inProgressRuns) < clusterSize && !is.na(run$queueState) && run$queueState == QUEUE_STATE$waiting) {
            tryCatch({
                run$queueState <- QUEUE_STATE$inProgress
                execRun <- self$executableRun(run$id)
                execRun$execute(run$actionId)
            },
            error = function(err) {
                run$queueState <- NA
                self$setOnError(run$id, err)
            })
        }
    },

    processRunsQueues = function() {
        # Determine which runs are queued and waiting
        queuedRuns <- Filter(function(r) { !is.na(r$queueState) && r$queueState == QUEUE_STATE$waiting }, self$getOrderedRuns())

        for (queuedRun in queuedRuns) {
            tryCatch({
                self$tryRun(queuedRun)
            },
            error = function(err) {
                self$setOnError(queuedRun$id, err)
            })
        }
    },

    unqueue = function(runId) {
        run <- self$runs[[toString(runId)]]
        if (!is.null(run)) {
            run$queueState <- NA
        }
        else {
            logger$print(paste("unqueue, runId:", runId, "not in", names(self$runs)))
        }
    },

    checkRunToExecute = function(runIds) {
        givenRuns <- lapply(runIds, function(runId) self$runs[[toString(runId)]])
        givenRuns <- if (is.null(givenRuns)) list() else givenRuns
        runsToExecute <- Filter(function(r) { !is.null(r) && r$status != launcherConst$STATUS$waiting && r$status != launcherConst$STATUS$running && r$status != launcherConst$STATUS$disabled }, givenRuns)

        runsWithNoSimulator <- Filter(function(r) { r$simulatorId == -1 }, runsToExecute)
        for (r in runsWithNoSimulator) {
            r$queueState <- NA
            self$setOnError(r$id, "Simulator not defined")
        }

        return(Filter(function(r) { r$simulatorId != -1 }, runsToExecute))
    },

    executableRun = function(runId) {
        run <- self$runs[[toString(runId)]]
        if (is.null(run)) {
            error(paste("executableRun, runId:", runId, "not in", names(self$runs)))
        }
        simulatorConfId <- run$simulatorId
        simulatorConfig <- self$checkSimulatorConfig(simulatorConfId)
        if (simulatorConfig$host == "localhost") {
            return(LocalRun$new(self, runId, simulatorConfig))
        }
        else {
            stop("Not yet implemented")
        }
    },

    checkSimulatorConfig = function(simulatorConfId) {
        simulatorConfig <- purrr::detect(launcherServer$launcherConfig$simulator_configs, function(config) config$id == simulatorConfId)
        if (is.null(simulatorConfig)) {
            stop(paste(simulatorConfId, "is an unknown simulator configuration"))
        }
        return(simulatorConfig);
    },

    getStoringDir = function() {
        if (is.null(self$simulationsConnection$setId)) {
            return(launcherServer$launcherConfig$storing_dir)
        }
        else {
            return(file.path(launcherServer$launcherConfig$storing_dir, self$simulationsConnection$setId))
        }
    },

    getRunParamInfo = function(runId) {
        run <- self$runs[[toString(runId)]]
        if (is.null(run)) {
            error(paste("getRunParamInfo, runId:", runId, "not in", names(self$runs)))
        }
        return(list(
            paramNames = run$paramNames,
            paramValues = run$paramValues
        ))
    },

    setStatus = function(runId, status) {
        run <- self$runs[[toString(runId)]]
        if (!is.null(run)) {
            run$status <- status
            self$processRunsQueues()
        }
        else {
            logger$print(paste("setStatus, runId:", runId, "not in", names(self$runs)))
        }
    },

    setResult = function(runId, result) {
        run <- self$runs[[toString(runId)]]
        if (!is.null(run)) {
            run$result <- result
        }
        else {
            logger$print(paste("setResult, runId:", runId, "not in", names(self$runs)))
        }
    }

  )
)
