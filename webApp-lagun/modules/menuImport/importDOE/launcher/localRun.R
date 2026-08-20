LocalRun <- R6Class("LocalRun",
  public = list(

    runsSet = NULL,

    runParamInfo = NULL,

    runId = NULL,

    simulatorConfig = NULL,

    runningDir = NULL,

    initialize = function(runsSet, runId, simulatorConfig) {
        self$runsSet <- runsSet
        self$runParamInfo <- runsSet$getRunParamInfo(runId)
        self$runId <- runId
        self$simulatorConfig <- simulatorConfig

        self$runningDir <- file.path(runsSet$getStoringDir(), paste0("run", (self$runId + 1)))
    },

    execute = function(actionId) {
        if (actionId == launcherConst$RUN_ACTION$configureLaunchLoad) {
            self$configureLaunchLoad()
        }
        else if (actionId == launcherConst$RUN_ACTION$configure) {
            self$configure()
        }
        else if (actionId == launcherConst$RUN_ACTION$load) {
            self$load()
        }
        else {
            logger$print(paste("Unknown actionId:", actionId))
        }        
    },

    configureLaunchLoad = function() {
        tryCatch({
            self$runsSet$setConfiguring(self$runId)
            self$configSimulation()
            self$createSimuInfoFile(self$simulatorConfig$config_name)

            self$runsSet$setRunning(self$runId)
            self$launchSimulator()

            self$runsSet$setLoading(self$runId)
            data <- self$loadResults()

            self$runsSet$unqueue(self$runId)
            # Send an 'ended' event containing the result for the current simulation
            self$runsSet$setEnded(self$runId, data)
        },
        error = function(error) {
            self$runsSet$unqueue(self$runId)
            # Send a 'onerror' event containing an error description
            self$runsSet$setOnError(self$runId, error)
        })
    },

    configure = function() {
        tryCatch({
            self$runsSet$setConfiguring(self$runId)
            self$configSimulation()
            self$createSimuInfoFile()

            self$runsSet$unqueue(self$runId)
            # Send a 'configured' event 
            self$runsSet$setConfigured(self$runId)
        },
        error = function(error) {
            # Send a 'onerror' event containing an error description
            self$runsSet$setOnError(self$runId, error)
        })
    },

    load = function() {
        tryCatch({
            self$runsSet$setLoading(self$runId)
            data <- self$loadResults()

            self$runsSet$unqueue(self$runId)
            # Send a 'ended' event containing the result for the current simulation
            self$runsSet$setEnded(self$runId, data)
        },
        error = function(error) {
            # Send a 'onerror' event containing an error description
            self$runsSet$setOnError(self$runId, error)
        })
    },

    configSimulation = function() {
        self$mkRunningDir()
        self$copyDataFiles()
        return(self$processParamFiles())
    },

    mkRunningDir = function() {
        tryCatch({
            dir.create(self$runningDir, recursive = T)
        },
        error = function(err) {
            logger$print(err)
        })
    },

    copyDataFiles = function() {
        for (dataFileName in self$simulatorConfig$simulation_data_files) {
            fullDataFileName <- file.path(self$simulatorConfig$simulation_dir, dataFileName)
            runningFileName <- file.path(self$runningDir, dataFileName)
            file.copy(fullDataFileName, runningFileName)
        }
    },

    createSimuInfoFile = function(simulatorConfigName) {
        context <- self$simuInfoFileContext()
        infoFileName <- file.path(self$runningDir, launcherConst$SIMU_INFO_JSON)

        infoFileContent <- list(xValues = context)
        if (!is.null(simulatorConfigName)) {
            infoFileContent$simulatorConfig <- simulatorConfigName
        }
        jsonlite::write_json(infoFileContent, infoFileName)
    },

    simuInfoFileContext = function(actionId) {
        context = list()
        for (paramIndex in seq_along(self$runParamInfo$paramNames)) {
            contextValue <- self$runParamInfo$paramValues[[paramIndex]]
            context[[self$runParamInfo$paramNames[paramIndex]]] <- contextValue
        }
        return(context)
    },

    processParamFiles = function() {
        context <- self$templateContext()

        for (paramFileName in self$simulatorConfig$simulation_param_files) {
            self$processParamFile(paramFileName, context)
        }
    },

    templateContext = function() {
        context = list()
        for (paramIndex in seq_along(self$runParamInfo$paramNames)) {
            contextValue <- self$runParamInfo$paramValues[[paramIndex]]
            if (length(contextValue) > 1) {
                context[[self$runParamInfo$paramNames[paramIndex]]] <- jsonlite::toJSON(contextValue)
            }
            else {
                context[[self$runParamInfo$paramNames[paramIndex]]] <- contextValue
            }
        }
        return(context)
    },

    processParamFile = function(paramFileName, templateContext) {
        fullParamFileName <- file.path(self$simulatorConfig$simulation_dir, paramFileName)
        runningFileName <- file.path(self$runningDir, paramFileName)

        fileContent <- readChar(fullParamFileName, file.info(fullParamFileName)$size)
        processedContent <- whisker.render(fileContent, templateContext)
        writeLines(processedContent, runningFileName)
    },

    launchSimulator = function() {
        runDirectory <- paste0('"', normalizePath(self$runningDir), '"')
        exeWithAbsolutePath <- paste0('"', normalizePath(self$simulatorConfig$simulator_exe), '"')
        command <- paste0("cd ", runDirectory," && ", exeWithAbsolutePath, " ", self$simulatorConfig$argument)
        if (nchar(Sys.getenv("COMSPEC")) == 0) {
          output <- system(command, intern = T)
        }
        else {
          output <- shell(command, intern = T)
        }
        # print(output)
    },

    loadResults = function(actionId) {
        # Retrieve the name of the files which are in the running directory
        fileNames <- list.files(self$runningDir)

        csvFileNames <- Filter(function(f) { f == self$simulatorConfig$result_file_name }, fileNames)
        if (length(csvFileNames) == 0) {
            stop("No 'csv' file found")
        }

        runCsvFileName <- file.path(self$runningDir, self$simulatorConfig$result_file_name)

        # Retrieve content of 'csvFile'
        csvFileContent <- readChar(runCsvFileName, file.info(runCsvFileName)$size)
        if (length(csvFileContent) == 0) {
            stop("Empty result file")
        }
        return(csvFileContent)
    }
  )
)