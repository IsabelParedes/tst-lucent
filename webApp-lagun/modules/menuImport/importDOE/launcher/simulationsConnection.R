SimulationsConnection <- R6Class("SimulationsConnection",
  public = list(

    setId = NULL,

    runsSet = NULL,

    initialize = function() {
      self$setId <- NULL
    },

    setRunSimulator = function(setRunSimulatorEvent) {
        if (is.null(self$runsSet)) {
            logger$print("'setRunSimulator' received but ignored (maybe previous launcher action is not yet completed)")
        }
        else {
            runs <- self$runsSet$runs;
            runningSetRunIds <- Filter(function(runId) { r <- runs[[toString(runId)]]; !is.null(r) && !is.na(r$queueState) }, setRunSimulatorEvent$runIds)
            if (length(runningSetRunIds) == 0) {
                self$runsSet$setSimulator(setRunSimulatorEvent$runIds, setRunSimulatorEvent$simulatorId)

                # Forward to all Launcher views
                self$emitRunSimulatorSet(setRunSimulatorEvent)
            }
            else {
                logger$print("'setRunSimulator' received but ignored (some selected simulations are running)")
            }
        }
    },

    addRuns = function(doe) {
        if (is.null(self$runsSet)) {
            logger$print("'setRunSimulator' received but ignored (maybe previous launcher action is not yet completed)")
            return(NULL)
        }
        else {
            addedRuns <- self$runsSet$addRuns(doe)
            return(addedRuns)
        }
    },

    getRuns = function() {
        if (is.null(self$runsSet)) {
            logger$print("'getRuns' received but ignored (maybe previous launcher action is not yet completed)")
            return(NULL)
        }
        else {
            return(self$runsSet$getOrderedRuns())
        }
    },

    runAction = function(runActionArg) {
        if (is.null(self$runsSet)) {
            logger$print(paste("Launcher action '", runActionArg$actionId, "' received but ignored ('runsSet' is not initialized)"))
        }
        else {
            self$runsSet$execute(runActionArg)
        }
    },

    attachSet = function(setId) {
        self$initRunsSet(setId)
    },

    initRunsSet = function(setId) {
        self$setId <- setId
        runsSet <- launcherServer$runsSets[[toString(setId)]]
        if (is.null(runsSet)) {
            runsSet <- RunsSet$new(self)
            runsSet$initFromStoringDir()
            launcherServer$runsSets[[toString(setId)]] <<- runsSet
        }
        self$runsSet <- runsSet
    },

    emitRunSimulatorSet = function(setSimulatorEvent) {
        # Forward 'run simulator set' to Shiny through a reactive input value
        eventAsJson <- jsonlite::toJSON(setSimulatorEvent, auto_unbox = T)
        runjs(paste0("Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-runSimulatorSet', JSON.parse(`", eventAsJson, "`.replace(/\\\\/g, '\\\\\\\\')), {priority: 'event'})"))
    },

    emitLauncherEvent = function(launcherEvent) {
        # Forward 'launcher event' to Shiny through a reactive input value
        eventAsJson <- jsonlite::toJSON(launcherEvent, auto_unbox = T)
        runjs(paste0("const launcherEvent = JSON.parse(`", eventAsJson, "`.replace(/\\r/g, '\\\\r').replace(/\\n/g, '\\\\n'));Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-launcherEvent', launcherEvent, {priority: 'event'})"))
    }
  )
)
