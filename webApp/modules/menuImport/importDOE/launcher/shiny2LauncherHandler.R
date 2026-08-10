shiny2LauncherHandler <- list(
    instanceMap = list()
)

shiny2LauncherHandler$getInstance <- function(session) {
    instance <- shiny2LauncherHandler$instanceMap[[session$token]]
    if (is.null(instance)) {
        instance <- list(
            simulatorsConnection = SimulatorsConnection$new(launcherServer),
            simulationsConnection = SimulationsConnection$new()
        )
        shiny2LauncherHandler$instanceMap[[session$token]] <<- instance
    }
    return(instance)
}


shiny2LauncherHandler$connectToSimulationsLauncher <- function(session, simulations_launcher_url) {
    print("!!!!! connectToSimulationsLauncher")

    shiny2LauncherHandler$getInstance(session)
    runjs(paste0("Shiny.setInputValue('nav-menuImport-importDOE-launcherProtocolVersion',", LAUNCHER_PROTOCOL_VERSION, ", {priority: 'event'})"))
}

shiny2LauncherHandler$disconnectFromSimulationsLauncher <- function(session) {
    # NOTHING TO DO
}

shiny2LauncherHandler$attachSet <- function(session, setId) {
    print("!!!!! attachSet")
    handlerInstance <- shiny2LauncherHandler$getInstance(session)
    
    handlerInstance$simulationsConnection$attachSet(setId)

    mustacheSet <- launcherServer$searchMustacheInputs(NULL)
    mustachesAsJson <- jsonlite::toJSON(mustacheSet, auto_unbox = T)
    runjs(paste0("Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-simulatorsInputs', JSON.parse(`", mustachesAsJson, "`.replace(/\\\\/g, '\\\\\\\\')), {priority: 'event'})"))

    runs <- handlerInstance$simulationsConnection$getRuns()
    runsAsJson <- jsonlite::toJSON(runs)
    runjs(paste0("Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-runList', JSON.parse('", runsAsJson, "'), {priority: 'event'})"))
}

shiny2LauncherHandler$retrieveLauncherData <- function(session) {
    print("!!!!! retrieveLauncherData")

    handlerInstance <- shiny2LauncherHandler$getInstance(session)
    launcherConfig <- handlerInstance$simulatorsConnection$getLauncherConfig()
    configAsJson <- jsonlite::toJSON(launcherConfig, auto_unbox = T)
    runjs(paste0("Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-simulatorsConfigs', JSON.parse(`", configAsJson, "`.replace(/\\\\/g, '\\\\\\\\')), {priority: 'event'})"))

    runSets <- handlerInstance$simulatorsConnection$getRunSets()
    runSetsAsJson <- jsonlite::toJSON(runSets)
    runjs(paste0("Shiny.setInputValue('nav-menuImport-importDOE-confSimulator-runsSets', JSON.parse(`", runSetsAsJson, "`.replace(/\\\\/g, '\\\\\\\\')), {priority: 'event'})"))
}

shiny2LauncherHandler$addRuns <- function(session, args) {
    print("!!!!! addRuns")
    if (length(args$runs) != 0) {
        handlerInstance <- shiny2LauncherHandler$getInstance(session)
        addedRuns <- handlerInstance$simulationsConnection$addRuns(args$runs)

        addedRunIds <- unlist(lapply(addedRuns, function(r) r$id))
        addedRunIds <- if (is.null(addedRunIds)) list() else addedRunIds

        handlerInstance$simulationsConnection$setRunSimulator(list(
            simulatorId = args$simulatorId,
            runIds = addedRunIds
        ))
        # TODO: Execute 'ConfigureLaunchLoad' in a dedicated R process
        handlerInstance$simulationsConnection$runAction(list(
            actionId = "ConfigureLaunchLoad",
            runIds = addedRunIds
        ))
        # futureRunAction <- function(args, TMP_DIR, INTERRUPT_FILE_PREFIX) {
        #     handlerInstance$simulationsConnection$runAction(list(
        #         actionId = "ConfigureLaunchLoad",
        #         runIds = addedRunIds
        #     ))
        # }
        # optimEnv$optimBgProcess <- callr::r_bg(
        #   func = futureRunAction,
        #   args = list(args, TMP_DIR, INTERRUPT_FILE_PREFIX),
        #   supervise = TRUE
        # )
    }
}

shiny2LauncherHandler$configureLaunchLoad <- function(session, args) {
    print("!!!!! configureLaunchLoad")
    # handlerInstance$simulationsConnection$setRunSimulator(list(
    #     simulatorId = args$simulatorId,
    #     runIds = args$runIds
    # ))
    handlerInstance$simulationsConnection$setRunSimulator(args)
    # TODO: Execute 'ConfigureLaunchLoad' in a dedicated R process
    handlerInstance$simulationsConnection$runAction(list(
        actionId = "ConfigureLaunchLoad",
        runIds = args$runIds
    ))
}

shiny2LauncherHandler$cancel <- function(session, args) {
    print("!!!!! cancel") # TODO
    # runAction
}

shiny2LauncherHandler$loginPasswordNeeded <- function(session, args) {
    print("!!!!! loginPasswordNeeded") # OK

    runjs(paste0("Shiny.setInputValue('", args$neededInputId, "', null)"))

    toReturn <- launcherServer$loginPasswordNeeded(args$logPwdNeededArgs)
    toReturnAsJson <- jsonlite::toJSON(toReturn, auto_unbox = T)
    runjs(paste0("Shiny.setInputValue('", args$neededInputId, "', JSON.parse(`", toReturnAsJson, "`.replace(/\\\\/g, '\\\\\\\\')))"))
}

shiny2LauncherHandler$addLoginPassword <- function(session, args) {
    # distant simulator via ssh, NOTHING TO DO
}

shiny2LauncherHandler$getOptimList <- function(session) {
    # Optim by simulations launcher, NOTHING TO DO
}

shiny2LauncherHandler$getOptimDescrs <- function(session, args) {
    # Optim by simulations launcher, NOTHING TO DO
}

shiny2LauncherHandler$optimAction <- function(session, args) {
    # Optim by simulations launcher, NOTHING TO DO
}

shiny2LauncherHandler$stopOptim <- function(session, optimId) {
    # Optim by simulations launcher, NOTHING TO DO
}

shiny2LauncherHandler$tellY <- function(session, args) {
    # Optim by simulations launcher, NOTHING TO DO
}

shiny2LauncherHandler$storeBinFile <- function(session, args) {
    # Optim by simulations launcher, NOTHING TO DO
}