#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

DISTANT_LAUNCHER <- !(
    isTRUE(getOption("shiny.testmode")) || 
    isTRUE(getOption("lagun.localsimulationslauncher"))
)

simulationsLauncher <- list()

if (!DISTANT_LAUNCHER) {
    source("modules/menuImport/importDOE/launcher/launcherServer.R", local = TRUE)
}

simulationsLauncher$connectToSimulationsLauncher <- function(session, simulations_launcher_url) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("connectToSimulationsLauncher", simulations_launcher_url)
    }
    else {
        shiny2LauncherHandler$connectToSimulationsLauncher(session, simulations_launcher_url)
    }
}

simulationsLauncher$disconnectFromSimulationsLauncher <- function(session) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("disconnectFromSimulationsLauncher", "")
    }
    else {
        shiny2LauncherHandler$disconnectFromSimulationsLauncher(session)
    }
}

simulationsLauncher$attachSet <- function(session, runSet) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("attachSet", runSet)
    }
    else {
        shiny2LauncherHandler$attachSet(session, runSet)
    }
}

simulationsLauncher$retrieveLauncherData <- function(session) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("retrieveLauncherData", "")
    }
    else {
        shiny2LauncherHandler$retrieveLauncherData(session)
    }
}

simulationsLauncher$addRuns <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("addRuns", args)
    }
    else {
        shiny2LauncherHandler$addRuns(session, args)
    }
}

simulationsLauncher$configureLaunchLoad <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("configureLaunchLoad", args)
    }
    else {
        shiny2LauncherHandler$configureLaunchLoad(session, args)
    }
}

simulationsLauncher$cancel <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("Cancel", args)
    }
    else {
        shiny2LauncherHandler$cancel(session, args)
    }
}

simulationsLauncher$loginPasswordNeeded <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("loginPasswordNeeded", args)
    }
    else {
        shiny2LauncherHandler$loginPasswordNeeded(session, args)
    }
}

simulationsLauncher$addLoginPassword <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("addLoginPassword", args)
    }
    else {
        shiny2LauncherHandler$addLoginPassword(session, args)
    }
}

simulationsLauncher$getOptimList <- function(session) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("getOptimList", "")
    }
    else {
        shiny2LauncherHandler$getOptimList(session)
    }
}

simulationsLauncher$getOptimDescrs <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("getOptimDescrs", args)
    }
    else {
        shiny2LauncherHandler$getOptimDescrs(session, args)
    }
}

simulationsLauncher$optimAction <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("optimAction", args)
    }
    else {
        shiny2LauncherHandler$optimAction(session, args)
    }
}

simulationsLauncher$stopOptim <- function(session, optimId) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("stopOptim", optimId)
    }
    else {
        shiny2LauncherHandler$stopOptim(session, optimId)
    }
}

simulationsLauncher$tellY <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("tellY", args)
    }
    else {
        shiny2LauncherHandler$tellY(session, args)
    }
}

simulationsLauncher$storeBinFile <- function(session, args) {
    if (DISTANT_LAUNCHER) {
        session$sendCustomMessage("storeBinFile", args)
    }
    else {
        shiny2LauncherHandler$storeBinFile(session, args)
    }
}