#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module confSimulator
source("modules/menuImport/importDOE/launcher/simulationsLauncher.R", local = TRUE)
source("modules/shared/askLoginPassword.R", local = TRUE)

buildXApprox <- function(xValues) {
  runApprox <- data.frame(lapply(xValues, function(x) {
    if (is.numeric(x)) signif(x) else x 
  }))
  colnames(runApprox) <- colnames(xValues)
  rownames(runApprox) <- NULL
  return(runApprox)
}

buildXHashcode <- function(xValues) {
  return(digest(xValues, algo = "sha1"));
}

all.eq <- function(v1, v2) {
  v1 <- rbind(v1)
  v2 <- rbind(v2)

  v1length <- length(v1)
  if (v1length != length(v2)) {
    return(FALSE)
  }

  v1num <- v1[, unlist(lapply(v1, is.numeric))]
  v2num <- v2[, unlist(lapply(v2, is.numeric))]
  v1numLength <- length(v1num)
  if (v1numLength != length(v2num)) {
    return(FALSE)
  }
  for (i in seq_len(v1numLength)) {
    if (signif(v1num[i]) != signif(v2num[i])) {
      return(FALSE)
    }
  }

  if (v1length == v1numLength) {
    return(TRUE)
  }
  
  v1notnum <- v1[, unlist(lapply(v1, function(x) !is.numeric(x)))]
  v2notnum <- v2[, unlist(lapply(v2, function(x) !is.numeric(x)))]
  if (length(v1notnum) != length(v2notnum)) {
    return(FALSE)
  }
  for (i in seq_len(length(v1notnum))) {
    if (v1notnum[i] != v2notnum[i]) {
      return(FALSE)
    }
  }
  return(TRUE)
}

factor2String <- function(xValues) {
  for(i in seq_along(xValues)) {
    if (class(xValues[[i]]) == "factor") {
        xValues[[i]] <- as.character(xValues[[i]])
    }
  }
  return(xValues)
}

searchLauncherRunRef <- function(
  xValues, 
  launcherRunsAsList,
  xHashMap,
  simulatorId
) {
  approx <- buildXApprox(xValues)
  xHashcode <- buildXHashcode(approx)
  foundRunIdAndPosition <- .subset2(xHashMap, xHashcode)
  if (!is.null(foundRunIdAndPosition)) {
    foundRunIdAsString <- foundRunIdAndPosition$runIdAsString
    foundPosition <- foundRunIdAndPosition$position
    for (runIdAsString in c(foundRunIdAsString, names(launcherRunsAsList))) {
      # Improve speed, accessor '[[' is too slow
      # See section 'Extracting a single value from a data frame'
      # at http://adv-r.had.co.nz/Performance.html
      launcherRun <- .subset2(launcherRunsAsList, runIdAsString) # launcherRuns[[runIdAsString]]
      curXValuesBlock <- launcherRun$paramValues
      for (i in c(foundPosition, seq_len(nrow(launcherRun$paramValues)))) {
        curXValues <- curXValuesBlock[i, , drop = F]
        if (
          length(xValues) == length(curXValues) &&
          all(names(xValues) == names(curXValues)) &&
          all.eq(xValues, curXValues) &&
          launcherRun$simulatorId == simulatorId
        ) {
          return(list(runIdAsString = runIdAsString, position = i))
        }
      }
    }
  }
  return(NULL)
}

buildYNames <- function(nY, prefix) {
  offset <- 0
  buildSubYNames <- function(subNY) {
    if (is.list(subNY)) {
      unlist(lapply(seq_along(subNY), function(i) paste0(prefix, offset + i, "@", 1:subNY[[i]])))
    }
    else {
      paste0(prefix, 1:subNY)
    }
  }

  if (is.list(nY)) {
    isFunctional <- unlist(lapply(seq_along(nY), function(i) is.list(nY[[i]])))

    if (any(isFunctional)) {
      yNames <- unlist(lapply(seq_along(nY), function(i) {
        curYNames <- buildSubYNames(nY[[i]])
        if (is.list(nY[[i]])) {
          offset <<- offset + sum(unlist(nY[[i]]))
        }
        curYNames
      }))
    }
    else {
      yNames <- buildSubYNames(nY)
    }
  }
  else {
    yNames <- paste0(prefix, 1:nY)
  }
  return(yNames)
}

getOutputGroups <- function(nY) {
  if (is.list(nY)) {
    isFunctional <- unlist(lapply(seq_along(nY), function(i) is.list(nY[[i]])))

    if (any(isFunctional)) {
      unlist(lapply(seq_along(nY), function(i) {
        if (is.list(nY[[i]])) {
          nYsum <- sum(unlist(nY[[i]]))
          return(rep("Functional", nYsum))
        }
        else {
          return(rep("Interest", nY[[i]]))
        }
      }))
    }
    else {
      nYsum <- sum(unlist(nY))
      rep("Functional", nYsum)
    }
  }
  else {
    rep("Interest", nY)
  }
}

confSimulator.ui <- function(id) {
  ns <- NS(id)

  defineSimulatorPanel <- wellPanel(
    fluidRow(
      column(12,
             actionButton(ns("config.simulator"), label = "Configure Simulator", class = "btn-primary",
                          width = '100%'))
    ),
    fluidRow(
      column(12, textOutput(ns("selected.simulator.text")))
    )
  )
  tagList(
    # Modal dialog behind the 'Configure Simulator' button
    bsModal(ns("modalConfigSimu"), "Configure Simulator", NULL,
            uiOutput(ns('config.simulator.dynui'))
    ),
    
    fluidRow(
      column(12, textOutput(ns("run.set.text")))
    ),
    br(),
    fluidRow(
      column(3,
        YinfosChange.ui(ns("outtype"),label=HTML(paste("Change","Output Groups",sep="<br>"))),
        h5("Here you can change the outputs types (interest, control, status, constant)."),
        align = "center"
      ),
      importDiscretization.ui(ns("disc"))
    ),
    fluidRow(
      column(4, defineSimulatorPanel),
      column(8,
             fluidRow(
               column(6, actionButton(ns("launchSimu"), label = "Launch Simulations", class = "btn-primary", icon = icon("play-circle"),
                                      width = '100%')),
               column(6, actionButton(ns("stopSimu"), label = "Stop Simulations", class = "btn-warning", icon = icon("stop-circle"),
                                      width = '100%'))
             ),
             hr(),
             fluidRow(
               column(2,align="center",h4(textOutput(ns("advance.simutotal.text")))),
               column(2,align="center",h4(textOutput(ns("advance.simucompleted.text")))),
               column(2,align="center",h4(textOutput(ns("advance.simufailed.text")))),
               column(2,align="center",h4(textOutput(ns("advance.simurunning.text")))),
               column(2,align="center",h4(textOutput(ns("advance.simuwaiting.text"))))
               ),
             hr()
      )
    ),
    fluidRow(
      DT::dataTableOutput(ns('DTcontents.simu'))
    ),
    br(),
    uiOutput(ns('ddl.ui'))
  )
}

confSimulator.server <- function(input, output, session, DOEX, DOE.manual, Xadd, XaddUQ, XaddSeqOptim, XaddUnconstOptim, XaddConstOptim, doeProblemDef,
                                 persistence, settings) {

  ns <- session$ns

  # initialize reactives
  advance.simu <- reactiveValues(
    total = 0, # nb of doe elements (used in 'exploreDOE.R' and 'surrogate.R')
    status = NULL, # list of string, one for each doe element, representing its status (used in 'sequentialSolve.R', 'exploreDOE.R' and 'surrogate.R')
    info = NULL, # list of boolean, one for each doe element, indicating its origin (DOE, confirmation, etc.)
    stop = FALSE, # TRUE if launching process has been stopped
    launcherEvent = NULL, # simulation event received from simulations launcher (used in 'directOptim.R')
    simulatorsConfigs = NULL, # response to 'retrieveLauncherData' message, list of simulators (used in 'importDOE.R')
    simulatorsInputs = NULL, # response to 'searchMustacheInputs' message, list of inputs, one for each simulators (used in 'directOptim.R')
    runsSets = NULL, # response to 'retrieveLauncherData' message, list of run set names (used in 'importDOE.R')
    yNames = NULL, # column names extracted from last simulation results
    xHashMap = list(), # Maps each X run hashcode to a launcher run id (used in 'directOptim.R')
    optimInfoList = list()
  )
  launcherRuns <- reactiveValues()
  run2doeIndexes <- list() # Maps a launcher run (using its id) to one or several doe indexes => useful to handle launcher event
  doeRunRefList <- list() # List of launcher run references, one for each experiment of the doe
  DOE <- reactiveValues(
    Xopt = NULL, nYsurrogate = NULL,
    Xinfos = NULL, Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL,
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL,
    discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesmenu = NULL, Fnamesvisu = NULL
  )
  Xadd.sel <- reactiveValues(Xadd = NULL, type = "DOE", bool = FALSE, isOptim = FALSE)
  listsimulators <- reactiveValues(names=NULL, nX=NULL, Xnames=NULL, nY=NULL, description=NULL, 
                                   result_file_name = NULL, storing_dir=NULL, vector_support = NULL, vector_size = NULL) 
  currentDOEX <- reactiveValues(nX = NULL,Xnames=NULL)
  advance.simu.buffer <- reactiveValues(total = NULL, allcompleted = NULL, allfailed = NULL, allrunning = NULL, allwaiting = NULL)

  YinfosChange <- callModule(YinfosChange.server, "outtype", DOE, FALSE)
  observeEvent(YinfosChange$Yinfos, {
    req(YinfosChange$Yinfos)
    logger$print("updating output group")
    DOE$Yinfos <- YinfosChange$Yinfos
    DOE$nYsurrogate <- YinfosChange$nY

    computeFunctionalInfos(DOE, YinfosChange$Yinfos$func.ids)
  })
  
  observeEvent(DOE$Yinfos, {
    if (is.null(DOE$Yinfos)) {
      disableActionButton(ns("outtype-change"),session)
    }
    else {
      enableActionButton(ns("outtype-change"),session)
    }
  })

  importDiscF <- callModule(importDiscretization.server, "disc", DOE)
  observeEvent(importDiscF$discF, {
    DOE$discF <- importDiscF$discF
  })
  
  # dynamic UI elements
  observeEvent(use_simulator,{
    disableActionButton(ns("launchSimu"),session)
    disableActionButton(ns("stopSimu"),session)
    disableActionButton(ns("outtype-change"),session)
  })

  upload_file_bool <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice==1 || doeProblemDef$choice==2)
    }
    return(bool)
  })
  
  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice != 1)
    }
    return(bool)
  })

  observe({
    req(use_simulator())
    if (upload_file_bool() & use_simulator()){
      currentDOEX$nX <- ncol(DOE.manual$Xopt)
      currentDOEX$Xnames <- DOE.manual$xnames
      currentDOEX$nY <- DOE.manual$nY
      currentDOEX$ynames <- DOE.manual$ynames
    }else{
      currentDOEX$nX <- ncol(DOEX$Xopt)
      currentDOEX$Xnames <- colnames(DOEX$Xopt)
    }
  })
  
  inChoiceSimulator <- reactive({
    req(currentDOEX$Xnames,length(listsimulators$nX)>0)
    idok <- unlist(lapply(1:length(listsimulators$names),
                          function(i) setequal(currentDOEX$Xnames,unlist(listsimulators$Xnames[[i]]))))
    return(listsimulators$names[idok])
  })
  
  outChoiceSimulator <- reactive({
    req(currentDOEX$Xnames,length(listsimulators$nX)>0)
    idok <- unlist(lapply(1:length(listsimulators$names), 
                          function(i) is.null(currentDOEX$nY) || currentDOEX$nY == sum(unlist(listsimulators$nY[[i]]))))
    return(listsimulators$names[idok])
  })
  
  choicesimulator <- reactive({
    req(inChoiceSimulator(), outChoiceSimulator())
    return(intersect(inChoiceSimulator(), outChoiceSimulator()))
  })
  
  pickedSimulatorName <- callModule(dynamicSelectpicker.server, "choosesimulator", label.title = "Select Simulator", choices = choicesimulator,
                      multiple = FALSE, selected = NULL, livesearch = TRUE)
  
  output$config.simulator.dynui <- renderUI({
    if (is.null(listsimulators$names)){
      tl <- tagList(
        h5("Please launch the simulations launcher and then click on the button below to load the list of available simulators."),
        fluidRow(
          column(12,
                 actionButton(ns("load.json"), label = "Load Simulators List", class = "btn-primary",
                              width = '100%'))
        )
      )
    }else{
      if (isTruthy(currentDOEX$Xnames) && length(listsimulators$nX) > 0) {
        if (length(choicesimulator())){
          tl <- tagList(
            h5("Please select below a simulator compatible with your inputs."),
            dynamicSelect.ui(ns("choosesimulator")),
            uiOutput(ns('config.simulator.description')),
            br(),
            fluidRow(
              column(3, ""),
              column(6,
                    actionButton(ns("confirm"), label = "Confirm", class = "btn-primary",
                                  width = "100%")),
              column(3, "")
            ),
            askLoginPassword.ui(ns("confirm.simulator"))
          )
        }else{
          if (length(inChoiceSimulator()) == 0){
            tl <- tagList(
              h5("No simulator is compatible with your inputs. Please check if the input names are coherent.")
            )
          }
          else if (length(outChoiceSimulator()) == 0){
            tl <- tagList(
              h5("No simulator is compatible with your outputs. Please check if the output counts are coherent.")
            )
          }
        }
      }
      else {
          tl <- tagList(
            h5("No simulator is compatible with your inputs. Please check your inputs are well defined.")
          )
      }
    }
    return(tl)
  })
  output$config.simulator.description <- renderUI({
    req(pickedSimulatorName())
    simulatorIndex <- min(which(pickedSimulatorName() == listsimulators$names))
    if (is.null(listsimulators$description[simulatorIndex]) | is.na(listsimulators$description[simulatorIndex])){
      text <- ""
    }else{
      text <- listsimulators$description[simulatorIndex]
    }
    return(h4(text))
  })
  observe({
    advance.simu.buffer$total <- advance.simu$total
    advance.simu.buffer$allcompleted <- sum(advance.simu$status == "ended")
    advance.simu.buffer$allfailed <- sum(advance.simu$status == "onerror")
    advance.simu.buffer$allrunning <- sum(advance.simu$status == "running")
    advance.simu.buffer$allwaiting <- sum(advance.simu$status == "waiting")
  })
    
  output$selected.simulator.text  <- renderText({
    req(use_simulator())
    if (is.null(doeProblemDef$simulatorName)) {
      return("Simulator: ")
    }
    return(paste0("Simulator: ", doeProblemDef$simulatorName))
  })
  output$run.set.text  <- renderText({
    paste0("Run Set: ", doeProblemDef$runSet)
  })
  output$advance.simutotal.text  <- renderText({
    paste0("Total: ", advance.simu.buffer$total)
  })
  output$advance.simucompleted.text  <- renderText({
    paste0("Completed: ", advance.simu.buffer$allcompleted)
  })
  output$advance.simufailed.text  <- renderText({
    paste0("Failed: ", advance.simu.buffer$allfailed)
  })
  output$advance.simurunning.text  <- renderText({
    paste0("Running: ", advance.simu.buffer$allrunning)
  })
  output$advance.simuwaiting.text  <- renderText({
    paste0("Waiting: ", advance.simu.buffer$allwaiting)
  })

  observeEvent(advance.simu.buffer$allcompleted, {
    if (advance.simu.buffer$allcompleted == advance.simu$total){
      disableActionButton(ns("stopSimu"),session)
      disableActionButton(ns("launchSimu"),session)
    }
  })

  observeEvent(input$confirm, {
    req(use_simulator(), pickedSimulatorName())
    doeProblemDef$simulatorName <- pickedSimulatorName()
    toggleModal(session, "modalConfigSimu", toggle = "close")
  })

  simulatorId <- reactive({
    req(use_simulator(), doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    listsimulators$ids[[simulatorIndex]]
  })

  simulatorHostAndPort <- reactive({
    req(use_simulator(), doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    list(host = listsimulators$host[[simulatorIndex]], port = listsimulators$port[[simulatorIndex]])
  })

  askLoginPassword <- reactiveVal(0)

  credential <- callModule(askLoginPassword.server, "confirm.simulator", askLoginPassword, simulatorHostAndPort, doeProblemDef)
  
  # Initialization when simulator is configured
  observeEvent(doeProblemDef$simulatorName, {
    req(use_simulator(), doeProblemDef$choice != 4, doeProblemDef$simulatorName, listsimulators$names)
    
    initWhenSimulatorIsSet()
  })

  initWhenSimulatorIsSet <- function() {
    # reset DOE
    advance.simu$total <- 0
    advance.simu$status <- NULL
    advance.simu$info <- NULL
    advance.simu$simulatorsConfigs <- NULL
    advance.simu$simulatorsInputs <- NULL
    advance.simu$yNames <- NULL
    DOE$Xopt <- NULL
    DOE$Xinfos <- NULL
    DOE$XY <- NULL
    DOE$X <- NULL
    DOE$Y <- NULL
    DOE$nobs <- NULL
    DOE$nX <- NULL
    DOE$nY <- NULL
    DOE$xnames <- NULL
    DOE$ynames <- NULL
    DOE$xnamesvisu <- NULL
    DOE$ynamesvisu <- NULL
    DOE$xnamesmenu <- NULL
    DOE$ynamesmenu <- NULL
    Xadd.sel$Xadd <- NULL
    Xadd.sel$type = "DOE"
    Xadd.sel$bool = FALSE
    Xadd.sel$isOptim <- FALSE

    if (upload_file_bool() & use_simulator()){
      DOE$Xopt = DOE.manual$Xopt
      DOE$Xinfos = DOE.manual$Xinfos
      DOE$X = as.data.frame(DOE.manual$X)
    }else{
      DOE$Xopt = DOEX$Xopt
      DOE$Xinfos = DOEX$Xinfos
      DOE$X = as.data.frame(DOEX$Xopt)
    }
    DOE$nobs = nrow(DOE$X)
    DOE$nX = ncol(DOE$X)
    DOE$xnames = unlist(lapply(DOE$Xinfos, function(Xinfo){Xinfo$name}))
    DOE$xnamesvisu = unlist(lapply(DOE$Xinfos, function(Xinfo){Xinfo$namevisu}))
    DOE$xnamesmenu = unlist(lapply(DOE$Xinfos, function(Xinfo){Xinfo$namemenu}))
    colnames(DOE$X) <- DOE$xnames
    
    if (advance.simu$total == 0){
      advance.simu$info <- rep("DOE",DOE$nobs)
    }
    advance.simu$total <- DOE$nobs

    if (
      is.null(listsimulators$names) ||
      length(which(doeProblemDef$simulatorName == listsimulators$names)) == 0
    ) {
      # When a study is loaded but associated simulator is not found, no more things can be updated
      return()
    }
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    nY <- listsimulators$nY[[simulatorIndex]]
    nYsum <- sum(unlist(nY))
    if (nYsum > 0){
      DOE$nY <- nYsum
      if (upload_file_bool() & use_simulator() & !is.null(DOE.manual$Y)){
        
        DOE$Y <- DOE.manual$Y
        DOE$ynames <- DOE.manual$ynames
        DOE$ynamesvisu <- DOE.manual$ynamesvisu
        DOE$ynamesmenu <- DOE.manual$ynamesmenu
        
        row.run <- apply(DOE$Y, 1, function(row){all(is.na(row))})
        index.init <- if (any(row.run)){min(which(row.run))}else{DOE$nobs + 1}
        
        DOE$Yinfos <- DOE.manual$Yinfos
        DOE$nYsurrogate <- DOE.manual$nYsurrogate

        addToLauncherRunsFromDoeManual(listsimulators$ids[[simulatorIndex]])
      }else{
        
        DOE$Y <- as.data.frame(matrix(NA, ncol = DOE$nY, nrow = DOE$nobs))
        DOE$ynames <- DOE$ynamesvisu <- DOE$ynamesmenu <- buildYNames(nY, "Output")
        index.init <- 1
        
        # All simulator outputs are numeric by default
        Yinfos <- list()
        Yinfos$all.ids <- getOutputGroups(nY)
        Yinfos$int.ids <- which(Yinfos$all.ids == "Interest")
        Yinfos$control.ids <- NULL
        Yinfos$status.ids <- NULL
        Yinfos$const.ids <- NULL
        Yinfos$func.ids <- which(Yinfos$all.ids == "Functional")
        Yinfos$surrogate.ids <- c(Yinfos$int.ids,Yinfos$control.ids)
        Yinfos$type <- rep("numeric", DOE$nY)
        DOE$Yinfos <- Yinfos
        DOE$nYsurrogate <- length(Yinfos$surrogate.ids)
      }
      
      advance.simu$status <- c()
      if (index.init > 1) {
        advance.simu$status <- c(advance.simu$status, rep("ended", index.init - 1))
      }
      if (index.init <= DOE$nobs) {
        advance.simu$status <- c(advance.simu$status, rep("waiting", DOE$nobs - index.init + 1))
      }
      advance.simu.buffer$total <- advance.simu$total
      advance.simu.buffer$allcompleted <- sum(advance.simu$status == "ended")
      advance.simu.buffer$allfailed <- sum(advance.simu$status == "onerror")
      advance.simu.buffer$allrunning <- sum(advance.simu$status == "running")
      advance.simu.buffer$allwaiting <- sum(advance.simu$status == "waiting")
      
      colnames(DOE$Y) <- DOE$ynames
      DOE$XY <- cbind(DOE$X, DOE$Y)
  
      computeFunctionalInfos(DOE, DOE$Yinfos$func.ids)

      any.waiting.simu <- any(apply(DOE$Y, 1, function(row){all(is.na(row))}))
      if (any.waiting.simu){
        enableActionButton(ns("launchSimu"),session)
      }

    }
  }

  # configure additional simulations
  observeEvent(list(Xadd$mode.manual, Xadd$mode.automatic), {
    req(Xadd$Xadd)
    Xadd.sel$Xadd <- Xadd$Xadd
    Xadd.sel$type <- Xadd$tagDOE
    Xadd.sel$isOptim <- FALSE
  }, priority = 2)
  observeEvent(list(XaddUQ$mode.manual, XaddUQ$mode.automatic), {
    req(XaddUQ$Xadd)
    Xadd.sel$Xadd <- XaddUQ$Xadd
    Xadd.sel$type <- XaddUQ$tagDOE
    Xadd.sel$isOptim <- FALSE
  }, priority = 2)
  observeEvent(XaddSeqOptim$launch.simu, {
    req(XaddSeqOptim$Xadd, isTRUE(XaddSeqOptim$launch.simu))
    Xadd.sel$Xadd <- XaddSeqOptim$Xadd
    Xadd.sel$type <- XaddSeqOptim$tagOptim
    Xadd.sel$isOptim <- TRUE
  }, priority = 2)
  observeEvent(list(XaddUnconstOptim$mode.manual, XaddUnconstOptim$mode.automatic), {
    req(XaddUnconstOptim$Xadd)
    Xadd.sel$Xadd <- XaddUnconstOptim$Xadd
    Xadd.sel$type <- XaddUnconstOptim$tagDOE
    Xadd.sel$isOptim <- FALSE
  }, priority = 2)
  observeEvent(list(XaddConstOptim$mode.manual, XaddConstOptim$mode.automatic), {
    req(XaddConstOptim$Xadd)
    Xadd.sel$Xadd <- XaddConstOptim$Xadd
    Xadd.sel$type <- XaddConstOptim$tagDOE
    Xadd.sel$isOptim <- FALSE
  }, priority = 2)
  observeEvent(list(Xadd$mode.manual, Xadd$mode.automatic, XaddUQ$mode.manual,
                    XaddUQ$mode.automatic, XaddSeqOptim$launch.simu, 
                    XaddUnconstOptim$mode.manual, XaddUnconstOptim$mode.automatic,
                    XaddConstOptim$mode.manual, XaddConstOptim$mode.automatic), {

    req(Xadd.sel$Xadd, (isTRUE(XaddSeqOptim$launch.simu) | !Xadd.sel$isOptim))
    req(doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    nsimu <- nrow(Xadd.sel$Xadd)
    ## extend DOE with new simu
    DOE$X <- rbind(DOE$X, Xadd.sel$Xadd)
    DOE$nobs <- nrow(DOE$X)
    rownames(DOE$X) <- 1:DOE$nobs
    DOEY <- data.frame(matrix(ncol = DOE$nY, nrow = nsimu))
    colnames(DOEY) <- DOE$ynames
    DOE$Y <- rbind(DOE$Y, DOEY)
    DOE$XY <- cbind(DOE$X, DOE$Y)
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]

    launcherRunsAsList <- reactiveValuesToList(launcherRuns)
    xAddStatus <- apply(Xadd.sel$Xadd, 1, function(row) {
      foundRunRef <- searchLauncherRunRef(factor2String(row), launcherRunsAsList, advance.simu$xHashMap, simulatorId)
      ifelse(is.null(foundRunRef), "waiting", "ended")
    })

    # update status
    advance.simu$total <- DOE$nobs
    advance.simu$status <- c(advance.simu$status, xAddStatus)
    advance.simu$info <- c(advance.simu$info, rep(Xadd.sel$type, nsimu))

    enableActionButton(ns("launchSimu"),session)

  }, priority = 1)

  observeEvent(list(input$launchSimu, Xadd$mode.automatic, XaddUQ$mode.automatic, XaddSeqOptim$launch.simu, XaddUnconstOptim$mode.automatic, XaddConstOptim$mode.automatic), {
    req(use_simulator(), DOE$nobs > 0, !is.null(doeProblemDef$simulatorName),
        (isTRUE(XaddSeqOptim$launch.simu) | !Xadd.sel$isOptim))
    askLoginPassword(askLoginPassword() + 1)
  })

  observeEvent(credential$ok, {
    req(!is.null(credential$ok), use_simulator(), doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    credential$ok <- NULL
    runsToLaunch <- initLaunchProcess()
    launchRuns(runsToLaunch)
  })

  initLaunchProcess <- function() {
    paramNames <- DOE$xnames
    doeX <- as.data.frame(DOE$X)
    dimnames(doeX) <-list(1:nrow(doeX), paramNames)

    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]
    vectorSupport <- listsimulators$vector_support[[simulatorIndex]]
    vectorSize <- ifelse(vectorSupport, listsimulators$vector_size[[simulatorIndex]], 1)

    launcherRunsAsList <- reactiveValuesToList(launcherRuns) # Use 'list' to improve speed
    newRunId <- length(launcherRunsAsList)
    if (newRunId != 0) {
      newRunId <- max(unlist(lapply(names(launcherRunsAsList), as.numeric))) + 1
    }
    runsToLaunch <- list()
    runsToLaunch["runToLaunchIds"] = list(c()) # References runs which don't have an associated run and are to launch
    runsToLaunch["runToRelaunchIds"] = list(c()) # References runs which already have an associated run and are to launch
    paramValuesBlock <- NULL
    run2doeIndexes <<- list()
    doeRunRefList <<- list()
    for (i in seq_len(nrow(doeX))) {
      paramValues <- factor2String(doeX[i, , drop = F])
      foundRunRef <- searchLauncherRunRef(paramValues, launcherRunsAsList, advance.simu$xHashMap, simulatorId)
      if (is.null(foundRunRef)) {
        approx <- buildXApprox(paramValues)
        paramValuesBlock <- rbind(paramValuesBlock, paramValues)
        newRunIdAsString <- toString(newRunId)
        launcherRunRef <- list(runIdAsString = newRunIdAsString, position = nrow(paramValuesBlock))
        advance.simu$xHashMap[[buildXHashcode(approx)]] <<- launcherRunRef
        if (
          nrow(paramValuesBlock) == vectorSize ||
          i == nrow(doeX)
        ) {
          launcherRuns[[newRunIdAsString]]$paramValues <<- paramValuesBlock
          launcherRuns[[newRunIdAsString]]$simulatorId <<- simulatorId
          launcherRunsAsList <- reactiveValuesToList(launcherRuns) # update 'launcherRunsAsList'
          runsToLaunch$runToLaunchIds <- c(runsToLaunch$runToLaunchIds, newRunIdAsString)
          newRunId <- newRunId + 1
          paramValuesBlock <- NULL
        }
      }
      else {
        launcherRunRef <- foundRunRef
        foundRun <- launcherRunsAsList[[foundRunRef$runIdAsString]]
        if (!is.null(foundRun$status)) {
          advance.simu$status[i] <- foundRun$status

          if (!is.null(foundRun$result)) {
            yNames <- colnames(foundRun$result)
            advance.simu$yNames <- yNames
            DOE$ynamesmenu <- yNames
            DOE$ynamesvisu <- yNames
            DOE$ynames <- yNames
            colnames(DOE$Y) <- yNames
            colnames(DOE$XY) <- c(DOE$xnames, yNames)
            DOE$Y[i,] <- foundRun$result[foundRunRef$position,]
            # Try to convert Y which are declared as 'numeric'
            if (ncol(DOE$Y) == length(DOE$Yinfos$type)) {
              for (outputIndex in seq_along(DOE$Y)) {
                if (DOE$Yinfos$type[outputIndex] == "numeric") {
                  DOE$Y[i, outputIndex] <- as.numeric(DOE$Y[i, outputIndex])
                }
              }
            }
            DOE$XY[,(DOE$nX+1):(DOE$nX+DOE$nY)] <- DOE$Y
          }

          if (foundRun$status != "ended" && foundRun$status != "disabled") {
            runsToLaunch$runToRelaunchIds <- c(runsToLaunch$runToRelaunchIds, foundRunRef$runIdAsString)
          }
        }
      }
      doeRunRefList <<- append(doeRunRefList, list(launcherRunRef))
      doeIndexList <- run2doeIndexes[[launcherRunRef$runIdAsString]]
      run2doeIndexes[[launcherRunRef$runIdAsString]] <<- if (is.null(doeIndexList)) c(i) else c(doeIndexList, i)
    }
    return(runsToLaunch)
  }

  launchRuns <- function(runsToLaunch) {
    disableActionButton(ns("launchSimu"), session)

    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]

    if (length(runsToLaunch$runToRelaunchIds) != 0) {
      enableActionButton(ns("stopSimu"), session)
      advance.simu$stop <- FALSE

      args <- list(
        runIds = as.numeric(runsToLaunch$runToRelaunchIds),
        simulatorId = simulatorId
      )
      simulationsLauncher$configureLaunchLoad(session, args)
    }
    
    if (length(runsToLaunch$runToLaunchIds) != 0) {
      enableActionButton(ns("stopSimu"),session)
      advance.simu$stop <- FALSE
      
      launcherRunsAsList <- reactiveValuesToList(launcherRuns)
      xTodo <- lapply(runsToLaunch$runToLaunchIds, function(runIdAsString) {
        values <- launcherRunsAsList[[runIdAsString]]$paramValues
        colnames(values) <- NULL
        return(lapply(values, function(e) e))
      })

      args <- list(
        runs = list(
          paramNames = DOE$xnames,
          mat = xTodo
        ),
        simulatorId = simulatorId
      )
      simulationsLauncher$addRuns(session, args)
    }
  }

  observeEvent(input$stopSimu, {
    req(use_simulator(), DOE$nobs > 0, !is.null(doeProblemDef$simulatorName), input$launchSimu)
    simulationsLauncher$cancel(session, list(runIds = unlist(lapply(names(launcherRuns), as.numeric))))
    disableActionButton(ns("stopSimu"), session)
    advance.simu$stop <- TRUE
  })
  
  observeEvent(input$config.simulator, {
    
    # open modal
    toggleModal(session, "modalConfigSimu", toggle = "open")
    
  })
  
  observeEvent(input$load.json, {
    # send messages to retrieve simulator info
    simulationsLauncher$retrieveLauncherData(session)
  })
  
  observe({
    if (length(input$simulatorsConfigs) > 0) {
      simulator_configs <- input$simulatorsConfigs$simulator_configs
      idstemp <- lapply(simulator_configs, function(config) config$id)
      names(idstemp) <- NULL
      listsimulators$ids <- idstemp
      namestemp <- lapply(simulator_configs, function(config) config$config_name)
      names(namestemp) <- NULL
      listsimulators$names <- namestemp
      listsimulators$nY <- lapply(simulator_configs, function(config) config$res_count)
      nsimulators <- length(listsimulators$names)

      if (length(input$simulatorsInputs) > 0 && length(input$simulatorsInputs) == nsimulators) {
        listparamnames <- input$simulatorsInputs
        listsimulators$nX <- unlist(lapply(1:nsimulators,function(i) length(listparamnames[[i]])))
        listsimulators$Xnames <- listparamnames
        listsimulators$description <- lapply(simulator_configs, function(config) config$config_descr)
        listsimulators$host <- lapply(simulator_configs, function(config) config$host)
        listsimulators$port <- lapply(simulator_configs, function(config) config$port)
        listsimulators$result_file_name <- lapply(simulator_configs, function(config) config$result_file_name)
        listsimulators$vector_support <- lapply(simulator_configs, function(config) isTruthy(config$vector_support))
        listsimulators$vector_size <- lapply(simulator_configs, function(config) { if (is.numeric(config$vector_size)) config$vector_size else 1 })
        listsimulators$vector_size <- lapply(simulator_configs, function(config) { if (is.numeric(config$vector_size) && config$vector_size%%1 == 0 && config$vector_size > 0) config$vector_size else 1 })
      }
    }
  })
  
  observeEvent(input$launcherEvent, {
    # 'ended' or 'error' events
    if (input$launcherEvent$type == "ended" || input$launcherEvent$type == "onerror") {
      runIdAsString <- toString(input$launcherEvent$id)
      if (input$launcherEvent$type == "ended") {
        launcherRuns[[runIdAsString]]$result <- parseLauncherResults(input$launcherEvent$data, runIdAsString)
        advance.simu$yNames <- colnames(launcherRuns[[runIdAsString]]$result)
      }
      else {
        launcherRuns[[runIdAsString]]$result <- as.data.frame(input$launcherEvent$data)
      }
    }

    if (!is.null(input$launcherEvent$ids)) {
      for (runId in input$launcherEvent$ids) {
        launcherRuns[[toString(runId)]]$status <- input$launcherEvent$type
      }
    }
    else if (!is.null(input$launcherEvent$id)) {
      runIdAsString <- toString(input$launcherEvent$id)
      launcherRuns[[runIdAsString]]$status <- input$launcherEvent$type
    }
    # printLauncherRuns("launcherEvent", launcherRuns)

    launcherRunsChanged(input$launcherEvent)
  })

  parseLauncherResults <- function(launcherEventData, runIdAsString) {
      # separator for simulation result file has to be ';'. TO DO: add UI selector for separator
      results <- read.csv(text = launcherEventData, check.names = F, sep = ";")
      if (nrow(results) == nrow(launcherRuns[[runIdAsString]]$paramValues)) {
        return(results)
      }
      read.csv(text = launcherEventData, header = F, check.names = F, sep = ";")
  }
  
  # Update simulatorId through reactive 'input$runSimulatorSet'
  observeEvent(input$runSimulatorSet, {
    for (runId in unlist(input$runSimulatorSet$runIds)) {
      launcherRuns[[toString(runId)]]$simulatorId <- input$runSimulatorSet$simulatorId
    }
  })

  # Retrieve existing runs through reactive 'input$runList'
  observeEvent(input$runList, {
    logger$print("Retrieving run list ...")
    runList <- jsonlite::fromJSON(input$runList, simplifyVector = F, simplifyDataFrame = F)
    addToLauncherRuns(runList)
  })

  # Update with new runs through reactive 'input$runsAdd'
  observeEvent(input$runsAdd, {
    runList <- jsonlite::fromJSON(input$runsAdd, simplifyVector = F, simplifyDataFrame = F)
    addToLauncherRuns(runList)
  })

  addToLauncherRuns <- function(runList) {
    req(length(runList) != 0)
    for (i in seq_len(length(runList))) {
      runIdAsString <- toString(runList[[i]]$id)
      xValuesBlock <- data.frame(lapply(runList[[i]]$paramValues, function(x) Reduce(c, x)))
      if (length(runList[[i]]$paramNames) == ncol(xValuesBlock)) { # basic consistency check
        colnames(xValuesBlock) <- runList[[i]]$paramName
        for (j in seq_len(nrow(xValuesBlock))) {
          xValues <- xValuesBlock[j, , drop = F]
          colnames(xValues) <- runList[[i]]$paramName
          approx <- buildXApprox(xValues)
          advance.simu$xHashMap[[buildXHashcode(approx)]] <<- list(runIdAsString = runIdAsString, position = j)
        }
      }
      launcherRuns[[runIdAsString]]$paramValues <- xValuesBlock
      launcherRuns[[runIdAsString]]$status <- runList[[i]]$status
      if (runList[[i]]$status == "ended") {
        launcherRuns[[runIdAsString]]$result <- parseLauncherResults(runList[[i]]$result, runIdAsString)
      }
      launcherRuns[[runIdAsString]]$simulatorId <- runList[[i]]$simulatorId
    }
    # printLauncherRuns("addToLauncherRuns", launcherRuns)
  }

  addToLauncherRunsFromDoeManual <- function(simulatorId) {
    req(upload_file_bool(), use_simulator(), !is.null(DOE.manual$Y))
    runIdAsString <- toString(-1)  # special value "-1" means "Upload file" has been choosen to get DOE Y values
    xValuesBlock <- as.data.frame(DOE.manual$Xopt)
    colnames(xValuesBlock) <- DOE.manual$xnames
    for (j in seq_len(nrow(xValuesBlock))) {
      xValues <- xValuesBlock[j, , drop = F]
      colnames(xValues) <- DOE.manual$xnames
      approx <- buildXApprox(xValues)
      advance.simu$xHashMap[[buildXHashcode(approx)]] <<- list(runIdAsString = runIdAsString, position = j)
    }
    launcherRuns[[runIdAsString]]$paramValues <- xValuesBlock
    launcherRuns[[runIdAsString]]$status <- "ended"
    launcherRuns[[runIdAsString]]$result <- DOE.manual$Y
    launcherRuns[[runIdAsString]]$simulatorId <- simulatorId
  }

  observeEvent(input$simulatorsConfigs, {
    advance.simu$simulatorsConfigs <- input$simulatorsConfigs
  })

  observeEvent(input$simulatorsInputs, {
    advance.simu$simulatorsInputs <- input$simulatorsInputs
  })

  observeEvent(input$runsSets, {
    advance.simu$runsSets <- input$runsSets
  })

  observeEvent(input$optimInfoList, {
    req(input$optimInfoList)
    advance.simu$optimInfoList <- jsonlite::fromJSON(input$optimInfoList, simplifyVector = T, simplifyDataFrame = F, simplifyMatrix  = F, flatten = F)
    for (i in seq_len(length(advance.simu$optimInfoList))) {
      advance.simu$optimInfoList[[i]]$optimArgs$optimFuncArgs$PbDefinition$calibration$Z <- as.data.frame(rbind(unlist(advance.simu$optimInfoList[[i]]$optimArgs$optimFuncArgs$PbDefinition$calibration$Z)))
      advance.simu$optimInfoList[[i]]$optimArgs$optimFuncArgs$PbDefinition$calibration$sigZ <- as.data.frame(rbind(unlist(advance.simu$optimInfoList[[i]]$optimArgs$optimFuncArgs$PbDefinition$calibration$sigZ)))
      advance.simu$optimInfoList[[i]]$iterations <- lapply(advance.simu$optimInfoList[[i]]$iterations, function(iter) { 
        Reduce(rbind, lapply(iter, as.data.frame)) 
      })
    }
  })

  launcherRunsChanged <- function(launcherEvent) {
    # About $ and partial matching: https://cran.r-project.org/doc/manuals/r-devel/R-lang.html#Indexing-by-vectors
    # => 'launcherEvent$id' can return value of 'launcherEvent$ids'
    if (!is.null(DOE$Y) & isTruthy(launcherEvent$type) & isTruthy(launcherEvent[["id"]]) & !is.null(doeProblemDef$simulatorName)) {
      runIdAsString <- toString(launcherEvent$id)
      doeIndexes <- run2doeIndexes[[runIdAsString]]
      if (launcherEvent$type == 'running') {
        advance.simu$status[doeIndexes] <- "running"
      }

      if (launcherEvent$type == "ended" & isTruthy(launcherRuns[[runIdAsString]]$result)) {
        if (ncol(DOE$Y) == ncol(launcherRuns[[runIdAsString]]$result)) {
          if (all(is.na(DOE$Y))){
            DOE$ynames <- DOE$ynamesvisu <- DOE$ynamesmenu <- colnames(launcherRuns[[runIdAsString]]$result)
            colnames(DOE$Y) <- DOE$ynames
            colnames(DOE$XY) <- c(DOE$xnames, DOE$ynames)
          }
          for (doeIndex in doeIndexes) {
            doeRunRef <- doeRunRefList[[doeIndex]]
            DOE$Y[doeIndex,] <- launcherRuns[[doeRunRef$runIdAsString]]$result[doeRunRef$position,]
            # Try to convert Y which are declared as 'numeric'
            if (ncol(DOE$Y) == length(DOE$Yinfos$type)) {
              for (outputIndex in seq_along(DOE$Y)) {
                if (DOE$Yinfos$type[outputIndex] == "numeric") {
                  DOE$Y[doeIndex, outputIndex] <- as.numeric(DOE$Y[doeIndex, outputIndex])
                }
              }
            }
          }
        }
        else {
          DOE$Y[doeIndexes, 1] <- "Wrong results count"
        }
        DOE$XY[,(DOE$nX+1):(DOE$nX+DOE$nY)] <- DOE$Y

        # Tag the simulation as completed
        advance.simu$status[doeIndexes] <- "ended"
        if (advance.simu$stop){
          enableActionButton(ns("launchSimu"),session)
        }
      }

      if (launcherEvent$type == 'onerror') {
        advance.simu$status[doeIndexes] <- "onerror"
        DOE$Y[doeIndexes, 1] <- launcherRuns[[runIdAsString]]$result
        DOE$XY[,(DOE$nX+1):(DOE$nX+DOE$nY)] <- DOE$Y
      }
    }
  }

  yColNames <- NULL

  initDone <- function(dtdisplay) {
    newYColNames <- colnames(dtdisplay)[(4 + ncol(DOE$X)):ncol(dtdisplay)]
    return(
      !is.null(yColNames) && 
      length(newYColNames) == length(yColNames) &&
      all(newYColNames == yColNames)
    )
  }

  # output simu
  dtdisplay <- reactive({
    req(use_simulator(), DOE$XY,advance.simu$info, advance.simu$status, nrow(DOE$XY) == length(advance.simu$status))
    runningDir <- unlist(lapply(seq_len(length(advance.simu$status)), function(i) {
      if (
        i > length(doeRunRefList) || 
        is.null(doeRunRefList[[i]]) || 
        doeRunRefList[[i]]$runIdAsString == "-1" # special value "-1" means "Upload file" has been choosen to get DOE Y values
      ) {
        ""
      }
      else {
        paste0("run", as.numeric(doeRunRefList[[i]]$runIdAsString) + 1)
      }
    }))
    if (length(DOE$Yinfos$int.ids) > 0) {
      data <- cbind(DOE$X, DOE$Y[DOE$Yinfos$int.ids])
    }
    else {
      data <- DOE$XY
    }
    # if there are too many columns, show only some of them
    if (ncol(data) > 100) {
      data <- DOE$X
    }
    cbind(data.frame(Directory = runningDir, Info=advance.simu$info,Status=advance.simu$status), data)
  })
  throttle.dtdisplay <- throttle(dtdisplay, 1000)
  output$DTcontents.simu <- DT::renderDataTable({
    dtdisplay <- throttle.dtdisplay()
    req(doeProblemDef$simulatorName, dtdisplay, !initDone(dtdisplay), cancelOutput = TRUE)
    DOE$ynames
    list(Xadd$mode.manual, XaddUQ$mode.manual, XaddUnconstOptim$mode.manual, XaddConstOptim$mode.manual)
    yColNames <<- colnames(dtdisplay)[(4 + ncol(DOE$X)):ncol(dtdisplay)]
    DT::datatable(
      dtdisplay,
      extensions = c('FixedColumns','Scroller'),
      options = list(
        dom = 't',
        scrollX = T, scrollY = 400,scroller = TRUE,fixedColumns = list(leftColumns = 4),
        autoWidth = F,
        columnDefs = list(
          list(orderable = TRUE, targets = 0), # Add row header ordering
          list(targets = seq_len(ncol(dtdisplay)), render = JS(
                "function(data, type, row, meta) {
                    return (type == 'display' && row[3] == 'ended' && data == null) ? '<b>Not a number</b>' : data;
                 }"
            )
          )
        )
      )
    )
  })
  proxy <- dataTableProxy('DTcontents.simu')

  observe({
    dtdisplay <- throttle.dtdisplay()
    req(doeProblemDef$simulatorName, dtdisplay, initDone(dtdisplay), cancelOutput = TRUE)
    replaceData(proxy, dtdisplay, resetPaging = FALSE)
  })
  
  output$ddl.ui <- renderUI({
    req(req(DOE$X))
    fluidRow(
      column(9,""),
      column(3,downloadButton(ns("downloadDOE"), label = "Export DOE", class = "btn-primary"),align='right')
    )
  })
  
  output$downloadDOE <- downloadHandler(
    filename = 'DOEXY.csv',
    content = function(con) {
      write.table(x = DOE$XY, file = con, row.names = F, col.names = T, sep = ",")
    })

  
  ##################################################################
  # LOADED STUDY
  
  tryCount <- reactiveVal(0)

  observe({
    if (persistence$updatingStep == "confSimulator-simulatorName") {
      if (isolate(is.null(persistence$loadedStudy$doeProblemDef$choice) || persistence$loadedStudy$doeProblemDef$choice == 1)) {
        progressToNextStep(persistence)
        return()
      }

      maxRetryReached <- isolate(tryCount()) %% 10 == 9
      performLoading <- isolate(
        is.null(persistence$loadedStudy$doeProblemDef$simulatorName) ||
        !is.null(advance.simu$simulatorsInputs) ||
        maxRetryReached
      )
      if (!performLoading) {
        # Re-execute this reactive expression after 500 milliseconds
        invalidateLater(500, session)
      }
      tryCount(isolate(tryCount()) + 1)
      if (performLoading) {
        isolate({
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          if (maxRetryReached) {
            persistence$report <- c(persistence$report, "Failed to update simulator configuration, please launch the simulations launcher")
          }
          doeProblemDef$simulatorName <- persistence$loadedStudy$doeProblemDef$simulatorName
          if (
            !is.null(listsimulators$names) &&
            !is.null(persistence$loadedStudy$doeProblemDef$simulatorName) &&
            length(which(persistence$loadedStudy$doeProblemDef$simulatorName == listsimulators$names)) == 0
          ) {
            persistence$report <- c(persistence$report, paste("Failed to update simulator configuration, simulator '", persistence$loadedStudy$doeProblemDef$simulatorName, "' is unknown"))
          }
        })
        progressToNextStep(persistence)
      }
    }
    else if (persistence$updatingStep == "confSimulator-runs") {
      if (isolate(is.null(persistence$loadedStudy$doeProblemDef$choice) || persistence$loadedStudy$doeProblemDef$choice == 1)) {
        progressToNextStep(persistence)
        return()
      }
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      isolate({
        if (
          !is.null(persistence$loadedStudy$doeProblemDef$simulatorName) &&
          length(which(persistence$loadedStudy$doeProblemDef$choice == c(2, 3))) != 0
        ) {
          # Update Y parts
          DOE$X <- persistence$loadedStudy$DOE$X
          DOE$nobs <- persistence$loadedStudy$DOE$nobs
          DOE$nY <- persistence$loadedStudy$DOE$nY
          DOE$Y <- persistence$loadedStudy$DOE$Y
          DOE$ynamesmenu <- DOE$ynamesvisu <- DOE$ynames <- persistence$loadedStudy$DOE$ynames #
          DOE$XY <- cbind(DOE$X, DOE$Y)
          DOE$Yinfos <- persistence$loadedStudy$DOE$Yinfos #
          DOE$nYsurrogate <- persistence$loadedStudy$DOE$nY
            
          # Update functional part
          DOE$discF <- persistence$loadedStudy$DOE$discF
          DOE$nF <- persistence$loadedStudy$DOE$nF
          DOE$idF <- persistence$loadedStudy$DOE$idF
          DOE$Fnames <- persistence$loadedStudy$DOE$Fnames
          DOE$Fnamesmenu <- persistence$loadedStudy$DOE$Fnamesmenu
          DOE$Fnamesvisu <- persistence$loadedStudy$DOE$Fnamesvisu

          if (!is.null(persistence$loadedStudy$DOE$OFtot)) {
            # Remove OF from DOE$Y
        
            DOE$nY <- DOE$nY - length(DOE$nF) - 1
            DOE$Y <- DOE$Y[1:DOE$nY]
            DOE$XY <- cbind(DOE$X, DOE$Y)
            DOE$ynames <- DOE$ynames[1:DOE$nY]
            DOE$ynamesmenu <- DOE$ynamesmenu[1:DOE$nY]
            DOE$ynamesvisu <- DOE$ynamesvisu[1:DOE$nY]

            DOE$Yinfos$all.ids <- DOE$Yinfos$all.ids[1:DOE$nY]
            DOE$Yinfos$int.ids <- which(DOE$Yinfos$all.ids == "Interest")
            DOE$Yinfos$surrogate.ids <- c(DOE$Yinfos$int.ids, DOE$Yinfos$control.ids)
            DOE$Yinfos$type <- DOE$Yinfos$type[1:DOE$nY]
          }

          if (
            !is.null(listsimulators$names) &&
            length(which(persistence$loadedStudy$doeProblemDef$simulatorName == listsimulators$names)) != 0
          ) {
            # Update simulations launcher part
            # advance.simu$status <- rep("waiting", nrow(DOE$X))
            
            launcherRunsAsList <- reactiveValuesToList(launcherRuns)
            simulatorIndex <- min(which(persistence$loadedStudy$doeProblemDef$simulatorName == listsimulators$names))
            simulatorId <- listsimulators$ids[[simulatorIndex]]

            run2doeIndexes <<- list()
            doeRunRefList <<- list()
            notFoundRuns <- c()
            for (expIndex in seq_len(nrow(DOE$X))) {
              row <- factor2String(DOE$X[expIndex, ])
              foundRunRef <- searchLauncherRunRef(row, launcherRunsAsList, advance.simu$xHashMap, simulatorId)
              if (!is.null(foundRunRef)) {
                foundRunIdAsString <- foundRunRef$runIdAsString
                doeIndexList <- run2doeIndexes[[foundRunIdAsString]]
                run2doeIndexes[[foundRunIdAsString]] <<- ifelse(is.null(doeIndexList), c(expIndex), c(doeIndexList, expIndex))
                
                doeRunRefList <<- append(doeRunRefList, list(foundRunRef))
              }
              else {
                notFoundRuns <- c(notFoundRuns, expIndex)
              }
            }

            if (length(notFoundRuns) == 0) {
              for (expIndex in seq_along(doeRunRefList)) {
                runRef <- doeRunRefList[[expIndex]]
                launcherRun <- launcherRunsAsList[[runRef$runIdAsString]]
                launcherEvent = list(
                  type = launcherRun$status,
                  id = runRef$runIdAsString,
                  data = launcherRun$result
                )
                launcherRunsChanged(launcherEvent)
              }
              advance.simu$info <- append(rep("DOE", nrow(DOE$Xopt)), rep("-", nrow(DOE$X) - nrow(DOE$Xopt)))
              advance.simu$total <- DOE$nobs
            }
            else {
                persistence$report <- c(persistence$report, paste("Failed to find runs associated to experiments:", toString(notFoundRuns)))
            }
          }
        }
        progressToNextStep(persistence)
      })
    }
  }, priority = -1)
  
  return(list(DOE=DOE,advance.simu=advance.simu, launcherRuns = launcherRuns))
}

computeFunctionalInfos <- function(DOE, func.ids) {
    if (length(func.ids) > 0) {
      # compute functional info
      Fheader <- strsplit(DOE$ynamesmenu, '@')
      Fnamesraw <- unlist(sapply(Fheader, function(x) { if (length(x) > 1) x[1] else "" }))
      Fnames <- Fnamesmenu <- Fnamesvisu <- unique(unlist(sapply(Fheader, function(x) { if (length(x) > 1) x[1] else NULL })))
      idF <- lapply(Fnames, function(name){
        which(Fnamesraw == name)
      })
      nF <- sapply(idF, length)
      discF <- lapply(1:length(Fnames), function(j){
        df <- sapply(Fheader[idF[[j]]], function(x){as.numeric(x[2])})
        df <- as.data.frame(df)
        colnames(df) <- paste0('t', j)
        return(df)
      })
      
      DOE$discF <- discF
      DOE$nF <- nF
      DOE$idF <- idF
      DOE$Fnames <- Fnames
      DOE$Fnamesmenu <- Fnamesmenu
      DOE$Fnamesvisu <- Fnamesvisu
    }else{
      DOE$discF <- NULL
      DOE$nF <- NULL
      DOE$idF <- NULL
      DOE$Fnames <- NULL
      DOE$Fnamesmenu <- NULL
      DOE$Fnamesvisu <- NULL
    }
}