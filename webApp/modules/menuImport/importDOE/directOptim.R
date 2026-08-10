#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module directOptim

LAGUN_WF <- "LagunWf"
LAUNCHER_WF <- "LauncherWf"
LAUNCHER_WF_ASKY_BY_LAGUN <- "LauncherWfAskyByLagun"

# Number of milliseconds to wait between checks of the file by 'reactiveFileReader'
INTERVAL_MILLIS <- 100
# Prefix for a temp file used to send interrupt message to optim process
INTERRUPT_FILE_PREFIX <- "interrupt"
TMP_DIR <- tempdir()

source("modules/optimizations/asktell_fun.R", local = TRUE)
source("modules/menuImport/importDOE/directOptim/smp.R", local = TRUE)
source("modules/menuImport/importDOE/directOptim/inputsPlot.R", local = TRUE)
source("modules/menuImport/importDOE/directOptim/resultsPlot.R", local = TRUE)
source("modules/menuImport/exploreDOE/qualitativeExploration/functionalPlot.R", local = TRUE)
source("modules/menuImport/importDOE/directOptim/simTable.R", local = TRUE)
source("modules/optimizations/getOptimDescr.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/askLoginPassword.R", local = TRUE)

get.initialXinfosFromSimulator <- function(param.name) {
  name <- namevisu <- namemenu <- param.name
  type <- "numeric"
  bounds <- c(0, 1)
  nlevels <- NA
  levels <- NA
  return(list(name = name, namevisu = namevisu, namemenu = namemenu, type = type, bounds = bounds, nlevels = nlevels, levels = levels))
}

getParamUI <- function(labelparamName, paramName, paramDef, paramValue) {
  label <- labelparamName
  if (!is.null(paramDef$enum)) {
    paramInput <- selectInput(paramName, label = label, choices = paramDef$enum, selected = paramValue)
  } else if (paramDef$type == "logical") {
    paramInput <- checkboxInput(paramName, label = label, value = paramValue)
  } else if (paramDef$type == "integer") {
    paramInput <- numericInput(paramName, label = label, value = paramValue)
  } else if (paramDef$type == "float") {
    if (is.infinite(paramValue)) {
      paramValue <- sign(paramValue) * .Machine$double.xmax
    }
    paramInput <- numericInput(paramName, label = label, value = paramValue)
  } else {
    paramInput <- textInput(paramName, label = label, value = paramValue)
  }
  return(paramInput)
}

extractParametersValues <- function(input, paramDefs) {
  paramValues <- list()
  for (paramName in names(paramDefs)) {
    paramValues[paramName] <- input[[paramName]]

    # adjust value according to parameter type
    paramDef <- paramDefs[[paramName]]
    if (paramDef$type == "integer") {
      p <- as.numeric(paramValues[paramName])
      paramValues[paramName] <- round(p)
    } else if (paramDef$type == "float") {
      p <- as.numeric(paramValues[paramName]) - .Machine$double.xmin
      if (is.infinite(p)) {
        paramValues[paramName] <- as.double(sign(p) * .Machine$double.xmax)
      } else {
        paramValues[paramName] <- p
      }
    }
  }
  return(paramValues)
}

futureOptim <- function(args, TMP_DIR, INTERRUPT_FILE_PREFIX) {
  setwd("./modules/optimizations")

  # Diverts R output to file 'bgProcessOutput.txt'
  sink(file.path(args$optimFuncArgs$PbDefinition$savepath, "bgProcessOutput.txt"))

  source("asktell_fun.R", local = TRUE)
  # Function useful for calibration
  computeOF <- function(ofData, yValues) {
    objFunc <- list()
    if (ofData$norm == "L2") {
      objFunc$OF <- sapply(seq_along(ofData$idZ), function(j) {
        z <- ofData$Z[, ofData$idZ[[j]], drop = F]
        sigZ <- ofData$sigZ[ofData$idZ[[j]]]
        apply(yValues[, ofData$idZY[[j]], drop = F], 1, function(y) {
          sum((y - z)^2 / sigZ^2)
        })
      })
    } else if (ofData$norm == "L1") {
      objFunc$OF <- sapply(seq_along(ofData$idZ), function(j) {
        z <- ofData$Z[, ofData$idZ[[j]], drop = F]
        sigZ <- ofData$sigZ[ofData$idZ[[j]]]
        apply(yValues[, ofData$idZY[[j]], drop = F], 1, function(y) {
          sum(abs(y - z) / sigZ)
        })
      })
    }
    
    objFunc$OFtot <- objFunc$OF %*% ofData$weights
    objFunc$OF <- as.data.frame(rbind(objFunc$OF))
    objFunc$OFtot <- as.data.frame(rbind(objFunc$OFtot))
    colnames(objFunc$OF) <- paste0("OF", seq_along(ofData$idZ))
    colnames(objFunc$OFtot) <- "OFtotal"

    return(objFunc)
  }

  # Function to optimize
  args$optimFuncArgs$PbDefinition$ObjFunc <- function(x, isNewIteration = TRUE) {
    pbDefinition <- args$optimFuncArgs$PbDefinition
    optimId <- args$optimId
    # Check for user interrupts
    interruptFileName <- tmp.file(optimId, INTERRUPT_FILE_PREFIX, TMP_DIR)
    if (file.exists(file = interruptFileName)) {
      del.file(interruptFileName)
      stop("User Interrupt")
    }

    x <- rbind(x) # to force to store a matrix in x, even if the input is a vector
    colnames(x) <- colnames(pbDefinition$x0)
    rownames(x) <- NULL
    nsim <- nrow(x)
    indC <- which(pbDefinition$inputflag != "Cat")
    xorig <- t(apply(x, 1, function(xtemp) {
      (
        if (!any(is.nan(xtemp))) {
          out <- xtemp
          out[indC] <- pbDefinition$lb_orig[indC] + xtemp[indC] *
            (pbDefinition$ub_orig[indC] - pbDefinition$lb_orig[indC])
          out[-indC] <- pbDefinition$levels[[-indC]][[round(xtemp[-indC])]]
          out
        } else {
          NA
        })
    }))
    x <- as.data.frame(xorig)
    x <- cbind(x, rep(isNewIteration, nrow(x)))
    colnames(x) <- c(colnames(pbDefinition$x0),"isNewIteration")
    print("x:")
    print(x)

    # Send 'x' through the 'xTodoFile' reactiveFileReader and retrieve Y through 'launcherRuns'
    y <- as.numeric(ask.Y(x, id = optimId, dev.path = TMP_DIR))
    y <- matrix(y, nrow = nsim, byrow = F)

    if (!is.null(pbDefinition$calibration)) {
      ofRes <- computeOF(pbDefinition$calibration, y)
      y <- cbind(y, as.matrix(ofRes$OF), as.matrix(ofRes$OFtot))
    }

    COformulation.temp <- pbDefinition$COformulation
    nO <- length(COformulation.temp$idO)
    nC <- length(COformulation.temp$idC)
    yRes <- matrix(NA, nrow = nsim, ncol = nO + nC)
    for (j in 1:nO) {
      yRes[, j] <- -COformulation.temp$COobj[j] * y[, COformulation.temp$idO[j], drop = F]
    }

    for (j in seq_len(nC)) {
      yRes[, nO + j] <- -COformulation.temp$COsign[j] * (y[, COformulation.temp$idC[j], drop = F] - as.numeric(COformulation.temp$COt[j]))
    }

    return(yRes)
  }

  source(args$optimFileName)
  solution <- do.call(args$optimFuncName, args$optimFuncArgs)

  namedResults <- as.data.frame(solution)

  return(namedResults)
}

directOptim.ui <- function(id) {
  ns <- NS(id)

  defineSimulatorPanel <- wellPanel(
    fluidRow(
      column(
        6,
        actionButton(ns("config.simulator"),
          label = "Configure Simulator", class = "btn-primary",
          width = "100%"
        )
      ),
      column(
        6,
        actionButton(ns("previous.optim"),
          label = "Use a previous optimization", class = "btn-primary",
          width = "100%"
        )
      )
    ),
    fluidRow(
      column(12, textOutput(ns("selected.simulator.text")))
    )
  )

  tagList(
    bsModal(
      # Modal dialog behind the 'Configure Simulator' button
      ns("modalConfigSimu"), "Configure Simulator", NULL,
      uiOutput(ns("config.simulator.dynui"))
    ),
    bsModal(
      # Modal dialog behind the 'Use a previous optimization' button
      ns("modalPreviousOptim"), "Use a previous optimization", NULL,
      uiOutput(ns("previous.optim.dynui"))
    ),
    fluidRow(
      column(12, textOutput(ns("run.set.text")))
    ),
    br(),
    bsCollapse(
      id = ns("collapseDirectOptim"),
      multiple = TRUE, open = "Define Simulator for Direct Optimization",
      bsCollapsePanel(
        "Define Simulator for Direct Optimization",
        style = "primary",
        fluidRow(
          column(12, defineSimulatorPanel, center = TRUE),
        )
      ),
      bsCollapsePanel(
        "Define Calibration",
        style = "primary",
        h5(id = ns("calibrationSuggestion"), "To perform this step, some results must be available. We suggest that you run the initial point (see next section 'Define Optimization Problem')."),
        div(id = ns("defineCalibDiv"), defineCalibration.ui(id = ns("defineCalibration")))
      ),
      bsCollapsePanel(
        "Define Optimization Problem",
        style = "primary",
        fluidRow(
          column(
            2,
            selectInput(
              inputId = ns("wfType"),
              label = "How optimizer is run",
              choices = c("Run by Lagun", "Run by Simulations Launcher")
            ),
            selectInput(
              inputId = ns("optimizer"),
              label = "Select Optimizer",
              choices = c("Optimization problem undefined")
            )
          ),
          column(
            10,
            constrainedDefine.ui(id = ns("constrainedDefine"))
          )
        ),
        hr(),
        fluidRow(
          column(
            12,
            wellPanel(
              fluidRow(
                column(
                  6,
                  actionButton(ns("launchInitValue"),
                    label = "Launch Initial Value", class = "btn-primary", icon = icon("play-circle"),
                    width = "100%"
                  )
                )
              ),
              fluidRow(
                column(2,align="center", h4(textOutput(ns("advance.simutotal.text")))),
                column(2,align="center", h4(textOutput(ns("advance.simucompleted.text")))),
                column(2,align="center", h4(textOutput(ns("advance.simuonerror.text")))),
                column(2,align="center", h4(textOutput(ns("advance.simurunning.text")))),
                column(2,align="center", h4(textOutput(ns("advance.simuwaiting.text"))))
              ),
              fluidRow(
                column(
                  6,
                  actionButton(ns("launchOptim"),
                    label = "Launch Optimization", class = "btn-primary", icon = icon("play-circle"),
                    width = "100%"
                  )
                ),
                column(
                  6,
                  actionButton(ns("stopOptim"),
                    label = "Stop Optimization", class = "btn-warning", icon = icon("stop-circle"),
                    width = "100%"
                  )
                )
              ),
              fluidRow(
                column(12, p(id = ns("advance.optim.text"), "")),
              )
            )
          )
        ),
        conditionalPanel(
            condition = paste0("output['", ns("conditionalPostOptim"), "']"),
            tagList(
              fluidRow(
                column(
                  12,
                  tabsetPanel(id = ns('tabs'),
                    tabPanel("Results Plot", value = ns("resultsPlot"), resultsPlot.ui(id = ns("resultsPlotTab"))), 
                    tabPanel("Inputs Plot", value = ns("inputsPlot"), inputsPlot.ui(id = ns("inputsPlotTab"))),
                    tabPanel("Parallel coordinate plot - Scatter Plot Matrix", value = ns("scatterPlotMatrix"), spm.ui(id = ns("scatterPlotMatrixTab"))),
                    tabPanel("Functional plot", value = ns("funcPlot"), functionalPlot.ui(id = ns("funcPlotTab")))
                  ),
                  simTable.ui(id = ns("table.simulations"))
                )
              ),
              br(),
              uiOutput(ns('downloadIterates.ui'))
            )
        )
      )
    ),
    # To open and close bsCollapsePanels in tests
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("activeCollapseDirectOptim"),
        label = "Active Panel:",
        choices = c("",
                    "Define Simulator for Direct Optimization",
                    "Define Calibration",
                    "Define Optimization Problem"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("desactiveCollapseDirectOptim"),
        label = "Desactive Panel:",
        choices = c("",
                    "Define Simulator for Direct Optimization",
                    "Define Calibration",
                    "Define Optimization Problem"),
        selected = ""
      )
    )
  )
}

directOptim.server <- function(input, output, session, advance.simu, launcherRuns, doeProblemDef, persistence, settings, window.dimension) {
  ns <- session$ns

  # To open and close bsCollapsePanels in tests
  observeEvent(input$activeCollapseDirectOptim, {
    updateCollapse(session, "collapseDirectOptim", open = input$activeCollapseDirectOptim)
  })
  observeEvent(input$desactiveCollapseDirectOptim, {
    updateCollapse(session, "collapseDirectOptim", close = input$desactiveCollapseDirectOptim)
  })
  
  logger$print(paste0("TMP_DIR=", TMP_DIR))
  
  initialXinfos <- reactiveValues(nX = NULL, Xinfos = NULL)
  Yinfos <- reactiveValues(ynames = NULL, ynamesvisu = NULL, ynamesmenu = NULL, nY = NULL, Yinfos = NULL)
  listmodels.fake <- reactiveValues(bestQ2loo = NULL)
  define <- constrainedDefine.server("constrainedDefine", constrainedDefineDOE,
    listmodels = listmodels.fake, persistence = persistence, nbcons.min = 0, nbobj = 2, 
    simulations = NULL, typeOptim = "directOptim"
  )

  # initialize reactives
  previousOptimArgs <- reactiveVal(NULL)
  DOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL,
    Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL,
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL
  )
  constrainedDefineDOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL,
    Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL,
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL
  )
  defineCalibDOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL,
    Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL,
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL
  )
  listsimulators <- reactiveValues(
    names = NULL, nX = NULL, Xnames = NULL, nY = NULL, description = NULL,
    result_file_name = NULL, storing_dir = NULL, vector_support = NULL, vector_size = NULL
  )
  advance.simu.buffer <- reactiveValues(total = NULL, allcompleted = NULL, allfailed = NULL, allrunning = NULL, allwaiting = NULL)

  # List of simulators with corresponding inputs
  choicesimulator <- reactive({
    req(length(listsimulators$nX) > 0, cancelOutput = TRUE)
    return(listsimulators$names)
  })

  pickedSimulatorName <- callModule(dynamicSelectpicker.server, "choosesimulator",
    label.title = "Select Simulator", choices = choicesimulator,
    multiple = FALSE, selected = NULL, livesearch = TRUE
  )

  # List of previous optimizations
  choiceOptim <- reactive({
    if (length(advance.simu$optimInfoList) != 0) {
      return(unlist(lapply(advance.simu$optimInfoList, function(optimInfo) { optimInfo$optimArgs$optimId })))
    }
    return(list())
  })

  pickedOptimId <- callModule(dynamicSelectpicker.server, "chooseOptimization",
    label.title = "Select Previous Optimisation", choices = choiceOptim,
    multiple = FALSE, selected = NULL, livesearch = TRUE
  )

  output$config.simulator.dynui <- renderUI({
    if (is.null(listsimulators$names)) {
      tl <- tagList(
        h5("Please launch the simulations launcher and then click on the button below to load the list of available simulators."),
        fluidRow(
          column(
            12,
            actionButton(ns("load.json"),
              label = "Load Simulators List", class = "btn-primary",
              width = "100%"
            )
          )
        )
      )
    } else {
      if (length(choicesimulator())) {
        tl <- tagList(
          h5("Please select below a simulator for your optimization."),
          dynamicSelect.ui(ns("choosesimulator")),
          uiOutput(ns("config.simulator.description")),
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
      } else {
        tl <- tagList(
          h5("No simulator is found.")
        )
      }
    }
    return(tl)
  })

  objFuncDOE <- callModule(defineCalibration.server, "defineCalibration", defineCalibDOE, persistence, settings)

  # Show/hide 'Define Calibration' collapsable panel
  observe({
    if (!is.null(doeProblemDef$simulatorName) && !is.null(listsimulators$names) && length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0) {
      simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
      if (is.list(listsimulators$nY[[simulatorIndex]])) {
        runjs("$('div[value=\"Define Calibration\"]').show();")
        return()
      }
    }
    runjs("$('div[value=\"Define Calibration\"]').hide();")
  })

  # Show/hide 'Define Calibration' availability
  observe({
    if (!is.null(advance.simu.buffer$allcompleted) && advance.simu.buffer$allcompleted > 0) {
      runjs(paste0(
        '$("#',
        ns("calibrationSuggestion"),
        '").hide();'
      ))
      runjs(paste0(
        '$("#',
        ns("defineCalibDiv"),
        '").show();'
      ))
    }
    else {
      runjs(paste0(
        '$("#',
        ns("calibrationSuggestion"),
        '").show();'
      ))
      runjs(paste0(
        '$("#',
        ns("defineCalibDiv"),
        '").hide();'
      ))
    }
  })

  observeEvent(doeProblemDef$simulatorName, {
    req(doeProblemDef$simulatorName, doeProblemDef$choice == 4, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    updateCalibrationDOE()
    updateStatusTextFields()
    updateFunctionalInfos()
  })

  observeEvent(reactiveValuesToList(launcherRuns), {
    req(doeProblemDef$simulatorName, doeProblemDef$choice == 4, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    updateCalibrationDOE()
    updateStatusTextFields()
  })

  updateCalibrationDOE <- function() {
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]
    nY <- listsimulators$nY[[simulatorIndex]]
    nYsum <- sum(unlist(nY))

    launcherRunsAsList <- reactiveValuesToList(launcherRuns)
    dataX <- NULL
    dataY <- NULL
    ordering <- order(as.numeric(names(launcherRuns)))

    # Build yNames
    yNames <- NULL
    if (is.null(advance.simu$yNames)) {
      yNames <- buildYNames(nY, "Y")
    }
    else {
      yNames <- advance.simu$yNames
    }
    
    # Determine rowCount
    rowCount <- 0
    for (runIdAsString in names(launcherRunsAsList[ordering])) {
      launcherRun <- launcherRunsAsList[[runIdAsString]]
      if (launcherRun$simulatorId == simulatorId && !is.null(launcherRun$paramValues)) {
        rowCount <- rowCount + nrow(launcherRun$paramValues)
      }
    }

    if (rowCount != 0) {
      dataY <- constrainedDefineDOE$Y
      if (is.null(dataY) || ncol(dataY) != nYsum || nrow(dataY) != rowCount) {
        dataY <- matrix(NA_real_, nrow = rowCount, ncol = nYsum)
      }
      colnames(dataY) <- yNames

      rowIndex <- 0
      for (runIdAsString in names(launcherRunsAsList[ordering])) {
        launcherRun <- launcherRunsAsList[[runIdAsString]]
        if (launcherRun$simulatorId == simulatorId) {
          xValues <- launcherRun$paramValues
          if (!is.null(xValues)) {
            if (is.null(dataX)) {
              dataX <- rbind(xValues)
            } else {
              colnames(dataX) <- colnames(xValues)
              dataX <- rbind(dataX, xValues)
            }

            if (!is.null(launcherRun$result) && !is.null(launcherRun$status) && launcherRun$status == "ended") {
              doeBlock <- rowIndex + seq_len(nrow(launcherRun$result))
              dataY[doeBlock, ] <- unlist(launcherRun$result)
            }
            rowIndex <- rowIndex + nrow(launcherRun$paramValues)
          }
        }
      }
    }

    constrainedDefineDOE$ynamesmenu <- defineCalibDOE$ynamesmenu <- yNames
    constrainedDefineDOE$ynamesvisu <- defineCalibDOE$ynamesvisu <- yNames
    constrainedDefineDOE$ynames <- defineCalibDOE$ynames <- yNames

    constrainedDefineDOE$X <- defineCalibDOE$X <- dataX
    constrainedDefineDOE$Y <- defineCalibDOE$Y <- dataY
    constrainedDefineDOE$XY <- defineCalibDOE$XY <- cbind(defineCalibDOE$X, defineCalibDOE$Y)
  }

  # if 'advance.simu$yNames' has changed, update DOE Y names and 'optimTypes/threshold' labels
  observeEvent(advance.simu$yNames, {
    req(advance.simu$yNames, doeProblemDef$simulatorName, doeProblemDef$choice == 4)

    constrainedDefineDOE$ynamesmenu <- defineCalibDOE$ynamesmenu <- advance.simu$yNames
    constrainedDefineDOE$ynamesvisu <- defineCalibDOE$ynamesvisu <- advance.simu$yNames
    constrainedDefineDOE$ynames <- defineCalibDOE$ynames <- advance.simu$yNames

    colnames(constrainedDefineDOE$Y) <- advance.simu$yNames
    colnames(defineCalibDOE$Y) <- advance.simu$yNames
    colnames(constrainedDefineDOE$XY) <- c(colnames(defineCalibDOE$X), advance.simu$yNames)

    define$COformulation$thresholds <- getCOformulationThresholds(define$COformulation, constrainedDefineDOE$ynames)
    define$COformulation$optimTypes <- getCOformulationOptimTypes(define$COformulation, constrainedDefineDOE$ynames)
  })

  # Do 'computeFunctionalInfos' when 'Yinfos' is changed
  observeEvent(defineCalibDOE$Yinfos, {
    updateFunctionalInfos()
  })

  updateFunctionalInfos <- function() {
    req(doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0, doeProblemDef$choice == 4)
    
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]
    nY <- listsimulators$nY[[simulatorIndex]]

    if (is.list(nY)) {
      outpuGroups <- getOutputGroups(nY)
      func.ids <- which(outpuGroups == "Functional")
      computeFunctionalInfos(defineCalibDOE, func.ids)
    }
  }

  updateStatusTextFields <- function() {
    req(doeProblemDef$simulatorName, doeProblemDef$choice == 4)

    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]

    launcherRunsAsList <- reactiveValuesToList(launcherRuns)
    total <- allcompleted <- allfailed <- allrunning <- allwaiting <- 0
    for (runIdAsString in names(launcherRunsAsList)) {
      launcherRun <- launcherRunsAsList[[runIdAsString]]
      if (launcherRun$simulatorId == simulatorId) {
        xValues <- launcherRun$paramValues
        if (!is.null(xValues) && !is.null(launcherRun$status)) {
          nRowX <- nrow(xValues)
          total <- total + nRowX
          allcompleted <- allcompleted + ifelse(launcherRun$status == "ended", nRowX, 0)
          allfailed <- allfailed + ifelse(launcherRun$status == "onerror", nRowX, 0)
          allrunning <- allrunning + ifelse(launcherRun$status == "running", nRowX, 0)
          allwaiting <- allwaiting + ifelse(launcherRun$status == "waiting", nRowX, 0)
        }
      }
    }
    advance.simu.buffer$total <- total
    advance.simu.buffer$allcompleted <- allcompleted
    advance.simu.buffer$allfailed <- allfailed
    advance.simu.buffer$allrunning <- allrunning
    advance.simu.buffer$allwaiting <- allwaiting
  }

  # If doe from 'calib' module has changed, update doe for 'constrainedDefine' module
  observeEvent(list(objFuncDOE$OF, objFuncDOE$OFtot), {
    req(objFuncDOE$OF, objFuncDOE$OFtot)
    req(doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    # Append OF to DOE$Y
    DOEOF <- cbind(objFuncDOE$OF, objFuncDOE$OFtot)
    constrainedDefineDOE$Yinfos$all.ids <- c(defineCalibDOE$Yinfos$all.ids, rep('Interest', ncol(DOEOF)))
    constrainedDefineDOE$Yinfos$int.ids <- c(defineCalibDOE$Yinfos$int.ids, defineCalibDOE$nY + 1:ncol(DOEOF))
    constrainedDefineDOE$Yinfos$surrogate.ids <- c(defineCalibDOE$Yinfos$surrogate.ids, defineCalibDOE$nY + 1:ncol(DOEOF))
    constrainedDefineDOE$Yinfos$type <- c(defineCalibDOE$Yinfos$type, rep('numeric', ncol(DOEOF)))
    constrainedDefineDOE$nY <- defineCalibDOE$nY + ncol(objFuncDOE$OF) + 1
    constrainedDefineDOE$Y <- as.data.frame(cbind(defineCalibDOE$Y, DOEOF))
    constrainedDefineDOE$XY <- cbind(constrainedDefineDOE$X, constrainedDefineDOE$Y)
    OFnames <- colnames(DOEOF)
    constrainedDefineDOE$ynames <- c(defineCalibDOE$ynames, OFnames)
    constrainedDefineDOE$ynamesvisu <- c(defineCalibDOE$ynamesvisu, OFnames)
    constrainedDefineDOE$ynamesmenu <- c(defineCalibDOE$ynamesmenu, OFnames)
    # Add calibration data structures
    constrainedDefineDOE$discF <- defineCalibDOE$discF
    constrainedDefineDOE$nF <- defineCalibDOE$nF
    constrainedDefineDOE$idF <- defineCalibDOE$idF
    constrainedDefineDOE$Fnames <- defineCalibDOE$Fnames
    constrainedDefineDOE$Fnamesvisu <- defineCalibDOE$Fnamesvisu
    constrainedDefineDOE$Z <- objFuncDOE$Z
    constrainedDefineDOE$sigZ <- objFuncDOE$sigZ
    constrainedDefineDOE$nZ <- objFuncDOE$nZ
    constrainedDefineDOE$idZ <- objFuncDOE$idZ
    constrainedDefineDOE$idZY <- objFuncDOE$idZY
    constrainedDefineDOE$discZ <- objFuncDOE$discZ
    constrainedDefineDOE$OF <- objFuncDOE$OF
    constrainedDefineDOE$OFtot <- objFuncDOE$OFtot

    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    
    constrainedDefineDOE$Xinfos <- initialXinfos$Xinfos
    constrainedDefineDOE$nX <- listsimulators$nX[[simulatorIndex]]
    xNames <- unlist(listsimulators$Xnames[[simulatorIndex]])
    constrainedDefineDOE$xnames <- xNames
    constrainedDefineDOE$xnamesvisu <- xNames
    constrainedDefineDOE$xnamesmenu <- xNames
  })
  
  output$previous.optim.dynui <- renderUI({
    if (length(advance.simu$optimInfoList) == 0) {
      tl <- tagList(
        h5("No previous optimization is found.")
      )
    } else {
      tl <- tagList(
        h5("Please select below a previous optimization."),
        dynamicSelect.ui(ns("chooseOptimization")),
        uiOutput(ns("optimization.description")),
        br(),
        fluidRow(
          column(3, ""),
          column(6, 
                 actionButton(ns("confirm.optim"), label = "Confirm", class = "btn-primary",
                              width = "100%")),
          column(3, "")
        ),
        fluidRow(
          column(12, p(id = ns("advance.updating.text"), "")),
        )
      )
    }
    return(tl)
  })

  observeEvent(persistence$updatingStep, {
    req(pickedOptimId())
    if (persistence$updatingStep == "OFF" && length(persistence$report) != 0) {
      showModal(modalDialog(HTML(paste(persistence$report, collapse = '<br/>')), title = "Loading Report", size = 'l'))
    }
    if (!is.null(persistence$loadedStudy)) {
      if (persistence$updatingStep == "OFF") {
        html("advance.updating.text", "")
      }
      else {
        html("advance.updating.text", paste(persistence$updatingStep, "..."))
      }
    }
  })
  
  output$config.simulator.description <- renderUI({
    req(pickedSimulatorName(), cancelOutput = TRUE)
    simulatorIndex <- min(which(pickedSimulatorName() == listsimulators$names))
    if (is.null(listsimulators$description[[simulatorIndex]]) | is.na(listsimulators$description[[simulatorIndex]])) {
      text <- ""
    } else {
      text <- listsimulators$description[[simulatorIndex]]
    }
    return(h4(text))
  })

  output$optimization.description <- renderUI({
    req(pickedOptimId(), cancelOutput = TRUE)
    optimIds <- unlist(lapply(advance.simu$optimInfoList, function(optimInfo) { optimInfo$optimArgs$optimId }))
    optimIndex <- min(which(pickedOptimId() == optimIds))
    return(list(
      tags$li("simulator: ", advance.simu$optimInfoList[[optimIndex]]$optimArgs$simulatorName),
      tags$li("optimFuncName: ", advance.simu$optimInfoList[[optimIndex]]$optimArgs$optimFuncName)
    ))
  })

  output$selected.simulator.text  <- renderText({
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
  output$advance.simuonerror.text  <- renderText({
    paste0("Failed: ", advance.simu.buffer$allfailed)
  })
  output$advance.simurunning.text  <- renderText({
    paste0("Running: ", advance.simu.buffer$allrunning)
  })
  output$advance.simuwaiting.text  <- renderText({
    paste0("Waiting: ", advance.simu.buffer$allwaiting)
  })

  observe({
    req(input$launchOptim < 1)
    disableActionButton(ns("stopOptim"), session)
  })

  setAdvanceOptimText <- function(text) {
    html("advance.optim.text", text)
  }

  simulatorId <- reactive({
    req(doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    listsimulators$ids[[simulatorIndex]]
  })

  simulatorHostAndPort <- reactive({
    req(doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    list(host = listsimulators$host[[simulatorIndex]], port = listsimulators$port[[simulatorIndex]])
  })

  askLoginPassword <- reactiveVal(0)

  credential <- callModule(askLoginPassword.server, "confirm.simulator", askLoginPassword, simulatorHostAndPort, doeProblemDef)

  output$conditionalPostOptim <- reactive({
      length(lastOptimEnv$iterations$xTodoList) != 0
  })
  outputOptions(output, "conditionalPostOptim", suspendWhenHidden = FALSE)
  
  # Initialization when simulator is configured
  observeEvent(input$confirm, {
    req(pickedSimulatorName(), listsimulators$names, length(which(pickedSimulatorName() == listsimulators$names)) != 0)
    doeProblemDef$simulatorName <- pickedSimulatorName()

    initWhenSimulatorIsSet()

    toggleModal(session, "modalConfigSimu", toggle = "close")
  })

  initWhenSimulatorIsSet <- function() {
    data.plot$dataY <- NULL
    data.plot$dataX <- NULL
    data.plot$rowNames <- NULL
    data.plot$runRefs <- NULL

    optimEnv$pbDefinition <- NULL

    define$COformulation$idC <- NULL
    define$COformulation$idO <- NULL
    define$COformulation$COsign <- NULL
    define$COformulation$COt <- NULL
    define$COformulation$COobj <- NULL

    # Initialize Xinfos
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    initialXinfos$nX <- listsimulators$nX[[simulatorIndex]]
    initialXinfos$Xinfos <- lapply(listsimulators$Xnames[[simulatorIndex]], get.initialXinfosFromSimulator)

    # Initialize Yinfos
    nY <- listsimulators$nY[[simulatorIndex]]
    nYsum <- sum(unlist(nY))
    Yinfos$nY <- nYsum
    yNames <- buildYNames(nY, "Y")

    Yinfos$ynames <- yNames
    Yinfos$ynamesvisu <- yNames
    Yinfos$ynamesmenu <- yNames
    outpuGroups <- getOutputGroups(nY)
    Yinfostemp <- list()
    Yinfostemp$all.ids <- outpuGroups
    Yinfostemp$int.ids <- which(outpuGroups == "Interest")
    Yinfostemp$control.ids <- NULL
    Yinfostemp$status.ids <- which(outpuGroups == "Status")
    Yinfostemp$const.ids <- which(outpuGroups == "Constant")
    Yinfostemp$func.ids <- which(outpuGroups == "Functional")
    Yinfostemp$surrogate.ids <- c(Yinfostemp$int.ids, Yinfostemp$control.ids)
    Yinfostemp$type <- rep("numeric", Yinfos$nY)
    Yinfos$Yinfos <- Yinfostemp

    # reset DOE
    constrainedDefineDOE$Xopt <- defineCalibDOE$Xopt <- NULL
    constrainedDefineDOE$Xinfos <- defineCalibDOE$Xinfos <- initialXinfos$Xinfos
    constrainedDefineDOE$Yinfos <- defineCalibDOE$Yinfos <- Yinfos$Yinfos
    constrainedDefineDOE$XY <- defineCalibDOE$XY <- NULL
    constrainedDefineDOE$X <- defineCalibDOE$X <- NULL
    constrainedDefineDOE$Y <- defineCalibDOE$Y <- NULL
    constrainedDefineDOE$nobs <- defineCalibDOE$nobs <- NULL
    constrainedDefineDOE$nX <- defineCalibDOE$nX <- listsimulators$nX[[simulatorIndex]]
    constrainedDefineDOE$nY <- defineCalibDOE$nY <- Yinfos$nY
    xNames <- unlist(listsimulators$Xnames[[simulatorIndex]])
    constrainedDefineDOE$xnames <- defineCalibDOE$xnames <- xNames
    constrainedDefineDOE$ynames <- defineCalibDOE$ynames <- yNames
    constrainedDefineDOE$xnamesvisu <- defineCalibDOE$xnamesvisu <- xNames
    constrainedDefineDOE$ynamesvisu <- defineCalibDOE$ynamesvisu <- yNames
    constrainedDefineDOE$xnamesmenu <- defineCalibDOE$xnamesmenu <- xNames
    constrainedDefineDOE$ynamesmenu <- defineCalibDOE$ynamesmenu <- yNames

    # Fake listmodels to use constrainedDefine module
    bestQ2loo <- list()
    bestQ2loo$id <- 1
    listmodels.fake$bestQ2loo <- bestQ2loo
  }

  observeEvent(input$config.simulator, {
    # open modal
    toggleModal(session, "modalConfigSimu", toggle = "open")
  })

  # Initialization when simulator is configured via previous optim
  observeEvent(input$confirm.optim, {
    req(pickedOptimId())
    optimIds <- unlist(lapply(advance.simu$optimInfoList, function(optimInfo) { optimInfo$optimArgs$optimId }))
    optimIndex <- min(which(pickedOptimId() == optimIds))
    previousOptimArgs <- advance.simu$optimInfoList[[optimIndex]]$optimArgs
    req(previousOptimArgs$simulatorName, listsimulators$names, length(which(previousOptimArgs$simulatorName == listsimulators$names)) != 0)
    persistence$loadedStudy$doeProblemDef$choice <- doeProblemDef$choice
    doeProblemDef$simulatorName <- previousOptimArgs$simulatorName

    PbDefinition <- previousOptimArgs$optimFuncArgs$PbDefinition
    persistence$updatingSteps <- c(
      "defineObjective-clean",
      "directOptim-simulator",
      "importExperimentalData-directoptim",
      "defineObjective-directOptim",
      "constrainedDefine-directOptim-initialXinfos",
      "constrainedDefine-directOptim-COformulation",
      "constrainedDefine-directOptim-x0",
      "directOptim-optimArgs",
      "directOptim-results"
    )
    persistence$updatingStep <- persistence$updatingSteps[1]

    persistence$loadedStudy$doeProblemDef$simulatorName <- previousOptimArgs$simulatorName
    persistence$loadedStudy$directOptim$workflowType <- previousOptimArgs$workflowType
    persistence$loadedStudy$directOptim$optimFileName <- previousOptimArgs$optimFileName
    persistence$loadedStudy$directOptim$parametersValues <- previousOptimArgs$optimFuncArgs$ParametersValues
    persistence$loadedStudy$directOptim$advParametersValues <- previousOptimArgs$optimFuncArgs$AdvancedParametersValues
    persistence$loadedStudy$directOptim$iterations$xTodoList <- advance.simu$optimInfoList[[optimIndex]]$iterations
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    persistence$loadedStudy$directOptim$simulatorId <- listsimulators$ids[[simulatorIndex]]
    nY <- listsimulators$nY[[simulatorIndex]]
    
    yNames <- buildYNames(nY, "Y")
    if (!is.null(PbDefinition$calibration)) {
      persistence$loadedStudy$directOptim$calibration <- PbDefinition$calibration
      yNames <- c(
        yNames,
        paste0("OF", seq_along(PbDefinition$calibration$idZ)),
        "OFtotal"
      )
    }
    # DOE$nX <- listsimulators$nX[[simulatorIndex]]
    persistence$loadedStudy$directOptim$COformulation <- PbDefinition$COformulation
    persistence$loadedStudy$directOptim$COformulation$thresholds <- getCOformulationThresholds(PbDefinition$COformulation, yNames)
    persistence$loadedStudy$directOptim$COformulation$optimTypes <- getCOformulationOptimTypes(PbDefinition$COformulation, yNames)

    # Initialize Xinfos
    persistence$loadedStudy$directOptim$Xinfos <- lapply(seq_len(PbDefinition$nd), function(xIndex) {
      name <- namevisu <- namemenu <- listsimulators$Xnames[[simulatorIndex]][[xIndex]]
      type <- "numeric"
      bounds <- c(PbDefinition$lb_orig[[1]][[xIndex]], PbDefinition$ub_orig[[1]][[xIndex]])
      nlevels <- NA
      levels <- NA
      if (PbDefinition$inputflag == "Cat") {
        type <- "categorical"
        levels <- PbDefinition$levels[[xIndex]]
        nlevels <- length(levels)
      }
      return(list(name = name, namevisu = namevisu, namemenu = namemenu, type = type, bounds = bounds, nlevels = nlevels, levels = levels))
    })

    persistence$loadedStudy$directOptim$initialXVal <- as.data.frame(lapply(seq_len(PbDefinition$nd), function(xIndex) {
      x0comp <- PbDefinition$x0[[1]][xIndex]
      if (PbDefinition$inputflag[[1]][[xIndex]] == "C") {
        lbOrigComp <- PbDefinition$lb_orig[[1]][[xIndex]]
        ubOrigComp <- PbDefinition$ub_orig[[1]][[xIndex]]
        return(lbOrigComp + x0comp * (ubOrigComp - lbOrigComp))
      }
      if (PbDefinition$inputflag[[1]][[xIndex]] == "Cat") {
        levels <- PbDefinition$levels[[xIndex]]
        return(levels[x0comp])
      }
      return(NaN)
    }))
  })

  observeEvent(persistence$updatingStep, {
    req(pickedOptimId())
    if (persistence$updatingStep == "OFF" && !is.null(persistence$loadedStudy)) {
      toggleModal(session, "modalPreviousOptim", toggle = "close")
      pickedOptimId(NULL)
      persistence$loadedStudy <- NULL
    }
  })

  observeEvent(input$previous.optim, {
    # open modal
    toggleModal(session, "modalPreviousOptim", toggle = "open")
  })

  observeEvent(input$load.json, {
    # send message to retrieve simulator info
    simulationsLauncher$retrieveLauncherData(session)
  })

  observe({
    if (length(advance.simu$simulatorsConfigs) > 0) {
      simulationsLauncher$getOptimList(session)
      simulator_configs <- advance.simu$simulatorsConfigs$simulator_configs
      idstemp <- lapply(simulator_configs, function(config) config$id)
      names(idstemp) <- NULL
      listsimulators$ids <- idstemp
      namestemp <- lapply(simulator_configs, function(config) config$config_name)
      names(namestemp) <- NULL
      listsimulators$names <- namestemp
      listsimulators$nY <- lapply(simulator_configs, function(config) config$res_count)
      nsimulators <- length(listsimulators$names)

      if (length(advance.simu$simulatorsInputs) > 0 && length(advance.simu$simulatorsInputs) == nsimulators) {
        listparamnames <- advance.simu$simulatorsInputs
        listsimulators$nX <- unlist(lapply(1:nsimulators,function(i) length(listparamnames[[i]])))
        listsimulators$Xnames <- listparamnames
        listsimulators$description <- lapply(simulator_configs, function(config) config$config_descr)
        listsimulators$host <- lapply(simulator_configs, function(config) config$host)
        listsimulators$port <- lapply(simulator_configs, function(config) config$port)
        listsimulators$result_file_name <- lapply(simulator_configs, function(config) config$result_file_name)
        listsimulators$vector_support <- lapply(simulator_configs, function(config) isTruthy(config$vector_support))
        listsimulators$vector_size <- lapply(simulator_configs, function(config) { if (is.numeric(config$vector_size) && config$vector_size%%1==0 && config$vector_size > 0) config$vector_size else 1 })
      }
    }
  })

  observeEvent(optimEnv$workflowType, {
    if (optimEnv$workflowType == LAGUN_WF) {
      updateSelectInput(session, inputId = "wfType", choices = c("Run by Lagun", "Run by Simulations Launcher"), selected = "Run by Lagun")
    }
    if (optimEnv$workflowType == LAUNCHER_WF) {
      updateSelectInput(session, inputId = "wfType", choices = c("Run by Lagun", "Run by Simulations Launcher"), selected = "Run by Simulations Launcher")
    }
  })

  optimDescrs <- function() {
    if(optimEnv$workflowType == LAGUN_WF) {
      return(optimEnv$lagunOptimDescrs)
    }
    else {
      return(optimEnv$launcherOptimDescrs)
    }
  }

  optimDescrsError <- function() {
    if(optimEnv$workflowType == LAGUN_WF) {
      return(NULL)
    }
    else {
      return(optimEnv$launcherOptimDescrsError)
    }
  }

  observe({
    if (is.null(optimDescrs())) {
      if (is.null(optimDescrsError())) {
        updateSelectInput(session, inputId = "optimizer", choices = c("Optimization problem undefined"))
      }
      else {
        updateSelectInput(session, inputId = "optimizer", choices = c("Not available, please check simulations launcher installation"))
      }
    }
    else {
      choices <- unlist(lapply(seq_len(length(optimDescrs())), function(i) {
        optimDescrs()[[i]]$dispname
      }))
      if (is.null(choices)) {
        updateSelectInput(session, inputId = "optimizer", choices = c("No optimizer compatible with the defined problem"))
      }
      else if (is.null(optimEnv$optimFileName)) {
        updateSelectInput(session, inputId = "optimizer", choices = choices)
      }
      else {
        optimIndex <- which(unlist(lapply(seq_len(length(optimDescrs())), function(i) {
          optimDescrs()[[i]]$Filename
        })) == optimEnv$optimFileName)
        if (isTruthy(optimIndex)) {
          updateSelectInput(session, inputId = "optimizer", choices = choices, selected = optimDescrs()[[optimIndex]]$dispname)
        }
        else {
          updateSelectInput(session, inputId = "optimizer", choices = choices)
        }
      }
    }
  })

  optimEnv <- reactiveValues(
    workflowType = LAGUN_WF, # Where the different steps are executed (LAGUN_WF, LAUNCHER_WF and LAUNCHER_WF_ASKY_BY_LAGUN)
    pbDefinition = NULL, # Definition problem of this optim
    lagunOptimDescrs = NULL, # List of optimizers compatible with problem definition (retrieved from Lagun)
    launcherOptimDescrs = NULL, # List of optimizers compatible with problem definition (retrieved from Simulations Launcher)
    launcherOptimDescrsError = NULL, # If retrieval of optimizers descriptions from Simulations Launcher failed, contains returned error message
    inProgress = FALSE, # Tells if an optimization is in progress
    optimFileName = NULL, # Identifies the optimizer used for optimization,
    optimBgProcess = NULL
  )

  lastOptimEnv <- reactiveValues(
    id = 0, # Unique identifier associated to this optim
    COformulation = NULL,
    Xinfos = NULL,
    initialXVal = NULL,
    workflowType = LAGUN_WF, # Where the different steps are executed (LAGUN_WF, LAUNCHER_WF and LAUNCHER_WF_ASKY_BY_LAGUN)
    optimFileName = NULL, # Identifies the optimizer used for optimization
    parametersValues = NULL,
    advParametersValues = NULL,
    simulatorId = NULL, # Simulator used for this optim
    calibration = NULL, # Calibration data used for this optim
    res = NULL, # Result returned by this optim
    iterationInProgress = FALSE, # Tells if an iteration is in progress
    iterations = list(
      xTodoList = list(), # List containing 'xTodo' of each iteration
      runRefsList = list() # List of list containing run refs (i.e. couple (runIdAsString, position)) for each iteration
    )
  )

  observeEvent(c(define$COformulation$idO, define$COformulation$idC, define$Xinfos$Xinfos), {
    req(!is.null(define$COformulation$idO) || !is.null(define$COformulation$idO), define$Xinfos$Xinfos)
    COformulation <- define$COformulation
    pbDefinition <- list()
    pbDefinition$tags <- list(
      constraints = FALSE,
      categorical = FALSE,
      monoobj = TRUE,
      multiobj = FALSE,
      derivative = FALSE,
      inversion = FALSE
    )
    pbDefinition$indmin <- COformulation$idO
    pbDefinition$indmax <- NULL
    pbDefinition$indcons <- COformulation$idC
    pbDefinition$ncons <- length(pbDefinition$indcons)
    pbDefinition$tags$inversion <- COformulation$isInversion
    if (isTRUE(pbDefinition$tags$inversion)) {
      pbDefinition$tags$constraints <- F
      pbDefinition$nobj <- 1
      pbDefinition$tags$multiobj <- (pbDefinition$ncons > 1)
      pbDefinition$tags$monoobj <- !pbDefinition$tags$multiobj
    }
    else {
      pbDefinition$tags$constraints <- length(pbDefinition$indcons) != 0
      pbDefinition$nobj <- length(pbDefinition$indmin)
      pbDefinition$tags$multiobj <- (pbDefinition$nobj > 1)
      pbDefinition$tags$monoobj <- !pbDefinition$tags$multiobj
    }

    pbDefinition$ncat <- length(which(sapply(define$Xinfos$Xinfos, function(x) {
      "categorical" %in% x$type
    })))
    pbDefinition$tags$categorical <- pbDefinition$ncat > 0
    optimEnv$pbDefinition <- pbDefinition
  })

  observeEvent(optimEnv$pbDefinition, {
    wdToRestore <- setwd("modules/optimizations")
    optimEnv$lagunOptimDescrs <- getOptimDescr(optimEnv$pbDefinition)
    setwd(wdToRestore)

    # Send a message to retrieve optim descriptions (which are known by the simulations launcher)
    optimEnv$optimDescrsInputId <- ns("optimDescrs")
    simulationsLauncher$getOptimDescrs(session, list(
      optimPbDef = optimEnv$pbDefinition,
      optimDescrsInputId = ns("optimDescrs")
    ))
  })

  # Retrieve optimizers descriptions (which are known by the simulations launcher) through reactive 'input$optimDescrs'
  observeEvent(input$optimDescrs, {
    if (input$optimDescrs$success) {
      optimEnv$launcherOptimDescrs <- jsonlite::fromJSON(input$optimDescrs$results, simplifyDataFrame = FALSE)
      optimEnv$launcherOptimDescrsError <- NULL
    }
    else {
      optimEnv$launcherOptimDescrsError <- input$optimDescrs$results
      logger$print("retrieval of optimizers descriptions from simulations launcher failed")
      logger$print(input$optimDescrs$results)
    }
  })

  observeEvent(input$wfType, {
    if (input$wfType == "Run by Lagun") {
      optimEnv$workflowType <- LAGUN_WF
    }
    if (input$wfType == "Run by Simulations Launcher") {
      optimEnv$workflowType <- LAUNCHER_WF
    }
  })

  observeEvent(c(input$wfType, input$optimizer), {
    req(length(optimDescrs()) != 0)
    optimIndex <- which(unlist(lapply(seq_len(length(optimDescrs())), function(i) {
      optimDescrs()[[i]]$dispname
    })) == input$optimizer)
    req(length(optimDescrs()) >= optimIndex)
    optimEnv$optimFileName <- optimDescrs()[[optimIndex]]$Filename
  })

  observeEvent(list(optimEnv$optimFileName, optimEnv$lagunOptimDescrs, optimEnv$launcherOptimDescrs, input$wfType), {
    removeUI(
      selector = "div[class=\"optimParams\"]",
      immediate = TRUE # 'addPopover" seems to fail if 'immediate' is not set to 'TRUE'
    )

    req(length(optimDescrs()) != 0, cancelOutput = TRUE)
    # Check if there're available optim descriptions for given problem definition
    req(optimEnv$optimFileName, cancelOutput = TRUE)

    optimIndex <- which(unlist(lapply(seq_len(length(optimDescrs())), function(i) {
      optimDescrs()[[i]]$Filename
    })) == optimEnv$optimFileName)
    req(length(optimDescrs()) >= optimIndex, cancelOutput = TRUE)

    # Build UI elements for parameters of current optimizer
    optimIds <- unlist(lapply(advance.simu$optimInfoList, function(optimInfo) { optimInfo$optimArgs$optimId }))
    paramDefs <- optimDescrs()[[optimIndex]]$param
    paramsUI <- lapply(names(paramDefs), function(paramName) {
      paramDef <- paramDefs[[paramName]]
      if (is.null(previousOptimArgs()) || is.null(previousOptimArgs()$optimFileName) || previousOptimArgs()$optimFileName != optimEnv$optimFileName) {
        # if 'previousOptimArgs' is not defined, use the default value for the current optimizer parameter
        return(getParamUI(paramName, ns(paramName), paramDef, paramDef$default))
      }
      else {
        # if 'previousOptimArgs' is defined, use the value given by 'previousOptimArgs()$ParametersValues
        ParametersValues <- previousOptimArgs()$optimFuncArgs$ParametersValues
        if (is.null(ParametersValues[[paramName]])) {
          # 'NULL' value means JSON transformation has ignored a value like 'Inf' or 'NaN' 
          # => it corresponds to the default value of the parameter
          return(getParamUI(paramName, ns(paramName), paramDef, paramDef$default))
        }
        else {
          return(getParamUI(paramName, ns(paramName), paramDef, ParametersValues[[paramName]]))
        }
      }
    })

    # Build UI elements for advanced parameters of current optimizer
    advParamDefs <- optimDescrs()[[optimIndex]]$advparam
    advParamsUI <- lapply(names(advParamDefs), function(paramName) {
      advParamDef <- advParamDefs[[paramName]]
      if (is.null(previousOptimArgs()) || is.null(previousOptimArgs()$optimFileName) || previousOptimArgs()$optimFileName != optimEnv$optimFileName) {
        # if 'previousOptimArgs' is not defined, use the default value for the current optimizer parameter
        return(getParamUI(paramName, ns(paramName), advParamDef, advParamDef$default))
      }
      else {
        # if 'previousOptimArgs' is defined, use the value given by 'previousOptimArgs()$AdvancedParametersValues
        AdvancedParametersValues <- previousOptimArgs()$optimFuncArgs$AdvancedParametersValues

        if (is.null(AdvancedParametersValues[[paramName]])) {
          # 'NULL' value means JSON transformation has ignored a value like 'Inf' or 'NaN' 
          # => it corresponds to the default value of the parameter
          return(getParamUI(paramName, ns(paramName), advParamDef, advParamDef$default))
        }
        else {
          return(getParamUI(paramName, ns(paramName), advParamDef, AdvancedParametersValues[[paramName]]))
        }
      }
    })

    # Insert the built UI elements
    optimDescrText <- optimDescrs()[[optimIndex]]$descr
    insertUI(
      selector = paste0("div:has(> #", ns("optimizer"), ")"),
      where = "afterEnd",
      ui = tags$div(
        class = "optimParams",
        h6(optimDescrText),
        bsCollapse(
          id = "optimCollapse",
          open = "Parameters",
          bsCollapsePanel("Parameters", paramsUI, style = "primary"),
          bsCollapsePanel("Advanced Parameters", advParamsUI, style = "primary")
        )
      ),
      immediate = TRUE # 'addPopover" seems to fail if 'immediate' is not set to 'TRUE'
    )

    # Add popover to UI elements of parameters
    for (paramName in names(paramDefs)) {
      paramDef <- paramDefs[[paramName]]
      addPopover(session, id = ns(paramName), title = paramName, content = paramDef$description, placement = "top")
    }
    # Add popover to UI elements of advanced parameters
    for (paramName in names(advParamDefs)) {
      advParamDef <- advParamDefs[[paramName]]
      addPopover(session, id = ns(paramName), title = paramName, content = advParamDef$description, placement = "top")
    }
  })

  observeEvent(input$launchInitValue, {
    req(define$initialXVal(), doeProblemDef$simulatorName)
    credential$action <- "launchInitValue"
    askLoginPassword(askLoginPassword() + 1)
  })

  observeEvent(credential$ok, {
    req(!is.null(credential$ok), doeProblemDef$simulatorName, listsimulators$names, length(which(doeProblemDef$simulatorName == listsimulators$names)) != 0)
    credential$ok <- NULL
    if (credential$action == "launchInitValue") {
      runsToLaunch <- initX0LaunchProcess()
      launchX0Runs(runsToLaunch)
    }
    if (credential$action == "launchOptim") {
      launchOptim()
    }
  })

  initX0LaunchProcess <- function() {
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]
    vectorSupport <- listsimulators$vector_support[[simulatorIndex]]
    vectorSize <- ifelse(vectorSupport, listsimulators$vector_size[[simulatorIndex]], 1)

    paramNames <- unlist(advance.simu$simulatorsInputs[[simulatorIndex]])
    x0 <- define$initialXVal()
    colnames(x0) <- paramNames
    
    launcherRunsAsList <- reactiveValuesToList(launcherRuns) # Use 'list' to improve speed
    newRunId <- length(launcherRunsAsList)
    if (newRunId != 0) {
      newRunId <- max(unlist(lapply(names(launcherRunsAsList), as.numeric))) + 1
    }
    runsToLaunch <- list()
    runsToLaunch["runToLaunchIds"] = list(c()) # References runs which don't have an associated run and are to launch
    runsToLaunch["runToRelaunchIds"] = list(c()) # References runs which already have an associated run and are to launch
    paramValuesBlock <- NULL
    for (i in seq_len(nrow(x0))) {
      paramValues <- factor2String(x0[i, , drop = F])
      foundRunRef <- searchLauncherRunRef(paramValues, launcherRunsAsList, advance.simu$xHashMap, simulatorId)
      if (is.null(foundRunRef)) {
        approx <- buildXApprox(paramValues)
        paramValuesBlock <- rbind(paramValuesBlock, paramValues)
        newRunIdAsString <- toString(newRunId)
        launcherRunRef <- list(runIdAsString = newRunIdAsString, position = nrow(paramValuesBlock))
        advance.simu$xHashMap[[buildXHashcode(approx)]] <<- launcherRunRef
        if (
          nrow(paramValuesBlock) == vectorSize ||
          i == nrow(x0)
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
        foundRun <- launcherRunsAsList[[foundRunRef$runIdAsString]]
        if (!is.null(foundRun$status) && foundRun$status != "ended" && foundRun$status != "disabled") {
          runsToLaunch$runToRelaunchIds <- c(runsToLaunch$runToRelaunchIds, foundRunRef$runIdAsString)
        }
      }
    }
    return(runsToLaunch)
  }

  launchX0Runs <- function(runsToLaunch) {
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    simulatorId <- listsimulators$ids[[simulatorIndex]]

    if (length(runsToLaunch$runToRelaunchIds) != 0) {
      args <- list(
        runIds = as.numeric(runsToLaunch$runToRelaunchIds),
        simulatorId = simulatorId
      )
      simulationsLauncher$configureLaunchLoad(session, args)
    }
    
    if (length(runsToLaunch$runToLaunchIds) != 0) {
      launcherRunsAsList <- reactiveValuesToList(launcherRuns)
      vectorSupport <- listsimulators$vector_support[[simulatorIndex]]

      paramNames <- unlist(advance.simu$simulatorsInputs[[simulatorIndex]])

      xTodo <- lapply(runsToLaunch$runToLaunchIds, function(runIdAsString) {
        values <- launcherRunsAsList[[runIdAsString]]$paramValues
        colnames(values) <- NULL
        return(lapply(values, function(e) e))
      })

      args <- list(
        runs = list(
          paramNames = paramNames,
          mat = xTodo
        ),
        simulatorId = simulatorId
      )
      simulationsLauncher$addRuns(session, args)
    }
  }

  observeEvent(input$launchOptim, {
    req(!is.null(optimEnv$optimFileName), cancelOutput = TRUE)
    credential$action <- "launchOptim"
    askLoginPassword(askLoginPassword() + 1)
  })

  launchOptim <- function() {
    setAdvanceOptimText("Starting ...")
    disableActionButton(ns("launchOptim"), session)
    enableActionButton(ns("stopOptim"), session)
    optimEnv$inProgress <- TRUE
    lastOptimEnv$id <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
    lastOptimEnv$res <- NULL
    lastOptimEnv$iterationInProgress <- FALSE
    lastOptimEnv$iterations$xTodoList <- list()
    lastOptimEnv$iterations$runRefsList <- list()
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    lastOptimEnv$simulatorId <- listsimulators$ids[[simulatorIndex]]
    optimIndex <- which(unlist(lapply(seq_len(length(optimDescrs())), function(i) {
      optimDescrs()[[i]]$Filename
    })) == optimEnv$optimFileName)
    optimDescr <- optimDescrs()[[optimIndex]]
    optimBaseName <- optimDescr$name
    optimFuncName <- paste0(optimBaseName, ".run")
    lastOptimEnv$COformulation <- reactiveValuesToList(define$COformulation)
    lastOptimEnv$Xinfos <- define$Xinfos$Xinfos
    lastOptimEnv$initialXVal <- define$initialXVal()
    lastOptimEnv$workflowType <- optimEnv$workflowType
    lastOptimEnv$optimFileName <- optimEnv$optimFileName

    if (is.list(listsimulators$nY[[simulatorIndex]])) {
      lastOptimEnv$calibration <- list(
        sigZ = objFuncDOE$sigZ,
        Z = objFuncDOE$Z,
        idZ = objFuncDOE$idZ,
        idZY = objFuncDOE$idZY,
        zFileName = objFuncDOE$zFileName,
        norm = objFuncDOE$norm,
        weights = objFuncDOE$weights
      )
    }
    else {
      lastOptimEnv$calibration <- NULL
    }

    lastOptimEnv$parametersValues <- extractParametersValues(input, optimDescr$param)
    lastOptimEnv$advParametersValues <- extractParametersValues(input, optimDescr$advparam)
    pbDefinition <- optimEnv$pbDefinition
    pbDefinition$calibration <- lastOptimEnv$calibration
    pbDefinition$COformulation <- reactiveValuesToList(define$COformulation)
    paramNames <- unlist(advance.simu$simulatorsInputs[[simulatorIndex]])
    nd <- length(paramNames)
    pbDefinition$nd <- nd

    if (lastOptimEnv$workflowType == LAGUN_WF) {
      pbDefinition$savepath <- file.path(TMP_DIR, "Saved_Optimizations", lastOptimEnv$id)
      dir.create(pbDefinition$savepath, showWarnings = F, recursive = T)
    }
    # Input variable types
    indcat <- NULL
    indcont <- seq_len(nd)
    pbDefinition$inputflag <- matrix("C", ncol = nd) # by default all the input variables are continuous
    pbDefinition$x0 <- matrix(unlist(lapply(seq_len(nd), function(i) {
      as.numeric(define$initialXVal()[, i])})), nrow = nrow(define$initialXVal()))
    pbDefinition$lb_orig <- matrix(unlist(lapply(seq_len(nd), function(i) {
      as.numeric(define$Xinfos$Xinfos[[i]]$bounds[1]) })), nrow = 1)
    pbDefinition$ub_orig <- matrix(unlist(lapply(seq_len(nd), function(i) {
      as.numeric(define$Xinfos$Xinfos[[i]]$bounds[2]) })), nrow = 1)
    pbDefinition$lb <- pbDefinition$lb_orig
    pbDefinition$ub <- pbDefinition$ub_orig
    
    # cat("pbDefinition$lb_orig : \n",pbDefinition$lb_orig, "\n")
    # cat("pbDefinition$ub_orig : \n",pbDefinition$ub_orig, "\n")
    # cat("pbDefinition$x0 before normalization: \n",pbDefinition$x0, "\n")
    
    if (pbDefinition$ncat > 0) {
      indcat <- which(sapply(define$Xinfos$Xinfos, function(x) "categorical" %in% x$type))
      indcont <- indcont[-indcat]
      pbDefinition$inputflag[indcat] <- "Cat"
      pbDefinition$lb[indcat] <- 1
      pbDefinition$ub[indcat] <- define$Xinfos$Xinfos[[indcat]]$nlevels
      pbDefinition$lb_orig[indcat] <- pbDefinition$lb[indcat]
      pbDefinition$ub_orig[indcat] <- pbDefinition$ub[indcat]
	    pbDefinition$levels <- sapply(define$Xinfos$Xinfos, function(x) x$levels)
	    pbDefinition$x0[, indcat] <- as.integer(unlist(sapply(indcat, function(i, Xinfos) {
        unlist(lapply(define$initialXVal()[, i], function(v) which(pbDefinition$levels[[i]] == v)))
	    })))
    }

    # Input bounds [0 ; 1] and normalization of x0 for numeric inputs 
    pbDefinition$lb[indcont] <- 0 * pbDefinition$lb_orig[indcont]
    pbDefinition$ub[indcont] <- 1 + pbDefinition$lb[indcont]
    # rbind is here to force to store a matrix, even if the input is a vector (in case of single initial point) to avoid an error in apply
    pbDefinition$x0[,indcont] <- t(unlist(apply(rbind(pbDefinition$x0[,indcont]), 1, 
                 function(x) (x - pbDefinition$lb_orig[indcont]) / (pbDefinition$ub_orig[indcont] - pbDefinition$lb_orig[indcont]))))
                 
    # cat("pbDefinition$lb : ",pbDefinition$lb, "\n")
    # cat("pbDefinition$ub : ",pbDefinition$ub, "\n")
    # cat("pbDefinition$x0 after normalization : ",pbDefinition$x0, "\n")
    
    colnames(pbDefinition$x0) <- paramNames
    pbDefinition$paramNames <- paramNames
    
    args <- list(
      optimFileName = optimDescr$Filename,
      optimFuncName = optimFuncName,
      optimFuncArgs = list(
        PbDefinition = pbDefinition,
        ParametersValues = lastOptimEnv$parametersValues,
        AdvancedParametersValues = lastOptimEnv$advParametersValues
      ),
      optimId = lastOptimEnv$id,
      simulatorName = doeProblemDef$simulatorName,
      workflowType = lastOptimEnv$workflowType,
      optimResultsInputId = ns("optimResults")
    )
    if (lastOptimEnv$workflowType == LAGUN_WF) {
      optimEnv$optimBgProcess <- callr::r_bg(
          func = futureOptim,
          args = list(args, TMP_DIR, INTERRUPT_FILE_PREFIX),
          supervise = TRUE
      )
    }

    # Send to client-side a 'optimAction' message (even if 'workflowType == LAGUN_WF', to store optim info)
    simulationsLauncher$optimAction(session, args)
    simulationsLauncher$getOptimList(session)
  }

  observe({
    req(optimEnv$optimBgProcess)
    if (optimEnv$optimBgProcess$is_alive()) {
      invalidateLater(millis = 1000, session = session)
    } else {
      # Catch interrupt (or any other error) and notify user
      result <- tryCatch({
          lastOptimEnv$res <- optimEnv$optimBgProcess$get_result()
          setAdvanceOptimText("Optimization completed")
          logger$print("Optimization completed")
          showNotification("Optimization completed")
        },
        error = function(e) {
          setAdvanceOptimText(paste0("Optimization interrupted (", e$parent, ")"))
          logger$print(e)
          showNotification(e$message)
        },
        finally = {
          savepath <- file.path(TMP_DIR, "Saved_Optimizations", lastOptimEnv$id)
          iterationsFile <- file.path(savepath, "iterations.json")
          cat(paste0(jsonlite::toJSON(lastOptimEnv$iterations$xTodoList, pretty = T), '\n'), file = iterationsFile)
          sendOptimLogs(savepath)
          del.file(tmp.file(lastOptimEnv$id, INTERRUPT_FILE_PREFIX, TMP_DIR))
          optimEnv$inProgress <- FALSE
          optimEnv$optimBgProcess <- NULL
          disableActionButton(ns("stopOptim"), session)
          enableActionButton(ns("launchOptim"), session)
          simulationsLauncher$getOptimList(session)
          isolate({
            persistence$autoSavingCount <- persistence$autoSavingCount + 1
            persistence$autoSavingCaller <- "directOptim-optimEnv$optimBgProcess-observe"
          })
        }
      )
    }
  })
  
  sendOptimLogs <- function(savepath) {
    wdToRestore <- getwd()
    tryCatch({
      setwd(savepath)
      logFiles <- gsub("./", "", dir(".", full.names = TRUE, recursive = TRUE))
      if (length(logFiles) > 0) {
        zipFileName <- paste0(savepath, ".zip")
        zip(zipFileName, logFiles)
        encodedZipContent <- jsonlite::base64_enc(readBin(zipFileName, "raw", 10e6))
        simulationsLauncher$storeBinFile(session, list(
          optimId = lastOptimEnv$id,
          fileName = paste0("Optim", lastOptimEnv$id, ".zip"),
          content = encodedZipContent
        ))
        unlink(zipFileName)
      }
      else {
        logger$print("No log files found")
      }
      # Try to remove 'save' directory (it seems to often fail, maybe locked by another operation not yet completed)
      unlink(savepath, recursive = TRUE)
    },
    error = function(e) {
      logger$print("'sendOptimLogs' failed")
      logger$print(e)
    },
    finally = {
      setwd(wdToRestore)
    })
  }

  # When client-side javascript changes the reactive input 'optimResults' ...
  observeEvent(input$optimResults, {
    if (lastOptimEnv$workflowType == LAGUN_WF) {
      logger$print("Unexpected 'optimResults' message received")
      return()
    }
    # display 'results'
    req(input$optimResults)
    results <- jsonlite::fromJSON(input$optimResults$results)
    req(results$optimId == lastOptimEnv$id)
    if (input$optimResults$success) {
      lastOptimEnv$res <- results$result
      setAdvanceOptimText("Optimization completed")
      logger$print("Optimization completed")
      showNotification("Optimization completed")
    } else {
      if (length(results$result) == 0) {
        lastOptimEnv$res <- "optim without result"
      }
      else {
        lastOptimEnv$res <- paste("optim failed:", results$result)
      }
      setAdvanceOptimText(lastOptimEnv$res)
      logger$print(results$result)
      showNotification(results$result)
    }
    disableActionButton(ns("stopOptim"), session)
    enableActionButton(ns("launchOptim"), session)
    simulationsLauncher$getOptimList(session)
    optimEnv$inProgress <- FALSE
    
    persistence$autoSavingCount <- persistence$autoSavingCount + 1
    persistence$autoSavingCaller <- "directOptim-optimResults-changed"
  })

  observeEvent(input$stopOptim, {
    setAdvanceOptimText("Stopping ...")
    if (lastOptimEnv$workflowType == LAGUN_WF) {
      cat("interrupt\n", file = tmp.file(lastOptimEnv$id, INTERRUPT_FILE_PREFIX, TMP_DIR))
      ask.Y.unlock(id = lastOptimEnv$id, dev.path = TMP_DIR)
    } else {
      # Send to client-side a 'stopOptim' message
      simulationsLauncher$stopOptim(session, lastOptimEnv$id)
      optimEnv$inProgress <- FALSE
    }
    disableActionButton(ns("stopOptim"), session)
    enableActionButton(ns("launchOptim"), session)
  })

  # 'reactiveVal' used to retrieve X from optim process
  # (no more use 'reactivePoll': When 'options(shiny.trace=TRUE)', generate successive 'SEND {"busy":"busy"} / SEND {"busy":"idle"}')
  # https://github.com/rstudio/shiny/issues/1829
  xTodoFile <- reactiveVal("")

  observe({
    if (optimEnv$inProgress == FALSE || lastOptimEnv$workflowType != LAGUN_WF || lastOptimEnv$iterationInProgress) {
      xTodoFile("")
    }
    else {
      xFile <- X.file(id = lastOptimEnv$id, dev.path = TMP_DIR)
      if (file.exists(xFile)) {
        xTodoFile(read.io(xFile))
      } else {
        xTodoFile("")
      }
      # Re-execute this reactive expression after 'INTERVAL_MILLIS' milliseconds
      invalidateLater(INTERVAL_MILLIS, session)
    }
  })  

  # If 'xTodoFile' or 'input$askYEvent' have been changed ...
  #' Compute Y for given X.
  #' If 'workflowType == LAGUN_WF', X are given through 'xTodo' file;
  #' if 'workflowType == LAUNCHER_WF or LAUNCHER_WF_ASKY_BY_LAGUN', X are given through 'askYEvent' reactive variable.
  observeEvent(c(xTodoFile(), input$askYEvent), {
    if (lastOptimEnv$workflowType == LAGUN_WF) {
      xTodo <- req(xTodoFile(), cancelOutput = TRUE)
    } else {
      req(input$askYEvent)
      askYEvent <- jsonlite::fromJSON(input$askYEvent)
      req(askYEvent$optimId == lastOptimEnv$id)
      xTodo <- as.data.frame(askYEvent$xTodo)
    }
    setAdvanceOptimText(paste("Running iteration", length(lastOptimEnv$iterations$xTodoList) + 1, "..."))
    isNewIteration <- all(xTodo[, ncol(xTodo)]) # is 'FALSE' when optimizer asks again same points to check constraints
    xTodo <- xTodo[, seq_len(ncol(xTodo) - 1)]
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))
    paramNames <- unlist(advance.simu$simulatorsInputs[[simulatorIndex]])
    colnames(xTodo) <- paramNames

    vectorSupport <- listsimulators$vector_support[[simulatorIndex]]
    vectorSize <- ifelse(vectorSupport, listsimulators$vector_size[[simulatorIndex]], 1)

    runTodoRefs <- c()
    launcherRunsAsList <- reactiveValuesToList(launcherRuns) # Use 'list' to improve speed
    newRunId <- length(launcherRunsAsList)
    if (newRunId != 0) {
      newRunId <- max(unlist(lapply(names(launcherRunsAsList), as.numeric))) + 1
    }
    runsToLaunch <- list()
    runsToLaunch["runToLaunchIds"] = list(c()) # References runs which don't have an associated run and are to launch
    runsToLaunch["runToRelaunchIds"] = list(c()) # References runs which already have an associated run and are to launch
    paramValuesBlock <- NULL
    for (i in seq_len(nrow(xTodo))) {
      paramValues <- factor2String(xTodo[i, , drop = F])
      foundRunRef <- searchLauncherRunRef(paramValues, launcherRunsAsList, advance.simu$xHashMap, lastOptimEnv$simulatorId)
      if (is.null(foundRunRef)) {
        approx <- buildXApprox(paramValues)
        paramValuesBlock <- rbind(paramValuesBlock, paramValues)
        newRunIdAsString <- toString(newRunId)
        launcherRunRef <- list(runIdAsString = newRunIdAsString, position = nrow(paramValuesBlock))
        advance.simu$xHashMap[[buildXHashcode(approx)]] <<- launcherRunRef
        runTodoRefs <- append(runTodoRefs, list(launcherRunRef))
        if (
          nrow(paramValuesBlock) == vectorSize ||
          i == nrow(xTodo)
        ) {
          launcherRuns[[newRunIdAsString]]$paramValues <<- paramValuesBlock
          launcherRuns[[newRunIdAsString]]$simulatorId <<- lastOptimEnv$simulatorId
          launcherRunsAsList <- reactiveValuesToList(launcherRuns) # update 'launcherRunsAsList'
          runsToLaunch$runToLaunchIds <- c(runsToLaunch$runToLaunchIds, newRunIdAsString)
          newRunId <- newRunId + 1
          paramValuesBlock <- NULL
        }
      }
      else {
        runTodoRefs <- append(runTodoRefs, list(foundRunRef))
        foundRun <- launcherRunsAsList[[foundRunRef$runIdAsString]]
        if (!is.null(foundRun$status) && foundRun$status != "ended" && foundRun$status != "disabled") {
          runsToLaunch$runToRelaunchIds <- c(runsToLaunch$runToRelaunchIds, foundRunRef$runIdAsString)
        }
      }
    }
    lastOptimEnv$iterationInProgress <- TRUE
    if (isNewIteration) {
      lastOptimEnv$iterations$xTodoList[[length(lastOptimEnv$iterations$xTodoList) + 1]] <- xTodo
      lastOptimEnv$iterations$runRefsList[[length(lastOptimEnv$iterations$runRefsList) + 1]] <- runTodoRefs
    }
    # printLauncherRuns("askYEvent", launcherRuns)
    checkIfTodoCompleted()
    launcherDataHasChanged()

    # if 'workflowType == LAGUN_WF or LAUNCHER_WF_ASKY_BY_LAGUN', send a message to add new runs
    if (lastOptimEnv$workflowType == LAGUN_WF || lastOptimEnv$workflowType == LAUNCHER_WF_ASKY_BY_LAGUN) {
      if (length(runsToLaunch$runToRelaunchIds) != 0) {
        args <- list(
          runIds = as.numeric(runsToLaunch$runToRelaunchIds),
          simulatorId = lastOptimEnv$simulatorId
        )
        simulationsLauncher$configureLaunchLoad(session, args)
      }
      
      if (length(runsToLaunch$runToLaunchIds) != 0) {
        launcherRunsAsList <- reactiveValuesToList(launcherRuns)

        xTodo <- lapply(runsToLaunch$runToLaunchIds, function(runIdAsString) {
          values <- launcherRunsAsList[[runIdAsString]]$paramValues
          colnames(values) <- NULL
          return(lapply(values, function(e) e))
        })

        args <- list(
          runs = list(
            paramNames = paramNames,
            mat = xTodo
          ),
          simulatorId = lastOptimEnv$simulatorId
        )
        simulationsLauncher$addRuns(session, args)
      }
    }
  })

  observeEvent(reactiveValuesToList(launcherRuns), {
    checkIfTodoCompleted()
    launcherDataHasChanged()
  })

  checkIfTodoCompleted <- function() {
    if (!lastOptimEnv$iterationInProgress || optimEnv$inProgress == FALSE) {
      return()
    }
    runTodoRefs <- lastOptimEnv$iterations$runRefsList[[length(lastOptimEnv$iterations$runRefsList)]]
    completed <- sapply(runTodoRefs, function(runRef) {
      launcherRun <- launcherRuns[[runRef$runIdAsString]]
      status <- launcherRun$status
      return(!is.null(status) && (status == "ended" || status == "onerror"))
    })

    # if all iteration points are completed
    if (all(completed)) {
      # Extract Y values for this iteration and send them to optim 'ask.Y'
      y <- NULL
      for (runRef in runTodoRefs) {
        resultBlock <- launcherRuns[[runRef$runIdAsString]]$result
        result <- resultBlock[runRef$position, , drop = F]
        if (is.null(y)) {
          y <- result
        } else {
          y <- rbind(y, result)
        }
      }
      # Clean 'lastOptimEnv$iterationInProgress' to avoid to send optim results if other 'runs' event occured
      lastOptimEnv$iterationInProgress <- FALSE

      yRes <- as.matrix(y)

      setAdvanceOptimText("Sending iteration results ...")
      if (lastOptimEnv$workflowType == LAGUN_WF) {
        # Send result through a file which will be read by 'ask.Y'
        tell.Y(yRes, id = lastOptimEnv$id, dev.path = TMP_DIR)
      }
      else if (lastOptimEnv$workflowType == LAUNCHER_WF_ASKY_BY_LAGUN) {
        # Send result to simulations launcher
        simulationsLauncher$tellY(
          session, 
          list(y = yRes, optimId = lastOptimEnv$id)
        )
      }
    }
  }

  data.plot <- reactiveValues(dataX = NULL, rowNames = NULL, runIds = NULL, dataY = NULL)

  launcherDataHasChanged <- function() {
    if (optimEnv$inProgress == FALSE || length(lastOptimEnv$iterations$xTodoList) == 0) {
      return()
    }
    dataX <- NULL
    dataY <- NULL
    rowNames <- NULL
    runRefs <- list()
    simulatorIndex <- min(which(doeProblemDef$simulatorName == listsimulators$names))

    # Build yNames
    yNames <- NULL
    if (is.null(advance.simu$yNames)) {
      yNames <- defineCalibDOE$ynames
    }
    else {
      yNames <- isolate(advance.simu$yNames)
    }
    if (is.list(listsimulators$nY[[simulatorIndex]])) {
      yNames <- c(
        yNames,
        paste0("OF", seq_along(lastOptimEnv$calibration$idZ)),
        "OFtotal"
      )
    }
    
    # Determine rowCount
    rowCount <- 0
    for (iterIndex in seq_along(lastOptimEnv$iterations$xTodoList)) {
      xTodo <- lastOptimEnv$iterations$xTodoList[[iterIndex]]
      rowCount <- rowCount + nrow(xTodo)
    }

    if (rowCount != 0) {
      dataY <- data.plot$dataY
      if (is.null(dataY) || ncol(dataY) != length(yNames) || nrow(dataY) != rowCount) {
        dataY <- matrix(NA_real_, nrow = rowCount, ncol = length(yNames))
      }
      colnames(dataY) <- yNames
      rownames(dataY) <- seq_len(rowCount)

      rowIndex <- 0
      for (iterIndex in seq_along(lastOptimEnv$iterations$xTodoList)) {
        xTodo <- lastOptimEnv$iterations$xTodoList[[iterIndex]]
        rownames(xTodo) <- rowIndex + seq_len(nrow(xTodo))
        if (is.null(dataX)) {
          dataX <- xTodo
        } else {
          colnames(dataX) <- colnames(xTodo)
          dataX <- rbind(dataX, xTodo)
        }

        if (nrow(xTodo) == 1) {
          rowName <- paste0(iterIndex, "_")
        }
        else {
          rowName <- paste0(iterIndex, "_", seq_len(nrow(xTodo)))
        }

        if (is.null(rowNames)) {
          rowNames <- rowName
        } else {
          rowNames <- c(rowNames, rowName)
        }

        for (iterRunIndex in seq_along(lastOptimEnv$iterations$runRefsList[[iterIndex]])) {
          runRef <- lastOptimEnv$iterations$runRefsList[[iterIndex]][[iterRunIndex]]
          launcherRun <- launcherRuns[[runRef$runIdAsString]]

          runRefs <- append(runRefs, list(runRef))

          if (is.null(launcherRun$result) || is.null(launcherRun$status) || launcherRun$status != "ended") {
            if (is.list(listsimulators$nY[[simulatorIndex]])) {
              launcherRun$ofRes <- NULL
            }
          }
          else {
            yValues <- as.matrix(launcherRun$result)
            dataY[rowIndex + 1, seq_len(ncol(yValues))] <- yValues[runRef$position, , drop = F]

            if (is.list(listsimulators$nY[[simulatorIndex]])) {
              nY <- listsimulators$nY[[simulatorIndex]]
              ofRes <- launcherRun$ofRes
              if (is.null(ofRes)) {
                OFtot <- c()
                OF <- c()
                for (i in seq_len(nrow(yValues))) {
                  ofRes <- computeOF(lastOptimEnv$calibration, yValues[i, ])
                  OFtot <- rbind(OFtot, unlist(ofRes$OFtot))
                  OF <- rbind(OF, unlist(ofRes$OF))
                }
                ofRes <- list(OF = OF, OFtot = OFtot)
                launcherRuns[[runRef$runIdAsString]]$ofRes <- ofRes
              }
              funcCount <- length(lastOptimEnv$calibration$idZ)
              dataY[rowIndex + 1, ncol(yValues) + seq_len(funcCount + 1)] <- as.matrix(cbind(ofRes$OF[runRef$position, , drop = F], ofRes$OFtot[runRef$position, , drop = F]))
            }
          }

          rowIndex <- rowIndex + 1
        }
      }
    }
    data.plot$dataX <- dataX
    data.plot$rowNames <- rowNames
    data.plot$runRefs <- runRefs
    data.plot$dataY <- dataY
  }

  output$downloadIterates.ui <- renderUI({
    req(data.plot$dataX, data.plot$dataY, nrow(data.plot$dataY) > 0)
    fluidRow(
      column(9, ""),
      column(3, downloadButton(ns("downloadIterates"), label = "Export Iterates", class = "btn-primary"), align = "right")
    )
  })
  
  output$downloadIterates <- downloadHandler(
    filename = 'Iterates.csv',
    content = function(con) {
      xy <- cbind(
        head(data.plot$dataX, nrow(data.plot$dataY)),
        data.plot$dataY
      )
      # rownames(xy) <- data.plot$rowNames

      write.table(x = xy, file = con, row.names = F, col.names = T, sep = ",")
    }
  )

  callModule(simTable.server, "table.simulations", define = define, data.plot = data.plot, launcherRuns = launcherRuns, advance.simu = advance.simu)

  callModule(resultsPlot.server, "resultsPlotTab", define = define, data.plot = data.plot)

  callModule(inputsPlot.server, "inputsPlotTab", define = define, data.plot = data.plot)

  callModule(spm.server, "scatterPlotMatrixTab", define = define, data.plot = data.plot)

  callModule(functionalPlot.server, "funcPlotTab", constrainedDefineDOE, window.dimension)

  isFunctional <- reactive({
    length(constrainedDefineDOE$Yinfos$func.ids) > 0
  })

  observeEvent(isFunctional(), {
    if (isFunctional()) {
      showTab(inputId = "tabs", target = ns("funcPlot"))
    }
    else {
      hideTab(inputId = "tabs", target = ns("funcPlot"))
      updateTabsetPanel(inputId = "tabs", selected = ns("resultsPlot"))
    }
  })

  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "directOptim-simulator") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      
      if (
        !is.null(persistence$loadedStudy$doeProblemDef) &&
        !is.null(persistence$loadedStudy$doeProblemDef$choice) &&
        persistence$loadedStudy$doeProblemDef$choice == 4
      ) {
        doeProblemDef$simulatorName <- persistence$loadedStudy$doeProblemDef$simulatorName
        initWhenSimulatorIsSet()
        updateCalibrationDOE()
      }
      progressToNextStep(persistence)
    }
    else if (persistence$updatingStep == "directOptim-optimArgs") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      
      if (
        !is.null(persistence$loadedStudy$doeProblemDef) &&
        !is.null(persistence$loadedStudy$doeProblemDef$choice) &&
        persistence$loadedStudy$doeProblemDef$choice == 4
      ) {
        optimEnv$workflowType <- persistence$loadedStudy$directOptim$workflowType
        optimEnv$optimFileName <- persistence$loadedStudy$directOptim$optimFileName
        previousOptimArgs(list(
          optimFileName = persistence$loadedStudy$directOptim$optimFileName,
          optimFuncArgs = list(
            ParametersValues = persistence$loadedStudy$directOptim$parametersValues,
            AdvancedParametersValues = persistence$loadedStudy$directOptim$advParametersValues
          )
        ))
      }
      progressToNextStep(persistence)
    }
    else if (persistence$updatingStep == "directOptim-results") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      
      if (
        !is.null(persistence$loadedStudy$doeProblemDef) &&
        !is.null(persistence$loadedStudy$doeProblemDef$choice) &&
        persistence$loadedStudy$doeProblemDef$choice == 4
      ) {
        lastOptimEnv$simulatorId <- persistence$loadedStudy$directOptim$simulatorId

        lastOptimEnv$iterations$xTodoList <- list()
        lastOptimEnv$iterations$runRefsList <- list()
        lastOptimEnv$calibration <- NULL
        data.plot$dataY <- NULL
        data.plot$dataX <- NULL
        data.plot$rowNames <- NULL
        data.plot$runRefs <- NULL
        
        if (!is.null(persistence$loadedStudy$directOptim$iterations)) {
          lastOptimEnv$iterations <- persistence$loadedStudy$directOptim$iterations

          launcherRunsAsList <- reactiveValuesToList(launcherRuns)
          for (iterIndex in seq_along(lastOptimEnv$iterations$xTodoList)) {
            xTodo <- lastOptimEnv$iterations$xTodoList[[iterIndex]]
            runTodoRefs <- c()
            for (i in seq_len(nrow(xTodo))) {
              paramValues <- factor2String(xTodo[i, , drop = F])
              foundRunRef <- searchLauncherRunRef(paramValues, launcherRunsAsList, advance.simu$xHashMap, lastOptimEnv$simulatorId)
              if (is.null(foundRunRef)) {
                persistence$report <- c(persistence$report, paste("Failed to find run associated to iteration:", iterIndex, "simulation:", i))
                break
              }
              else {
                runTodoRefs <- append(runTodoRefs, list(foundRunRef))
              }
            }
            lastOptimEnv$iterations$runRefsList[[length(lastOptimEnv$iterations$runRefsList) + 1]] <- runTodoRefs
          }
          
          lastOptimEnv$calibration <- persistence$loadedStudy$directOptim$calibration
          optimEnv$inProgress <- TRUE
          launcherDataHasChanged()
          optimEnv$inProgress <- FALSE
        }
      }
      progressToNextStep(persistence)
    }
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  return(list(
    DOE = DOE, 
    optimConf = lastOptimEnv
  ))
}
