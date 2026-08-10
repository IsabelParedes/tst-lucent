#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module importDOE
LAUNCHER_PROTOCOL_VERSION <- 3

source("modules/menuImport/importDOE/uploadDOE.R", local = TRUE)
source("modules/menuImport/importDOE/confSimulator.R", local = TRUE)
source("modules/menuImport/importDOE/directOptim.R", local = TRUE)
source("modules/saveLoadStudy/loadStudy.R", local = TRUE)

importDOE.ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(condition =  paste0("output['", ns("upload_file_bool"), "']"),
      bsCollapse(
        multiple = TRUE, open = "Import",
        bsCollapsePanel(
          "Import",  style = "primary",
          uploadDOE.ui(id = ns("uploadDOE"))
        )
      )
    ),
    conditionalPanel(condition =  paste0("output['", ns("perform_DOE"), "']"),
                     bsCollapse(
                       open = "Configure simulator",
                       bsCollapsePanel(
                         "Configure simulator",  style = "primary",
                         confSimulator.ui(id = ns("confSimulator"))
                       )
                     )
    ),
    conditionalPanel(condition =  paste0("output['", ns("perform_direct_optim"), "']"),
                     bsCollapse(
                       open = "Direct Optimization",
                       bsCollapsePanel(
                         "Direct Optimization",  style = "primary",
                         directOptim.ui(id = ns("directOptim"))
                       )
                     )
    )
  )
}

importDOE.server <- function(input, output, session, DOEX, Xadd, XaddUQ, XaddSeqOptim, XaddUnconstOptim, XaddConstOptim, persistence, settings, import.clicked, window.dimension) {
  
  ns <- session$ns
  simulations_launcher_url <- Sys.getenv("simulations_launcher_url")

  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["simulations_launcher_url"]])) {
      simulations_launcher_url <<- query[["simulations_launcher_url"]]
    }
  })

  dataModal <- function(){
    simulations_launcher_url <<- ifelse(nchar(simulations_launcher_url) != 0, simulations_launcher_url, "http://localhost:3000")
    ns <- session$ns
    modalDialog(
      tagList(
        useShinyjs(),
        radioGroupButtons(
          inputId = ns("mode"),
          choiceNames = c("New study", "Load study", "Use simulator"),
          choiceValues = c("new", "load", "simulator"),
          justified = TRUE,
          size = "lg",
          status = "primary"
        ),
        hidden(tags$div(id=ns("loadUI"), loadStudyUI(ns("loadStudy")))),
        hidden(
          tags$div(
            id=ns("useSimulator"),
            fluidRow(column(3,""), 
                     column(6, h6(paste0("Used URL for simulations launcher is: ", simulations_launcher_url)))
            ),
            fluidRow(column(3,""), 
                     column(6,
                            conditionalPanel(paste0("input['", ns("launcherProtocolVersion"), "'] == 0",
                                              " || input['", ns("launcherProtocolVersion"), "'] == null"),
                                             tagList(
                                               h5("Please launch the simulations launcher.")
                                             )
                            ),
                            conditionalPanel(paste0("input['", ns("launcherProtocolVersion"), "'] == -1"),
                                             tagList(
                                               h5("Waiting for the simulations launcher protocol version ... (please check that the running simulations launcher is not too old)")
                                             )
                            ),
                            conditionalPanel(paste0("input['", ns("launcherProtocolVersion"), "'] == ", LAUNCHER_PROTOCOL_VERSION),
                                             selectizeInput(ns("runSet"), "Run Set:", character(0), options = list(
                                               create = TRUE,
                                               createOnBlur = TRUE,
                                               createFilter = I("/^([0-9A-Za-z_-]+)$/")
                                             ))
                            ),
                            conditionalPanel(paste0("input['", ns("launcherProtocolVersion"), "'] == ", LAUNCHER_PROTOCOL_VERSION),
                                             prettyRadioButtons(
                                               inputId = ns("import.type"),
                                               label = "",
                                               choiceNames = c("Study with a DOE: Upload file", 
                                                               "Study with a DOE: Import DOE from previous tab",
                                                               "Study without a DOE: Direct optimization"),
                                               choiceValues = c("Upload file", 
                                                                "Import DOE from previous tab",
                                                                "Direct optimization"),
                                               selected = "Import DOE from previous tab",
                                               icon = icon("ok", lib = "glyphicon")
                                             )
                            ),
                            conditionalPanel(paste0("input['", ns("launcherProtocolVersion"), "'] != ", LAUNCHER_PROTOCOL_VERSION, 
                                              " && input['", ns("launcherProtocolVersion"), "'] != -1",
                                              " && input['", ns("launcherProtocolVersion"), "'] != null",
                                              " && input['", ns("launcherProtocolVersion"), "'] != 0"),
                                             tagList(
                                               h5(textOutput(ns("launcherProtocolVersion")))
                                             )
                            )
                     )            
                     
                     
            )
          )
        ),
        fluidRow(
          column(3,""),
          column(6, actionButton(ns("confirm.import.mode"), label = "Confirm", class = "btn-primary",width = '100%')),
          column(3,"")
        )
      ),
      title = "Choose Mode", footer = NULL, easyClose = FALSE
    )
  }

  output$launcherProtocolVersion  <- renderText({
    paste0("Please use a simulations launcher with protocol version ", LAUNCHER_PROTOCOL_VERSION, " (current one is: ", input$launcherProtocolVersion, ")")
  })

  finalProblemDef <- reactiveValues(choice = NULL, runSet = NULL, simulatorName = NULL)
  
  observeEvent(input$mode, {
    
    if (input$mode=="load"){
      simulationsLauncher$disconnectFromSimulationsLauncher(session)
      showElement("loadUI", time = 0.5, anim = TRUE, animType = "slide")
      hideElement("useSimulator", time = 0.5, anim = TRUE, animType = "slide")
      hideElement("confirm.import.mode", time = 0.25, anim = TRUE, animType = "slide")
      
    }else if (input$mode=="simulator"){
      simulationsLauncher$connectToSimulationsLauncher(session, simulations_launcher_url)
      hideElement("loadUI", time = 0.5, anim = TRUE, animType = "slide")
      showElement("useSimulator", time = 0.5, anim = TRUE, animType = "slide")
      showElement("confirm.import.mode", time = 0.25, anim = TRUE, animType = "slide")
      
    }else{
      simulationsLauncher$disconnectFromSimulationsLauncher(session)
      hideElement("loadUI", time = 0.5, anim = TRUE, animType = "slide")
      hideElement("useSimulator", time = 0.5, anim = TRUE, animType = "slide")
      showElement("confirm.import.mode", time = 0.25, anim = TRUE, animType = "slide")
      
    }
  })
  
  observeEvent(import.clicked(), {
    if(import.clicked() & is.null(finalProblemDef$choice)){
      showModal(dataModal())
      updateRunsSetInput()
    }
  })
  
  observeEvent(input$confirm.import.mode,{
    if (input$mode=="simulator"){
      finalProblemDef$runSet <- input$runSet
      
      allchoices <- c("Upload file", 
                      "Import DOE from previous tab",
                      "Direct optimization")
      finalProblemDef$choice <- which(input$import.type==allchoices) + 1
    }else{
      finalProblemDef$choice <- 1
    }
    removeModal(session)
  })
  
  observeEvent(finalProblemDef$runSet, {
    req(finalProblemDef$runSet)
    # Send a message to attach Lagun to the selected runSet
    simulationsLauncher$attachSet(session, finalProblemDef$runSet)
  })
  
  observeEvent(advance.simu$runsSets, {
    updateRunsSetInput()
  })

  updateRunsSetInput <- function() {
    defaultRunSet <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
    if (length(advance.simu$runsSets) > 0) {
      choices <- c(defaultRunSet, rev(advance.simu$runsSets))
    }
    else {
      choices <- defaultRunSet
    }
    updateSelectInput(session, inputId = "runSet", choices = choices)
  }
  
  upload_file_bool <- reactive({
    bool <- FALSE
    if (!is.null(finalProblemDef$choice)){
      bool <- (finalProblemDef$choice==1 || finalProblemDef$choice==2)
    }
    return(bool)
  })
  
  output$upload_file_bool <- upload_file_bool
  outputOptions(output, 'upload_file_bool', suspendWhenHidden = FALSE)
  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(finalProblemDef$choice)){
      bool <- (finalProblemDef$choice != 1)
    }
    return(bool)
  })
  perform_direct_optim <- reactive({
    bool <- FALSE
    if (!is.null(finalProblemDef$choice)){
      bool <- (finalProblemDef$choice == 4)
    }
    return(bool)
  })
  perform_DOE <- reactive({
    bool <- FALSE
    if (!is.null(finalProblemDef$choice)){
      bool <- finalProblemDef$choice == 2 || finalProblemDef$choice == 3
    }
    return(bool)
  })
  output$perform_direct_optim <- perform_direct_optim
  outputOptions(output, 'perform_direct_optim', suspendWhenHidden = FALSE)
  output$perform_DOE <- perform_DOE
  outputOptions(output, 'perform_DOE', suspendWhenHidden = FALSE)

  DOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL, 
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL,
    idon = NULL, compositeInfos = NULL,
    discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesvisu = NULL
  )
  advance.simu <- reactiveValues(total = NULL, completed = NULL, running = NULL, waiting = NULL, status = NULL, info = NULL)
  
  outputConfSimulator <- callModule(confSimulator.server, "confSimulator", DOEX, DOE.manual, Xadd, XaddUQ, XaddSeqOptim, XaddUnconstOptim, XaddConstOptim,
                         finalProblemDef, persistence, settings)
  outputDirectOptim <- callModule(directOptim.server, "directOptim", advance.simu, outputConfSimulator$launcherRuns, finalProblemDef, persistence, settings, window.dimension)
  loadStudyServer("loadStudy", persistence)
  DOE.manual <- callModule(uploadDOE.server, "uploadDOE", finalProblemDef, persistence, settings)

  observe({
      advance.simu.current <- outputConfSimulator$advance.simu
      advance.simu$total <- advance.simu.current$total
      advance.simu$completed <- advance.simu.current$completed
      advance.simu$running <- advance.simu.current$running
      advance.simu$waiting <- advance.simu.current$waiting
      advance.simu$status <- advance.simu.current$status
      advance.simu$info <- advance.simu.current$info
      advance.simu$simulatorsConfigs <- advance.simu.current$simulatorsConfigs
      advance.simu$simulatorsInputs <- advance.simu.current$simulatorsInputs
      advance.simu$optimInfoList <- advance.simu.current$optimInfoList
      advance.simu$runsSets <- advance.simu.current$runsSets
      advance.simu$yNames <- advance.simu.current$yNames
      advance.simu$xHashMap <- advance.simu.current$xHashMap
  })

  observe({
    if (use_simulator()){
      if (perform_DOE()){
        DOE.simu <- outputConfSimulator$DOE
      }else{
        DOE.simu <- outputDirectOptim$DOE
      }
      DOE$Xopt = DOE.simu$Xopt
      DOE$Xinfos = DOE.simu$Xinfos
      DOE$Yinfos = DOE.simu$Yinfos
      DOE$nYsurrogate = DOE.simu$nYsurrogate
      DOE$XY = DOE.simu$XY
      DOE$X = DOE.simu$X
      DOE$Y = DOE.simu$Y
      DOE$nobs = DOE.simu$nobs
      DOE$nX = DOE.simu$nX
      DOE$nY = DOE.simu$nY
      xnames <- DOE.simu$xnames
      xnamesmenu <- DOE.simu$xnamesmenu
      xnamesvisu <-  DOE.simu$xnamesvisu
      names(xnames) <- xnamesmenu
      names(xnamesvisu) <- xnamesmenu
      ynames <- DOE.simu$ynames
      ynamesmenu <- DOE.simu$ynamesmenu
      ynamesvisu <-  DOE.simu$ynamesvisu
      names(ynames) <- ynamesmenu
      names(ynamesvisu) <- ynamesmenu
      DOE$xnames <- xnames
      DOE$ynames <- ynames
      DOE$xnamesvisu <- xnamesvisu
      DOE$ynamesvisu <- ynamesvisu
      DOE$xnamesmenu <- xnamesmenu
      DOE$ynamesmenu <- ynamesmenu
      DOE$adapt.visu = FALSE
      if (!is.null(DOE$nX)){
        DOE$idon <- 1:DOE$nX
      }
      #DOE$idref <- DOE.simu$idref
      #DOE$compositeInfos <- DOE.simu$compositeInfos
      DOE$discF <- DOE.simu$discF
      DOE$nF <- DOE.simu$nF
      DOE$idF <- DOE.simu$idF
      DOE$Fnames <- DOE.simu$Fnames
      DOE$Fnamesmenu <- DOE.simu$Fnamesmenu
      DOE$Fnamesvisu <- DOE.simu$Fnamesvisu
    }else{
      DOE$Xopt = DOE.manual$Xopt
      DOE$Xinfos = DOE.manual$Xinfos
      DOE$Yinfos = DOE.manual$Yinfos
      DOE$nYsurrogate = DOE.manual$nYsurrogate
      DOE$XY = DOE.manual$XY
      DOE$X = DOE.manual$X
      DOE$Y = DOE.manual$Y
      DOE$nobs = DOE.manual$nobs
      DOE$nX = DOE.manual$nX
      DOE$nY = DOE.manual$nY
      xnames <- DOE.manual$xnames
      xnamesmenu <- DOE.manual$xnamesmenu
      xnamesvisu <-  DOE.manual$xnamesvisu
      names(xnames) <- xnamesmenu
      names(xnamesvisu) <- xnamesmenu
      ynames <- DOE.manual$ynames
      ynamesmenu <- DOE.manual$ynamesmenu
      ynamesvisu <-  DOE.manual$ynamesvisu
      names(ynames) <- ynamesmenu
      names(ynamesvisu) <- ynamesmenu
      DOE$xnames = xnames
      DOE$ynames = ynames
      DOE$xnamesvisu = xnamesvisu
      DOE$ynamesvisu = ynamesvisu
      DOE$xnamesmenu = xnamesmenu
      DOE$ynamesmenu = ynamesmenu
      DOE$adapt.visu = DOE.manual$adapt.visu
      DOE$idon <- DOE.manual$idon
      DOE$idref <- DOE.manual$idref
      DOE$compositeInfos <- DOE.manual$compositeInfos
      DOE$discF <- DOE.manual$discF
      DOE$nF <- DOE.manual$nF
      DOE$idF <- DOE.manual$idF
      DOE$Fnames <- DOE.manual$Fnames
      DOE$Fnamesvisu <- DOE.manual$Fnamesvisu
    }
  })

  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "importDOE") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))

      if (is.null(persistence$loadedStudy$doeProblemDef)) {
        finalProblemDef$choice <- 1
      }
      else {
        finalProblemDef$choice <- persistence$loadedStudy$doeProblemDef$choice
        finalProblemDef$runSet <- persistence$loadedStudy$doeProblemDef$runSet
        if (finalProblemDef$choice > 1) {
          simulationsLauncher$connectToSimulationsLauncher(session, simulations_launcher_url)
        }
      }

      progressToNextStep(persistence)
    }
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)

  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "OFF" && !is.null(persistence$loadedStudy)) {
      removeModal(session)
      persistence$loadedStudy <- NULL
      persistence$showReport <- length(persistence$report) != 0
    }
  })

  observeEvent(input$launcherProtocolVersion, {
    if (input$launcherProtocolVersion  == -1) {
      logger$print(paste("Retrieving 'launcherProtocolVersion' value from the simulations launcher ..."))
    }
    else if (input$launcherProtocolVersion  == 0) {
      logger$print(paste("'simulatorsSocket' is disconnected"))
    }
    else if (input$launcherProtocolVersion  == LAUNCHER_PROTOCOL_VERSION) {
      logger$print(paste("'launcherProtocolVersion' value received:",  input$launcherProtocolVersion))
      retrieveLauncherData()
    }
  })
  
  retrieveLauncherData <- function() {
    # send a message to retrieve names of existing run sets, simulator info, etc.
    simulationsLauncher$retrieveLauncherData(session)
  }
  
  return(list(DOE.manual = DOE.manual, DOE = DOE, advance.simu = advance.simu, problemDef = finalProblemDef, directOptim = outputDirectOptim$optimConf))
}
