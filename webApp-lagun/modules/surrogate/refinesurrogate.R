#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module refinesurrogate
source("modules/shared/dynamicSelect.R", local = TRUE)

# load all surrogate functions
source('modules/surrogate/surrogate_functions.R', local = TRUE)

computeAdditionalSimulations <- function(DOE, nadd, ntest, yname, models, callback) {

  dimx <- DOE$nX
  dimy <- DOE$nY
  Xadd <- as.data.frame(matrix(nrow = nadd, ncol = dimx))
  colnames(Xadd) <- colnames(DOE$X)
  Xbounds <- get.bounds(DOE$Xinfos)
  if ("All" %in% yname) {
    idyadd <- 1:dimy 
  } else {
    idyadd <- which(DOE$ynames %in% yname)
  }
  nidyadd <- length(idyadd)
  Xtest <- generateXtest(Xbounds,ntest,DOE)
  modelstemp <- list()
  for (i in 1:nidyadd) {
    modelstemp[[i]] <- models[[idyadd[i]]]
  }
  # Begin loop on points to add
  Xtemp <- Xtest
  for (a in 1:nadd) {
    nsy <- nrow(Xtemp)
    nextpoint <- onestep.improve.metamodel(modelstemp,Xtemp,criterion="mse")
    Xadd[a,] <- nextpoint$Xbest
    Xtemp <- Xtemp[-nextpoint$idbest,]
    constantliar <- as.data.frame(matrix(NA, nrow = 1, ncol = nidyadd))
    for (i in 1:nidyadd){
      constantliar[i] <- predict.metamodel(modelstemp[[i]],Xadd[a,],computesd=FALSE)$mean
    }
    
    # Update kriging models
    for (i in 1:nidyadd){
      modelstemp[[i]] <- update.metamodel(modelstemp[[i]],Xadd = Xadd[a,],Yadd = constantliar[1,i])
    }
    callback(a)
  }
  
  nsimu <- nrow(Xadd)
  rownames(Xadd) <- paste0("Simu", 1:nsimu)
  return(Xadd)
}

refinesurrogate.ui <- function(id) {
  ns <- NS(id)
  
  panel <-  wellPanel(
    selectInput(
      ns("criteria"), 
      label = "Improvement Criteria",
      choices = list("Global Accuracy"),
      selected = "Global Accuracy"
    ),
    dynamicSelect.ui(ns("chooseY")),
    fluidRow(
      column(6, numericInput(ns("nadd"), "Number of Additional Simulations", 1, min = 1)),
      uiOutput(ns("tagDOEUI"))
    ),
    fluidRow(
      column(7, disabled(actionButton(ns("generate"), "Generate Additional Simulations",
                             icon = icon("table"), class = "btn-info", width = '100%'))),
      conditionalPanel(condition =  paste0("output['", ns("use_simulator"), "']"),
        column(5, disabled(actionButton(ns("launch.simu"), "Launch Simulations", class = 'btn-primary',
                                 icon = icon('cog'), width = '100%')))
      )
    )
  )

  mainPanel(
    conditionalPanel(
      condition = paste0("output['", ns("conditionalRefine"), "']"),
      tagList(
        bsModal(ns("modalLaunchSimu"), "Choose Mode  to Launch Additional Simulations", NULL,
          fluidRow(
            column(6, h5('Send the additional simulations to the "importDOE" panel where they can be
                        manually launched.')),
            column(6, h5('Automatically launch the additional simulations with the current simulator settings.'))
          ),
          fluidRow(
            column(6, actionButton(ns('simu.manual'), 'Manual', class='btn-primary', width = '100%',
                                icon = icon("table"))),
            column(6, actionButton(ns('simu.automatic'), 'Automatic', class='btn-warning', width = '100%',
                                icon = icon("play-circle")))
          )
        ),
        fluidRow(
          column(4, panel),
          column(8, uiOutput(ns("preview.dynui")))
        )
      )
    ),
    conditionalPanel(
      condition = paste0("output['", ns("conditionalRefine"), "'] == false"),
      fluidRow(
        p("Refinement criteria not available for selected surrogate model")
      )
    )
  )
}

refinesurrogate.server <- function(input, output, session,  DOE, listmodels, doeProblemDef, settings) {
  
  ns <- session$ns
  
  output$conditionalRefine <- reactive({
      req(any(!is.na(listmodels$selected$id)))
      refinableModels <- lapply(listmodels$selected$id,function(i){listmodels$withsdmodels[i]})
      return(any(unlist(refinableModels)))
  })

  outputOptions(output, "conditionalRefine", suspendWhenHidden = FALSE)

  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice != 1)
    }
    return(bool)
  })

  choicesY <- reactive({
    req(DOE$ynamesmenu,DOE$Yinfos)
    
    listCombineMode <- unlist(sapply(DOE$compositeInfos, function(x) if(x$modelMode == "Combine") x$id))
    idSurrogate <- which(!is.na(listmodels$selected$id))[listmodels$selected$id %in% which(listmodels$withsdmodels)]
    int <- setdiff(intersect(DOE$Yinfos$int.ids, idSurrogate), listCombineMode)
    ctl <- setdiff(intersect(DOE$Yinfos$control.ids, idSurrogate), listCombineMode)
    
    l <- list()
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[int])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[ctl])
    return(l)
  })
  
  yname <- callModule(dynamicSelectpicker.server, "chooseY", label.title = "Choose Output(s) to Improve", choices = choicesY, 
                      selected = DOE$ynamesmenu[DOE$Yinfos$int.ids[1]], multiple = TRUE, livesearch = TRUE)
  
  output$use_simulator <- use_simulator
  outputOptions(output, 'use_simulator', suspendWhenHidden = FALSE)
  
  simulations = reactiveValues(Xadd = NULL, mode.manual = NULL, mode.automatic = NULL, tagDOE = "Refine Global 1", nRefine = 1)
  
  observeEvent(DOE$XY, {
    simulations$Xadd <- NULL
  })
  
  observeEvent(input$simu.manual, {
    req(simulations$Xadd, input$launch.simu)
    simulations$mode.manual <- input$simu.manual
    simulations$tagDOE <- input$tagDOE
    toggleModal(session, "modalLaunchSimu", toggle = "close")
  })
  
  observeEvent(input$simu.automatic, {
    req(simulations$Xadd, input$launch.simu)
    simulations$mode.automatic <- input$simu.automatic
    simulations$tagDOE <- input$tagDOE
    toggleModal(session, "modalLaunchSimu", toggle = "close")
  })
  
  models <- reactiveValues(models=NULL)
  observe({
    req(!is.null(listmodels$trainedModels))
    models$models <- lapply(1:DOE$nY, function (i){
      if (length(listmodels$selected$id[[i]])){
        return(listmodels$models[[listmodels$selected$id[[i]]]][[i]])
      }else{
        return(NULL)
      }
    })
  })
  
  observeEvent(list(yname(), listmodels$trainedModels, models$models), {
    if (!is.null(listmodels$trainedModels)) {
      shinyjs::enable("generate")
    }
    else {
      shinyjs::disable("generate")
    }
  })
  
  observeEvent(input$generate, {
    req(yname(), !is.null(listmodels$trainedModels),models$models)
    nadd <- input$nadd
    callback <- function(a) {
      incProgress(1/nadd, detail = paste("Adding", a,"/", nadd))
    }
    withProgress(message = 'Identifying Additional Simulations...', value = 0, {
      Xadd <- computeAdditionalSimulations(
        DOE, nadd, settings$nfaure, DOE$ynames[yname()], models$models, callback
      )
    })
    simulations$Xadd <- Xadd
    if (use_simulator()){
      if (simulations$tagDOE == input$tagDOE){
        simulations$tagDOE <- paste("Refine Global ", simulations$nRefine)
      }else{
        simulations$tagDOE <- input$tagDOE
      }
      simulations$nRefine <- simulations$nRefine + 1
    }
  })
  
  output$tagDOEUI <- renderUI({
    req(use_simulator())
    column(6, textInput(ns("tagDOE"), label = 'Tag DOE Info', value = simulations$tagDOE, width = '100%'))
  })
  
  output$preview.dynui <- renderUI({
    req(simulations$Xadd)
    ns <- session$ns
    tagList(
      fluidRow(
        column(8, h4("Proposed Additional Simulations")),
        column(4, downloadButton(ns("download"), label = "Export Additional Simulations", class = "btn-primary"))
      ),
      hr(),
      DT::dataTableOutput(ns("table"))
    )
  })

  observeEvent(simulations$Xadd, {
    if (!is.null(simulations$Xadd)) {
      shinyjs::enable("launch.simu")
    }
    else {
      shinyjs::disable("launch.simu")
    }
  })
  
  observeEvent(input$launch.simu, {
    toggleModal(session, "modalLaunchSimu", toggle = "open")
  })
  
  output$table <- DT::renderDataTable({
    req(simulations$Xadd)
    df <- simulations$Xadd
    dimd <- ncol(df)
    colnames(df) <- DOE$xnamesvisu
    DT::datatable(
      df, escape = FALSE,
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip',
        buttons = list(list(extend = 'colvis', columns = 1:dimd)),
        scrollX = TRUE,scrollY = 200,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  output$download <- downloadHandler(
    filename = 'AdditionalSimulations.csv',
    content = function(con) {
      df <- simulations$Xadd
      colnames(df) <- DOE$xnamesmenu
      write.table(x = simulations$Xadd, file = con, row.names = F, col.names = T, sep=",")
    }
  )
  
  return(simulations)
}
