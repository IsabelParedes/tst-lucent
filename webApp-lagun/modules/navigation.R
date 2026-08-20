#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module navigation

# source children modules
source("modules/introduction/introduction.R", local = TRUE)
source("modules/prepareDOE/prepareDOE.R", local = TRUE)
source("modules/menuImport/menuImport.R", local = TRUE)
source("modules/surrogate/surrogate.R", local = TRUE)
source("modules/menuExplore/menuExplore.R", local = TRUE)
source("modules/menuOptimize/menuOptimize.R", local = TRUE)
source("modules/menuMore/menuMore.R", local = TRUE)
source("modules/workflow/workflow.R", local = TRUE)
source("modules/saveLoadStudy/saveStudy.R", local = TRUE)


navigation.ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    includeScript(path = "modules/hideTabs.js"),
    navbarPage(
      "Computer Code Exploration Platform Lagun (V1.0.3)", position = "fixed-top", inverse = T, windowTitle = "Lagun",
      tabPanel("Introduction", introduction.ui(id = ns("introduction")), icon = NULL),
      tabPanel("Prepare DOE", prepareDOE.ui(id = ns("prepareDOE")), icon = icon("table"), value =ns("tabprepareDOE")),
      menuImport.ui(id = ns("menuImport")),
      tabPanel("Surrogate Model", surrogate.ui(id = ns("surrogate")), icon = icon("calculator"), value =ns("tabsurrogateModel")),
      menuExplore.ui(id = ns("menuExplore")),
      menuOptimize.ui(id = ns("menuOptimize")),
      tabPanel("Save Study", 
               saveStudyUI(ns("saveStudy")), 
               icon = icon("save"), 
               value = ns("tabSaveStudy")),
      menuMore.ui(id = ns("menuMore")),
      tabPanel("Info", workflow.ui(id = ns("workflow")), icon = icon("list")), 
      id=ns("clickedtab")
    )
  )
}

navigation.server <- function(input, output, session, window.dimension) {
  
  autoSavingId <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
  
  persistence <- reactiveValues(
    loadedStudy = NULL, # Content of the currently loaded study
    updatingSteps = c(), # List of the updating steps to proceed
    updatingStep = "OFF", # Current updating step to proceed
    report = c(), # List of messages to display at the end of the updating process
    autoSavingEnabled = getOption("lagun.autosaving", default = T),
    autoSavingCount = 0, # Number of autosaving done (changes trigger saving)
    autoSavingCaller = NULL, # Tells which part of the code has triggered autosaving
    autoSavingId = autoSavingId, # Identifier of this study
    autoSavingFileName = autoSavingFileName(autoSavingId) # Name of file where content of autosaving is stored
  )
  
  ns <- session$ns
  
  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice != 1)
    }
    return(bool)
  })

  importDOEtab.clicked <- reactive({
    if (input$clickedtab == "nav-menuImport-tabimportDOE"){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })
  prelimexploDOEtab.clicked <- reactive({
    if (input$clickedtab == "nav-menuImport-tabprelimExplo"){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })
  surrogateDOEtab.clicked <- reactive({
    if (input$clickedtab == "nav-tabsurrogateModel"){
      return(TRUE)
    }else{
      return(FALSE)
    }
  })
  
  observeEvent(displayModules$prepareDOE,{
    if (!displayModules$prepareDOE){
      runjs("$('a[data-value=\"nav-tabprepareDOE\"]').hide();")
    }else{
      runjs("$('a[data-value=\"nav-tabprepareDOE\"]').show();")
    }
  })
  
  observeEvent(displayModules$surrogateModel,{
    if (!displayModules$surrogateModel){
      runjs("$('a[data-value=\"nav-tabsurrogateModel\"]').hide();")
    }else{
      runjs("$('a[data-value=\"nav-tabsurrogateModel\"]').show();")
    }
  })
  
  observe({
    req(!is.null(displayModules$importDOE) & !is.null(displayModules$prelimExplo))
    if (!displayModules$importDOE & !displayModules$prelimExplo){
      runjs("$('a[data-value=\"Problem Definition\"]').hide();")
    }else{
      runjs("$('a[data-value=\"Problem Definition\"]').show();")
      if (!displayModules$importDOE){
        runjs("$('a[data-value=\"nav-menuImport-tabimportDOE\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuImport-tabimportDOE\"]').show();")
      }
      if (!displayModules$prelimExplo){
        runjs("$('a[data-value=\"nav-menuImport-tabprelimExplo\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuImport-tabprelimExplo\"]').show();")
      }
      
      if (tabs.completed$importDOEFunctional){
        runjs("$('a[data-value=\"nav-menuImport-tabdefineCalib\"]').show();")
      }else{
        runjs("$('a[data-value=\"nav-menuImport-tabdefineCalib\"]').hide();")
      }
      
    }
    
  })
  
  observe({
    req(!is.null(displayModules$exploreSurrogate) & !is.null(displayModules$UQGSASurrogate))
    if (!displayModules$exploreSurrogate & !displayModules$UQGSASurrogate){
      runjs("$('a[data-value=\"Explore\"]').hide();")
    }else{
      runjs("$('a[data-value=\"Explore\"]').show();")
      if (!displayModules$exploreSurrogate){
        runjs("$('a[data-value=\"nav-menuExplore-tabexploreSurrogate\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuExplore-tabexploreSurrogate\"]').show();")
      }
      if (!displayModules$UQGSASurrogate){
        runjs("$('a[data-value=\"nav-menuExplore-tabUQGSASurrogate\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuExplore-tabUQGSASurrogate\"]').show();")
      }
    }
  })
  
  observe({
    req(!is.null(displayModules$optimSurrogate) & !is.null(displayModules$roboptimSurrogate) 
        & !is.null(displayModules$seqoptimSurrogate))
    if (!displayModules$optimSurrogate & !displayModules$roboptimSurrogate & !displayModules$seqoptimSurrogate){
      runjs("$('a[data-value=\"Optimize\"]').hide();")
    }else{
      runjs("$('a[data-value=\"Optimize\"]').show();")
      if (!displayModules$optimSurrogate){
        runjs("$('a[data-value=\"nav-menuOptimize-taboptimSurrogate\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuOptimize-taboptimSurrogate\"]').show();")
      }
      if (!use_simulator() | !displayModules$seqoptimSurrogate){
        runjs("$('a[data-value=\"nav-menuOptimize-taboptimSequential\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuOptimize-taboptimSequential\"]').show();")
      }
      if (!displayModules$roboptimSurrogate){
        runjs("$('a[data-value=\"nav-menuOptimize-tabroboptimSurrogate\"]').hide();")
      }else{
        runjs("$('a[data-value=\"nav-menuOptimize-tabroboptimSurrogate\"]').show();")
      }
    }
  })
  
  # Change display according to modifications made in workflow panel
  observe({
    displayModules$prepareDOE <- workflowdisplayModules$prepareDOE
    displayModules$importDOE <- workflowdisplayModules$importDOE
    displayModules$prelimExplo <- workflowdisplayModules$prelimExplo
    displayModules$surrogateModel <- workflowdisplayModules$surrogateModel
    displayModules$exploreSurrogate <- workflowdisplayModules$exploreSurrogate
    displayModules$UQGSASurrogate <- workflowdisplayModules$UQGSASurrogate
    displayModules$optimSurrogate <- workflowdisplayModules$optimSurrogate
    displayModules$seqoptimSurrogate <- workflowdisplayModules$seqoptimSurrogate
    displayModules$roboptimSurrogate <- workflowdisplayModules$roboptimSurrogate
  })

  tabs.completed <- reactiveValues(importDOE = FALSE, surrogate = FALSE, importDOEFunctional = FALSE)
  observe({
    tabs.completed$importDOE <- !all(is.na(DOE$Y))
    tabs.completed$surrogate <- tabs.completed$importDOE & !is.null(unlist(listmodels$models))
    tabs.completed$importDOEFunctional <- ifelse(length(DOE$Yinfos$func.ids)==0, FALSE, TRUE)
  })
  
  observeEvent(list(tabs.completed$importDOE, workflow.selected$complete, workflow.selected$exploreData,
                    workflow.selected$surrogate), {
    if (workflow.selected$exploreData){
      displayModules$prelimExplo <- tabs.completed$importDOE
    }
    if (workflow.selected$complete | workflow.selected$surrogate){
      displayModules$prelimExplo <- displayModules$surrogateModel <- tabs.completed$importDOE
    }
  })
  
  observeEvent(tabs.completed$surrogate, {
    if (workflow.selected$surrogate){
      displayModules$exploreSurrogate <- displayModules$UQGSASurrogate <- tabs.completed$surrogate
    }
    if (workflow.selected$complete){
      displayModules$exploreSurrogate <- displayModules$UQGSASurrogate <-
      displayModules$optimSurrogate <- displayModules$seqoptimSurrogate <- tabs.completed$surrogate
      if (length(DOE$Yinfos$func.ids) == 0){
        displayModules$roboptimSurrogate <- tabs.completed$surrogate
      }
    }
  })
  
  output$importDOEtab.clicked <- importDOEtab.clicked
  outputOptions(output, 'importDOEtab.clicked', suspendWhenHidden = FALSE)
  output$prelimexploDOEtab.clicked <- prelimexploDOEtab.clicked
  outputOptions(output, 'prelimexploDOEtab.clicked', suspendWhenHidden = FALSE)
  output$surrogateDOEtab.clicked <- surrogateDOEtab.clicked
  outputOptions(output, 'surrogateDOEtab.clicked', suspendWhenHidden = FALSE)
  introModule <- callModule(introduction.server, "introduction")
  displayModules <- introModule$displayModules
  workflow.selected <- introModule$workflow.selected
  workflowdisplayModules <- callModule(workflow.server, "workflow", displayModules, tabs.completed)
  settings <- callModule(menuMore.server, "menuMore", listmodels)
  import <- callModule(menuImport.server, "menuImport", DOEX, Xadd, XaddUQ, outputMenuOptimize$XaddSeqOptim, outputMenuOptimize$XaddUnconstOptim, outputMenuOptimize$XaddConstOptim, persistence, settings, import.clicked = importDOEtab.clicked,
                       prelimexplo.clicked = prelimexploDOEtab.clicked, window.dimension) 
  DOE <- import$DOE
  ML <- import$ML
  DOEX <- callModule(prepareDOE.server, "prepareDOE", persistence, settings)
  advance.importDOE <- import$advance.importDOE
  doeProblemDef <- import$doeProblemDef
  surrogate <- callModule(surrogate.server, "surrogate", DOE, settings, doeProblemDef,
                           surrogate.clicked = surrogateDOEtab.clicked, advance.importDOE = advance.importDOE, persistence)
  listmodels <- surrogate$listmodels
  Xadd <- surrogate$simulations
  UQModel <- callModule(menuExplore.server, "menuExplore", DOE, ML, listmodels, doeProblemDef, window.dimension, persistence, settings)
  XaddUQ <- UQModel$simulations
  optim <- callModule(menuOptimize.server, "menuOptimize", DOE, listmodels, advance.importDOE, persistence, settings, doeProblemDef)
  outputMenuOptimize <- optim$outputMenuOptimize
  saveStudyServer("saveStudy", persistence, DOEX, import$DOE.manual, DOE, doeProblemDef, import$calibration, listmodels, UQModel$sensitivityAnalysis, UQModel$UQparams, UQModel$UQres, UQModel$UQproba, optim$unconstrOptim, optim$constrOptim, import$directOptim)
}
