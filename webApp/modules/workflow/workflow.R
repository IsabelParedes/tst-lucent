#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module workflow

workflow.ui <- function(id) {
  ns <- NS(id)
  
  infoContent <-  tagList(
    h2("Workflow Information"), hr(), 
    uiOutput(ns("infoDOE")), hr(), 
    uiOutput(ns("infoTrainedModels")), hr(), 
    uiOutput(ns("infoFinalModel"))
  )
  tagList(
    fluidRow(
      column(4,
             wellPanel(
               checkboxGroupButtons(inputId = ns("displayPrepareDOE"),
                                    label = "Prepare DOE",
                                    choices = "Prepare DOE",
                                    selected = "Prepare DOE",
                                    status = "primary",
                                    checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                     no = icon("remove",lib = "glyphicon")))
             )
      ),
      column(4, 
             wellPanel(
               checkboxGroupButtons(inputId = ns("displayImportData"),
                                    label = "Problem Definition",
                                    choices = c("Import DOE","Preliminary Exploration"),
                                    selected = c("Import DOE","Preliminary Exploration"),
                                    status = "primary",
                                    checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                     no = icon("remove",lib = "glyphicon")),
                                    individual = TRUE)
             )
      ),
      column(4,
             wellPanel(
               checkboxGroupButtons(inputId = ns("displaySurrogateModel"),
                                    label = "Surrogate Model",
                                    choices = "Surrogate Model",
                                    status = "primary",
                                    checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                     no = icon("remove",lib = "glyphicon"))
               )
             )
      )
    ),
    fluidRow(
      column(4,
             wellPanel(
               checkboxGroupButtons(inputId = ns("displayExplore"),
                                    label = "Explore",
                                    choices = c(HTML(paste("Exploration","with Surrogate Model", sep = '<br/>')),HTML(paste("UQ & GSA","with Surrogate Model", sep = '<br/>'))),
                                    status = "primary",
                                    checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                     no = icon("remove",lib = "glyphicon")),
                                    individual = TRUE)
             )
      ),
      column(5, 
             wellPanel(
               checkboxGroupButtons(inputId = ns("displayOptimize"),
                                    label = "Optimize",
                                    choices = c(HTML(paste("Optimization","with Surrogate Model", sep = '<br/>')),
                                                HTML(paste("Sequential Optimization","with Surrogate Model", sep = '<br/>')),
                                                HTML(paste("Robust Optimization","with Surrogate Model", sep = '<br/>'))),
                                    status = "primary",
                                    checkIcon = list(yes = icon("ok",lib = "glyphicon"),
                                                     no = icon("remove",lib = "glyphicon")),
                                    individual = TRUE)
             )
      ),
      column(3,""
      )
    )
  )
}

workflow.server <- function(input, output, session, intro.displayModules, tabs.completed) {
  
  ns <- session$ns
  
  # displayModules stores changes of display modules done in the workflow panel
  displayModules <- reactiveValues(prepareDOE = NULL, importDOE = NULL, prelimExplo = NULL, surrogateModel = NULL,
                                   exploreSurrogate = NULL, UQGSASurrogate = NULL, optimSurrogate = NULL, seqoptimSurrogate = NULL,
                                   roboptimSurrogate = NULL)
  
  # Initialize checkboxes with display modules defined in the intro panel
  observe({
    if (is.null(intro.displayModules$prepareDOE)){
      sel <- "Prepare DOE"
    }else{
      if (intro.displayModules$prepareDOE){
        sel <- "Prepare DOE"
      }else{
        sel <- character(0)
      }
    }
    updateCheckboxGroupButtons(session = session, inputId = "displayPrepareDOE", selected = sel)
  })
  
  observe({
    if (is.null(intro.displayModules$importDOE)){
      sel <- "Import DOE"
    }else{
      if (intro.displayModules$importDOE){
        sel <- "Import DOE"
      }else{
        sel <- character(0)
      }
    }
    if (is.null(intro.displayModules$prelimExplo)){
      sel <- c(sel,"Preliminary Exploration")
    }else{
      if (intro.displayModules$prelimExplo & tabs.completed$importDOE){
        sel <- c(sel,"Preliminary Exploration")
      }
    }
    updateCheckboxGroupButtons(session = session, inputId = "displayImportData", selected = sel)
  })
  
  observe({
    if (is.null(intro.displayModules$surrogateModel)){
      sel <- "Surrogate Model"
    }else{
      if (intro.displayModules$surrogateModel & tabs.completed$importDOE){
        sel <- "Surrogate Model"
      }else{
        sel <- character(0)
      }
    }
    updateCheckboxGroupButtons(session = session, inputId = "displaySurrogateModel", selected = sel)
  })
  
  observe({
    if (is.null(intro.displayModules$exploreSurrogate)){
      sel <- HTML(paste("Exploration","with Surrogate Model", sep = '<br/>'))
    }else{
      if (intro.displayModules$exploreSurrogate & tabs.completed$surrogate){
        sel <- HTML(paste("Exploration","with Surrogate Model", sep = '<br/>'))
      }else{
        sel <- character(0)
      }
    }
    if (is.null(intro.displayModules$UQGSASurrogate)){
      sel <- c(sel,HTML(paste("UQ & GSA","with Surrogate Model", sep = '<br/>')))
    }else{
      if (intro.displayModules$UQGSASurrogate & tabs.completed$surrogate){
        sel <- c(sel,HTML(paste("UQ & GSA","with Surrogate Model", sep = '<br/>')))
      }
    }
    updateCheckboxGroupButtons(session = session, inputId = "displayExplore", selected = sel)
  })
  
  observe({
    if (is.null(intro.displayModules$optimSurrogate)){
      sel <- HTML(paste("Optimization","with Surrogate Model", sep = '<br/>'))
    }else{
      if (intro.displayModules$optimSurrogate & tabs.completed$surrogate){
        sel <- HTML(paste("Optimization","with Surrogate Model", sep = '<br/>'))
      }else{
        sel <- character(0)
      }
    }
    if (is.null(intro.displayModules$seqoptimSurrogate)){
      sel <- c(sel, HTML(paste("Sequential Optimization","with Surrogate Model", sep = '<br/>')))
    }else{
      if (intro.displayModules$seqoptimSurrogate & tabs.completed$surrogate){
        sel <- c(sel, HTML(paste("Sequential Optimization","with Surrogate Model", sep = '<br/>')))
      }
    }
    if (is.null(intro.displayModules$roboptimSurrogate)){
      sel <- c(sel,HTML(paste("Robust Optimization","with Surrogate Model", sep = '<br/>')))
    }else{
      if (intro.displayModules$roboptimSurrogate & tabs.completed$surrogate){
        sel <- c(sel,HTML(paste("Robust Optimization","with Surrogate Model", sep = '<br/>')))
      }
    }
    updateCheckboxGroupButtons(session = session, inputId = "displayOptimize", selected = sel)
  })
  
  # Record changes made in the workflow panel
  observe({
    
    if (is.null(input$displayPrepareDOE)){
      displayModules$prepareDOE <- FALSE
    }else{
      displayModules$prepareDOE <- TRUE
    }
    
    if (is.null(input$displayImportData)){
      displayModules$importDOE <- FALSE
      displayModules$prelimExplo <- FALSE
    }else{
      if ("Import DOE" %in% input$displayImportData){
        displayModules$importDOE <- TRUE
      }else{
        displayModules$importDOE <- FALSE
      }
      if ("Preliminary Exploration" %in% input$displayImportData){
        displayModules$prelimExplo <- TRUE
      }else{
        displayModules$prelimExplo <- FALSE
      }
    }
    
    if (is.null(input$displaySurrogateModel)){
      displayModules$surrogateModel <- FALSE
    }else{
      displayModules$surrogateModel <- TRUE
    }
    
    if (is.null(input$displayExplore)){
      displayModules$exploreSurrogate <- FALSE
      displayModules$UQGSASurrogate <- FALSE
    }else{
      if (HTML(paste("Exploration","with Surrogate Model", sep = '<br/>')) %in% input$displayExplore){
        displayModules$exploreSurrogate <- TRUE
      }else{
        displayModules$exploreSurrogate <- FALSE
      }
      if (HTML(paste("UQ & GSA","with Surrogate Model", sep = '<br/>')) %in% input$displayExplore){
        displayModules$UQGSASurrogate <- TRUE
      }else{
        displayModules$UQGSASurrogate <- FALSE
      }
    }
    
    if (is.null(input$displayOptimize)){
      displayModules$optimSurrogate <- FALSE
      displayModules$seqoptimSurrogate <- FALSE
      displayModules$roboptimSurrogate <- FALSE
    }else{
      if (HTML(paste("Optimization","with Surrogate Model", sep = '<br/>')) %in% input$displayOptimize){
        displayModules$optimSurrogate <- TRUE
      }else{
        displayModules$optimSurrogate <- FALSE
      }
      if (HTML(paste("Sequential Optimization","with Surrogate Model", sep = '<br/>')) %in% input$displayOptimize){
        displayModules$seqoptimSurrogate <- TRUE
      }else{
        displayModules$seqoptimSurrogate <- FALSE
      }
      if (HTML(paste("Robust Optimization","with Surrogate Model", sep = '<br/>')) %in% input$displayOptimize){
        displayModules$roboptimSurrogate <- TRUE
      }else{
        displayModules$roboptimSurrogate <- FALSE
      }
    }
    
  })
  

  return(displayModules)
  
}