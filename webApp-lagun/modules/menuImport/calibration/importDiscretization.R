#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

importDiscretization.ui <- function(id) {
  ns <- NS(id)

  modalContent <- tagList(
    fluidRow(
      uiOutput(ns("selectOutputUI")),
      column(2, radioButtons(ns("separator"), "Separator",
                             choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"))),
      column(2, radioButtons(ns("decimal"), "Decimal",
                             choices = list(". (point)" = ".", ", (comma)" = ","))
      ),
      column(3,
             fileInput(ns('file'), 'Select file', accept = c('.txt', '.dat','.csv')),
             tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });')),
             uiOutput(ns('warning.file'))
      ),
      column(2, br(), actionButton(ns("save"), label = HTML(paste("Save Output", "Discretization", sep='<br>')), class = "btn-warning",
                             width = '100%'),
                uiOutput(ns("saveMessage")))
    ),
    hr(),
    DT::dataTableOutput(ns('preview')),
    hr(),
    fluidRow(
      column(4, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                             width = '100%'), offset = 4)
    )
  )
  
  tagList(
    uiOutput(ns("modalButton")),
    bsModal(ns("modal"), "Import Output Discretization", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}",
                                        " .modal-lg{width: 70%}"))))
  )
    
}

importDiscretization.server <- function(input, output, session, DOE) {
  
  ns <- session$ns
  
  importDiscF <- reactiveValues(discF = list(), discFtemp = NULL, saveMessage = NULL)
  
  observeEvent(input$import, {
    toggleModal(session, "modal", toggle = "open")
    disableActionButton(ns("save"), session)
  })
  
  output$modalButton <- renderUI({
    
    if (length(DOE$nF)>0){
      column(2, actionButton(ns("import"), label = HTML(paste("Import", "Discretization", sep='<br>')), class = "btn-primary"),
             h5("Here you can import the discretization of functional outputs for calibration."), align="center")
    }else{
      column(2, "")
    }

  })
  
  output$selectOutputUI <- renderUI({
    req(DOE$Fnames)
    column(3, selectInput(ns("selectedOutput"), label = "Select Output", choices = c("All", DOE$Fnames)))
  })
  
  observeEvent(input$file$datapath, {
    lines <- readLines(input$file$datapath, warn = F)
    lines.split <- lapply(lines, function(line){
      line <- as.character(unlist(strsplit(line, input$separator)))
    }) 
    header <- anyNA(suppressWarnings(unlist(lapply(lines.split[[1]], as.numeric))))
    if (header){
      discFnames <- lines.split[[1]]
      lines.split <- lines.split[-1]
    }else{
      discFnames <- paste0('t', 1:length(lines.split[[1]]))
    }
    discFtemp <- lapply(lines.split, function(line){
      unlist(lapply(line, as.numeric))
    })
    discFtemp <- as.data.frame(do.call('rbind', discFtemp))
    colnames(discFtemp) <- discFnames
    importDiscF$discFtemp <- discFtemp
    importDiscF$saveMessage <- NULL
    enableActionButton(ns("save"), session)
  })
  
  observeEvent(input$save, {
    req(input$selectedOutput)
    if (input$selectedOutput == "All"){
      for (j in 1:length(DOE$nF)){
        importDiscF$discF[[j]] <- importDiscF$discFtemp
      }
      toggleModal(session, "modal", toggle = "close")
    }else{
      importDiscF$discF[[which(input$selectedOutput == DOE$Fnames)]] <- importDiscF$discFtemp
      importDiscF$saveMessage <- paste("Discretization for Output", input$selectedOutput, "is updated !")
    }
    disableActionButton(ns("save"), session)
  })
  
  observeEvent(input$selectedOutput, {
    importDiscF$saveMessage <- NULL
    enableActionButton(ns("save"), session)
  })
  
  observeEvent(input$close, {
    toggleModal(session, "modal", toggle = "close")
  })
  
  output$saveMessage <- renderText({
    importDiscF$saveMessage
  })
  
  output$preview <- DT::renderDataTable({
    req(importDiscF$discFtemp)
    DT::datatable(
      importDiscF$discFtemp, escape = FALSE,
      extensions = c('FixedColumns','Scroller'),
      options = list(
        dom = 't', ordering=F,
        scrollX = TRUE, scrollY = 400, scroller = TRUE, fixedColumns = TRUE
      )
    )
  })
  
  return(importDiscF)
  
}
