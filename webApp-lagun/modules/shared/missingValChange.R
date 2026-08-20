#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module missingValChange

missingValChange.ui <- function(id, label = "Define Missing Values", width = NULL) {
  ns <- NS(id)
  
  modalContent <- tagList(
    fluidRow(
      column(8,actionButton(ns("setcommon"), label = h5("Set Common Value for All Outputs"), class = "btn-primary")),
      column(4,textInput(ns('defNAall'),label=NULL,value="NA"))
    ),
    hr(),
    br(),
    uiOutput(outputId = ns("defineNA_dynui")),
    uiOutput(ns("footer"))
  )
  
  tagList(
    actionButton(ns("change"), label = label, class = "btn-primary", width = width),
    bsModal(ns("modal"), "Define Missing Values", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}"))))
  )
}

missingValChange.server <- function(input, output, session, initialNA) {
  
  ns <- session$ns
  
  NAvalues.temp <- reactiveValues(val=NULL)
  NAvalues <- reactiveValues(val=NULL)
  
  # Initialize values with NAs
  observe({
    NAvalues.temp$val <- initialNA$val
    NAvalues$val <- initialNA$val
  })
  
  output$defineNA_dynui <- renderUI({
    req(initialNA$nY>0,!is.null(NAvalues.temp$val))
    lapply(1:initialNA$nY,function(i){
      fluidRow(
        column(8,h5(initialNA$ynames[i])),
        column(4,textInput(ns(paste0('defNA', i)),label=NULL,value=NAvalues.temp$val[i]))
      )
    })
  })
  
  observeEvent(input$setcommon,{
    req(initialNA$nY>0)
    lapply(1:initialNA$nY, function(i){updateTextInput(session,paste0('defNA', i), value=input$defNAall)})
  })
  
  manual.trigger.type <- reactive({
    req(initialNA$nY>0)
    lapply(1:isolate(initialNA$nY), function(i){
      input[[paste0('defNA', i)]]
    })
  })
  
  observeEvent(manual.trigger.type(),{
    req(initialNA$nY>0)
    NAvalues.temp$val <- unlist(lapply(1:initialNA$nY, function(i){input[[paste0('defNA', i)]]}))
  })
  
  observeEvent(input$change, {
    toggleModal(session, "modal", toggle = "open")
  })
  
  observeEvent(input$close, {
    toggleModal(session, "modal", toggle = "close")
    NAvalues.temp$val <- NAvalues$val
  })

  observeEvent(input$save, {
    toggleModal(session, "modal", toggle = "close")
    NAvalues$val <- NAvalues.temp$val
  })
  
  output$footer <- renderUI({
    list(
      fluidRow(
        column(3, actionButton(ns("save"), label = "Save and Close", class = "btn-warning",
                               width = '100%'), offset = 2),
        column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                               width = '100%'), offset = 2)
      )
    )
  })
  
  return(NAvalues)
}