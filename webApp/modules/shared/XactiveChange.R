#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module XactiveChange

XactiveChange.ui <- function(id, label = "Change Outputs Type", width = NULL) {
  ns <- NS(id)
  
  modalContent <- tagList(
    fluidRow(
      column(4, h4("Manually Change Inputs Activation", div("(please save)", class = "small"))),
      column(4, actionButton(ns("reset"), label = "Reset", class = "btn-primary", icon = icon("sync")))
    ),
    hr(),
    fluidRow(
      column(3,""),
      column(6,
             uiOutput(ns("change.activation.ui"))),
      column(3,"")
    ),
    hr(),
    uiOutput(ns("footer"))
    )
  
  tagList(
    actionButton(ns("change"), label = label, class = "btn-primary", width = width),
    bsModal(ns("modal"), "Change Inputs Activation", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}"))))
  )
}

XactiveChange.server <- function(input, output, session, Xinfos) {
  
  ns <- session$ns
  
  nX <- reactive(length(Xinfos$Xinfos))
  
  # initialXactivation is the initial activations coming from Xinfos (used when the user wants to reset)
  # Xactivationtemp contains the changes made by the user inside the modal
  # Xactivation is updated only when the user saves
  initialXactivation <- reactiveValues(idon = NULL, idoff = NULL)
  Xactivation <- reactiveValues(idon = NULL, idoff = NULL)
  Xactivationtemp <- reactiveValues(idon = NULL, idoff = NULL)
  
  # we reinitialize initialXactivation, Xactivation and Xactivationtemp when Xinfos has changed
  observe({
    req(Xinfos$Xinfos)
    idon <- unlist(lapply(1:nX(),function(i){if (!Xinfos$Xinfos[[i]]$type=="constant") return(i)}))
    idoff <- setdiff(1:nX(),idon)
    initialXactivation$idon <- idon
    initialXactivation$idoff <- idoff
    Xactivation$idon <- idon
    Xactivation$idoff <- idoff
    Xactivationtemp$idon <- idon
    Xactivationtemp$idoff <- idoff
  })
  
  # We populate the switches with Xactivation
  output$change.activation.ui <- renderUI({
    req(Xactivation$idon)
    lapply(1:nX(),function(i) switchInput(ns(paste0('inputsactive', i)), label=Xinfos$Xinfos[[i]]$namemenu,
                                        value=(i%in%Xactivation$idon), size = "large"))
  })
  
  # Update Xactivationtemp if switches are changed
  manual.trigger.switch <- reactive({
    req(Xactivation$idon)
    lapply(1:nX(), function(i){
      input[[paste0('inputsactive', i)]]
    })
  })
  observeEvent(manual.trigger.switch(), {
    req(!is.null(input[["inputsactive1"]]))
    idon <- unlist(lapply(1:nX(),function(i){if (input[[paste0('inputsactive', i)]]) return(i)}))
    idoff <- setdiff(1:nX(),idon)
    Xactivationtemp$idon <- idon
    Xactivationtemp$idoff <- idoff
  })

  # reinitialize switches when the user actively reset the Outputs
  observeEvent(input$reset, {
    lapply(1:nX(),function(i) updateSwitchInput(session,paste0('inputsactive', i), value=(i%in%initialXactivation$idon)))
  })
  observeEvent(input$change, {
    toggleModal(session, "modal", toggle = "open")
  })
  observeEvent(input$save, {
    Xactivation$idon <- Xactivationtemp$idon
    Xactivation$idoff <- Xactivationtemp$idoff
    toggleModal(session, "modal", toggle = "close")
  })
  observeEvent(input$close, {
    lapply(1:nX(),function(i) updateSwitchInput(session,paste0('inputsactive', i), value=(i%in%Xactivation$idon)))
    toggleModal(session, "modal", toggle = "close")
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
  
  return(Xactivation)
}
