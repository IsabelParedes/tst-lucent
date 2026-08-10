#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module dynamicSelect
dynamicSelect.ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("select"))
}

dynamicSelect.server <- function(input, output, session, label, choices, 
                                 multiple = FALSE, selected.ind = 1) {
  ns <- session$ns
  
  output$select <- renderUI({
    req(choices())
    selectInput(
      ns("choice"), 
      label = label,
      choices = choices(),
      selected = choices()[selected.ind],
      multiple = multiple
    )
  })
  
  choiceVal <- reactiveVal(NULL)
  
  choice <- reactive({
    req(choices())
    input$choice
  })
  
  observeEvent(choice(), {
    choiceVal(choice())
  })
  
  return(choiceVal)
}

# testing
dynamicSelect.test <- function() {
  source("loadPackages.R")
  
  ui <- fluidPage(
    theme = "bootstrap_spacelab.css",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tags$head(tags$script(src = "custom.js")),
    numericInput("nbr", "Number of Inputs", 1, min = 1),
    dynamicSelect.ui("chooseX"),
    dynamicSelect.ui("chooseY"),
    hr(),
    verbatimTextOutput("rawX"),
    verbatimTextOutput("rawY")
  )
  
  server <- function(input, output, session) {
    choicesX <- reactive({
      # to test handling of NULL, chooseX should appear only when nbr>1
      req(input$nbr>1)
      paste0("X", 1:input$nbr)
    })
    choiceX <- callModule(dynamicSelect.server, "chooseX", label = "choose X", choicesX)
    output$rawX <- renderPrint({
      list(X = choiceX())
    })
    
    choicesY <- reactive({
      # to test handling of NULL, chooseY should appear only when nbr>3
      req(input$nbr>3)
      paste0("Y", 1:input$nbr)
    })
    choiceY <- callModule(dynamicSelect.server, "chooseY", label = "choose Y", choicesY)
    output$rawY <- renderPrint({
      list(Y = choiceY())
    })
  }
  
  # app
  shinyApp(ui, server)
}
