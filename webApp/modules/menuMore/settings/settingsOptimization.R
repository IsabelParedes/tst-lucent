#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsOptimization
settingsOptimization.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    numericInput(ns("nmultistart"), "Number of Multistart", 50, min = 1),
    selectInput(
      ns("COalgo"), 
      label = "Select Constrained Optimization Algorithm",
      choices = list("COBYLA","ISRES","AUGLAG + COBYLA","SQP"),
      selected = "ISRES"
    )
  )
}

settingsOptimization.server <- function(input, output, session, settings) {
  observe({
    settings$nmultistart <- input$nmultistart
    settings$COalgo <- input$COalgo
  })
}