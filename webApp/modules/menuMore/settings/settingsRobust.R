#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsRobust
settingsRobust.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    numericInput(ns("nRmultistart"), "Number of Multistart", 50, min = 1),
    selectInput(
      ns("ROalgo"), 
      label = "Select Optimization Algorithm",
      choices = list("COBYLA","ISRES","AUGLAG + COBYLA","SQP"),
      selected = "ISRES"
    )
  )
}

settingsRobust.server <- function(input, output, session, settings) {
  observe({
    settings$nRmultistart <- input$nRmultistart
    settings$ROalgo <- input$ROalgo
  })
}