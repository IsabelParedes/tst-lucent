#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsSurrogate
settingsSurrogate.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    selectInput(
      ns("KMalgo"), 
      label = "Select Kriging Estimation Method",
      choices = list("Maximum Likelihood","Leave-One-Out"),
      selected = "Maximum Likelihood"
    ),
    numericInput(ns("nfaure"), "Number of Points Tested for Kriging Improvement", 10000, min = 1),
    numericInput(ns("tlength"), "Grid Search Size for Advanced Models", 5, min = 1)
  )
}

settingsSurrogate.server <- function(input, output, session, settings) {
  observe({
    settings$KMalgo <- input$KMalgo
    settings$nfaure <- input$nfaure
    settings$tlength <- input$tlength
  })
}