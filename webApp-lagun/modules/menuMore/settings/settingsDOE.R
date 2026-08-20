#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsDOE
settingsDOE.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    numericInput(ns("ntestMC"), "Number of MC DOE for Comparison", 100, min = 1),
    br(),
    numericInput(ns("nrepML"), "Number of Resamples for ML Tool", 50, min = 1)
  )
}

settingsDOE.server <- function(input, output, session, settings) {
  observe({
    settings$ntestMC <- input$ntestMC
    settings$nrepML <- input$nrepML
  })
}