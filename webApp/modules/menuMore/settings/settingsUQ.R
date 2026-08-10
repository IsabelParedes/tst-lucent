#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsGSA
settingsUQ.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    numericInput(ns("nrepproba"), "Number of Resamples for Probability Estimation", 50, min = 1)
  )
}

settingsUQ.server <- function(input, output, session, settings) {
  observe({
    settings$nrepproba <- input$nrepproba
  })
}