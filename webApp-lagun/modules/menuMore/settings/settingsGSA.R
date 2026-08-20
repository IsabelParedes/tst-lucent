#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsGSA
settingsGSA.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    numericInput(ns("nsample"), "Sample Size for Sobol", 1000, min = 1),
    numericInput(ns("nsampleShapley"), "Sample Size for Shapley", 5000, min = 1),
    numericInput(ns("nrepGSA"), "Number of Resamples for GSA", 50, min = 1)
  )
}

settingsGSA.server <- function(input, output, session, settings) {
  observe({
    settings$nsample <- input$nsample
    settings$nsampleShapley <- input$nsampleShapley
    settings$nrepGSA <- input$nrepGSA
  })
}