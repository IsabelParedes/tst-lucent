#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsExploration
settingsExploration.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    numericInput(ns("nobsparcoords"), "Number of Trajectories in Parallel Coordinates", 5000, min = 1),
    numericInput(ns("nsteps"), "Number of Steps for Sliders", 20, min = 1),
    numericInput(ns("nsignif"), "Number of Significant Digits for Sliders", 3, min = 1),
    numericInput(ns("ftemp"), "Range Factor for Y-Axis (% of data range)", 0.5, min = 0),
    numericInput(ns("ncontours"), "Number of Contour Levels", 20, min = 5)
  )
}

settingsExploration.server <- function(input, output, session, settings) {
  observe({
    settings$nobsparcoords <- input$nobsparcoords
    settings$nsteps <- input$nsteps
    settings$nsignif <- input$nsignif
    settings$ftemp <- input$ftemp
    settings$ncontours <- input$ncontours
  })
}