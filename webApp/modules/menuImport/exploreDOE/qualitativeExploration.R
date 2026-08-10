#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module qualitativeExploration

source("modules/menuImport/exploreDOE/qualitativeExploration/regressionPlotOneByOne.R", local = TRUE)
source("modules/menuImport/exploreDOE/qualitativeExploration/parallelCoordPlot.R", local = TRUE)
source("modules/menuImport/exploreDOE/qualitativeExploration/regressionPlotAllInOne.R", local = TRUE)
source("modules/menuImport/exploreDOE/qualitativeExploration/functionalPlot.R", local = TRUE)

qualitativeExploration.ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    tabsetPanel(id = ns('tabs'), type = "tabs",
                tabPanel(h4("Functional plot"), value = ns("funcPlot"), functionalPlot.ui(id = ns("funcPlotTab"))),
                tabPanel(h4("Regression plot - One by One"), value = ns("regression-plot"), regressionPlotOneByOne.ui(id = ns("regression-plot-tab"))),
                tabPanel(h4("Parallel coordinate plot - Scatter Plot Matrix"), value = ns('pcp'), parallelCoordPlot.ui(id = ns("pcpTab"))),
                tabPanel(h4("Regression plot - All in One"), value = ns('regression-plot-all'), regressionPlotAllInOne.ui(id = ns("regression-plot-all-tab")))
    )
  )
}

qualitativeExploration.server <- function(input, output, session, DOE, window.dimension) {
  
  ns <- session$ns
  
  callModule(regressionPlotOneByOne.server, "regression-plot-tab", DOE, window.dimension)

  callModule(parallelCoordPlot.server, "pcpTab", DOE, window.dimension)

  callModule(regressionPlotAllInOne.server, "regression-plot-all-tab", DOE, window.dimension)

  callModule(functionalPlot.server, "funcPlotTab", DOE, window.dimension)

  isFunctional <- reactive({
    length(DOE$Yinfos$func.ids) > 0
  })

  observeEvent(isFunctional(), {
    if (isFunctional()) {
      showTab(inputId = "tabs", target = ns("funcPlot"), select = TRUE)
    }
    else {
      hideTab(inputId = "tabs", target = ns("funcPlot"))
      updateTabsetPanel(inputId = "tabs", selected = ns("regression-plot"))
    }
  })

  return(DOE)
}
