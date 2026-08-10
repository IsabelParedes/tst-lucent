#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module exploreModel
source("modules/menuExplore/exploreModel/modelQualitativeExploration.R", local = TRUE)
source("modules/menuExplore/exploreModel/modelQuantitativeExploration.R", local = TRUE)
source("modules/menuExplore/exploreModel/modelManualExploration.R", local = TRUE)

exploreModel.ui <- function(id) {
  ns <- NS(id)
  tagList(
    bsCollapse(
      id = ns("collapseExploreModel"),
      multiple = TRUE, open = "Qualitative Exploration",
      bsCollapsePanel(
        "Qualitative Exploration",
        modelQualitativeExploration.ui(id = ns("modelQualitativeExploration"))
      ),
      bsCollapsePanel(
        "Quantitative Exploration",
        modelQuantitativeExploration.ui(id = ns("modelQuantitativeExploration"))
      ),
      bsCollapsePanel(
        "Manual Exploration",
        modelManualExploration.ui(id = ns("modelManualExploration"))
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("openPanelExploreModel"),
        label = "Open Panel",
        choices = c("",
                    "Qualitative Exploration",
                    "Quantitative Exploration",
                    "Manual Exploration"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("closePanelExploreModel"),
        label = "Close Panel",
        choices = c("",
                    "Qualitative Exploration",
                    "Quantitative Exploration",
                    "Manual Exploration"),
        selected = ""
      )
    )
  )
}

exploreModel.server <- function(input, output, session, DOE, listmodels, window.dimension, settings) {
  modelQualitativeExploration.server("modelQualitativeExploration", DOE, listmodels, window.dimension, settings)
  callModule(modelQuantitativeExploration.server, "modelQuantitativeExploration", DOE, listmodels, window.dimension, settings)
  callModule(modelManualExploration.server, "modelManualExploration", DOE, listmodels)
  
  observeEvent(input$openPanelExploreModel,{
    updateCollapse(session,
                   "collapseExploreModel",
                   open = input$openPanelExploreModel)
  })
  
  observeEvent(input$closePanelExploreModel,{
    updateCollapse(session,
                   "collapseExploreModel",
                   close = input$closePanelExploreModel)
  })
  
  
}
