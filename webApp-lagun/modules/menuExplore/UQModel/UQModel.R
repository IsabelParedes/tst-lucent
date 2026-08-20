#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module UQModel
source("modules/menuExplore/UQModel/uncertaintyDefinition.R", local = TRUE)
source("modules/menuExplore/UQModel/uncertaintyPropagation.R", local = TRUE)
source("modules/menuExplore/UQModel/sensitivityAnalysis.R", local = TRUE)

UQModel.ui <- function(id) {
  ns <- NS(id)
  tagList(
    bsCollapse(
      id = ns("collapseUQModel"), multiple = TRUE, open = "Step 1: Uncertainty Definition",
      bsCollapsePanel(
        "Step 1: Uncertainty Definition",
        uncertaintyDefinition.ui(id = ns("uncertaintyDefinition"))
      ),
      bsCollapsePanel(
        "Step 2: Uncertainty Propagation",
        uncertaintyPropagation.ui(id = ns("uncertaintyPropagation"))
      ),
      bsCollapsePanel(
        "Step 3: Sensitivity Analysis",
        sensitivityAnalysis.ui(id = ns("sensitivityAnalysis"))
      )
    ),
    # To open and close panels in tests
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("openPanelUQModel"),
        label = "Open Panel",
        choices = c("",
                    "Step 1: Uncertainty Definition",
                    "Step 2: Uncertainty Propagation",
                    "Step 3: Sensitivity Analysis"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("closePanelUQModel"),
        label = "Close Panel",
        choices = c("",
                    "Step 1: Uncertainty Definition",
                    "Step 2: Uncertainty Propagation",
                    "Step 3: Sensitivity Analysis"),
        selected = ""
      )
    )
  )
}

UQModel.server <- function(input, output, session, DOE, ML, listmodels, doeProblemDef, window.dimension, persistence, settings) {
  
  # To open and close panels in tests
  observeEvent(input$openPanelUQModel,{
    updateCollapse(session,
                   "collapseUQModel",
                   open = input$openPanelUQModel)
  })
  
  observeEvent(input$closePanelUQModel,{
    updateCollapse(session,
                   "collapseUQModel",
                   close = input$closePanelUQModel)
  })
  
  
  UQparams <- callModule(uncertaintyDefinition.server, "uncertaintyDefinition", DOE, persistence)
  uncertaintyPropagation <- uncertaintyPropagation.server("uncertaintyPropagation", DOE, listmodels, doeProblemDef, UQparams, persistence, settings)
  sensitivityAnalysis <- sensitivityAnalysis.server("sensitivityAnalysis", DOE, ML, listmodels, UQparams, window.dimension, persistence, settings)
  return(list(simulations = uncertaintyPropagation$simulations, 
              UQproba = uncertaintyPropagation$UQproba, 
              UQres = uncertaintyPropagation$UQres, 
              sensitivityAnalysis = sensitivityAnalysis, 
              UQparams = UQparams))
}
