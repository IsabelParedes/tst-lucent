#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module define calibration
source("modules/menuImport/calibration/importExperimentalData.R", local = TRUE)
source("modules/menuImport/calibration/defineObjective.R", local = TRUE)

defineCalibration.ui <- function(id) {
  ns <- NS(id)
  tagList(
    bsCollapse(
      id = ns("collapseCalibration"),
      multiple = TRUE, open = "Import Experimental Data",
      bsCollapsePanel(
        "Import Experimental Data",  style = "primary",
        importExperimentalData.ui(id = ns("importXPdata"))
      ),
      bsCollapsePanel(
        "Define Objective Function", style = "primary",
        defineObjective.ui(id = ns("defineObjective"))
      )
    ),
    # To open and close bsCollapsePanels in tests
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("openCollapseCalibration"),
        label = "Open Panel:",
        choices = c("",
                    "Import Experimental Data",
                    "Define Objective Function"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("closeCollapseCalibration"),
        label = "Close Panel:",
        choices = c("",
                    "Import Experimental Data",
                    "Define Objective Function"),
        selected = ""
      )
    )
  )
}

defineCalibration.server <- function(input, output, session, DOE, persistence, settings) {
  
  xpData <- callModule(importExperimentalData.server, "importXPdata", DOE, persistence, settings)
  objFunc <- callModule(defineObjective.server, "defineObjective", DOE, xpData, persistence, settings)
  
  calibDOE <- reactiveValues(Z = NULL, sigZ = NULL, nZ = NULL, idZ = NULL, idZY = NULL, discZ = NULL,
                             OF = NULL, OFtot = NULL, norm = NULL, weights = NULL)
  
  # To open and close bsCollapsePanels in tests
  observeEvent(input$openCollapseCalibration,
               {
                 updateCollapse(session,
                                "collapseCalibration",
                                open = input$openCollapseCalibration)
               })
  observeEvent(input$closeCollapseCalibration,
               {
                 updateCollapse(session,
                                "collapseCalibration",
                                close = input$closeCollapseCalibration)
               })
  
  observe({
    calibDOE$Z <- xpData$Z 
    calibDOE$sigZ <- xpData$sigZ 
    calibDOE$nZ <- xpData$nZ
    calibDOE$idZ <- xpData$idZ
    calibDOE$idZY <- xpData$idZY
    calibDOE$discZ <- xpData$discZ
    calibDOE$zFileName <- xpData$zFileName
    calibDOE$OF <- objFunc$OF
    calibDOE$OFtot <- objFunc$OFtot
    calibDOE$norm <- objFunc$norm
    calibDOE$weights <- objFunc$weights
  })
  
  return(calibDOE)

}
