#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module menuExplore
source("modules/menuExplore/exploreModel/exploreModel.R", local = TRUE)
source("modules/menuExplore/UQModel/UQModel.R", local = TRUE)

menuExplore.ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Explore",
    tabPanel(
      HTML(paste("Exploration","with Surrogate Model", sep = '<br/>')),
      tagList(
        exploreModel.ui(id = ns("exploreModel"))
      ),
      icon = icon("chart-area"), value = ns("tabexploreSurrogate")
    ),
    tabPanel(
      HTML(paste("UQ & GSA","with Surrogate Model", sep = '<br/>')),
      UQModel.ui(id = ns("UQModel")),
      icon = icon("chart-area"), value = ns("tabUQGSASurrogate")
    ),
    icon = icon("chart-area")
  )
}

menuExplore.server <- function(input, output, session, DOE, ML, listmodels, doeProblemDef, window.dimension, persistence, settings) {
  callModule(exploreModel.server, "exploreModel", DOE, listmodels, window.dimension, settings)
  UQModel <- callModule(UQModel.server, "UQModel", DOE, ML, listmodels, doeProblemDef, window.dimension, persistence, settings)
  return(UQModel)
}
