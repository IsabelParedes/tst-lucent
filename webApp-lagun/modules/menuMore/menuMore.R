#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module menuMore
source("modules/menuMore/settings/settings.R", local = TRUE)
source("modules/menuMore/logs/logging.R", local = TRUE)
source("modules/menuMore/about/about.R", local = TRUE)

menuMore.ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "More",
    tabPanel("Advanced Settings", settings.ui(id = ns("settings")), icon = NULL),
    tabPanel("Logs", logging.ui(id = ns("logging")), icon = NULL),
    tabPanel("About", about.ui(id = ns("about")), icon = NULL)
  )
}

menuMore.server <- function(input, output, session, listmodels = NULL) {
  settings <- callModule(settings.server, "settings", listmodels)
  callModule(logging.server, "logging")
  callModule(about.server, "about")
  return(settings)
}
