#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module prepareDOE

source("modules/prepareDOE/generateDOE.R", local = TRUE)

prepareDOE.ui <- function(id) {
  ns <- NS(id)
  bsCollapse(
    multiple = TRUE, open = "Generate DOE",
    bsCollapsePanel(
      "Generate DOE",  style = "primary",
      generateDOE.ui(id = ns("generateDOE"))
    )
  )
}

prepareDOE.server <- function(input, output, session, persistence, settings) {
  DOE <- generateDOE.server("generateDOE", persistence, settings)
  return(DOE)
}
