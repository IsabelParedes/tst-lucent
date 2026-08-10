#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module sequential
source("modules/menuOptimize/constrained/constrainedDefine.R", local = TRUE)
source("modules/menuOptimize/sequential/sequentialSolve.R", local = TRUE)

tablesign <- matrix(c("<",">"),ncol = 2)

sequential.ui <- function(id) {
  ns <- NS(id)
  bsCollapse(
    multiple = TRUE, open = "Define Optimization Problem",
    bsCollapsePanel(
      "Define Optimization Problem",
      constrainedDefine.ui(id = ns("sequentialDefine"))
    ),
    bsCollapsePanel(
      "Solve Optimization Problem",
      sequentialSolve.ui(id = ns("sequentialSolve"))
    )
  )
}

sequential.server <- function(input, output, session, DOE, listmodels, persistence, advance.importDOE, settings, doeProblemDef) {
  define <- constrainedDefine.server("sequentialDefine", DOE, listmodels, persistence, nbcons.min = 0, nbobj = 1, simulations, typeOptim = "sequential")
  Xinfos <- define$Xinfos
  COformulation <- define$COformulation
  simulations <- callModule(sequentialSolve.server, "sequentialSolve", DOE, 
                            listmodels, Xinfos, COformulation, 
                            advance.importDOE, settings, doeProblemDef)
  return(simulations)
}
