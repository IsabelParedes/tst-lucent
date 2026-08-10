#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module robust
source("modules/menuOptimize/robust/robustDefine.R", local = TRUE)
source("modules/menuOptimize/robust/robustAnalysis.R", local = TRUE)
source("modules/menuOptimize/robust/robustSolve.R", local = TRUE)

q10 <- function(v){
  return(quantile(v,0.1))
}
q90 <- function(v){
  return(quantile(v,0.9))
}
namesROcrit <- c("Mean","Quantile 10%","Quantile 90%")
listROcrit <- list(mean,q10,q90)

robust.ui <- function(id) {
  ns <- NS(id)
  bsCollapse(
    multiple = TRUE, open = "Define Optimization Problem",
    bsCollapsePanel(
      "Define Optimization Problem",
      robustDefine.ui(id = ns("robustDefine"))
    ),
    bsCollapsePanel(
      "Preliminary Analysis",
      robustAnalysis.ui(id = ns("robustAnalysis"))
    ),
    bsCollapsePanel(
      "Solve Problem",
      robustSolve.ui(id = ns("robustSolve"))
    )
  )
}

robust.server <- function(input, output, session, DOE, listmodels, settings) {
  ROformulation <- callModule(robustDefine.server, "robustDefine", DOE)
  callModule(robustAnalysis.server, "robustAnalysis", DOE, listmodels, ROformulation)
  callModule(robustSolve.server, "robustSolve", DOE, listmodels, ROformulation, settings)
}
