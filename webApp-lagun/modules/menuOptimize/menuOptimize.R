#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module menuOptimize
source("modules/menuOptimize/unconstrained/unconstrained.R", local = TRUE)
source("modules/menuOptimize/constrained/constrained.R", local = TRUE)
source("modules/menuOptimize/sequential/sequential.R", local = TRUE)
source("modules/menuOptimize/robust/robust.R", local = TRUE)

menuOptimize.ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Optimize",
    tabPanel(
      HTML(paste("Optimization","with Surrogate Model", sep = '<br/>')),
      bsCollapse(
        id = ns("collapseOptim"),
        multiple = TRUE,
        bsCollapsePanel(
          "Unconstrained Optimization",
          unconstrained.ui(id = ns("unconstrained"))
        ),
        bsCollapsePanel(
          "Constrained Optimization",
          constrained.ui(id = ns("constrained"))
        )
      ),
      # To open and close bsCollapsePanels in tests
      conditionalPanel(
        condition = "false",
        selectInput(
          ns("openCollapseOptim"),
          label = "Open Panel:",
          choices = c("",
                      "Unconstrained Optimization",
                      "Constrained Optimization"),
          selected = ""
        )
      ),
      conditionalPanel(
        condition = "false",
        selectInput(
          ns("closeCollapseOptim"),
          label = "Close Panel:",
          choices = c("",
                      "Unconstrained Optimization",
                      "Constrained Optimization"),
          selected = ""
        )
      ),
      icon = icon("chart-area"), value = ns("taboptimSurrogate")
    ),
    tabPanel(
      HTML(paste("Sequential Optimization","with Surrogate Model", sep = '<br/>')),
      sequential.ui(id = ns("sequential")),
      icon = icon("chart-area"), value = ns("taboptimSequential")
    ),
    tabPanel(
      HTML(paste("Robust Optimization","with Surrogate Model", sep = '<br/>')),
      robust.ui(id = ns("robust")),
      icon = icon("chart-area"), value = ns("tabroboptimSurrogate")
    ),
    icon = icon("chart-area")
  )
}

menuOptimize.server <- function(input, output, session, DOE, listmodels, advance.importDOE, persistence, settings, doeProblemDef) {
  
  outputMenuOptimize <- list(XaddUnconstOptim = NULL,
                             XaddConstOptim = NULL,
                             XaddSeqOptim = NULL)
  
  # To open and close bsCollapsePanels in tests
  observeEvent(input$openCollapseOptim,
               {
                 updateCollapse(session,
                                "collapseOptim",
                                open = input$openCollapseOptim)
               })
  observeEvent(input$closeCollapseOptim,
               {
                 updateCollapse(session,
                                "collapseOptim",
                                close = input$closeCollapseOptim)
               })
  
  unconstrOptim <- callModule(unconstrained.server, "unconstrained", DOE, listmodels, persistence, settings, doeProblemDef)
  outputMenuOptimize$XaddUnconstOptim <- unconstrOptim$simulations
  
  constr <- callModule(constrained.server, "constrained", DOE, listmodels, persistence, settings, doeProblemDef)
  outputMenuOptimize$XaddConstOptim <- constr$simulations
  
  
  outputMenuOptimize$XaddSeqOptim <- callModule(sequential.server, "sequential", 
                                                DOE, listmodels, persistence, 
                                                advance.importDOE, settings,
                                                doeProblemDef)

  
  callModule(robust.server, "robust", DOE, listmodels, settings)
  
  return(list(outputMenuOptimize = outputMenuOptimize, 
              unconstrOptim = list(resoptim = unconstrOptim$resoptim,
                                   Xinfos = unconstrOptim$Xinfos),
              constrOptim = constr$optim))
}
