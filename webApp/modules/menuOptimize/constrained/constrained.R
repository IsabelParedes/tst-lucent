#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module constrained
source("modules/menuOptimize/constrained/constrainedDefine.R", local = TRUE)
source("modules/menuOptimize/constrained/constrainedSolve.R", local = TRUE)
source("modules/menuOptimize/constrained/constrainedExplore.R", local = TRUE)

tablesign <- matrix(c("<",">"),ncol = 2)

constrained.ui <- function(id) {
  ns <- NS(id)
  tagList(
    bsCollapse(
      id = ns("collapseConstrainedOptim"),
      multiple = TRUE, open = "Define Optimization Problem",
      bsCollapsePanel(
        "Define Optimization Problem",
        constrainedDefine.ui(id = ns("constrainedDefine"))
      ),
      bsCollapsePanel(
        "Solve Optimization Problem",
        constrainedSolve.ui(id = ns("constrainedSolve"))
      ),
      bsCollapsePanel(
        "Explore Solution(s)",
        constrainedExplore.ui(id = ns("constrainedExplore"))
      )
    ),
    
    # To open and close bsCollapsePanels in tests
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("openCollapseConstrainedOptim"),
        label = "Open Panel:",
        choices = c("",
                    "Define Optimization Problem",
                    "Solve Optimization Problem",
                    "Explore Solution(s)"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("closeCollapseConstrainedOptim"),
        label = "Close Panel:",
        choices = c("",
                    "Define Optimization Problem",
                    "Solve Optimization Problem",
                    "Explore Solution(s)"),
        selected = ""
      )
    )
  )
}

constrained.server <- function(input, output, session, DOE, listmodels, persistence, settings, doeProblemDef) {
  
  # To open and close bsCollapsePanels in tests
  observeEvent(input$openCollapseConstrainedOptim,
               {
                 updateCollapse(session,
                                "collapseConstrainedOptim",
                                open = input$openCollapseConstrainedOptim)
               })
  observeEvent(input$closeCollapseConstrainedOptim,
               {
                 updateCollapse(session,
                                "collapseConstrainedOptim",
                                close = input$closeCollapseConstrainedOptim)
               })
  
  define <- constrainedDefine.server("constrainedDefine", DOE, listmodels, persistence, nbcons.min = 1, nbobj = 2, simulations = NULL)
  Xinfos <- define$Xinfos
  COformulation <- define$COformulation
  OutputConstrainedSolve <- constrainedSolve.server("constrainedSolve", DOE, listmodels, persistence, Xinfos, COformulation, settings, doeProblemDef)
  callModule(constrainedExplore.server, "constrainedExplore", DOE, listmodels, COformulation, OutputConstrainedSolve$resCoptim, settings)
  
  return(list(simulations = OutputConstrainedSolve$simulations, 
              optim = list(COformulation = COformulation,
                           Xinfos = Xinfos,
                           resCoptim = OutputConstrainedSolve$resCoptim)))
}
