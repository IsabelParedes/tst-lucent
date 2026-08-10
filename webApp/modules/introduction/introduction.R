#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module introduction

introduction.ui <- function(id) {
  ns <- NS(id)

  tagList(
    column(2,
      bsButton(ns('git'), 'Gitlab', size = 'large', type = 'action', class = "btn-primary", width = '60%',
               onclick = "window.open('https://gitlab.com/drti/lagun',
               '_blank')"),
      offset = 10
    ),
    includeMarkdown("modules/introduction/introductiontitle.md"),
    fluidRow(
      column(3,
             actionButton(ns('workflowcomplete'), HTML(paste('Workflow', 'Complete', sep = '<br>')), 
                          class = "btn-primary", size = "large", width = '100%'),
             align="center"
      ),
      column(3,
             actionButton(ns('workflowprepareDOE'), HTML(paste('Workflow', 'Prepare DOE', sep = '<br>')), 
                          class = "btn-primary", size = "large", width = '100%'),
             align="center"
             ),
      column(3,
             actionButton(ns('workflowexploreData'), HTML(paste('Workflow', 'Explore Data', sep = '<br>')), 
                          class = "btn-primary", size = "large", width = '100%'),
             align="center"
             ),
      column(3,
             actionButton(ns('workflowsurrogate'), HTML(paste('Workflow', 'Build Surrogate & Explore', sep = '<br>')), 
                          class = "btn-primary", size = "large", width = '100%'),
             align="center")
    ),
    br(),
    hr(),
    includeMarkdown("modules/introduction/introduction.md"),
    actionButton(ns('tuto_intro'), HTML(paste('Introduction', 'Tutorial', sep = '<br>')), class = "btn-primary",
                 onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial0_Introduction.pdf')"),
    HTML('<h4> <br> History <br>'),
    HTML('<h5> The first version of LAGUN was initiated at Safran Tech (under a different name), the corporate research center of Safran. 
          Its goal was to give an easy access to methods and algorithms to all Safran engineers with a user-friendly interface. 
          A collaboration was later launched with IFPEN in 2019 to share algorithms and developments in a common platform now named LAGUN ("Assistance" in Basque language).'),
    HTML('<h3 align="center">  The platform is organized in tabs, each one of them corresponding to a step above. </h3>
          <h5 align="center"> Click to expand the panels below to learn more through tutorials and test cases (links to Gitlab).</h5>'),
    bsCollapse(
      id = ns("collapseIntro"), multiple = TRUE, open = NULL,
      bsCollapsePanel(
        h4("General Layout & Color Code"),
        # see https://github.com/ebailey78/shinyBS/issues/50
        # in bsCollaspePanel we must specify the value when using a HTML element for the title
        # otherwise it raises a warning
        includeMarkdown("modules/introduction/layout.md"), value = "General Layout & Color Code"
      ),
      bsCollapsePanel(
        h4("Prepare DOE"),
        actionButton(ns('tuto1'), HTML(paste('Tutorial 1', 'Design of experiments', sep = '<br>')), class = "btn-primary",
                 onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial1_Design%20of%20experiments.pdf')"),
        value = "Prepare DOE"
      ),
      bsCollapsePanel(
        h4("Problem Definition"),
        actionButton(ns('tuto2.1'), HTML(paste('Tutorial 2.1', 'Import DOE & Simulator Configuration', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial2.1_DOE%20Import%20and%20Simulator%20Configuration.pdf')"),
        actionButton(ns('tuto2.2'), HTML(paste('Tutorial 2.2', 'Exploration', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial2.2_Exploration.pdf')"),
        actionButton(ns('castest1_1'), HTML(paste('Test Case 1', 'Exploration', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase1_Exploration/Lagun_TestCase1_Exploration.pdf')"),
        value = "Problem Definition"
      ),
      bsCollapsePanel(
        h4("Surrogate Model"),
        actionButton(ns('tuto3'), HTML(paste('Tutorial 3', 'Surrogate Models', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial3_Surrogate%20Models.pdf')"),
        value = "Surrogate Model"
      ),
      bsCollapsePanel(
        h4("Explore"),
        actionButton(ns('tuto4'), HTML(paste('Tutorial 4', 'Surrogate Exploration', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial4_Surrogate%20Exploration.pdf')"),
        actionButton(ns('castest1_2'), HTML(paste('Test Case 1', 'Exploration', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase1_Exploration/Lagun_TestCase1_Exploration.pdf')"),
        br(),
        br(),
        actionButton(ns('tuto5'), HTML(paste('Tutorial 5', 'Uncertainty Propagation', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial5_Uncertainy%20Propagation.pdf')"),
        actionButton(ns('castest2'), HTML(paste('Test Case 2', 'Uncertainty Propagation', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase2_Uncertainty%20propagation/Lagun_TestCase2_Uncertainty%20propagation.pdf')"),
        br(),
        br(),
        actionButton(ns('tuto6'), HTML(paste('Tutorial 6', 'Sensitivity Analysis', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial6_Sensitivity%20Analysis.pdf')"),
        actionButton(ns('castest3'), HTML(paste('Test Case 3', 'Sensitivity Analysis', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase3_Sensitivity%20analysis/Lagun_TestCase3_Sensitivity%20analysis.pdf')"),
        value = "Explore"
        
      ),
      bsCollapsePanel(
        h4("Optimize"),
        actionButton(ns('tuto7'), HTML(paste('Tutorial 7', 'Optimization', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial7_Optimisation.pdf')"),
        actionButton(ns('castest4'), HTML(paste('Test Case 4', 'Bi-objective Optimization', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase4_Bi-objective%20optimization/Lagun_TestCase4_Biobjective%20optimization.pdf')"),
        actionButton(ns('castest5'), HTML(paste('Test Case 5', 'Constrained Optimization', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase5_Constrained%20optimization/Lagun_TestCase5_Constrained%20optimization.pdf')"),
        br(),
        br(),
        actionButton(ns('tuto9'), HTML(paste('Tutorial 9', 'Sequential Optimization', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial9_Sequential%20optimization.pdf')"),
        br(),
        br(),
        actionButton(ns('tuto8'), HTML(paste('Tutorial 8', 'Robust Optimization', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/tutorials/Lagun_Tutorial8_Optimization%20under%20uncertainty.pdf')"),
        actionButton(ns('castest6'), HTML(paste('Test Case 6', 'Robust Opimization', sep = '<br>')), class = "btn-primary",
                     onclick = "window.open('https://gitlab.com/drti/lagun/-/tree/master/documentation/test-cases/Lagun_TestCase6_Robust%20optimization/Lagun_TestCase6_Robust%20optimization.pdf')"),
        value = "Optimize"
      ),
      bsCollapsePanel(
        h4("More"),
        includeMarkdown("modules/introduction/more.md"), value = "More"
      )
    ),
    # To open and close bsCollapsePanels in tests
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("activeCollapseIntro"),
        label = "Active Panel:",
        choices = c("",
                    "General Layout & Color Code",
                    "Prepare DOE",
                    "Problem Definition",
                    "Surrogate Model",
                    "Explore",
                    "Optimize",
                    "More"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("desactiveCollapseIntro"),
        label = "Desactive Panel:",
        choices = c("",
                    "General Layout & Color Code",
                    "Prepare DOE",
                    "Problem Definition",
                    "Surrogate Model",
                    "Explore",
                    "Optimize",
                    "More"),
        selected = ""
      )
    ),
    br(),br(),br(),
    p("Please contact ", a("saf.lagun@safrangroup.com", href="saf.lagun@safrangroup.com"), ",", a("ifpen.lagun@ifpen.fr", href="ifpen.lagun@ifpen.fr"), "or create a post at", a("https://ifpen-discourse.appcollaboratif.fr/c/lagun/", href="https://ifpen-discourse.appcollaboratif.fr/c/lagun/"),"for information.")
  )
}

introduction.server <- function(input, output, session) {
  
  ns <- session$ns
  
  # To open and close bsCollapsePanels in tests
  observeEvent(input$activeCollapseIntro,
               {
                 updateCollapse(session,
                                "collapseIntro",
                                open = input$activeCollapseIntro)
               })
  observeEvent(input$desactiveCollapseIntro,
               {
                 updateCollapse(session,
                                "collapseIntro",
                                close = input$desactiveCollapseIntro)
               })
  
  # By default all tabs are visible
  displayModules <- reactiveValues(prepareDOE = TRUE, importDOE = TRUE, prelimExplo = FALSE, surrogateModel = FALSE,
                                   exploreSurrogate = FALSE, UQGSASurrogate = FALSE, optimSurrogate = FALSE, roboptimSurrogate = FALSE)
  # So detect the first launch and disable the workflow 'complete'
  start <- reactiveValues(bool = TRUE)
  observe({
    req(start$bool)
    disableActionButton(ns("workflowcomplete"),session)
    start$bool <- FALSE
  })
  workflow.selected <- reactiveValues(complete = TRUE, prepareDOE = FALSE, exploreData = FALSE, surrogate = FALSE)
  
  observeEvent(input$workflowcomplete,{
    displayModules$prepareDOE = TRUE
    displayModules$importDOE = TRUE
    displayModules$prelimExplo = TRUE
    displayModules$surrogateModel = TRUE
    displayModules$exploreSurrogate = TRUE
    displayModules$UQGSASurrogate = TRUE
    displayModules$optimSurrogate = TRUE
    displayModules$seqoptimSurrogate = TRUE
    displayModules$roboptimSurrogate = TRUE
    workflow.selected$complete = TRUE
    workflow.selected$prepareDOE = FALSE
    workflow.selected$exploreData = FALSE
    workflow.selected$surrogate = FALSE
    disableActionButton(ns("workflowcomplete"),session)
    enableActionButton(ns("workflowprepareDOE"),session)
    enableActionButton(ns("workflowexploreData"),session)
    enableActionButton(ns("workflowsurrogate"),session)
  })

  observeEvent(input$workflowprepareDOE,{
    displayModules$prepareDOE = TRUE
    displayModules$importDOE = FALSE
    displayModules$prelimExplo = FALSE
    displayModules$surrogateModel = FALSE
    displayModules$exploreSurrogate = FALSE
    displayModules$UQGSASurrogate = FALSE
    displayModules$optimSurrogate = FALSE
    displayModules$seqoptimSurrogate = FALSE
    displayModules$roboptimSurrogate = FALSE
    workflow.selected$complete = FALSE
    workflow.selected$prepareDOE = TRUE
    workflow.selected$exploreData = FALSE
    workflow.selected$surrogate = FALSE
    disableActionButton(ns("workflowprepareDOE"),session)
    enableActionButton(ns("workflowexploreData"),session)
    enableActionButton(ns("workflowcomplete"),session)
    enableActionButton(ns("workflowsurrogate"),session)
  })
  
  observeEvent(input$workflowexploreData,{
    displayModules$prepareDOE = FALSE
    displayModules$importDOE = TRUE
    displayModules$prelimExplo = TRUE
    displayModules$surrogateModel = FALSE
    displayModules$exploreSurrogate = FALSE
    displayModules$UQGSASurrogate = FALSE
    displayModules$optimSurrogate = FALSE
    displayModules$seqoptimSurrogate = FALSE
    displayModules$roboptimSurrogate = FALSE
    workflow.selected$complete = FALSE
    workflow.selected$prepareDOE = FALSE
    workflow.selected$exploreData = TRUE
    workflow.selected$surrogate = FALSE
    disableActionButton(ns("workflowexploreData"),session)
    enableActionButton(ns("workflowprepareDOE"),session)
    enableActionButton(ns("workflowcomplete"),session)
    enableActionButton(ns("workflowsurrogate"),session)
  })
  
  observeEvent(input$workflowsurrogate,{
    displayModules$prepareDOE = FALSE
    displayModules$importDOE = TRUE
    displayModules$prelimExplo = TRUE
    displayModules$surrogateModel = FALSE
    displayModules$exploreSurrogate = FALSE
    displayModules$UQGSASurrogate = FALSE
    displayModules$optimSurrogate = FALSE
    displayModules$seqoptimSurrogate = FALSE
    displayModules$roboptimSurrogate = FALSE
    workflow.selected$complete = FALSE
    workflow.selected$prepareDOE = FALSE
    workflow.selected$exploreData = FALSE
    workflow.selected$surrogate = TRUE
    disableActionButton(ns("workflowsurrogate"),session)
    enableActionButton(ns("workflowprepareDOE"),session)
    enableActionButton(ns("workflowcomplete"),session)
    enableActionButton(ns("workflowexploreData"),session)
  })
  
  return(list(displayModules = displayModules, workflow.selected = workflow.selected))
}
