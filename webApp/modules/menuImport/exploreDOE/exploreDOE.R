#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module exploreDOE
source("modules/menuImport/exploreDOE/qualitativeExploration.R", local = TRUE)
source("modules/menuImport/exploreDOE/quantitativeExploration.R", local = TRUE)

exploreDOE.ui <- function(id) {
  ns <- NS(id)
  tagList(
    bsModal(ns("modalViewDOE"), "View Currently Used DOE", NULL,
            DT::dataTableOutput(ns('DTcurrentDOE'))%>% withSpinner(), size = "large"
    ),
    fluidRow(
      column(2,h4(textOutput(ns("info.text0")))),
      column(2,h4(textOutput(ns("info.text1")))),
      column(2,h4(htmlOutput(ns("info.text2")))),
      uiOutput(ns('infoDOE.dynui'))
    ),
    hr(),
    bsCollapse(
      id=ns("collapseExplore"), multiple = TRUE, open = "Qualitative Exploration",
      bsCollapsePanel(
        "Qualitative Exploration",  style = "primary",
        qualitativeExploration.ui(id = ns("qualitativeExploration"))
      ),
      bsCollapsePanel(
        "Quantitative Exploration with Machine Learning Tools", style = "info",
        quantitativeExploration.ui(id = ns("quantitativeExploration"))
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("activePanel"),
        label = "Active Panel:",
        choices = c("",
                    "Qualitative Exploration",
                    "Quantitative Exploration with Machine Learning Tools"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("desactivePanel"),
        label = "Desactive Panel:",
        choices = c("",
                    "Qualitative Exploration",
                    "Quantitative Exploration with Machine Learning Tools"),
        selected = ""
      )
    )
  )
}

exploreDOE.server <- function(input, output, session, DOE, settings, prelimexplo.clicked, advance.importDOE, doeProblemDef, window.dimension) {
  
  ns <- session$ns
  
  asked.prelimexplo.refresh <- reactiveValues(bool = FALSE)
  observe({
    req(input$prelimexplo.refresh)
    asked.prelimexplo.refresh$bool <- (input$prelimexplo.refresh>0)
  })
  
  currentDOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL, 
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL, adapt.visu = FALSE,
    nobs = 0, idon = NULL, idref = NULL, discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesvisu = NULL,
    Z = NULL, sigZ = NULL, nZ = NULL, idZ = NULL, idZY = NULL, discZ = NULL,
    OF = NULL, OFtot = NULL
  )
  
  uploadDOE <- reactiveValues(bool = TRUE)
  
  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice != 1)
    }
    return(bool)
  })

  observe({
    req(DOE$XY, any(apply(!is.na(DOE$Y),1,any)))
    if (use_simulator()){
      # We are in "simulator" mode
      # This means we detect the progress of simulations with NAs
      if ((uploadDOE$bool & prelimexplo.clicked()) | asked.prelimexplo.refresh$bool){
        idnotna <- apply(!is.na(DOE$Y),1,all)
        currentDOE$Xopt <- DOE$X[idnotna,,drop=FALSE]
        currentDOE$XY <- DOE$XY[idnotna,,drop=FALSE]
        currentDOE$X <- DOE$X[idnotna,,drop=FALSE]
        currentDOE$Y <- DOE$Y[idnotna,,drop=FALSE]
        currentDOE$nX <- DOE$nX
        currentDOE$nY <- DOE$nY
        currentDOE$xnames <- DOE$xnames
        currentDOE$ynames <- DOE$ynames
        currentDOE$xnamesvisu <- DOE$xnamesvisu
        currentDOE$ynamesvisu <- DOE$ynamesvisu
        currentDOE$xnamesmenu <- DOE$xnamesmenu
        currentDOE$ynamesmenu <- DOE$ynamesmenu
        currentDOE$adapt.visu <- DOE$adapt.visu
        currentDOE$Xinfos <- DOE$Xinfos
        currentDOE$nobs <- sum(idnotna)
        currentDOE$Yinfos <- DOE$Yinfos
        currentDOE$nYsurrogate <- DOE$nYsurrogate
        currentDOE$idon <- DOE$idon
        uploadDOE$bool <- FALSE
        asked.prelimexplo.refresh$bool <- FALSE
        currentDOE$idref <- DOE$idref
        # Add calibration data structures
        currentDOE$discF <- DOE$discF
        currentDOE$nF <- DOE$nF
        currentDOE$idF <- DOE$idF
        currentDOE$Fnames <- DOE$Fnames
        currentDOE$Fnamesmenu <- DOE$Fnamesmenu
        currentDOE$Fnamesvisu <- DOE$Fnamesvisu
        currentDOE$Z <- DOE$Z
        currentDOE$sigZ <- DOE$sigZ
        currentDOE$nZ <- DOE$nZ
        currentDOE$idZ <- DOE$idZ
        currentDOE$idZY <- DOE$idZY
        currentDOE$discZ <- DOE$discZ
        currentDOE$OF <- DOE$OF
        currentDOE$OFtot <- DOE$OFtot
      }
    }else{
      currentDOE$Xopt <- DOE$X
      currentDOE$XY <- DOE$XY
      currentDOE$X <- DOE$X
      currentDOE$Y <- DOE$Y
      currentDOE$nobs <- nrow(DOE$X)
      currentDOE$nX <- DOE$nX
      currentDOE$nY <- DOE$nY
      currentDOE$xnames <- DOE$xnames
      currentDOE$ynames <- DOE$ynames
      currentDOE$xnamesvisu <- DOE$xnamesvisu
      currentDOE$ynamesvisu <- DOE$ynamesvisu
      currentDOE$xnamesmenu <- DOE$xnamesmenu
      currentDOE$ynamesmenu <- DOE$ynamesmenu
      currentDOE$adapt.visu <- DOE$adapt.visu
      currentDOE$Xinfos <- DOE$Xinfos
      currentDOE$Yinfos <- DOE$Yinfos
      currentDOE$nYsurrogate <- DOE$nYsurrogate
      currentDOE$idon <- DOE$idon
      currentDOE$idref <- DOE$idref
      # Add calibration data structures
      currentDOE$discF <- DOE$discF
      currentDOE$nF <- DOE$nF
      currentDOE$idF <- DOE$idF
      currentDOE$Fnames <- DOE$Fnames
      currentDOE$Fnamesvisu <- DOE$Fnamesvisu
      currentDOE$Z <- DOE$Z
      currentDOE$sigZ <- DOE$sigZ
      currentDOE$nZ <- DOE$nZ
      currentDOE$idZ <- DOE$idZ
      currentDOE$idZY <- DOE$idZY
      currentDOE$discZ <- DOE$discZ
      currentDOE$OF <- DOE$OF
      currentDOE$OFtot <- DOE$OFtot
    }
  })
  
  observeEvent(input$activePanel,
               {
                 updateCollapse(session,
                                "collapseExplore",
                                open = input$activePanel)
               })
  observeEvent(input$desactivePanel,
               {
                 updateCollapse(session,
                                "collapseExplore",
                                close = input$desactivePanel)
               })
  
  callModule(qualitativeExploration.server, "qualitativeExploration", currentDOE, window.dimension)
  
  observeEvent(input$prelimexplo.viewDOE, {
    req(currentDOE$XY)
    toggleModal(session, "modalViewDOE", toggle = "open")
  })
  
  output$DTcurrentDOE <- DT::renderDataTable({
    req(currentDOE$XY)
    # if there are too many outputs, show only inputs
    if (ncol(currentDOE$XY > 100)) {
      d <- currentDOE$X
      colnames(d) <- currentDOE$xnamesvisu
      dimd <- ncol(currentDOE$X)
    }
    else {
      d <- currentDOE$XY
      colnames(d) <- c(currentDOE$xnamesvisu,currentDOE$ynamesvisu)
      dimd <- ncol(currentDOE$XY)
    }
    DT::datatable(
      d, escape = FALSE,
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip',
        buttons = list(list(extend = 'colvis', columns = 1:dimd)),
        scrollX = TRUE,scrollY = 400,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  output$info.text0 <- renderText({
    req(use_simulator() & advance.importDOE$total & currentDOE$nobs>0)
    paste0("Total Simulations ",advance.importDOE$total)
  })

  output$info.text1 <- renderText({
    req(use_simulator() & advance.importDOE$total & currentDOE$nobs>0)
    paste0("Currently Used ",currentDOE$nobs)
  })


  output$info.text2 <- renderText({
    req(use_simulator() & advance.importDOE$total & currentDOE$nobs>0)
    if (sum(advance.importDOE$status == "ended") > currentDOE$nobs){
      font <- 'red'
    }else{
      font <- 'black'
    }
    formatedFont <- sprintf('<font color="%s">%s</font>',font,paste0("Completed Simulations ",sum(advance.importDOE$status == "ended")))
  })
  
  output$infoDOE.dynui <- renderUI({
    req(use_simulator() & advance.importDOE$total & currentDOE$nobs>0)
    
    uilist <- list()
    uilist[[1]] <- 
        column(3,
               actionButton(ns("prelimexplo.viewDOE"), label = "View DOE", class = "btn-primary",icon = icon("table"))
        )
    uilist[[2]] <- 
        column(3,
               actionButton(ns("prelimexplo.refresh"), label = "Refresh DOE", class = "btn-primary",icon = icon("sync"))
        )
    return(uilist)
  })
  
  ML <- callModule(quantitativeExploration.server, "quantitativeExploration", currentDOE, window.dimension, settings)
  return(ML)
}
