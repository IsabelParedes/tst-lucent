#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module generateDOE
source("modules/shared/XinfosChange.R", local = TRUE)
source("modules/prepareDOE/evaluateDOE.R", local = TRUE)
source("modules/prepareDOE/visualizeDOE.R", local = TRUE)


sobolsequencecat <- function(u,lvls,weights){
  nlvls <- length(lvls)
  if (length(weights)==1) weights <- rep(weights,nlvls)
  nobs <- length(u)
  cump <- c(0,cumsum(weights))
  s <- matrix(NA,nobs,1)
  for (r in 1:(nlvls-1)){
    id <- (u > cump[r] & u <= cump[r+1])
    s[id] <- lvls[r]
  }
  id <- (u > cump[nlvls])
  s[id] <- lvls[nlvls]
  return(s)
}

generate.DOE.Xopt <- function(nX, nobs, nobs.slide, DOEtype, Xinfos, names.num, names.cat) {
  Xbounds <- get.bounds(Xinfos)
  if (DOEtype == 'LHS for Categorical Inputs (Optimal but large sample size)'){
    # compute mixed DOE
    # get number of numeric var, number of slices
    nvar <- get.nb.num(Xinfos)
    nslices <- nobs/nobs.slide
    # DOE
    Xopt <- maximinSLHD(t = nslices, m = nobs.slide, k = nvar)$Design
    # rescale DOE
    Xopt <- Xopt[,2:(nvar + 1)]/nobs
    Xopt <- repmat(Xbounds[1,,drop=F], nobs, 1) + repmat(Xbounds[2,,drop=F] - Xbounds[1,,drop=F], nobs, 1)*Xopt
    colnames(Xopt) <- names.num
    # assign level for each point
    levels <- lapply(Xinfos, function(var.info){unlist(var.info$levels)})
    levels <- levels[!is.na(levels)]
    levels <- expand.grid(levels)
    levels <- as.data.frame(levels[sort(rep(1:nslices, nobs.slide)),])
    Xopt <- cbind(levels, Xopt)
    colnames(Xopt) <- c(names.cat, names.num)
    Xopt <- Xopt[,sapply(Xinfos, function(Xinfos){Xinfos$name})]
    row.names(Xopt) <- NULL
  }
  
  if (DOEtype == 'Mixed Sobol Sequence (Fast but suboptimal exploration)'){
    levels <- lapply(Xinfos, function(var.info){unlist(var.info$levels)})
    levels <- levels[!is.na(levels)]
    Xopt <- runif.sobol(nobs, nX, scrambling = 1)
    nbcat <- length(levels)
    Xoptnum <- as.data.frame(repmat(Xbounds[1,,drop=F], nobs, 1) + repmat(Xbounds[2,,drop=F] - Xbounds[1,,drop=F], nobs, 1)*Xopt[,(nbcat+1):nX])
    Xoptcat <- sapply(1:nbcat,function(col) sobolsequencecat(Xopt[,col],levels[[col]],1/length(levels[[col]])))
    Xopt <- cbind(Xoptcat,Xoptnum)
    colnames(Xopt) <- c(names.cat, names.num)
    Xopt <- Xopt[,sapply(Xinfos, function(Xinfos){Xinfos$name})]
    row.names(Xopt) <- NULL
  }
  
  if (DOEtype == "LHS Maximin") {
    X <- lhsDesign(nobs, nX)$design
    Xopt <- maximinESE_LHS(X,T0 = 0.005*phiP(X),inner_it = 100,J = 50,it = 2)$design
    Xopt <- repmat(Xbounds[1,,drop=F], nobs, 1) + repmat(Xbounds[2,,drop=F] - Xbounds[1,,drop=F], nobs, 1)*Xopt
    colnames(Xopt) <- names.num
  }
  if (DOEtype == "LHS Discrepancy") {
    X <- lhsDesign(nobs, nX)$design
    Xopt <- discrepESE_LHS(X,T0 = 0.005*discrepancyCriteria(X,type = 'C2')[[1]],inner_it = 100,J = 50,it = 2)$design
    Xopt <- repmat(Xbounds[1,,drop=F], nobs, 1) + repmat(Xbounds[2,,drop=F] - Xbounds[1,,drop=F], nobs, 1)*Xopt
    colnames(Xopt) <- names.num
  }
  if (DOEtype == "LHS MaxProj (Optimal but slow)") {
    Xopt <- MaxProLHD(nobs, nX, nstarts = 50)$Design
    Xopt <- repmat(Xbounds[1,,drop=F], nobs, 1) + repmat(Xbounds[2,,drop=F] - Xbounds[1,,drop=F], nobs, 1)*Xopt
    colnames(Xopt) <- names.num
  }
  if (DOEtype == "Sobol Sequence (Fast but suboptimal exploration)") {
    Xopt <- runif.sobol(nobs, nX, scrambling = 1)
    Xopt <- repmat(Xbounds[1,,drop=F], nobs, 1) + repmat(Xbounds[2,,drop=F] - Xbounds[1,,drop=F], nobs, 1)*Xopt
    colnames(Xopt) <- names.num
  }
    

  return(Xopt)
}

check.input.size <- function(input, min.value){
  if (is.na(input) | input < min.value){
    showModal(modalDialog(HTML(paste(
      "At least 2 numeric inputs.", 
      "Minimum sample size: 3",
      "For mixed DOE, minimum sample size for each category: 2", sep = '<br/>')), title = "Warning")
    )
    return(FALSE)
  }else{
    return(TRUE)
  }
}

generateDOE.ui <- function(id) {
  ns <- NS(id)

  panel <- wellPanel(
    fluidRow(
      column(6,
        numericInput(ns("nX"), "Number of Inputs", 2, min = 2),
        XinfosChange.ui(ns("bounds"), label = "Define/Edit Inputs")
      ),
      column(6, br(), h5('Warning: Updating inputs resets DOE.'))
    ),
    hr(),
    uiOutput(ns('DOE.type')),
    uiOutput(ns('nobs.UI')),
    bsModal(
      ns("prepare-modalcheckDOE"), "Evaluate DOE", NULL, size = "large",
      evaluateDOE.ui(id = ns("evaluateDOE"))
    )
  )

  tagList(
    fluidRow(
      column(3, panel),
      column(
        9,XinfosChange.ui.preview(ns("bounds"))
      )
    ),
    hr(),
    fluidRow(
      column(8, actionButton(ns("goDOE"), "Generate DOE", class = "btn-primary", width='80%'), align = "center"),
      column(2, actionButton(ns("prepare.gocheckDOE"), "Evaluate DOE", class = "btn-info", width='100%')),
      column(2, downloadButton(ns("download"), label = "Export DOE", class = "btn-primary", style = "width:100%;"))
    ),
    hr(), 
    visualizeDOEUI(ns("plotVisualize"))
  )
}

generateDOE.server <- function(id, persistence, settings) {
  moduleServer(
    id,
    function(input, output, session) {
  
      ns <- session$ns

      initialXinfos = reactiveValues(nX = NULL, Xinfos = NULL)
      DOE <- reactiveValues(Xopt = NULL, Xinfos = NULL, mode.import = FALSE)
      
      # initialize inputs definition
      observeEvent(input$nX, {
          initialXinfos$nX <- input$nX
      })
      
      observeEvent(initialXinfos$nX, {
        if (!is.null(initialXinfos$nX) && initialXinfos$nX != input$nX) {
          updateNumericInput(session, "nX", value = initialXinfos$nX)
        }

        if (isTruthy(initialXinfos$nX > 0)){
          initialXinfos$Xinfos <- lapply(1:input$nX, get.initialXinfos)
        }else{
          initialXinfos$Xinfos <- NULL
        }
      })
      
      Xinfos <- callModule(XinfosChange.server, "bounds", initialXinfos)
      
      # reset variable names choices for pairplot and DOE when inputs are modified
      var.num <- reactiveValues(names = NULL)
      var.cat <- reactiveValues(names = NULL)
      mapNames <- reactiveValues(df = NULL)
      observeEvent(Xinfos$Xinfos, {
        var.num$names <- NULL
        var.cat$names <- NULL
        mapNames$df <- NULL
        DOE$Xopt <- NULL
        DOE$Xinfos <- NULL
      })

      # UI selector for DOE optimization algorithm
      all.numeric <- reactiveValues(bool = NULL)
      DOE.choices <- reactive({
        choices <- c("Mixed Sobol Sequence (Fast but suboptimal exploration)")
        nvar.num <- get.nb.num(Xinfos$Xinfos)
        if (nvar.num >= 2){
          choices <- c("LHS for Categorical Inputs (Optimal but large sample size)", choices)
        }
        return(choices)
      })
      
      output$DOE.type <- renderUI({
        req(input$nX > 0)
        # boolean for numeric/mixed DOE
        all.numeric$bool <- (get.nb.num(Xinfos$Xinfos) == input$nX)
        if (all.numeric$bool){
          choices = list("LHS MaxProj (Optimal but slow)", "LHS Maximin", "LHS Discrepancy", "Sobol Sequence (Fast but suboptimal exploration)")
        } else{
          choices = DOE.choices()
        }
        selectInput(ns("DOEtype"), label = "Select DOE method", choices = choices)
      })
      
      is.nobs.slice <- reactiveValues(bool = FALSE)
      # define DOE sample size (numeric/mixed DOE)
      nobs <- reactive(
        if (is.nobs.slice$bool){
          if (isTruthy(input$nX > 0)){
            nlevels.all <- sapply(Xinfos$Xinfos, function(Xinfos){Xinfos$nlevels})
            nlevels.all <- nlevels.all[!is.na(nlevels.all)]
            input$nobs.slide*prod(nlevels.all)
          }else{0}
        }else{
          input$nobs
        }
      )
      
      observeEvent(input$DOEtype, {
        req(input$DOEtype)
        is.nobs.slice$bool = (input$DOEtype == "LHS for Categorical Inputs (Optimal but large sample size)")
      })
      
      output$nobs.UI <- renderUI({
        if (is.nobs.slice$bool){
          output$nobs.total <- renderTable(setNames(as.data.frame(as.integer(nobs())), c('Total Sample Size')), 
                                          caption = 'recommended 10 x Inputs')
          fluidRow(
            column(6, numericInput(ns("nobs.slide"), "Sample Size for each Category ", 3, min = 2)),
            column(6, tableOutput(ns('nobs.total')))
          )
        }else{
          if (is.null(DOE$Xopt)) {
            numericInput(ns("nobs"), "Sample Size (recommended 10 x Inputs)", 10, min = 3)
          }
          else {
            # Useful if a study has been loaded
            numericInput(ns("nobs"), "Sample Size (recommended 10 x Inputs)", nrow(DOE$Xopt), min = 3)
          }
        }
      })
      
      observeEvent(input$goDOE, {
        
        #  check for mininum nb of numeric inputs
        numericNb <- get.nb.num(Xinfos$Xinfos)
        numInputValid <- check.input.size(numericNb, 2)
        
        if (numInputValid){
        
          # check for mininum sample size (numeric/mixed DOE)
          if (all.numeric$bool){
            nobs.check <- check.input.size(input$nobs, 3)
          }else{
            nobs.check <- check.input.size(input$nobs.slide, 2)
          }
      
          if (nobs.check){
          
            # warning modal for long computations
            if (input$nX > 20 | nobs() > 200){
              showModal(modalDialog(HTML(paste(
                "The sample size is greater than 200 or the number of inputs is greater than 20,
                the DOE construction can take a while...",
                "This window will close automatically when computations are done.", sep = '<br/>'
                )), title = "Warning: Construction of DOE")
              )
            }
        
            initForPairPlot()

            withProgress(message = 'Generating DOE: this may take a while...', value = 0, {
              Xopt <- generate.DOE.Xopt(input$nX, nobs(), input$nobs.slide, input$DOEtype, Xinfos$Xinfos,
                                        var.num$names, var.cat$names)
            })
            
            removeModal()
            
            DOE$Xinfos <- Xinfos$Xinfos
            DOE$Xopt <- Xopt
            
            persistence$autoSavingCount <- persistence$autoSavingCount + 1
            persistence$autoSavingCaller <- "generateDOE-goDOE"
          }
          
        }
          
      })

      initForPairPlot <- function() {
        # list of variable names of cat and num variables for pairplots
        var.num$names <- unlist(sapply(Xinfos$Xinfos, function(var.info){
          if (var.info$type == 'numeric'){
            var.info$name
          }else{NULL}
        }))
        
        var.cat$names <-  unlist(sapply(Xinfos$Xinfos, function(var.info){
          if (var.info$type == 'categorical'){
            var.info$name
          }else{NULL}
        }))
        
        # Populate mapNames for the pairplot (visualize DOE)
        names <- sapply(Xinfos$Xinfos, function(x) x$name)
        menu <- sapply(Xinfos$Xinfos, function(x) x$namemenu)
        visu <- sapply(Xinfos$Xinfos, function(x) x$namevisu)
        
        mapNames$df <- data.frame(names, menu, visu)
      }

      output$download <- downloadHandler(
        filename = 'DOE.csv',
        content = function(con) {
          write.table(x = DOE$Xopt, file = con, row.names = F, quote = FALSE, col.names = T, sep = ",")
        }
      )

      observeEvent(input$mode_import, {
        DOE$mode.import <- input$mode_import
      })
      
      observeEvent(input$prepare.gocheckDOE, {
        req(DOE$Xopt)
        toggleModal(session, "prepare-modalcheckDOE", toggle = "open")
      })
      
      callModule(evaluateDOE.server, "evaluateDOE", DOE, settings)
      
      # Call visualizeDOE module
      visualizeDOEServer("plotVisualize",
                        data = reactive(DOE$Xopt),
                        numericVariables = reactive(var.num$names),
                        categoricalVariables = reactive(var.cat$names),
                        mapNames = reactive(mapNames$df))
      
      #### Loaded study ####
      
      observeEvent(persistence$updatingStep, {
        if (is.null(persistence$loadedStudy$DOE.manual)) { # Old saving doesn't have the field 'DOE.manual' => use field 'DOE'
          DOEX <- persistence$loadedStudy$DOE
        }
        else {
          DOEX <- persistence$loadedStudy$DOEX
        }
        if (persistence$updatingStep == "generateDOE-nX") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))

          if (!is.null(persistence$loadedStudy$doeProblemDef) && (is.null(persistence$loadedStudy$doeProblemDef$choice) || persistence$loadedStudy$doeProblemDef$choice == 3)) {
            initialXinfos$nX <- ncol(DOEX$Xopt)
          }

          progressToNextStep(persistence)
        }
        else if (persistence$updatingStep == "generateDOE-xInfos") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))

          if (!is.null(persistence$loadedStudy$doeProblemDef) && (is.null(persistence$loadedStudy$doeProblemDef$choice) || persistence$loadedStudy$doeProblemDef$choice == 3)) {
            initialXinfos$Xinfos <- DOEX$Xinfos
          }

          progressToNextStep(persistence)
        }
        else if (persistence$updatingStep == "generateDOE-results") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))

          if (!is.null(persistence$loadedStudy$doeProblemDef) && (is.null(persistence$loadedStudy$doeProblemDef$choice) || persistence$loadedStudy$doeProblemDef$choice == 3)) {
            DOE$Xopt <- DOEX$Xopt
            DOE$Xinfos <- DOEX$Xinfos

            initForPairPlot()
          }

          progressToNextStep(persistence)
        }
      }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
      
      return(DOE)
    }
  )
}
