#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module constrainedDefine
source("modules/shared/XinfosChange.R", local = TRUE)
source("modules/shared/X0Change.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)

getCOformulationThresholds <- function(COformulation, ynames) {
  dimy <- length(COformulation$idC)
  if (dimy == 0) return(NULL)
  
  threshconstraints <- matrix(as.matrix(COformulation$COt),ncol = dimy)
  b <- matrix(NA,ncol = dimy)
  for (j in 1:dimy) {
    idsign <- (as.numeric(COformulation$COsign[j]) + 3)/2
    b[j] <- paste0(tablesign[idsign],threshconstraints[j])
  }
  rownames(b) <- "Thresholds"
  colnames(b) <- ynames[COformulation$idC]
  b
}

getCOformulationOptimTypes <- function(COformulation, ynames) {
  dimy <- length(COformulation$idO)
  if (dimy == 0) return(NULL)
  
  m <- matrix(as.matrix(COformulation$COobj),ncol = dimy)
  b <- matrix(rep("Max",dimy),ncol = dimy)
  idmin <- which(m == -1)
  b[idmin] <- "Min"
  rownames(b) <- "Optim. Type"
  colnames(b) <- ynames[COformulation$idO]
  b
}

getCOformulation <- function(dinit, ynames) {
  cnames <- dinit[1, ]
  id <- dinit[2, ]
  if (all(dinit[2,] == 'C')) { # inversion pb 
      idCtemp <- which(id == "C")
      idC <- unlist(lapply(idCtemp, function(i) {
        which(ynames == as.character(cnames[i]))
      }))
      if (length(idCtemp)>1) {
        d <- data.frame(lapply(dinit[-c(1, 2), ], as.numeric))
        COsign <- d[1, idCtemp]
        COt <- d[2, idCtemp]
      } else {
        COsign <- dinit[3, idCtemp]
        COt <- dinit[4, idCtemp]
      }
      resCOformulation <- list(idC = idC,idO = idC,COsign = COsign,COt = COt,COobj = -1)
      resCOformulation$thresholds <- getCOformulationThresholds(resCOformulation, ynames)
      resCOformulation$optimTypes <- getCOformulationOptimTypes(resCOformulation, ynames)
      resCOformulation$isInversion <- TRUE

  }else{
      idOtemp <- which(id == "O")
      idO <- unlist(lapply(idOtemp, function(i) {
        which(ynames == as.character(cnames[i]))
      }))
      idCtemp <- which(id == "C")
      d <- data.frame(lapply(dinit[-c(1, 2), ], as.numeric))
      if (length(idCtemp) != 0) {
        idC <- unlist(lapply(idCtemp, function(i) {
          which(ynames == as.character(cnames[i]))
        }))
        COsign <- d[1, idCtemp]
        COt <- d[2, idCtemp]
      }
      else {
        idC <- NULL
        COsign <- NULL
        COt <- NULL
        COobj <- NULL
      }
      COobj <- d[1, idOtemp]
      
      resCOformulation <- list(idC = idC,idO = idO,COsign = COsign,COt = COt,COobj = COobj)
      resCOformulation$thresholds <- getCOformulationThresholds(resCOformulation, ynames)
      resCOformulation$optimTypes <- getCOformulationOptimTypes(resCOformulation, ynames)
      resCOformulation$isInversion <- FALSE
  }
  return(resCOformulation)
}

check.CO.formulation <- function(dinit, ynames, nbobj){
  nY <- length(ynames)
  yNamesGeneric <- buildYNames(nY, "Y")
  #if(all(ynames == yNamesGeneric))
  if (all(dinit[2,] == 'C')) { # inversion problem
    valid <- list()
    valid[[1]] <- !all(dinit[1,] %in% ynames)
    if(isTRUE(valid[[1]])){
      valid[[2]] <- !all(dinit[1,] %in% yNamesGeneric)
      valid[[3]] <- !all(ynames == yNamesGeneric)
    }
    valid[[4]] <- !all(dinit[3,] %in% c(-1,1))
    valid[[5]] <- anyNA(as.numeric(dinit[4, ]))
    error.tag <- list()
    error.tag$names <- 'Names must be consistent with DOE definition.'
    error.tag$namesnotgeneric <- 'Warning: simulator outputs names not known by LAGUN. Give generic names or launch an initial point to let LAGUN identify outputs names.'
    error.tag$namesgeneric <- 'Generic names given that do not correspond to simulator outputs names.'
    error.tag$side <- 'Constraints: -1 (<) or 1 (>)'
    error.tag$numeric <- 'Threshold have to be numeric.'

  }
  else{
    valid <- list()
    valid[[1]] <- !all(dinit[1,] %in% ynames)
    valid[[2]] <- !all(dinit[2,] %in% c('C','O'))
    valid[[3]] <- !all(dinit[3,] %in% c(-1,1))
    valid[[4]] <- anyNA(as.numeric(dinit[4, dinit[2,] == 'C']))
    valid[[5]] <- !(sum(dinit[2,] == 'O') >= 1 & sum(dinit[2,] == 'O') <= nbobj)
    valid[[6]] <- any(duplicated(t(dinit[1:3,dinit[2,] == 'C'])))
    valid[[7]] <- any(dinit[1, dinit[2, ] == "C"] %in% dinit[1, dinit[2, ] == "O"])

    error.tag <- list()
    error.tag$names <- 'Names must be consistent with DOE definition. If surrogate optimization: names must include only outputs for which you have built a surrogate model.'
    error.tag$type <- 'Possible types are C (constraint) and O (objective).'
    error.tag$side <- 'Constraints: -1 (<) or 1 (>) / Objectives: -1 (min) or 1 (max)'
    error.tag$numeric <- 'Thresholds have to be numeric.'
    error.tag$obj <- 'None or too many objectives.'
    error.tag$cst <- 'Duplicated constraints.'
    error.tag$cst_obj <- 'Outputs cannot be objective and constraint at the same time.'
    error.tag$namesnotgeneric <-'Warning: simulator outputs names not known by LAGUN. Use generic names or launch an initial point to let LAGUN identify outputs names.'
    error.tag$namesgeneric <- 'Generic names given that do not correspond to simulator outputs names.'
    if(isTRUE(valid[[1]])){
      valid[[8]] <- !all(dinit[1,] %in% yNamesGeneric)
      valid[[9]] <- !all(ynames == yNamesGeneric)
    }
  }
  return(list(valid = !any(unlist(valid)), error.msg = error.tag[unlist(valid)]))
}

constrainedDefine.ui <- function(id) {
  ns <- NS(id)
  modalContent <- tagList(
    fluidRow(
      column(4,""),
      column(8,
             fileInput(ns('file'), 'Import Problem Formulation', accept = c('.txt','.dat','.csv', '.json')),
             tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });')),
             uiOutput(ns('error.file'))
      )
    ),
    hr(),
    fluidRow(
      h4("Manually Change Formulation", div("(please save)", class = "small"))
    ),
    fluidRow(
      column(4,""),
      column(4,
             XinfosChange.ui(ns("bounds"), label = "Change Inputs Info for Optimization"),
             align = "center"),
      conditionalPanel(
        condition = paste0("output['", ns("conditionalDirectOptim"), "']"),
        column(4, switchInput(ns("switchinversion"), label = "Feasible set learning problem", size="small"), align="center")
      )
    ),
    br(),
    br(),
    fluidRow(
      column(4,""),
      column(4,
             uiOutput(ns("obj1.ui")), align = "center"
      ),
      column(4,
             uiOutput(ns("signobj1.ui")), align = "center"
      )
    ),
    fluidRow(
      column(4,""),
      column(4,
             uiOutput(ns("obj2.ui")), align = "center"
      ),
      column(4,
             uiOutput(ns("signobj2.ui")), align = "center"
      )
    ),
    hr(),
    fluidRow(
      column(4,""),
      column(4,
             uiOutput(ns("nbcons.ui")), align = "center"
      ),
      column(4,"")
    ),
    uiOutput(ns("listconstraints.ui")),
    hr(),
    uiOutput(ns("listinversionvars.ui")),
    hr(),
    uiOutput(ns('error.save')),
    fluidRow(
      column(3, actionButton(ns("saveoptimdef"), label = "Save and Close", class = "btn-warning",
                             width = '100%'), offset = 2),
      column(3, actionButton(ns("closeoptimdef"), label = "Dismiss", class = "btn-secondary",
                             width = '100%'), offset = 2)
    )
  )
  
  tagList(
    fluidRow(
      column(4, wellPanel(
        fluidRow(
          column(2,""),
          column(8,
                 actionButton(ns("optimdef"), label = "Define Problem Formulation", class = "btn-primary"),
                 align = "center"),
          column(2,"")
        ),
        conditionalPanel(
            condition = paste0("output['", ns("conditionalDirectOptim"), "']"),
            tagList(
              br(),
              br(),
              fluidRow(
                column(2,""),
                column(8,
                      X0Change.ui(ns("x0change"), label = "Define Initial Inputs Values"),
                      align = "center"),
                column(2,"")
              )
            )
        ),
        br(),
        br(),
        fluidRow(
          column(2,""),
          column(8,
                 downloadButton(ns("downloadformulation"), label = "Export Problem Formulation",class = "btn-info"), icon = icon("download"),
                 align = "center"),
          column(2,"")
        )
      )),
      column(
        8,
        actionButton(ns("resetConstrOptim"), 
                     "Reset", 
                     class="btn-warning", 
                     style="float:right",
                     width = "10%"),
        XinfosChange.ui.preview(ns("bounds")),
        conditionalPanel(
            condition = paste0("output['", ns("conditionalDirectOptim"), "']"),
            tagList(
              hr(),
              DT::dataTableOutput(ns('x0table'))
            )
        ),
        conditionalPanel(
            condition = paste0("output['", ns("conditionalOptimTypes"), "']"),
            tagList(
              hr(),
              DT::dataTableOutput(ns('thresholds')),
              DT::dataTableOutput(ns('optimTypes'))
            )
        )
      )
    ),
    bsModal(
      ns("modalDefineOptim"), "Define Problem Formulation", NULL, 
      size="large",modalContent,
      tags$head(tags$style(paste0("#", ns("modalDefineOptim")," .modal-footer{display:none}",
                                  " .modal-lg{width: 70%}")))
    )
  )
}

constrainedDefine.server <- function(id, DOE, listmodels, persistence, nbcons.min = 1, nbobj = 1, simulations = NULL, typeOptim = "constrained") {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Update output types for the visualization only if the surrogate models are updated
      Yinfos <- reactiveValues(int.ids=NULL, control.ids=NULL, const.ids=NULL, visu.ids=NULL, nY=NULL)
      observeEvent(list(listmodels$bestQ2loo$id, DOE$nY), {
        
        YwithSelectedModel <- seq(unlist(DOE$nY))
        
        if (!is.null(listmodels$selected))
          YwithSelectedModel <- YwithSelectedModel[sapply(listmodels$selected$id, function(x) !is.na(x[1]))]
        
        Yinfos$int.ids <- intersect(DOE$Yinfos$int.ids, YwithSelectedModel)
        Yinfos$control.ids <- intersect(DOE$Yinfos$control.ids, YwithSelectedModel)
        Yinfos$const.ids <- intersect(DOE$Yinfos$const.ids, YwithSelectedModel)
        Yinfos$visu.ids <- c(Yinfos$int.ids, Yinfos$control.ids, Yinfos$const.ids)
        Yinfos$nY <- length(Yinfos$visu.ids)
      })
      
      
      initialXinfos <- reactiveValues(nX = NULL, Xinfos = NULL)
      
      observeEvent(DOE$Xinfos, {
        initialXinfos$nX <- DOE$nX
        initialXinfos$Xinfos <- DOE$Xinfos
      })
      
      
      error.msg <- reactiveValues(file = NULL)
      
      if (typeOptim == "directOptim") {
        Xinfos <- callModule(XinfosChange.server, "bounds", initialXinfos, data = NULL, edit.disable = FALSE)
      }
      else {
        # initialize with bounds coming from DOE reactiveValues
        Xinfos <- callModule(XinfosChange.server, "bounds", initialXinfos, data = DOE, edit.disable = TRUE)
      }
      callModule(X0Change.server, "x0change", Xinfos, initialXVal, DOE)
      
      output$conditionalDirectOptim <- reactive({
          typeOptim == "directOptim"
      })
      outputOptions(output, "conditionalDirectOptim", suspendWhenHidden = FALSE)
      
      output$conditionalOptimTypes <- reactive({
          !is.null(COformulation$idO)
      })
      outputOptions(output, "conditionalOptimTypes", suspendWhenHidden = FALSE)
      
      COformulation <- reactiveValues(
        idC = NULL,idO = NULL,COsign = NULL,COt = NULL,COobj = NULL,thresholds = NULL,optimTypes = NULL, textConstr = NULL, haschanged=NULL
      )
      
      observeEvent(Xinfos$Xinfos, {
        
        req(COformulation$idO)
        
        COformulation$idC <- NULL
        COformulation$idO <- NULL
        COformulation$COsign <- NULL
        COformulation$COt <- NULL
        COformulation$COobj <- NULL
        COformulation$thresholds <- NULL
        COformulation$optimTypes <- NULL
        COformulation$textConstr <- NULL
        COformulation$haschanged <- COformulation$haschanged + 1
        
      }, ignoreInit = TRUE)
      
      
      observeEvent(input$resetConstrOptim, {
        
        COformulation$idC <- NULL
        COformulation$idO <- NULL
        COformulation$COsign <- NULL
        COformulation$COt <- NULL
        COformulation$COobj <- NULL
        COformulation$thresholds <- NULL
        COformulation$optimTypes <- NULL
        COformulation$textConstr <- NULL
        
        if(!is.null(COformulation$haschanged)){
          COformulation$haschanged <- COformulation$haschanged + 1
        }

        initialXinfos$nX <- DOE$nX
        initialXinfos$Xinfos <- DOE$Xinfos
      })
      
      
      
      observeEvent(persistence$updatingStep, {
        if (persistence$updatingStep == "constrainedDefine-constrained-initialXinfos" && typeOptim == "constrained") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          
          if (!is.null(persistence$loadedStudy$constrOptim$Xinfos)) {
            initialXinfos$nX <- persistence$loadedStudy$DOE$nX
            initialXinfos$Xinfos <- persistence$loadedStudy$constrOptim$Xinfos # why NULL?
          }
          
          progressToNextStep(persistence)
        }
        else if (persistence$updatingStep == "constrainedDefine-constrained-COformulation" && typeOptim == "constrained") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          
          if (length(persistence$loadedStudy$constrOptim$COformulation$idC) != 0 || length(persistence$loadedStudy$constrOptim$COformulation$idO) != 0) {
            COformulation$idC <- persistence$loadedStudy$constrOptim$COformulation$idC
            COformulation$idO <- persistence$loadedStudy$constrOptim$COformulation$idO
            COformulation$COsign <- persistence$loadedStudy$constrOptim$COformulation$COsign
            COformulation$COt <- persistence$loadedStudy$constrOptim$COformulation$COt
            COformulation$COobj <- persistence$loadedStudy$constrOptim$COformulation$COobj
            COformulation$thresholds <- persistence$loadedStudy$constrOptim$COformulation$thresholds
            COformulation$optimTypes <- persistence$loadedStudy$constrOptim$COformulation$optimTypes
            COformulation$textConstr <- persistence$loadedStudy$constrOptim$COformulation$textConstr
            COformulation$isInversion <- persistence$loadedStudy$constrOptim$COformulation$isInversion
            COformulation$haschanged <- 0
          }
          progressToNextStep(persistence)
        }
        else if (persistence$updatingStep == "constrainedDefine-directOptim-initialXinfos" && typeOptim == "directOptim") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          
          if (!is.null(persistence$loadedStudy$directOptim$Xinfos)) {
            initialXinfos$nX <- DOE$nX
            initialXinfos$Xinfos <- persistence$loadedStudy$directOptim$Xinfos
          }
          
          progressToNextStep(persistence)
        }
        else if (persistence$updatingStep == "constrainedDefine-directOptim-COformulation" && typeOptim == "directOptim") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          
          if (length(persistence$loadedStudy$directOptim$COformulation$idC) != 0 || length(persistence$loadedStudy$directOptim$COformulation$idO) != 0) {
            COformulation$idC <- persistence$loadedStudy$directOptim$COformulation$idC
            COformulation$idO <- persistence$loadedStudy$directOptim$COformulation$idO
            COformulation$COsign <- persistence$loadedStudy$directOptim$COformulation$COsign
            COformulation$COt <- persistence$loadedStudy$directOptim$COformulation$COt
            COformulation$COobj <- persistence$loadedStudy$directOptim$COformulation$COobj
            COformulation$thresholds <- persistence$loadedStudy$directOptim$COformulation$thresholds
            COformulation$optimTypes <- persistence$loadedStudy$directOptim$COformulation$optimTypes
            COformulation$textConstr <- persistence$loadedStudy$directOptim$COformulation$textConstr
            COformulation$isInversion <- isTRUE(persistence$loadedStudy$directOptim$COformulation$isInversion)
            updateSwitchInput(session = session, inputId  = "switchinversion", value = COformulation$isInversion)
            COformulation$haschanged <- 0
          }
          progressToNextStep(persistence)
        }
        else if (persistence$updatingStep == "constrainedDefine-directOptim-x0" && typeOptim == "directOptim") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          
          if (!is.null(persistence$loadedStudy$directOptim$initialXVal)) {
            initialXVal(as.data.frame(persistence$loadedStudy$directOptim$initialXVal))
          }
          progressToNextStep(persistence)
        }
      }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
      
      initialXVal <- reactiveVal(NULL)
      # COformulation.temp temporally stores all modifications from an imported file in the modal 
      # It is also used to initialze the dynamic ui if the user wants to further modify the problem
      # COformulation is set when the user saves the modifications and returns
      COformulation.temp <- reactiveValues(
        idC = NULL,idO = NULL,COsign = NULL,COt = NULL,COobj = NULL,thresholds = NULL,optimTypes = NULL, isInversion = NULL
      )
      
      choicesY <- reactive({
        req(DOE$ynamesmenu,Yinfos)
        idYCat <- which(DOE$Yinfos$type == 'categorical')
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$int.ids, idYCat)])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$control.ids, idYCat)])
        #if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$const.ids, idYCat)])
        return(l)
      })
      
      observeEvent(Xinfos$Xinfos, {
        req(define$Xinfos$Xinfos)
        x0 <- lapply(seq_len(length(define$Xinfos$Xinfos)), function(i) {
          var <- Xinfos$Xinfos[[i]]
          if (var$type == "numeric") {
            return((var$bounds[1] + var$bounds[2]) / 2)
          } else if (var$type == "categorical") {
            return(var$levels[[1]])
          }
          return(NaN)
        })

        initialXVal(as.data.frame(x0))
      })
      
      observeEvent(simulations$is.running, {
        if (simulations$is.running){
          disableActionButton(ns("optimdef"),session)
          disableActionButton(ns("bounds-change"),session)
        }else{
          enableActionButton(ns("optimdef"),session)
          enableActionButton(ns("bounds-change"),session)
        }
      })
      
      # If one output, disable constrained optimization
      # If more than one output, enable constrained optimization
      observeEvent(listmodels$selected$id, {
        if (sum(!is.na(listmodels$selected$id))==1 & typeOptim == "constrained") {
          disableActionButton(ns("optimdef"),session)
        } else {
          enableActionButton(ns("optimdef"),session)
        }
      })

      formulationToUse <- NULL
      
      observeEvent(input$optimdef, {
        formulationToUse <<- "useKnownFormulation"
        toggleModal(session, "modalDefineOptim", toggle = "open")
      })
      
      observeEvent(input$closeoptimdef, {
        toggleModal(session, "modalDefineOptim", toggle = "close")
        COformulation.temp$idC <- COformulation$idC
        COformulation.temp$idO <- COformulation$idO
        COformulation.temp$COsign <- COformulation$COsign
        COformulation.temp$COt <- COformulation$COt
        COformulation.temp$COobj <- COformulation$COobj
        COformulation.temp$thresholds <- COformulation$thresholds
        COformulation.temp$optimTypes <- COformulation$optimTypes
        COformulation.temp$isInversion <- COformulation$isInversion
      })
      
      observeEvent(input$saveoptimdef, {
        req(!isTRUE(input$switchinversion))
        
        validNbCons <- FALSE
        
        if (!is.na(input$nbcons)){
          if (typeOptim=="constrained" & input$nbcons > 0)
            validNbCons <- TRUE
          else if ((typeOptim=="sequential" | typeOptim=="directOptim") & input$nbcons >= 0)
            validNbCons <- TRUE
        }
        
        if (validNbCons){
          idO <- which(input$chooseObjective1==DOE$ynamesmenu)
          if (nbobj==2) idO <- c(idO,which(input$chooseObjective2==DOE$ynamesmenu))
          COobj <- switch(input$signobj1,Minimize=-1,Maximize=1)
          if (nbobj==2 & length(idO)==2)  COobj <- c(COobj,switch(input$signobj2,Minimize=-1,Maximize=1))
          nbcons <- input$nbcons
          idcons <- numeric(nbcons)
          signcons <- numeric(nbcons)
          thcons <- numeric(nbcons)
          constraintIndexes <- setdiff(1:(input$nbcons + length(removedConstraintIndexes)), removedConstraintIndexes)
          for (i in seq_len(nbcons)){
            idcons[i] <- which(input[[paste0('namecons', constraintIndexes[i])]]==DOE$ynamesmenu)
            signcons[i] <- 1-2*as.numeric(input[[paste0('signcons', constraintIndexes[i])]]==tablesign[1])
            thcons[i] <- input[[paste0('thcons', constraintIndexes[i])]]
          }
          
          newCOformulation <- list(
            idC = idcons, idO = idO, COsign = signcons, 
            COt = thcons, COobj = COobj, thresholds = getCOformulationThresholds(COformulation, DOE$ynamesmenu), 
            optimTypes = getCOformulationOptimTypes(COformulation, DOE$ynamesmenu), isInversion = FALSE
          )
          
          if (is.null(COformulation$idO)){
            # First time we update the object
            COformulation$idC <- idcons
            COformulation$idO <- idO
            COformulation$COsign <- signcons
            COformulation$COt <- thcons
            COformulation$COobj <- COobj
            COformulation$thresholds <- getCOformulationThresholds(COformulation, DOE$ynamesmenu)
            COformulation$optimTypes <- getCOformulationOptimTypes(COformulation, DOE$ynamesmenu) 
            COformulation$haschanged <- 0
            COformulation$isInversion <- FALSE
          }else{
            
            if (!all(sapply(names(newCOformulation), function(name){
              identical(reactiveValuesToList(COformulation)[[name]],newCOformulation[[name]])
            }))){
              # This means the formulation has changed
              COformulation$idC <- idcons
              COformulation$idO <- idO
              COformulation$COsign <- signcons
              COformulation$COt <- thcons
              COformulation$COobj <- COobj
              COformulation$thresholds <- getCOformulationThresholds(COformulation, DOE$ynamesmenu)
              COformulation$optimTypes <- getCOformulationOptimTypes(COformulation, DOE$ynamesmenu) 
              COformulation$haschanged <- COformulation$haschanged + 1
              COformulation$isInversion <- FALSE
            }
          }
          removedConstraintIndexes <<- c()
          toggleModal(session, "modalDefineOptim", toggle = "close")
        }
        
        output$error.save <- renderUI({
          req(!is.null(input$nbcons), !isTRUE(input$switchinversion))
          
          invalidNbCons <- invalidNbConsSeq <- FALSE
          
          if (typeOptim == "constrained")
            invalidNbCons <- (input$nbcons <= 0)
          else if (typeOptim == "sequential" | typeOptim=="directOptim")
            invalidNbConsSeq <- (input$nbcons < 0)
          
          if (is.na(input$nbcons))
            return(h4("Error: the number of constraints must not be empty"))
          else if(invalidNbCons)
            return(h4("Error: the number of constraints must be greater than 0"))
          else if(invalidNbConsSeq)
            return(h4("Error: the number of constraints must be greater than or equal to 0"))
          else
            return(NULL)
        })
        
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "constrainedDefine-saveoptimdef"
      })

     observeEvent(input$saveoptimdef, {
        req(isTRUE(input$switchinversion))
        
        if (!is.na(input$nbcons) && input$nbcons > 0) {
          nbcons <- input$nbcons
          idcons <- numeric(nbcons)
          signcons <- numeric(nbcons)
          thcons <- numeric(nbcons)
          constraintIndexes <- setdiff(1:(nbcons + length(removedConstraintIndexes)), removedConstraintIndexes)
          for (i in seq_len(nbcons)){
            idcons[i] <- which(input[[paste0('namecons', constraintIndexes[i])]]==DOE$ynamesmenu)
            signcons[i] <- 1-2*as.numeric(input[[paste0('signcons', constraintIndexes[i])]]==tablesign[1])
            thcons[i] <- input[[paste0('thcons', constraintIndexes[i])]]
          }
          idO <- idcons[1] # set the virtual objective to the first constraint id 
          COobj <- c(-1)
          optimTypes <- "Min"
          newCOformulation <- list(
            idC = idcons, idO = idO, COsign = signcons, 
            COt = thcons, COobj = COobj, thresholds = getCOformulationThresholds(COformulation, DOE$ynamesmenu), 
            optimTypes = optimTypes, isInversion = TRUE
          )
          
          if (is.null(COformulation$idO)) {
            # First time we update the object
            COformulation$idC <- idcons
            COformulation$idO <- idO
            COformulation$COsign <- signcons
            COformulation$COt <- thcons
            COformulation$COobj <- COobj
            COformulation$thresholds <- getCOformulationThresholds(COformulation, DOE$ynamesmenu)
            COformulation$optimTypes <- optimTypes
            COformulation$isInversion <- TRUE
            COformulation$haschanged <- 0

          } else {
            
            if (!all(sapply(names(newCOformulation), function(name) {
              identical(reactiveValuesToList(COformulation)[[name]], newCOformulation[[name]])
            }))) {
              # This means the formulation has changed
              COformulation$idC <- idcons
              COformulation$idO <- idO
              COformulation$COsign <- signcons
              COformulation$COt <- thcons
              COformulation$COobj <- COobj
              COformulation$thresholds <- getCOformulationThresholds(COformulation, DOE$ynamesmenu)
              COformulation$optimTypes <- optimTypes 
              COformulation$isInversion <- TRUE
              COformulation$haschanged <- COformulation$haschanged + 1
            }
          }
          removedConstraintIndexes <<- c()
          toggleModal(session, "modalDefineOptim", toggle = "close")
        }
      })
      
      observeEvent(input$file$datapath, {
        req(DOE$ynames)
        ext <- tools::file_ext(input$file$datapath)
        if (ext == "json") {
          objFromJson <- jsonlite::fromJSON(paste(readLines(input$file$datapath, warn = FALSE), collapse = "\n"))
          dinit <- objFromJson$COformulation
          dinit <- rbind(names(dinit), dinit)
          if (typeOptim == "directOptim") {
            setXinfos(objFromJson$Xinfos)
          }
        }
        else {
          dinit <- read.csv(input$file$datapath, header = F, colClasses = "character")
        }
        validation.CO <- check.CO.formulation(dinit, unlist(choicesY(), use.names = F), nbobj)
        if (validation.CO$valid){
          newCOformulation <- getCOformulation(dinit, DOE$ynamesmenu)
          COformulation.temp$idC <- newCOformulation$idC
          COformulation.temp$idO <- newCOformulation$idO
          COformulation.temp$COsign <- newCOformulation$COsign
          COformulation.temp$COt <- newCOformulation$COt
          COformulation.temp$COobj <- newCOformulation$COobj
          COformulation.temp$thresholds <- newCOformulation$thresholds
          COformulation.temp$optimTypes <- newCOformulation$optimTypes
          COformulation.temp$isInversion <- newCOformulation$isInversion
          error.msg$file <- NULL
          formulationToUse <<- "useImportedFormulation"
          updateSwitchInput(session=session, inputId  = "switchinversion", value=isTRUE(newCOformulation$isInversion))
      
        }else{
          error.msg$file <- validation.CO$error.msg
        }
      })

      setXinfos <- function(lines) {
        separator <- ","
        decimal <- "."
        data <- NULL
        edit.disable <- FALSE
        lines.split <- lapply(lines, function(line) {
          return(as.character(unlist(strsplit(line, separator))))
        })
        lines.split <- lines.split[lapply(lines.split, length) > 0]
        header <- all(sapply(lines.split, function(line){line[2] %in% c('constant', 'categorical', 'numeric')}))
        newXinfos <- get.Xinfos.from.file(initialXinfos$nX, lines.split, Xinfos$Xinfos, header,
                                          separator, decimal, data, edit.disable)
        if (!is.null(newXinfos$Xinfos)) {
          Xinfos$Xinfos <- newXinfos$Xinfos
        } else{
          error.msg$file <- newXinfos$error.msg
          logger$print(newXinfos$error.msg)
        }
      }
  
      observeEvent(input$switchinversion, {
        if(!input$switchinversion) {
          updateNumericInput(session, "nbcons", value = 0) # Useful when there is only one output (which then necessarily corresponds to the output to be optimized)
        }
      })
      
      output$error.file <- renderUI({
        req(error.msg$file)
        list(h4(strong("Error !")), 
             HTML(paste(paste(error.msg$file, collapse = '<br/>'), '<br/> <br/>')))
      })
      
      output$obj1.ui <- renderUI({
        req(choicesY(), !isTRUE(input$switchinversion))
        if (is.null(COformulation.temp$idO)){
          if (is.null(COformulation$idO)){
            selobj <- DOE$ynamesmenu[Yinfos$int.ids][1]
          }else{
            # This means we already saved an optim definition so we display it in the ui
            selobj <- DOE$ynamesmenu[COformulation$idO]
          }
        }else{
          # This means a file is loaded, so we update the ui (priority over a previous saved definition)
          selobj <- DOE$ynamesmenu[COformulation.temp$idO]
        }
        t <- tagList(
          selectInput(
            ns("chooseObjective1"), 
            label = "Choose Objective",
            choices = choicesY(),
            selected = selobj[1]
          )
        )
        return(t)
      })
      
      choicesY2 <- reactive({
        req(DOE$ynamesmenu,Yinfos,input$chooseObjective1)
        idYCat <- which(DOE$Yinfos$type == 'categorical')
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$int.ids, idYCat)])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$control.ids, idYCat)])
        if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$const.ids, idYCat)])
        return(l)
      })
      
      output$obj2.ui <- renderUI({
        req(nbobj==2,choicesY(),input$chooseObjective1, !isTRUE(input$switchinversion))
        if (is.null(COformulation.temp$idO)){
          if (is.null(COformulation$idO)){
            selobj <- "None"
          }else{
            # This means we already saved an optim definition so we display it in the ui
            selobj <- DOE$ynamesmenu[COformulation$idO]
          }
        }else{
          # This means a file is loaded, so we update the ui (priority over a previous saved definition)
          selobj <- DOE$ynamesmenu[COformulation.temp$idO]
        }
        if (is.na(selobj[2])) selobj[2] <- "None"
        t <- tagList(
          br(),
          selectInput(
            ns("chooseObjective2"), 
            label = "Choose Objective",
            choices = c("None",choicesY2()),
            selected = selobj[2]
          )
        )
        return(t)
      })
      
      output$signobj1.ui <- renderUI({
        req(choicesY(), !isTRUE(input$switchinversion))
        if (is.null(COformulation.temp$COobj)){
          if (is.null(COformulation$COobj)){
            selsign <- rep("Minimize",nbobj)
          }else{
            # This means we already saved an optim definition so we display it in the ui
            selsign <- unlist(lapply(seq_len(nbobj), function(i){switch(as.integer(COformulation$COobj[i]>0)+1,"Minimize","Maximize")}))
          }
        }else{
          # This means a file is loaded, so we update the ui (priority over a previous saved definition)
          selsign <- unlist(lapply(seq_len(nbobj), function(i){switch(as.integer(COformulation.temp$COobj[i]>0)+1,"Minimize","Maximize")}))
        }
        t <- tagList(
          selectInput(
            ns("signobj1"),  
            label = "Type", 
            choices = list("Minimize","Maximize"), 
            selected = selsign[1]
          )
        )
        return(t)
      })
      
      output$signobj2.ui <- renderUI({
        req(nbobj==2,choicesY(), !isTRUE(input$switchinversion))
        if (is.null(COformulation.temp$COobj)){
          if (is.null(COformulation$COobj)){
            selsign <- rep("Minimize",nbobj)
          }else{
            # This means we already saved an optim definition so we display it in the ui
            selsign <- unlist(lapply(seq_len(nbobj), function(i){switch(as.integer(COformulation$COobj[i]>0)+1,"Minimize","Maximize")}))
          }
        }else{
          # This means a file is loaded, so we update the ui (priority over a previous saved definition)
          selsign <- unlist(lapply(seq_len(nbobj), function(i){switch(as.integer(COformulation.temp$COobj[i]>0)+1,"Minimize","Maximize")}))
        }
        if (is.na(selsign[2])) selsign[2] <-"Minimize"
        t <- tagList(
          br(),
          selectInput(
            ns("signobj2"),  
            label = "Type", 
            choices = list("Minimize","Maximize"), 
            selected = selsign[2]
          )
        )
        return(t)
      })
      
      output$nbcons.ui <- renderUI({
        req(choicesY())
        if (length(COformulation.temp$idC) == 0){
          if (length(COformulation$idC) == 0){
            nbcons <- nbcons.min
          }else{
            # This means we already saved an optim definition so we display it in the ui
            nbcons <- length(COformulation$idC)
          }
        }else{
          # This means a file is loaded, so we update the ui (priority over a previous saved definition)
          nbcons <- length(COformulation.temp$idC)
        }
        numericInput(ns("nbcons"), "Number of Constraints", nbcons, min = nbcons.min)
      })

      removedConstraintIndexes <- c()
      
      output$listinversionvars.ui <- renderUI({
        input$optimdef # Update constraints list ui when 'Import Problem Formulation' dialog is displayed
        COformulation.temp$idC # Update constraints list ui when a file is loaded
        req(choicesY(), input$nbcons>0, isTRUE(input$switchinversion))
        # Constraints can't be imposed on objectives
        nbcons <- input$nbcons
        indices <- c(Yinfos$int.ids,Yinfos$control.ids)
        constraintIndexes <- setdiff(1:(nbcons + length(removedConstraintIndexes)), removedConstraintIndexes)

        constraintsUI <- lapply(1:nbcons, function(i){
          nameconsValue <- DOE$ynamesmenu[1]
          signconsValue <- '<'
          thconsValue <- 0

          if (is.null(formulationToUse)) {
            isolate({
              if (!is.null(input[[paste0('namecons', constraintIndexes[i])]])) {
                nameconsValue <- input[[paste0('namecons', constraintIndexes[i])]]
                signconsValue <- input[[paste0('signcons', constraintIndexes[i])]]
                thconsValue <- input[[paste0('thcons', constraintIndexes[i])]]
              }
            })
          }
          else if (formulationToUse == "useKnownFormulation" && length(COformulation$idC) != 0) {
            # This means we already saved an optim definition so we display it in the ui
            nameconsValue <- DOE$ynamesmenu[COformulation$idC[i]]
            signconsValue <- tablesign[(as.numeric(COformulation$COsign[i]) + 3)/2]
            thconsValue <- COformulation$COt[i]
          }
          else if (formulationToUse == "useImportedFormulation" && length(COformulation.temp$idC) != 0) {
            # This means a file is loaded, so we update the ui
            nameconsValue <- DOE$ynamesmenu[COformulation.temp$idC[i]]
            signconsValue <- tablesign[(as.numeric(COformulation.temp$COsign[i]) + 3)/2]
            thconsValue <- COformulation.temp$COt[i]
          }

          fluidRow(
            column(1,""),
            column(4,
                   selectInput(
                     ns(paste0('namecons', constraintIndexes[i])),  
                     label = "", 
                     choices = DOE$ynamesmenu[indices], 
                     selected = nameconsValue
                   ), align = "center"
            ),
            column(2,
                   selectInput(
                     ns(paste0('signcons', constraintIndexes[i])),  
                     label = "", 
                     choices = c("<",">"), 
                     selected = signconsValue
                   ), align = "center"
            ),
            column(3,
                   numericInput(ns(paste0('thcons', constraintIndexes[i])), "", thconsValue), align = "center"
            ),
            column(1,
                  br(),
                  actionButton(
                    ns(paste0('remove', constraintIndexes[i])),
                    "",
                    icon("remove"),
                    class = "btn-danger",
                    onclick = paste0("Shiny.setInputValue('", ns("removeEvent"), "', this.id)")
                  ),
                  align = "left",
            ),
            column(1,"")
          )
        })
        formulationToUse <<- NULL
        return(constraintsUI)
      })
      
      output$listconstraints.ui <- renderUI({
        input$optimdef # Update constraints list ui when 'Import Problem Formulation' dialog is displayed
        COformulation.temp$idC # Update constraints list ui when a file is loaded
        req(choicesY(),input$nbcons>0, !isTRUE(input$switchinversion))
        # Constraints can't be imposed on objectives
        objectives <- c(input$chooseObjective1,input$chooseObjective2)
        indices_non_status <- c(Yinfos$int.ids,Yinfos$control.ids)
        indices_non_objective <- which(!DOE$ynamesmenu %in% objectives)
        indices <- intersect(indices_non_status,indices_non_objective)
        constraintIndexes <- setdiff(1:(input$nbcons + length(removedConstraintIndexes)), removedConstraintIndexes)
        constraintsUI <- lapply(1:input$nbcons, function(i){
          nameconsValue <- DOE$ynamesmenu[1]
          signconsValue <- '<'
          thconsValue <- 0

          if (is.null(formulationToUse) ) {
            isolate({
              if (!is.null(input[[paste0('namecons', constraintIndexes[i])]])) {
                nameconsValue <- input[[paste0('namecons', constraintIndexes[i])]]
                signconsValue <- input[[paste0('signcons', constraintIndexes[i])]]
                thconsValue <- input[[paste0('thcons', constraintIndexes[i])]]
              }
            })
          }
          else if (formulationToUse == "useKnownFormulation" && length(COformulation$idC) != 0) {
            # This means we already saved an optim definition so we display it in the ui
            nameconsValue <- DOE$ynamesmenu[COformulation$idC[i]]
            signconsValue <- tablesign[(as.numeric(COformulation$COsign[i]) + 3)/2]
            thconsValue <- COformulation$COt[i]
          }
          else if (formulationToUse == "useImportedFormulation" && length(COformulation.temp$idC) != 0) {
            # This means a file is loaded, so we update the ui
            nameconsValue <- DOE$ynamesmenu[COformulation.temp$idC[i]]
            signconsValue <- tablesign[(as.numeric(COformulation.temp$COsign[i]) + 3)/2]
            thconsValue <- COformulation.temp$COt[i]
          }

          fluidRow(
            column(1,""),
            column(4,
                   selectInput(
                     ns(paste0('namecons', constraintIndexes[i])),  
                     label = "", 
                     choices = DOE$ynamesmenu[indices], 
                     selected = nameconsValue
                   ), align = "center"
            ),
            column(2,
                   selectInput(
                     ns(paste0('signcons', constraintIndexes[i])),  
                     label = "", 
                     choices = c("<",">"), 
                     selected = signconsValue
                   ), align = "center"
            ),
            column(3,
                   numericInput(ns(paste0('thcons', constraintIndexes[i])), "", thconsValue), align = "center"
            ),
            column(1,
                  br(),
                  actionButton(
                    ns(paste0('remove', constraintIndexes[i])),
                    "",
                    icon("remove"),
                    class = "btn-danger",
                    onclick = paste0("Shiny.setInputValue('", ns("removeEvent"), "', this.id)")
                  ),
                  align = "left",
            ),
            column(1,"")
          )
        })
        formulationToUse <<- NULL
        return(constraintsUI)
      })

      observeEvent(input$removeEvent, {
        req(input$nbcons, input$nbcons >= nbcons.min)
        nbcons <- input$nbcons
        removedIndex <- substr(input$removeEvent, nchar(ns("remove")) + 1, nchar(input$removeEvent))
        removedConstraintIndexes <<- append(removedConstraintIndexes, removedIndex)
        updateNumericInput(session, "nbcons", value = nbcons - 1)
      })

      output$x0table <- DT::renderDataTable({
        req(initialXVal())
        
        Xinfos.df.names <- sapply(Xinfos$Xinfos, function(row) {
          return(row[['namevisu']])
        })
        X.init <- initialXVal()
        colnames(X.init) <- Xinfos.df.names
        rownames(X.init) <- paste('Initial Value', seq_len(nrow(X.init)))

        xx <- data.frame(lapply(X.init, as.character), stringsAsFactors = FALSE)
        xx[is.na(xx)] <- "NA"
        rownames(xx) <- rownames(X.init)
        colnames(xx) <- colnames(X.init)
        DT::datatable(
          xx, escape = FALSE,
          extensions = c('FixedColumns','Scroller'),
          options = list(
            dom = 't', ordering=F,
            scrollX = TRUE, scrollY = 200, scroller = TRUE, fixedColumns = TRUE
          ))
      })
      
      output$thresholds <- DT::renderDataTable({
        req(COformulation$thresholds)
        DT::datatable(
          COformulation$thresholds, 
          extensions = c('FixedColumns','Scroller','Buttons'),
          options = list(dom = 't',scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE)
        )
      })
      
      output$optimTypes <- DT::renderDataTable({
        req(COformulation$optimTypes, !isTRUE(input$switchinversion))
        DT::datatable(COformulation$optimTypes, options = list(dom = 't'))
      })
      
      output$downloadformulation <- downloadHandler(
        filename = if (typeOptim == "directOptim") 'COformulation.json' else 'COformulation.txt',
        content = function(con) {
          if(isTRUE(COformulation$isInversion)){
            nc <- length(COformulation$idC)
            df <- matrix(NA,3,nc)
            df[1,] <- rep("C", nc)
            df[2,] <- COformulation$COsign
            df[3,] <- COformulation$COt
            df <- as.data.frame(df)
            colnames(df) <- DOE$ynamesmenu[COformulation$idC]
          }
          else{
            no <- length(COformulation$idO)
            nc <- length(COformulation$idC)
            df <- matrix(NA,3,no+nc)
            df[1,] <- c(rep("O",no),rep("C",nc))
            df[2,1:no] <- COformulation$COobj
            if (nc > 0) {
              df[2,(no+1):(no+nc)] <- COformulation$COsign
              df[3,(no+1):(no+nc)] <- COformulation$COt
            }
            df <- as.data.frame(df)
            colnames(df) <- DOE$ynamesmenu[c(COformulation$idO,COformulation$idC)]
          }
          if (typeOptim == "directOptim") {
            toSave <- list(Xinfos = get.Xinfos.download(Xinfos$Xinfos, ",", "."), COformulation = df)
            cat(paste0(jsonlite::toJSON(toSave, na = "string", pretty = T), "\n"), file = con)
          }
          else {
            write.table(x = df, file = con, row.names = F, quote = FALSE, col.names = T, sep = ",", na = "")
          }
        }
      )
      
      define <- list(COformulation = COformulation, Xinfos = Xinfos, initialXVal = initialXVal)
      return(define)
    }
  )
}