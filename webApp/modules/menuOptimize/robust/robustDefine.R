#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module robustDefine

getROformulationDesign <- function(DOE, ROformulation) {
  dimx <- DOE$nX
  type <- mu <- sig <- matrix(NA,1,dimx)
  if (length(ROformulation$idD) > 0) {
    type[ROformulation$idD] <- "Design"
  }
  if (length(ROformulation$idUD) > 0) {
    type[ROformulation$idUD] <- "Uncertain Design"
    mu[ROformulation$idUD] <- 0
    sig[ROformulation$idUD] <- as.matrix(ROformulation$ROsigUD)
  }
  if (length(ROformulation$idU) > 0) {
    type[ROformulation$idU] <- "Uncertain"
    mu[ROformulation$idU] <- as.matrix(ROformulation$ROmuU)
    sig[ROformulation$idU] <- as.matrix(ROformulation$ROsigU)
  }
  b <- rbind(type,mu,sig)
  rownames(b) <- c("Var. Type","Mean","Std")
  colnames(b) <- DOE$xnamesvisu
  b <- as.data.frame(b)
  b
}

getROformulationThresholds <- function(DOE, ROformulation) {
  dimy <- length(ROformulation$idC)
  if (dimy == 0) return(NULL)
  
  threshconstraints <- matrix(as.matrix(ROformulation$ROt),ncol=dimy)
  b <- matrix(NA,ncol = dimy)
  for (j in 1:dimy){
    idsign <- (as.numeric(ROformulation$ROsign[j]) + 3)/2
    b[j] <- paste0(tablesign[idsign],threshconstraints[j])
  }
  rownames(b) <- "Thresholds"
  colnames(b) <- DOE$ynamesvisu[ROformulation$idC]
  b
}

getROformulationOptimTypes <- function(DOE, ROformulation) {
  dimy <- length(ROformulation$idO)
  if (dimy == 0) return(NULL)
  
  m <- matrix(as.matrix(ROformulation$ROobj),ncol = dimy)
  b <- matrix(rep("Max",dimy),ncol = dimy)
  idmin <- which(m == -1)
  b[idmin] <- "Min"
  rownames(b) <- "Optim. Type"
  colnames(b) <- DOE$ynamesvisu[ROformulation$idO]
  b
}

getROformulation <- function(dinit, DOE) {  
  dimx <- DOE$nX
  id <- dinit[1,]
  idD <- which(id == "D")
  idUD <- which(id == "UD")
  idU <- which(id == "U")
  idC <- which(id == "C") - dimx
  idO <- which(id == "O") - dimx
  d <- data.frame(lapply(dinit[-1,],as.numeric))
  ROmuU <- d[1,idU]
  ROsigU <- d[2,idU]
  ROsigUD <- d[2,idUD]
  ROsign <- d[1,idC + dimx]
  ROt <- d[2,idC + dimx]
  ROobj <- d[1,idO + dimx]
  ROformulation <- list(
    idD = idD,idUD = idUD,idU = idU,idC = idC,idO = idO,
    ROmuU = ROmuU,ROsigU = ROsigU,ROsigUD = ROsigUD,ROsign = ROsign,ROt = ROt,ROobj = ROobj
  )
  ROformulation$design <- getROformulationDesign(DOE, ROformulation)
  ROformulation$thresholds <- getROformulationThresholds(DOE, ROformulation)
  ROformulation$optimTypes <- getROformulationOptimTypes(DOE, ROformulation)
  return(ROformulation)
}

check.RO.formulation <- function(dinit, nX, nY){
  
  if (ncol(dinit) > nX & ncol(dinit) <= nX + nY){
    valid <- list()
    dinit.inpt <- dinit[,1:nX]
    dinit.out <- dinit[,(nX + 1):(nX + nY),drop=FALSE]
    valid[[1]] <- !all(dinit.inpt[1,] %in% c('D', 'U', 'UD'))
    valid[[2]] <- !all(dinit.out[1,] %in% c('C','O'))
    valid[[3]] <- !all(dinit.out[2,] %in% c(-1,1,''))
    valid[[4]] <- !all(dinit.inpt[2:3,][,dinit.inpt[1,] == 'D'] == '')
    valid[[5]] <- anyNA(as.numeric(dinit.inpt[2:3,][,dinit.inpt[1,] == 'U']))
    valid[[6]] <- anyNA(as.numeric(dinit.inpt[2,][,dinit.inpt[1,] == 'UD'] != ''))
    valid[[7]] <- anyNA(as.numeric(dinit.inpt[3,][,dinit.inpt[1,] == 'UD']))
    valid[[8]] <- !(sum(dinit.out[1,] == 'O') == 1)
    valid[[9]] <- !all(dinit.out[3,dinit.out[1,] == 'O'] == '')
    valid[[10]] <- anyNA(as.numeric(dinit.out[3,dinit.out[1,] == 'C']))
    
    error.tag <- list()
    error.tag$type.inpt <- 'Possible types for inputs are D (design), U (uncertain) and UD (uncertain design).'
    error.tag$type.out <- 'Possible types for outputs are C (constraint) and O (objective).'
    error.tag$side.out <- 'Contraints: -1 (<) or 1 (>) / Objectives: -1 (min) or 1 (max) / Empty for other outputs.'
    error.tag$Dparam <- 'Parameters for D (design) inputs have to be empty.'
    error.tag$Uparam <- 'Parameters for U (uncertain) inputs have to be numeric.'
    error.tag$UD1param <- 'First Parameters for UD (uncertain design) inputs have to be numeric or empty.'
    error.tag$UD2Uparam <- 'Second Parameters for UD (uncertain design) inputs have to be numeric.'
    error.tag$obj <- 'None or multiple objectives.'
    error.tag$obj.param <- 'No threshold for objective.'
    error.tag$cst.param <- 'Thresholds for contraints have to be numeric.'
    error.msg = error.tag[unlist(valid)]
    valid <- !any(valid)
  }else{
    error.msg <- list('Number of columns should be between number of inputs + 1 and number inputs + outputs.')
    valid <- FALSE
  }
  
  return(list(valid = valid, error.msg = error.msg))
}

robustDefine.ui <- function(id) {
  ns <- NS(id)
  
  panel <- wellPanel(
    fileInput(ns('file'), 'Import Problem Formulation',accept = c('.txt','.dat','.csv')),
    tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });')),
    uiOutput(ns('error.file'))
  )   
  
  fluidRow(
    column(4,panel),
    column(
      8,
      DT::dataTableOutput(ns('design')),
      br(),
      DT::dataTableOutput(ns('thresholds')),
      DT::dataTableOutput(ns('optimTypes'))
    )
  )
}

robustDefine.server <- function(input, output, session, DOE) {
  
  ROformulation <- reactiveValues(
    idD = NULL,idUD = NULL,idU = NULL,idC = NULL,idO = NULL,
    ROmuU = NULL,ROsigU = NULL,ROsigUD = NULL,ROsign = NULL,ROt = NULL,ROobj = NULL,
    design = NULL,thresholds = NULL,optimTypes = NULL
  )
  
  error.msg <- reactiveValues(file = NULL)
  
  observeEvent(input$file$datapath, {
    req(DOE$nX)
    dinit <- read.csv(input$file$datapath, header = F, colClasses = "character")
    validation.RO <- check.RO.formulation(dinit, DOE$nX, DOE$nY)
    if (validation.RO$valid){
      newROformulation <- getROformulation(dinit, DOE)
      ROformulation$idD <- newROformulation$idD
      ROformulation$idUD <- newROformulation$idUD
      ROformulation$idU <- newROformulation$idU
      ROformulation$idC <- newROformulation$idC
      ROformulation$idO <- newROformulation$idO
      ROformulation$ROmuU <- newROformulation$ROmuU
      ROformulation$ROsigU <- newROformulation$ROsigU
      ROformulation$ROsigUD <- newROformulation$ROsigUD
      ROformulation$ROsign <- newROformulation$ROsign
      ROformulation$ROt <- newROformulation$ROt
      ROformulation$ROobj <- newROformulation$ROobj
      ROformulation$design <- newROformulation$design
      ROformulation$thresholds <- newROformulation$thresholds
      ROformulation$optimTypes <- newROformulation$optimTypes
      error.msg$file <- NULL
    }else{
      error.msg$file <- validation.RO$error.msg
    }
  })
  
  output$error.file <- renderUI({
    req(error.msg$file)
    list(h4(strong("Error !")), 
         HTML(paste(paste(error.msg$file, collapse = '<br/>'), '<br/> <br/>')))
  })
  
  output$design <- DT::renderDataTable({
    req(ROformulation$design)
    DT::datatable(
      ROformulation$design, 
      extensions = c('FixedColumns','Scroller','Buttons'),
      options = list(dom = 't',scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE)
    )
  })
  
  output$thresholds <- DT::renderDataTable({
    req(ROformulation$thresholds)
    DT::datatable(
      ROformulation$thresholds, 
      extensions = c('FixedColumns','Scroller','Buttons'),
      options = list(dom = 't',scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE)
    )
  })
  
  output$optimTypes <- DT::renderDataTable({
    req(ROformulation$optimTypes)
    DT::datatable(
      ROformulation$optimTypes, 
      extensions = c('FixedColumns','Scroller','Buttons'),
      options = list(dom = 't',scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE)
    )
  })
  
  return(ROformulation)
}