#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module UQparamsChange

tableUQ <- matrix(c(
  "N", "U", "E", "G", "LN", "B", "TN","Est","KDE",
  "norm","unif","exp","gamma","lnorm","beta", "truncnorm","estimated","kde",
  "Normal","Uniform","Exponential","Gamma","LogNormal","Beta", "Truncated Normal","Estimated","KDE",
  "Mean=","Lower=","Lambda=","a=","Mean=","alpha=", "Mean=","","",
  "Std=","Upper=","","b=","Std=","beta=", "Std=","","",
  "", "", "", "", "", "", "a","","",
  "", "", "", "", "", "", "b","",""
), ncol = 7)

setAllEstimated <- function(Xinfos){
  UQparams <- lapply(1:length(Xinfos), function(i) {
    typeDistr <- "estimated"
    P1Distr <- NA
    P2Distr <- NA
    P3Distr <- NA
    P4Distr <- NA
    if (Xinfos[[i]]$type=='categorical'){
      nlevels <- Xinfos[[i]]$nlevels
      levels <- Xinfos[[i]]$levels
    }else{
      nlevels <- NA
      levels <- NA
    }
    weights <- NA
    return(list(typeDistr = typeDistr, P1Distr = P1Distr, P2Distr = P2Distr,
                P3Distr = P3Distr, P4Distr = P4Distr, levels = levels, weights = weights))
  })
  return(UQparams)
}

get.UQparams <- function(ind, line, DOE, decimal){
  if (DOE$Xinfos[[ind]]$type=='categorical'){
    typeDistr <- "Cat"
    P1Distr <- NA
    P2Distr <- NA
    P3Distr <- NA
    P4Distr <- NA
    nlevels <- DOE$Xinfos[[ind]]$nlevels
    levels <- line[seq(1,2*nlevels-1,by=2)]
    weights <- line[seq(2,2*nlevels,by=2)]
    ind.levels <- match(DOE$Xinfos[[ind]]$levels, levels)
    weights <- weights[ind.levels]
    levels <- levels[ind.levels]
    if (decimal == ','){weights <- gsub('[,]', '.', weights)}
    weights <- as.numeric(weights)
    weights <- weights/sum(weights)
  }else{
    if (decimal == ','){line <- gsub('[,]', '.', line)}
    typeDistr <- tableUQ[which(tableUQ[,1] == line[1]),2]
    P1Distr <- as.numeric(line[2])
    P2Distr <- as.numeric(line[3])
    if (typeDistr == 'truncnorm'){
      P3Distr <- as.numeric(line[4])
      P4Distr <- as.numeric(line[5])
    }
    P3Distr <- NA
    P4Distr <- NA
    levels <- NA
    weights <- NA
  }
  UQparams <- list(typeDistr = typeDistr, P1Distr = P1Distr, P2Distr = P2Distr, 
                   P3Distr = P3Distr, P4Distr = P4Distr, levels = levels, weights = weights)
  return(UQparams)
}

check.UQparams.file <- function(nX, Xinfos, lines, decimal){
  
  error.msg <- list()
  if (length(lines) != nX){
    error.msg$size <- 'Wrong number of lines (different from number of inputs).'
  }else{
    format.tests <- unlist(lapply(1:nX, function(i){
      line <- lines[[i]]
      if (Xinfos[[i]]$type == 'numeric'){
        ### numeric inputs
        distr.type <- line[1] %in% c('N', 'U', 'E', 'G', 'LN', 'B', 'TN')
        if (decimal == ','){line <- gsub('[,]', '.', line)}
        param.type <- suppressWarnings(!anyNA(lapply(line[-1], as.numeric)))
        nb.param <-  if (line[1] == 'E'){length(line) == 2}else{length(line) == 3}
        if (line[1] == 'E'){
          nb.param <- length(line) == 2
        }
        if (line[1] == 'TN'){
          nb.param <- length(line) == 5
        }
        if (line[1] %in% c('N', 'U', 'G', 'LN', 'B')){
          nb.param <- length(line) == 3
        }
        return(list(distr.type = distr.type, param.type = param.type, nb.param = nb.param))
      }else{
        ### categorical inputs
        line.len <- length(line) == 2*Xinfos[[i]]$nlevels
        list.out <- list(line.len = line.len)
        if (line.len){
          levels.index <- 1 + 2*(0:(Xinfos[[i]]$nlevels-1))
          line.levels <- line[levels.index]
          levels.valid <- all(sapply(line.levels, function(level){level %in% Xinfos[[i]]$levels}))
          weights <- line[-levels.index]
          if (decimal == ','){weights <- gsub('[,]', '.', weights)}
          weights.type <- suppressWarnings(!anyNA(lapply(weights, as.numeric)))
          list.out <- c(list.out, list(levels.valid = levels.valid, weights.type = weights.type))
          if (weights.type){
            weights.pos <- all(weights >= 0)
            list.out <- c(list.out, list(weights.pos = weights.pos))
          }
        }
        return(list.out)
      }
    }))
    tests.false <- names(format.tests[format.tests == FALSE])
    if ('distr.type' %in% tests.false){
      error.msg$distr.type <- 'Wrong distribution type.'
    }
    if ('param.type' %in% tests.false){
      error.msg$param.type <- 'Distribution parameters have to be numeric.'
    }
    if ('nb.param' %in% tests.false){
      error.msg$nb.param <- 'Wrong number of parameters for continuous inputs (1 for exponential, 2 for other distributions).'
    }
    if ('line.len' %in% tests.false){
      error.msg$line.len <- 'Wrong number of parameters for categorical inputs (level, weight).'
    }
    if ('levels.valid' %in% tests.false){
      error.msg$levels.valid <- 'Imported levels are not consistent with DOE.'
    }
    if ('weights.type' %in% tests.false){
      error.msg$weights.type <- 'Weigths have to be numeric.'
    }
    if ('weights.pos' %in% tests.false){
      error.msg$weights.pos <- 'Weigths have to be greater or equal 0.'
    }
    
  }
  return(list(valid = (length(error.msg) == 0), error.msg = error.msg))
}

get.UQparams.from.file <- function(DOE, datapath, separator, decimal) {

  lines <- readLines(datapath)
  lines.split <- lapply(lines, function(line){
    line <- as.character(unlist(strsplit(line, separator)))
    return(line)
  })
  lines.split <- lines.split[lapply(lines.split, length) > 0]
  file.check <- check.UQparams.file(DOE$nX, DOE$Xinfos, lines.split, decimal)
  if (file.check$valid){
    UQparams <- lapply(1:length(lines.split), function(ind, lines.split, DOE, decimal){
      return(get.UQparams(ind, lines.split[[ind]], DOE, decimal))
    }, lines.split = lines.split, DOE = DOE, decimal = decimal)
    error.msg <- NULL
  }else{
    UQparams <- NULL
    error.msg <- file.check$error.msg
  }
  return(list(UQparams = UQparams, error.msg = error.msg))
  
}

get.UQparams.from.input <- function(input, Xinfos) {

  UQparams <- lapply(1:length(Xinfos), function(i) {

    if (Xinfos[[i]]$type=='categorical'){
      typeDistr <- input[[paste0('typeDistr', i)]]
      if(!isTruthy(typeDistr)){typeDistr <- 'Categorical'}
      if (typeDistr == "Categorical"){
        typeDistr <- "Cat"
        P1Distr <- NA
        P2Distr <- NA
        P3Distr <- NA
        P4Distr <- NA
        nlevels <- Xinfos[[i]]$nlevels
        levels <- Xinfos[[i]]$levels
        weights <- lapply(1:Xinfos[[i]]$nlevels, function(level.index){
          input[[paste('levelproba', i, level.index, sep = '-')]]
        })
      }else{
        typeDistr <- "estimated"
        P1Distr <- NA
        P2Distr <- NA
        P3Distr <- NA
        P4Distr <- NA
        nlevels <- Xinfos[[i]]$nlevels
        levels <- Xinfos[[i]]$levels
        weights <- NA
      }
    }else{
      typeDistr <- input[[paste0('typeDistr', i)]]
      typeDistr <- tableUQ[which(tableUQ[,3] == typeDistr),2]
      if(!isTruthy(typeDistr)){typeDistr <- 'unif'}
      P1Distr <- input[[paste0('P1Distr', i)]]
      # if(!isTruthy(P1Distr)){P1Distr <- 0}
      P2Distr <- input[[paste0('P2Distr', i)]]
      # if(!isTruthy(P2Distr)){P2Distr <- 0}
      P3Distr <- input[[paste0('P3Distr', i)]]
      if(!isTruthy(P3Distr)){P3Distr <- Xinfos[[i]]$bounds[1]}
      P4Distr <- input[[paste0('P4Distr', i)]]
      if(!isTruthy(P4Distr)){P4Distr <- Xinfos[[i]]$bounds[2]}
      # special treatment for estimated distribution that has no parameter
      if (typeDistr == "estimated"){P1Distr <- NA}
      # special treatment for exponential distribution that has only 1 parameter
      if (typeDistr %in% c("exp","estimated")){P2Distr <- NA}
      if (typeDistr != 'truncnorm'){
        P3Distr <- NA
        P4Distr <- NA
      }
      levels <- NA
      weights <- NA
    }
    
    return(list(typeDistr = typeDistr, P1Distr = P1Distr, P2Distr = P2Distr, 
                P3Distr = P3Distr, P4Distr = P4Distr, levels = levels, weights = weights))
  })
  
  return(UQparams)
}

UQparams.check <- function(UQparams.temp){
  error.msg <- list()
  modalNormalize <- FALSE
  
  for(i in 1:length(UQparams.temp)){
    currentParam <- UQparams.temp[[i]]
    
    # Check if inputs are correctly filled (numeric and not empty) 
    
    valuesToCheck <- NULL

    if (currentParam$typeDistr == 'truncnorm'){
      valuesToCheck <- c(currentParam$P1Distr, 
                         currentParam$P2Distr, 
                         currentParam$P3Distr,
                         currentParam$P4Distr)
    }else if (currentParam$typeDistr %in% c('norm', 'lnorm', 'gamma', 'beta', 'unif')){
      valuesToCheck <- c(currentParam$P1Distr, 
                         currentParam$P2Distr)
    }else if (currentParam$typeDistr == 'exp'){
      valuesToCheck <- currentParam$P1Distr
    }else if (currentParam$typeDistr == "Cat"){
      valuesToCheck <- unlist(currentParam$weights)
    }
    
    if(!is.null(valuesToCheck) && (any(!is.numeric(valuesToCheck) || any(is.na(valuesToCheck))))){
      error.msg$valueType <- "Values must be non-empty and numeric"
      next
    }
    
    
    
    # Check consistency of the values
    
    if (currentParam$typeDistr %in% c('norm', 'lnorm')){
      
      if(currentParam$P2Distr <= 0){
        error.msg$sd <- "Standard deviation must be strictly positive"
      }
      
    }else if (currentParam$typeDistr == 'truncnorm'){
      
      if(currentParam$P2Distr <= 0){
        error.msg$sd <- "Standard deviation must be strictly positive"
      }
      
      if(currentParam$P3Distr >= currentParam$P4Distr){
        error.msg$bounds <- "The lower bound must be strictly less than the upper bound"
      }
      
      
    }else if (currentParam$typeDistr %in% c('gamma', 'beta')){
      
      if(currentParam$P1Distr <= 0 || currentParam$P2Distr <= 0){
        error.msg$sd <- "Alpha and Beta must be strictly positive"
      }
      
    }else if (currentParam$typeDistr == 'unif'){
      
      if(currentParam$P1Distr >= currentParam$P2Distr){
        error.msg$bounds <- "The lower bound must be strictly less than the upper bound"
      }
      
    }else if (currentParam$typeDistr == 'exp'){
      
      if(currentParam$P1Distr <= 0){
        error.msg$sd <- "Lambda must be strictly positive"
      }
      
    }else if (currentParam$typeDistr == "Cat"){
      
      weights <- unlist(currentParam$weights)
      
      if(any(weights <= 0)){
        error.msg$weights <- "Probabilities must be strictly positive"
      }
      
      if(round(sum(weights),2) != 1){
        modalNormalize <- TRUE
      }
    }
  }
  return(list(valid = (length(error.msg)==0), error.msg = error.msg, modalNormalize = modalNormalize))
}

UQparams.normalize <- function(UQparams.temp){
  
  for(i in 1:length(UQparams.temp)){
    
    if (UQparams.temp[[i]]$typeDistr == "Cat"){
      
      weights <- unlist(UQparams.temp[[i]]$weights)
      normWeights <- round(weights / sum(weights), 3)
        
      for(w in 1:length(UQparams.temp[[i]]$weights)){
          UQparams.temp[[i]]$weights[[w]] <- normWeights[w]
      }
    }
  }
  return(UQparams.temp)
}

get.UQparams.df <- function(UQparams, Xinfos, ncolumns){
  UQparams.df <- lapply(UQparams, function(row){
    if (row$typeDistr != "Cat"){
      row$typeDistr <- tableUQ[which(tableUQ[,2] == row$typeDistr),3]
    }
    if (!all(is.na(row$levels))){
      row$levels <- paste(row$levels, collapse = ' ')
      row$weights <- paste(row$weights, collapse = ' ')
    }
    row <- row[c('typeDistr', 'P1Distr', 'P2Distr', 'P3Distr', 'P4Distr', 'levels', 'weights')]
    row <- as.vector(unlist(row))
    return(row)
  })
  Xinfos.df.names <- sapply(Xinfos, function(row){row$name})
  UQparams.df <- data.frame(t(matrix(unlist(UQparams.df), nrow = ncolumns, byrow = T)))
  colnames(UQparams.df) <- Xinfos.df.names
  rownames(UQparams.df) <- c('Type', 'P1', 'P2', 'P3', 'P4', 'Levels', 'Weights')
  return(UQparams.df)
}

get.UQparams.download <- function(UQparams, Xinfos, separator, decimal){
  UQparams.df <- lapply(1:length(UQparams), function(i){
    row <- UQparams[[i]]
    if (row$typeDistr == 'Cat'){
      paste0(apply(cbind(Xinfos[[i]]$levels, row$weights), 1, function(cat){
        paste0(cat, collapse=separator)
      }), collapse = separator)
    }else{
      if (decimal == ','){
        row$P1Distr <- gsub('[.]', ',', row$P1Distr)
        row$P2Distr <- gsub('[.]', ',', row$P2Distr)
        row$P3Distr <- gsub('[.]', ',', row$P3Distr)
        row$P4Distr <- gsub('[.]', ',', row$P4Distr)
      }
      paste0(c(tableUQ[tableUQ[,2] == row$typeDistr,1], row$P1Distr, row$P2Distr, 
               row$P3Distr, row$P4Distr), collapse = separator)
    }
  })
  return(unlist(UQparams.df))
}

UQparams.row.ui <- function(i, ns, input, UQparams, Xinfos, error.msg, choices) {

  fluidRow(
    column(3, textInput(ns(paste0('name', i)), 'Input', Xinfos$namemenu)),

    # numeric variable (define UI for bounds)
    if (Xinfos$type == 'numeric'){
      
      if (UQparams$typeDistr %in% c('norm', 'lnorm')){
        label1 <- 'Mean'
        label2 <- 'Standard deviation'
      }
      if (UQparams$typeDistr == 'truncnorm'){
        label1 <- 'Mean'
        label2 <- 'Standard deviation'
        label3 <- 'Lower bound'
        label4 <- 'Upper bound'
      }
      if (UQparams$typeDistr %in% c('gamma', 'beta')){
        label1 <- 'Alpha'
        label2 <- 'Beta'
      }
      if (UQparams$typeDistr == 'unif'){
        label1 <- 'Lower bound'
        label2 <- 'Upper bound'
      }
      if (UQparams$typeDistr == 'exp'){
        label1 <- 'Lambda'
      }
      
      typeDistr.sel <- tableUQ[which(tableUQ[,2] == UQparams$typeDistr),3]
      fluidRow(
        column(2, selectInput(ns(paste0('typeDistr', i)), 'Type', choices = choices,
                              selected = typeDistr.sel)),
        if (!(UQparams$typeDistr %in% c('estimated','kde'))){
          column(2, numericInput(ns(paste0('P1Distr', i)), label1, UQparams$P1Distr))
        },
        if (!(UQparams$typeDistr %in% c('exp','estimated','kde'))){
          column(2, numericInput(ns(paste0('P2Distr', i)), label2, UQparams$P2Distr))
        },
        if (UQparams$typeDistr == 'truncnorm'){
          list(column(2, numericInput(ns(paste0('P3Distr', i)), label3, UQparams$P3Distr)),
          column(2, numericInput(ns(paste0('P4Distr', i)), label4, UQparams$P4Distr)))
        }

      )
      
    # categorical variable (define UI for nlevels, levels)
    }else{
      if (UQparams$typeDistr=="Cat"){
        typeDistr.sel <- "Categorical"
      }else{
        typeDistr.sel <- "Estimated"
      }
      fluidRow(
        column(2, selectInput(ns(paste0('typeDistr', i)), 'Type', choices = c('Categorical','Estimated'),
                              selected = typeDistr.sel)),
        if (UQparams$typeDistr != 'estimated'){
          column(3, strong('Levels'),
                 lapply(1:Xinfos$nlevels, function(level.index, levels){
                   textInput(ns(paste('level', i, level.index, sep = '-')), label = NULL, value = Xinfos$levels[[level.index]])
                 }, levels = UQparams$levels)
          )
        },
        if (UQparams$typeDistr != 'estimated'){
          column(3, strong('Probability'),
                 lapply(1:Xinfos$nlevels, function(level.index, levels){
                   # textInput for each category
                   numericInput(ns(paste('levelproba', i, level.index, sep = '-')), label = NULL,
                                value = UQparams$weights[[level.index]])
                 }, levels = UQparams$levels)
          )
        }
      )
    }
  )
}

UQparamsChange.ui <- function(id, label = HTML(paste('Change Uncertainty Definition', '(Marginals)', sep = '<br>'))) {
  ns <- NS(id)
  
  modalContent <- tagList(
    fluidRow(
      column(4, h4("Import Inputs"))
    ),
    fluidRow(
      column(2, radioButtons(ns("separator"), "Separator",
                             choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"))),
      column(2, radioButtons(ns("decimal"), "Decimal",
                             choices = list(". (point)" = ".", ", (comma)" = ","))
      ),
      column(7,
             fileInput(ns('file'), 'Select file', accept = c('.txt', '.dat','.csv')),
             tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });')),
             uiOutput(ns('warning.file'))
      )
    ),
    hr(),
    fluidRow(
      column(4, h4("Manually Change Inputs", div("(please save)", class = "small"))),
      column(2, actionButton(ns("reset"), label = "Reset", class = "btn-primary", icon = icon("sync"), width = '100%')),
      column(2, actionButton(ns("unifReset"), label = "Reset to Uniform", class = "btn-warning", icon = icon("sync"), width = '100%')),
      column(2, actionButton(ns("allEstimated"), label = "Set all to Estimated", class = "btn-warning", icon = icon("sync"), width = '100%'))
    ),
    uiOutput(ns("rangeInputs")),
    hr(),
    uiOutput(ns("footer"))
  )
  
  tagList(
    fluidRow(
      column(6,
             actionButton(ns("change"), label = label, class = "btn-primary")
      ),
      column(6,
             uiOutput(ns("update.dynui"))
      )
    ),
    bsModal(ns("modal"), "Change Inputs", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}",
                                        " .modal-lg{width: 70%}"))))
  )
}

UQparamsChange.ui.preview <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, h4("Current Inputs")),
      column(4, ""),
      column(4, downloadButton(ns("download"), label = "Export Inputs", class = "btn-primary"), icon = icon("download"))
    ),
    DT::dataTableOutput(ns('preview'))
  )
}

UQparamsChange.server <- function(input, output, session, initialUQparams, listCopulas, fitdistUQparams, DOE, persistence, verbose = FALSE) {
  
  ns <- session$ns
  
  # UQparams.temp temporally stores all modifications when the modal is open
  # UQparams is set to UQparams.temp when the user saves the modifications and returned
  UQparams <- reactiveValues(UQparams = NULL, estimated = NULL)
  UQparams.temp <- reactiveValues(UQparams.temp = NULL)
  # Warning message for wrong updates of variable types
  error.msg <- reactiveValues(msg = NULL)
  
  # we reinitialize UQparams and UQparams.temp when initialUQparams has changed
  observeEvent(initialUQparams$UQparams, {
    if (verbose) {
      print("initializing UQparams and UQparams.temp with")
      print(reactiveValuesToList(initialUQparams))
    }
    UQparams.temp$UQparams <- initialUQparams$UQparams
    UQparams$UQparams <- initialUQparams$UQparams
  })
  
  output$update.dynui <- renderUI({
    req(fitdistUQparams$UQparams)
    if (fitdistUQparams$selection.marginals=="confirmed"){
      actionButton(ns("update"), label = HTML(paste('Update', 'with fitted distributions', sep = '<br>')), class = "btn-warning")
    }else{
      NULL
    }
  })

  # we reinitialize UQparams and UQparams.temp when fitdistUQparams is updated
  observeEvent(input$update, {
    UQparams.temp$UQparams <- fitdistUQparams$UQparams
    UQparams$UQparams <- fitdistUQparams$UQparams
  })

  observeEvent(input$unifReset, {
    if (verbose) {
      print("reset UQparams.temp with uniform distributions")
    }
    UQparams.temp$UQparams <- initialUQparams$UQparams
  })
  
  observeEvent(input$allEstimated, {
    UQparams.temp$UQparams <- setAllEstimated(DOE$Xinfos)
  })

  # reinitialize UQparams.temp when the user actively reset the Inputs
  observeEvent(input$reset, {
    if (verbose) {
      print("reseting UQparams.temp with")
      print(reactiveValuesToList(UQparams))
    }
    UQparams.temp$UQparams <- UQparams$UQparams
    error.msg$type <- list() 
    error.msg$save <- NULL 
  })
  
  observeEvent(input$change, {
    toggleModal(session, "modal", toggle = "open")
  })
  
  observeEvent(input$save, {
    req(initialUQparams$UQparams)
    
    UQparams.temp$UQparams <- get.UQparams.from.input(input, DOE$Xinfos)
    UQparams.validation <- UQparams.check(UQparams.temp$UQparams)

    if (UQparams.validation$valid){
      if (verbose) {
        print("setting UQparams manually with")
        print(UQparams.temp$UQparams)
      }
      
      UQparams.temp$UQparams <- UQparams.normalize(UQparams.temp$UQparams)
      
      UQparams$UQparams <- UQparams.temp$UQparams
      error.msg$save <- NULL
      error.msg$file <- NULL
      error.msg$type <- list()
      toggleModal(session, "modal", toggle = "close")
      
      if(UQparams.validation$modalNormalize){
        showModal(modalDialog(HTML(
          "Categorical input: the weights' sum is not 1, they have been normalized"
        )
        , title = "Information")
        )
      }
      persistence$autoSavingCount <- persistence$autoSavingCount + 1
      persistence$autoSavingCaller <- "UQparamsChange-save"
      
    }else{
      error.msg$save <- UQparams.validation$error.msg
    }
            
  })
  
  observeEvent(input$close, {
    
    if (verbose) {
      print("cancel UQparams modifications")
      print(UQparams)
    }
    
    toggleModal(session, "modal", toggle = "close")
    
    error.msg$file <- NULL
    error.msg$save <- NULL
    
    UQparams.temp$UQparams <- UQparams$UQparams
    
  })
  
  observeEvent(input$file$datapath, { 
    req(DOE$nX, input$separator != input$decimal)
    newUQparams <- get.UQparams.from.file(DOE, input$file$datapath, input$separator, input$decimal)
    error.msg$msg <- newUQparams$error.msg
    newUQparams <- newUQparams$UQparams
    if (is.null(newUQparams)){
      newUQparams <- initialize.UQparams(DOE$Xinfos)
    }
    print("updating UQparams from file input")
    UQparams.temp$UQparams <- newUQparams
  })
  
  output$warning.file <- renderUI({
    req(error.msg$msg)
    list(h4(strong("Error !")), 
         HTML(paste(paste(error.msg$msg, collapse = '<br/>'), '<br/> <br/>')))
  })
  
  observeEvent(UQparams.temp$UQparams, {
    output$rangeInputs <- renderUI({
      req(UQparams.temp$UQparams)
      if (any(unlist(lapply(UQparams.temp$UQparams,function(l) l$typeDistr))=="kde")){
        # If a KDE has been fitted, allow to choose it
        choices <- tableUQ[,3]
      }else{
        choices <- setdiff(tableUQ[,3],"KDE")
      }
      lapply(1:DOE$nX, function(i){
        UQparams.row.ui(i, ns, input, UQparams.temp$UQparams[[i]], DOE$Xinfos[[i]], error.msg, choices)
      })
    })
  })
  
  trigger.typeDistr <- reactive({
    req(DOE$nX)
    lapply(1:isolate(DOE$nX), function(i){
      input[[paste0('typeDistr', i)]]
    })
  })

  observeEvent(trigger.typeDistr(), {
    req(DOE$nX, DOE$Xinfos)
    UQparams.temp$UQparams <- get.UQparams.from.input(input, DOE$Xinfos)
  })

  output$footer <- renderUI({
    list(
      column(12,
        if (!is.null(error.msg$save)){
          list(h4(strong("Error !")),
          HTML(paste(paste(error.msg$save, collapse = '<br/>'), '<br/> <br/>')))
        }else{NULL}
      ),
      fluidRow(
        column(3, actionButton(ns("save"), label = "Save and Close", class = "btn-warning",
                               width = '100%'), offset = 2),
        column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                               width = '100%'), offset = 2)
      )
    )
  })

  output$preview <- DT::renderDataTable({
    req(UQparams$UQparams)
    df <- get.UQparams.df(UQparams$UQparams, DOE$Xinfos, DOE$nX)
    dimd <- ncol(df)
    df <- cbind(df,data.frame(mode=rep("Marginals",nrow(df))))
    if (!is.null(listCopulas$listCopulas$inputs)){
      dd <- lapply(1:DOE$nX,function(i){
        row <- list()
        #id <- which(listCopulas$listCopulas$inputs==DOE$Xinfos[[i]]$name)
        #id <- which(DOE$Xinfos[[i]]$name==DOE$xnames[listCopulas$listCopulas$inputs])
        if (listCopulas$listCopulas$inputs[i]){
          group <- listCopulas$listCopulas$groups[i]
          if (group!="0"){
            if (group=="Estimated"){
              row$group <- "Estimated"
              row$typeCopula <- "Estimated"
            }else{
              row$group <- paste("Group",group)
              row$typeCopula <- listCopulas$listCopulas$typeCopulas[as.numeric(listCopulas$listCopulas$groups[i])]
            }
          }else{
            row <- list(group=NA,typeCopula=NA)
          }
        }else{
          row <- list(group=NA,typeCopula=NA)
        }
        row <- as.vector(unlist(row))
        return(row)
      })
    }else{
      dd <- rep(NA,DOE$nX*2)
    }
    df2 <- cbind(data.frame(t(matrix(unlist(dd), nrow = DOE$nX, byrow = T))),data.frame(mode=rep("Copulas",2)))
    rownames(df2) <- c("Group","Name")
    colnames(df) <- colnames(df2) <- c(DOE$xnamesvisu,"mode")
    df.final <- rbind(df,df2)
    DT::datatable(
      df.final, escape = FALSE,
      extensions = c('FixedColumns','Scroller','RowGroup'),
      options = list(
        rowGroup = list(dataSrc = dimd+1),
        dom = 't',
        columnDefs = list(list(targets = dimd+1, visible = FALSE)),
        pageLength = 2, scrollX = TRUE,scroller = TRUE,
        fixedColumns = TRUE
      ))
  })
  
  output$download <- downloadHandler(
    filename = 'UQparams.txt',
    content = function(con) {
      writeLines(text = get.UQparams.download(UQparams$UQparams, DOE$Xinfos, input$separator, input$decimal),
                 con = con)
    })
  
  observeEvent(UQparams$UQparams, {
    names.estimated <- unlist(lapply(1:length(DOE$Xinfos),function(i){
      if (UQparams$UQparams[[i]]$typeDistr=="estimated"){
        return(DOE$xnames[i])
      }
    }))
    UQparams$estimated <- (DOE$xnames%in%names.estimated)
  })

  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "uqParamsChange-uncertaintyDefinition") {
      logger$print(paste("Loaded study, updating", persistence$updatingStep))
      
      if (!is.null(persistence$loadedStudy$UQparams)) {
        UQparams$UQparams <- persistence$loadedStudy$UQparams$UQparams
        UQparams.temp$UQparams <- UQparams$UQparams
      }
      progressToNextStep(persistence)
    }        
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  return(UQparams)
}
