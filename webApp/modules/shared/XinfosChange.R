#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module XinfosChange

get.Xinfos <- function(line, decimal, header){
  
  if (header){
    namevisu <- namemenu <- line[1]
    name <- make.names(namevisu)
    line <- line[-1]
  }else{name <- namevisu <- namemenu <- NA}
  is.cat = (line[1] == 'categorical')
  is.cst = (line[1] == 'constant')
  if (is.cst){
    type <- 'constant'
    bounds <- c(NA, NA)
    levels <- NA
    nlevels <- NA
  }
  if (is.cat){
    type <- 'categorical'
    bounds <- c(NA, NA)
    levels <- line[-1]
    nlevels <- length(levels)
  }
  if (!is.cat & !is.cst){
    type <- 'numeric'
    if (line[1] == 'numeric'){line <- line[-1]}
    if (decimal == ','){
      line <- gsub('[,]', '.', line)
    }
    bounds <- as.numeric(line)
    levels <- NA
    nlevels <- NA
  }
  Xinfos <- list(name = name, namevisu = namevisu, namemenu = namemenu, type = type, bounds = bounds, nlevels = nlevels, levels = levels)
  return(Xinfos)
  
}

check.Xinfos.file <- function(nX, lines, Xinfos.temp, separator, decimal, data, edit.disable, header){

  error.msg <- list()
  if (header){
    lines <- lapply(lines, function(line){line[-1]})
  }
  if (length(lines) != nX){
    error.msg$size <- 'Wrong number of lines (different from number of inputs).'
  }else{
    file.type <- sapply(lines, function(x){if(x[1] %in% c('constant', 'categorical')){x[1]}else{'numeric'}})
    if (edit.disable){
      Xinfos.type <- sapply(Xinfos.temp, function(Xinfos){if(Xinfos$type %in% c('constant', 'categorical')){Xinfos$type}else{'numeric'}})
      valid.type <- all(file.type == Xinfos.type)
      if (!valid.type){
        error.msg$type <- 'Wrong variable types.'
      }
    }else{
      valid.typecst <- !(is.null(data) & 'constant' %in% file.type)
      if (!valid.typecst){
        error.msg$typecst <- 'Types have to be numeric or categorical to build a DOE.'
      }
    }
  }
  if (separator == decimal){
    error.msg$sep <- 'Separator and decimal has to be different.'
  }else{
    valid.format <- all(sapply(lines, function(line){
      is.cst <- (line[1] == 'constant')
      is.cat <- (line[1] == 'categorical')
      if (line[1] == 'numeric'){line <- line[-1]}
      is.num <- (length(line) == 2 & suppressWarnings(!anyNA(lapply(line, as.numeric))))
      return(is.cat | is.num | is.cst)
    }))
    if(is.na(valid.format)){valid.format <- FALSE}
    if (!valid.format){
      error.msg$format <- 'Wrong format: please check documentation.'
    }
  }
  return(list(valid = (length(error.msg) == 0), error.msg = error.msg))
}

get.Xinfos.from.file <- function(nX, lines.split, Xinfos.temp, header, separator, decimal, data, edit.disable) {

  file.check <- check.Xinfos.file(nX, lines.split, Xinfos.temp, separator, decimal, data, edit.disable, header)
  if (file.check$valid){
    Xinfos <- lapply(lines.split, get.Xinfos, decimal = decimal, header = header)
    if (edit.disable){
      Xinfos.temp <- lapply(1:length(Xinfos.temp), function(i, Xinfos.temp, Xinfos){
        if (Xinfos[[i]]$type == Xinfos.temp[[i]]$type & Xinfos[[i]]$type != 'constant'){
          Xinfos.temp[[i]]$bounds <- Xinfos[[i]]$bounds
          Xinfos.temp[[i]]$levels <- Xinfos[[i]]$levels
        }
        return(Xinfos.temp[[i]])
      }, Xinfos.temp = Xinfos.temp, Xinfos = Xinfos)
      Xinfos <- Xinfos.temp
    }else{
      Xinfos <- lapply(1:nX, function(ind, Xinfos, Xinfos.temp){
        if (!header){
          Xinfos[[ind]]$name <- Xinfos.temp[[ind]]$name
          Xinfos[[ind]]$namevisu <- Xinfos.temp[[ind]]$namevisu
          Xinfos[[ind]]$namemenu <- Xinfos.temp[[ind]]$namemenu
        }
        if (Xinfos[[ind]]$type == 'constant'){
          Xinfos[[ind]]$nlevels <- Xinfos.temp[[ind]]$nlevels
          Xinfos[[ind]]$levels <- Xinfos.temp[[ind]]$levels
        }
        return(Xinfos[[ind]])
      }, Xinfos = Xinfos, Xinfos.temp = Xinfos.temp)
    }
  }else{Xinfos = NULL}

  return(list(Xinfos = Xinfos, error.msg = file.check$error.msg))
}

get.Xinfos.from.input <- function(input, initialXinfos, Xinfos.temp, edit.disable = FALSE) {

  nX <- min(initialXinfos$nX, length(Xinfos.temp))
  Xinfos <- lapply(1:nX, function(i) {
    if (edit.disable){ 
      if (initialXinfos$Xinfos[[i]]$type == 'categorical'){
        levels.bool <- unlist(lapply(1:initialXinfos$Xinfos[[i]]$nlevels, function(level.index){
          input[[paste0('level.sel', i, level.index)]]
        }))
        levels <- initialXinfos$Xinfos[[i]]$levels[levels.bool]
        Xinfos.temp[[i]]$levels <- levels
        Xinfos.temp[[i]]$nlevels <- length(levels)
      }
      if (initialXinfos$Xinfos[[i]]$type == 'numeric'){
        Xinfos.temp[[i]]$bounds <- c(input[[paste0('defLB',i)]], input[[paste0('defUB',i)]])
      }
      return(Xinfos.temp[[i]])
    }else{
      namevisu <- namemenu <- input[[paste0('name', i)]]
      name <- make.names(namevisu)
      if (!isTruthy(name)){name <- namevisu <- namemenu <- paste0('X', i)}
      type <- input[[paste0('type', i)]]
      if(!isTruthy(type)){type <- 'numeric'}
      
      if (type == 'constant'){
        bounds <- c(NA, NA)
        nlevels <- Xinfos.temp[[i]]$nlevels
        levels <- Xinfos.temp[[i]]$levels
      }
      if (type == 'numeric'){
        bounds <- c(input[[paste0('defLB',i)]], input[[paste0('defUB',i)]])
        if (!isTruthy(bounds[1])){bounds[1] <- 0}
        if (!isTruthy(bounds[2])){bounds[2] <- 1}
        nlevels <- NA
        levels <- NA
      }
      if (type == 'categorical'){
        bounds <- c(NA,NA)
        nlevels <- Xinfos.temp[[i]]$nlevels
        nlevels <- if(isTruthy(nlevels >= 2)){nlevels}else{2}
        if (isTruthy(nlevels > 0)){
          levels <- lapply(1:nlevels, function(level.index){
            level <- input[[paste0('level', i, level.index)]]
          })
        }else{
          nlevels <- NA
          levels <- NA
        }
      }
      return(list(name = name, namevisu = namevisu, namemenu = namemenu, type = type, bounds = bounds, nlevels = nlevels, levels = levels))
    }
  })

  return(Xinfos)
}

update.type <- function(input, Xinfos, data, error.msg, session){
  
  ind.max <- min(length(Xinfos), ncol(data))
  newXinfos <- lapply(1:ind.max, function(i, Xinfos, data, error.msg, session){
    var <- Xinfos[[i]]
    warning.type <- unlist(error.msg[i])
    var$namevisu <- var$namemenu <- input[[paste0('name', i)]]
    var$name <- make.names(var$namevisu)
    if (var$type == 'numeric'){var$bounds <- c(input[[paste0('defLB',i)]], input[[paste0('defUB',i)]])}
    if (isTruthy(input[[paste0('type', i)]])){
      levels <- unique(data[,i])
      nlevels <- length(levels)
      if (input[[paste0('type', i)]] == 'constant' & var$type != 'constant'){
        if (nlevels == 1){
          var$type = 'constant'
          var$bounds = c(NA, NA)
        }else{
          warning.type <- "Non-unique input values: cannot be defined as constant."
          updateSelectInput(session, paste0('type', i), "Type", choices = c('numeric', 'categorical', 'constant'),
                            selected = var$type)
        }
      }
      if (input[[paste0('type', i)]] == 'numeric' & var$type != 'numeric'){
          if (is.numeric(data[,i]) & nlevels > 1){
            var$type = 'numeric'
            var$bounds = c(min(data[,i]), max(data[,i]))
            var$nlevels = NA
            var$levels = NA
          }else{
            warning.type <- "Non-numeric or constant input values: cannot be defined as numeric."
            updateSelectInput(session, paste0('type', i), "Type", choices = c('numeric', 'categorical', 'constant'),
                              selected = var$type)
          }
      }
      if (input[[paste0('type', i)]] == 'categorical' & var$type != 'categorical'){
        if (nlevels < nrow(data) & nlevels > 1){
          var$type = 'categorical'
          var$bounds = c(NA, NA)
          var$levels = levels
          var$nlevels = nlevels
        }else{
          warning.type <- "All input values are distinct or identical: cannot be defined as categorical."
          updateSelectInput(session, paste0('type', i), "Type", choices = c('numeric', 'categorical', 'constant'),
                            selected = var$type)
        }
      }
    }
    return(list(Xinfos = var, warning.type = warning.type))
  }, Xinfos = Xinfos, data = data, error.msg = error.msg, session = session)

  error.msg <- lapply(newXinfos, function(x){x$warning.type})
  Xinfos <- lapply(newXinfos, function(x){x$Xinfos})
  
  return(list(Xinfos = Xinfos, error.msg = error.msg))
}

Xinfos.check <- function(Xinfos.temp, data, edit.disable){
  
  error.msg <- list()
  out.bounds <- NULL
 
  # check Xinfos is not null
  null.Xinfos <- is.null(Xinfos.temp)
  if (null.Xinfos){
    error.msg$null <- 'Empty inputs.'
  }else{
    # check numeric have non null bounds
    valid.bounds.na <- all(unlist(lapply(Xinfos.temp, function(Xinfos){
      if (Xinfos$type == 'numeric'){
        !any(sapply(Xinfos$bounds, is.na))
      }else{TRUE}
    })))
    if (!valid.bounds.na){
      error.msg$bounds.na <- 'Missing bounds.'
    }else{
      # check lower bounds are lower than upper bounds
      valid.bounds.order <- all(unlist(lapply(Xinfos.temp, function(Xinfos){
          if (Xinfos$type == 'numeric'){
            Xinfos$bounds[1] < Xinfos$bounds[2]
          }else{TRUE}
        })))
      if (!valid.bounds.order){
        error.msg$bounds.order <- 'Some lower bounds are greater than upper bounds.'
      }
    }
    if (edit.disable){
      # In refine sampling / optimization : check at least one level is selected per categorical
      valid.levels.select <- all(unlist(lapply(Xinfos.temp, function(Xinfos){
          if (Xinfos$type == 'categorical'){
            Xinfos$nlevels > 0
          }else{TRUE}
        })))
      if (!valid.levels.select){
        error.msg$levels.select <- 'Select at least one level per categorical variable.'
      }
    }else{
      if (is.null(data)){
        # In generate DOE : check at least 2 numeric
        valid.numerical.nb <- (get.nb.num(Xinfos.temp) > 0)
        if (!valid.numerical.nb){
          error.msg$numerical.nb <- 'Select at least 1 numerical variables.'
        }
        # In generate DOE : check at least 2 inputs
        valid.dim <- (length(Xinfos.temp) >= 2)
        if (!valid.dim){
          error.msg$dim <- 'Select at least 2 variables.'
        }
        # In generate DOE : check at least two levels per categorical
        valid.nlevels <- all(unlist(lapply(Xinfos.temp, function(Xinfos){
            if (Xinfos$type == 'categorical'){
              Xinfos$nlevels >= 2
            }else{TRUE}
          })))
        if (!valid.nlevels){
          error.msg$valid.nlevels <- 'Select at least two levels per categorical variable.'
        }
      }else{
        # In import DOE : check levels are consistent with imported DOE
        valid.levels <- all(unlist(lapply(1:length(Xinfos.temp), function(i, Xinfos, data){
          if (Xinfos[[i]]$type == 'categorical'){
            unique(data[,i]) %in% Xinfos[[i]]$levels
          }else{TRUE}
        }, Xinfos = Xinfos.temp, data = data)))
        if (!valid.levels){
          error.msg$valid.levels <- 'Selected levels are not consistent with imported DOE.'
        }
        # In import DOE : check constant inputs have a unique value
        valid.constant <- all(unlist(lapply(1:length(Xinfos.temp), function(i, Xinfos, data){
          if (Xinfos[[i]]$type == 'constant'){
            length(unique(data[,i])) == 1 
          }else{TRUE}
        }, Xinfos = Xinfos.temp, data = data)))
        if (!valid.constant){
          error.msg$valid.constant <- 'Some variables typed as "constant" take multiple values in imported DOE.'
        }
        # In import DOE: check numeric
        valid.numeric <- all(unlist(lapply(1:length(Xinfos.temp), function(i, Xinfos, data){
          if (Xinfos[[i]]$type == 'numeric'){
            is.numeric((data[,i]))
          }else{TRUE}
        }, Xinfos = Xinfos.temp, data = data)))
        if (!valid.numeric){
          error.msg$valid.numeric <- 'Some variables typed as "numeric" take non-numeric values in imported DOE.'
        }else{
          # In import DOE : check if any point is out of bounds
          is.out.bounds <- any(unlist(sapply(1:length(Xinfos.temp), function(ind, Xinfos, data){
              var.infos <- Xinfos[[ind]]
              if (var.infos$type == 'numeric'){
                ((var.infos$bounds[[1]] > min(data[ind],na.rm=TRUE))|(var.infos$bounds[[2]] < max(data[ind],na.rm=TRUE)))
              }else{FALSE}
            }, Xinfos = Xinfos.temp, data = data)))
          if (is.out.bounds){
            out.bounds <- 'Some DOE points are out of the selected bounds.'
          }
        }
      }
    }
  }
  return(list(valid = (length(error.msg)==0), error.msg = error.msg, out.bounds = out.bounds))
}

get.initialXinfos <- function(ind){
  name <- namevisu <- namemenu <-  paste0("X", ind)
  type <- 'numeric'
  bounds <- c(0,1)
  nlevels <- NA
  levels <- NA
  return(list(name = name, namevisu = namevisu, namemenu = namemenu, type = type, bounds = bounds, nlevels = nlevels, levels = levels))
}

get.bounds <- function(Xinfos){

  bounds <- sapply(Xinfos, function(var.info){
    if (var.info$type == 'numeric'){
      var.info$bound
    }
  })
  if (!is.null(unlist(bounds))){
    bounds <- matrix(unlist(bounds[sapply(bounds, Negate(is.null))]), nrow = 2)
  }else{
    bounds <- NULL
  }
  return(bounds)
  
}

get.levels <- function(Xinfos, var.cat.list){
  unlist(sapply(Xinfos, function(var.info, var.cat){
    if (var.info$name %in% var.cat){
      var.info$levels
    }
  }, var.cat = var.cat.list))
}

get.nb.num <- function(Xinfos){
  
  sum(sapply(Xinfos, function(var.info){
    if (var.info$type == 'numeric'){1}else{0}
  }))
  
}

get.Xinfos.df <- function(Xinfos, ncolumns){
  Xinfos.df <- lapply(Xinfos, function(row){
    if (!all(is.na(row$levels))){
      row$levels <- paste(row$levels, collapse = ' ')
    }
    row <- as.vector(unlist(row))
    return(row[-c(1,2,3)])
  })
  Xinfos.df.names <- sapply(Xinfos, function(row){
    return(row[['namevisu']])
  })
  Xinfos.df <- data.frame(t(matrix(unlist(Xinfos.df), nrow = ncolumns, byrow = T)))
  colnames(Xinfos.df) <- Xinfos.df.names
  rownames(Xinfos.df) <- c('Type', 'Lower Bound', 'Upper Bound', 'Number of levels', 'Levels')
  return(Xinfos.df)
}

get.Xinfos.download <- function(Xinfos, separator, decimal){
  Xinfos.df <- lapply(Xinfos, function(row){
    if (row$type == 'numeric'){
      if (decimal == ','){row$bounds <- gsub('[.]', ',', row$bounds)}
      line <- paste0(unlist(c(row$namemenu, row$type, row$bounds)), collapse = separator)
    }
    if (row$type == 'categorical'){
      line <- paste0(unlist(c(row$namemenu, row$type, row$levels)), collapse = separator)
    }
    if (row$type == 'constant'){
      line <- paste0(unlist(c(row$namemenu, row$type)), collapse = separator)
    }
    return(line)
  })
  return(unlist(Xinfos.df))
}

Xinfos.row.ui <- function(i, ns, input, Xinfos, initialXinfos, data, error.msg, edit.disable) {
  
  name.label <- 'Input Name'
  type.label <- 'Type'
  type.choices <- c('numeric', 'categorical')
  if (!is.null(data)){type.choices <- c(type.choices, 'constant')}
  if (edit.disable){
    name.label <- paste(name.label, '(Not editable)')
    type.label <- paste(type.label, '(Not editable)')
    type.choices <- Xinfos$type
  }
  
  fluidRow(
    column(3, textInput(ns(paste0('name', i)), name.label, Xinfos$namemenu)),
    column(3, selectInput(ns(paste0('type', i)), type.label, choices = type.choices,
                          selected = Xinfos$type),
           if (!is.null(unlist(error.msg$type[i]))){
             HTML(paste(unlist(error.msg$type[i]),'<br/> <br/>'))
           }
    ),
    
    # numeric variable (define UI for bounds)
    if (Xinfos$type == 'numeric'){
      fluidRow(
        column(3, numericInput(ns(paste0('defLB', i)), "Lower Bound", Xinfos$bounds[1])),
        column(3, numericInput(ns(paste0('defUB', i)), "Upper Bound", Xinfos$bounds[2]))
      )
    }else{
      # categorical variable (define UI for nlevels, levels)
      if (Xinfos$type == 'categorical'){
        # default nb of categories: 2
        nlevels.temp <- 2
        if(isTruthy(Xinfos$nlevels >= nlevels.temp)){
          nlevels.temp <- Xinfos$nlevels
        }
        nlevels.label = "Number of levels"
        if (!is.null(data)){
          nlevels.label <- paste(nlevels.label, '(Not editable)')
        }
        
        fluidRow(
          column(3, strong(nlevels.label),
                 fluidRow(
                   column(8, numericInput(ns(paste0('nlevels', i)), label = NULL, value = nlevels.temp, min = 2)),
                   if (is.null(data)){
                     column(3, actionButton(ns(paste0('go.nlevels', i)), 'Update', class = "btn-primary"),
                            style = 'padding:0px;')
                   }
                 )
          ),
          column(3, strong('Levels'),
                 if (edit.disable){
                   lapply(1:initialXinfos$nlevels, function(level.index, levels, levels.initial){
                     checkboxInput(ns(paste0('level.sel', i, level.index)), 
                                   label = levels.initial[level.index], 
                                   value = levels.initial[level.index] %in% levels)
                   }, levels = Xinfos$levels, 
                   levels.initial = initialXinfos$levels)
                 }else{
                   lapply(1:nlevels.temp, function(level.index, levels){
                     # default level value is index (check if already exists)
                     level <- level.index
                     if (level %in% levels){level = level + max(as.numeric(unlist(levels)))}
                     if (level.index <= length(unlist(levels))){
                       if (isTruthy(levels[[level.index]])){
                         level <- levels[[level.index]]
                       }
                     }
                     # textInput for each category
                     textInput(ns(paste0('level', i, level.index)), label = NULL, value = level)
                   }, levels = Xinfos$levels)
                 }
          )
        )
      }
    }
  )
}

XinfosChange.ui <- function(id, label = "Change Inputs", width = NULL) {
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
      column(4, actionButton(ns("reset"), label = "Reset", class = "btn-primary", icon = icon("sync")))
    ),
    uiOutput(ns("rangeInputs")),
    hr(),
    uiOutput(ns("footer"))
  )
  
  tagList(
    actionButton(ns("change"), label = label, class = "btn-primary", width = width),
    bsModal(ns("modal"), "Change Inputs", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}",
                                        " .modal-lg{width: 70%}"))))
  )
}

jsVarNameValidator <- readLines("modules/shared/jsVarNameValidator.txt")[1]

XinfosChange.ui.preview <- function(id, simple = FALSE) {
  ns <- NS(id)
  if (simple){
    tagList(
      DT::dataTableOutput(ns('preview'))
    )
  }else{
    tagList(
      fluidRow(
        column(4, h4("Current Inputs")),
        column(4, ""),
        column(4, downloadButton(ns("download"), label = "Export Inputs", class = "btn-primary"), icon = icon("download"))
      ),
      DT::dataTableOutput(ns('preview')),
      br(),
      textOutput(outputId = ns("columnNamesWarning")),
      tags$head(tags$style(paste0("#", ns("columnNamesWarning"), "{color: orange;font-style: italic;})")))
    )
  }
}

XinfosChange.server <- function(input, output, session, initialXinfos, data = NULL, edit.disable = FALSE, nvalues = NULL,
                                verbose = FALSE) {
  
  ns <- session$ns
  
  # Xinfos.temp temporally stores all modifications when the modal is open
  # Xinfos is set to Xinfos.temp when the user saves the modifications and returned
  Xinfos <- reactiveValues(Xinfos = NULL)
  Xinfos.temp <- reactiveValues(Xinfos.temp = NULL)
  # Warning message for wrong updates of variable types
  error.msg <- reactiveValues(type = list(), save = NULL, file = NULL, file.names = NULL)
  resetInitial <- reactiveVal(FALSE)
  
  # we reinitialize Xinfos and Xinfos.temp when initialXinfos has changed
  observeEvent(list(initialXinfos$nX, initialXinfos$Xinfos), {
    if (verbose) {
      print("initializing Xinfos and Xinfos.temp with")
      print(reactiveValuesToList(initialXinfos))
    }
    Xinfos$Xinfos <- initialXinfos$Xinfos
    Xinfos.temp$Xinfos <- initialXinfos$Xinfos
    resetInitial(TRUE)
  })
  
  # reinitialize Xinfos.temp when the user actively reset the Inputs
  observeEvent(input$reset, {
    if (verbose) {
      print("reseting Xinfos.temp with")
      print(reactiveValuesToList(Xinfos))
    }
    Xinfos.temp$Xinfos <- Xinfos$Xinfos
    error.msg$type <- list() 
    error.msg$save <- NULL 
  })
  
  # Xinfos.temp is updated whenever type is manually changed
  manual.trigger.type <- reactive({
    req(initialXinfos$nX)
    lapply(1:initialXinfos$nX, function(i){
      input[[paste0('type', i)]]
    })
  })
  observeEvent(manual.trigger.type(), {
    req(Xinfos.temp$Xinfos, !edit.disable)
    update.bool <- !resetInitial() & any(unlist(lapply(1:length(Xinfos.temp$Xinfos), function(i){
          Xinfos.temp$Xinfos[[i]]$type != input[[paste0('type', i)]]
        })))
    resetInitial(FALSE)
    if (update.bool){
      if (is.null(data)){
        newXinfos <- get.Xinfos.from.input(input, initialXinfos, Xinfos.temp$Xinfos)
      }else{
        newXinfos <- update.type(input, Xinfos.temp$Xinfos, data$X, error.msg$type, session)
        error.msg$type <- newXinfos$error.msg
        newXinfos <- newXinfos$Xinfos
      }
      if (verbose) {
        print("update Xinfos.temp with new type (initialized with DOE if exists)")
        print(newXinfos)
      }
      Xinfos.temp$Xinfos <- newXinfos
    }
  })
  observe({
    req(Xinfos.temp$Xinfos)
    lapply(1:length(Xinfos.temp$Xinfos), function(i){
      observeEvent(input[[paste0('go.nlevels', i)]], {
        Xinfos.temp$Xinfos[[i]]$nlevels <- input[[paste0('nlevels', i)]]
      })
    })
  })
  
  observeEvent(input$change, {
    toggleModal(session, "modal", toggle = "open")
  })
  
  observeEvent(input$save, {
    req(initialXinfos$nX)

    Xinfos.temp$Xinfos <- get.Xinfos.from.input(input, initialXinfos, Xinfos.temp$Xinfos, edit.disable)
    # reorder DOE with Xinfos if needed before validation
    if (!is.null(data)){
      xnames <- sapply(Xinfos.temp$Xinfos, function(Xinfo){Xinfo$name})
      if (!all(xnames == data$xnames) & length(intersect(xnames, data$xnames)) == data$nX){
        data$X <- data$XY[xnames]
      }
    }
    Xinfos.validation <- Xinfos.check(Xinfos.temp$Xinfos, data$X, edit.disable)
    if (Xinfos.validation$valid){
      if (verbose) {
        print("setting Xinfos manually with")
        print(Xinfos.temp$Xinfos)
      }
      Xinfos$Xinfos <- Xinfos.temp$Xinfos
      error.msg$save <- NULL
      error.msg$file <- NULL
      error.msg$file.names <- NULL
      error.msg$type <- list()
      toggleModal(session, "modal", toggle = "close")
      if (!is.null(Xinfos.validation$out.bounds)){
        showModal(modalDialog(HTML(Xinfos.validation$out.bounds), title = "Warning !"))
      }
    }else{
      error.msg$save <- Xinfos.validation$error.msg
    }
            
  })
  
  observeEvent(input$close, {
    
    if (verbose) {
      print("cancel Xinfos modifications")
      print(Xinfos)
    }
    
    toggleModal(session, "modal", toggle = "close")
    
    error.msg$file <- NULL
    error.msg$file.names <- NULL
    error.msg$save <- NULL
    
    Xinfos.temp$Xinfos <- Xinfos$Xinfos
    
  })
  
  observeEvent(input$file$datapath, {
    
    error.msg$file <- NULL
    error.msg$file.names <- NULL
    lines <- readLines(input$file$datapath)
    lines.split <- lapply(lines, function(line){
      line <- as.character(unlist(strsplit(line, input$separator)))
      if (!line[1] %in% c('constant', 'categorical') & input$decimal == ','){line <- gsub('[,]', '.', line)}
      return(line)
    })
    lines.split <- lines.split[lapply(lines.split, length) > 0]
    header <- all(sapply(lines.split, function(line){line[2] %in% c('constant', 'categorical', 'numeric')}))
    if (header){
      namesFile <- sapply(lines.split, function(line){line[1]})
      namesmenuInit <- sapply(Xinfos.temp$Xinfos, function(Xinfo){Xinfo$namemenu})
      xynamesmenu <- c(data$xnamesmenu, data$ynamesmenu)
      if (all(namesFile %in% xynamesmenu) & !all(namesFile == namesmenuInit) & !is.null(data) & !edit.disable){
        Xindex <- as.numeric(sapply(namesFile, function(name){which(name == xynamesmenu)}))
        XDOE <- data$XY[Xindex]
        xnamesNew <- colnames(XDOE) 
        xnamesvisuNew <- c(data$xnamesvisu, data$ynamesvisu)[Xindex]
        Xinfos.temp$Xinfos <- lapply(1:initialXinfos$nX, get.Xinfos.col, 
                              XDOE = XDOE, xnames = xnamesNew, xnamesvisu = xnamesvisuNew, xnamesmenu = namesFile, nvalues = nvalues)
        data$X <- XDOE
        error.msg$file.names <- 'DOE columns reordered: inputs first, outputs second.'
      }
    }
    newXinfos <- get.Xinfos.from.file(initialXinfos$nX, lines.split, Xinfos.temp$Xinfos, header,
                                      input$separator, input$decimal, data, edit.disable)
    if (!is.null(newXinfos$Xinfos)){
      if (verbose) {
        print("setting Xinfos from file with")
        print(newXinfos$Xinfos)
      }
      Xinfos.temp$Xinfos <- newXinfos$Xinfos
    }else{
      error.msg$file <- newXinfos$error.msg
    }
  })
  
  output$warning.file <- renderUI({
    if (!is.null(error.msg$file)){
      list(h4(strong("Error !")), 
           HTML(paste(paste(error.msg$file, collapse = '<br/>'), '<br/> <br/>')))
    }else{
      if (!is.null(error.msg$file.names)){
        list(h4(strong("Warning !")), 
             HTML(paste(paste(error.msg$file.names, collapse = '<br/>'), '<br/> <br/>')))
      }
    }
  })
  
  observeEvent(list(initialXinfos$nX, Xinfos.temp$Xinfos, input$reset), {
    output$rangeInputs <- renderUI({
      req(initialXinfos$nX, Xinfos.temp$Xinfos)
      lapply(1:initialXinfos$nX, function(i){
        Xinfos.row.ui(i, ns, input, Xinfos.temp$Xinfos[[i]], initialXinfos$Xinfos[[i]],
                      data$X, error.msg, edit.disable)
      })
    })
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
  
  inputNamesValidity <- reactive({
    req(Xinfos$Xinfos)
    columnNames <- unlist(lapply(Xinfos$Xinfos, function(xinfo) xinfo$name))
    # inputs are processed by simulations launcher thanks to 'nunjucks' javascript library => input names need to be syntactically valid javascript names
    stri_detect_regex(columnNames, jsVarNameValidator)
  })

  output$preview <- DT::renderDataTable({
    req(Xinfos$Xinfos)
    ncolumns <- length(Xinfos$Xinfos)
    Xinfos.df <- get.Xinfos.df(Xinfos$Xinfos, ncolumns)
    # Abbreviate character strings (useful if many levels)
    xx <- data.frame(lapply(Xinfos.df,as.character),stringsAsFactors = FALSE)
    xx[is.na(xx)] <- "NA"
    rownames(xx) <- rownames(Xinfos.df)
    visuColumnNames <- colnames(Xinfos.df)
    colnames(xx) <- ifelse(inputNamesValidity(), visuColumnNames, paste("(*)", visuColumnNames))
    DT::datatable(
      xx, escape = FALSE,
      extensions = c('FixedColumns','Scroller'),
      options = list(
        dom = 't', ordering=F,
        pageLength = 2, scrollX = TRUE,scroller = TRUE, fixedColumns = TRUE,
        columnDefs = list(list(
          targets = 1:ncolumns,
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 12 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
            "}")))
      ))
  })
  
  output$download <- downloadHandler(
    filename = 'Xinfos.txt',
    content = function(con) {
      writeLines(text = get.Xinfos.download(Xinfos$Xinfos, input$separator, input$decimal),
                 con = con)
  })
  
  output$columnNamesWarning <- renderText({
      req(Xinfos$Xinfos)
      if (all(inputNamesValidity())) {
        ""
      }
      else {
        paste0("(*) Warnings: some input names (",  length(which(!inputNamesValidity())), ") are not valid for use with the simulations launcher")
      }
  })
    
  return(Xinfos)
}

# testing
XinfosChange.test <- function() {
  source("loadPackages.R")
  
  ui <- fluidPage(
    theme = "bootstrap_spacelab.css",
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tags$head(tags$script(src = "custom.js")),
    numericInput("nbr", "Number of Inputs", 1, min = 1),
    XinfosChange.ui("select"),
    hr(),
    XinfosChange.ui.preview("select"),
    hr(),
    verbatimTextOutput("raw")
  )
  
  server <- function(input, output, session) {
    
    initialXinfos <- reactiveValues(nX = NULL, Xinfos = NULL)
    
    observeEvent(input$nbr, {
      initialXinfos$nX <- input$nbr
      initialXinfos$Xinfos <- lapply(1:input$nbr, get.initialXinfos)
    })
    
    Xinfos <- callModule(XinfosChange.server, "select", initialXinfos, verbose = TRUE)
    output$raw <- renderPrint({
      reactiveValuesToList(Xinfos)
    })
  }
  
  # app
  shinyApp(ui, server)
}
