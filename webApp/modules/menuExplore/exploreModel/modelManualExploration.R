#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module modelManualExploration
get.newData.from.input <- function(DOE, input, Yinfos) {
  dimx <- DOE$nX
  dimy <- Yinfos$nY
  newdata <- data.frame(lapply(1:dimx, function(i){input[[paste0('defpredict',i)]]}))
  # as.numeric to avoid integer values not handled by RobustGasp
  idnumeric <- sapply(DOE$Xinfos,function(l){return(l$type=="numeric")})
  newdata[,idnumeric] <- as.numeric(newdata[,idnumeric])
  newdata <- cbind(newdata, t(rep(NA, dimy)))
  colnames(newdata) <- c(DOE$xnames, DOE$ynames[Yinfos$visu.ids])
  return(newdata)
}

computeYpredForDataAdd <- function(DOE, predfun, Xvalues, Yinfos) {
  dimx <- DOE$nX
  dimy <- Yinfos$nY
  nadd <- nrow(Xvalues)
  Ypred <- matrix(NA, nrow = nadd, ncol = dimy)
  for (j in seq_len(length(Yinfos$surrogate.ids))) {
    Ypred[, j] <- predfun(Xvalues[,1:dimx], Yinfos$surrogate.ids[[j]])
  }
  return(Ypred)
}

modelManualExploration.ui <- function(id) {
  ns <- NS(id)
  
  footer <- fluidRow(
    column(3, actionButton(ns("save"), label = "Add Point to Database and Close", class = "btn-warning",
                           width = '100%'), offset = 2),
    column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                           width = '100%'), offset = 2)
  )
  
  panel.import.file <- fluidRow(
    column(2, radioButtons(ns("separator"), "Separator",
                           choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"))),
    column(2, radioButtons(ns("decimal"), "Decimal",
                           choices = list(". (point)" = ".", ", (comma)" = ","))
    ),
    column(7,
           fileInput(ns('file'), 'Select file', accept = c('.txt', '.dat','.csv')),
           tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });')),
           uiOutput(ns('error.file'))
    )
  )
  
  fluidRow(
    column(
      2,
      wellPanel(
        radioButtons(
          ns("choice"), 
          label = "Define Inputs to Predict",
          choices = list("Import File" = 1, "Manual" = 2)
        ),
        actionButton(ns("proceed"), "Proceed", class = "btn-primary")
      )
    ),
    column(
      2,
      wellPanel(
        strong("Predict"),
        br(), br(),
        actionButton(ns("compute"), "Compute Predictions", class = "btn-primary"),
        br(), br(),
        downloadButton(ns("download"), label = "Export Predictions", class = "btn-primary"),
        bsModal(
          ns("modal.file"), "Choose Inputs File", NULL, size = "large",
          panel.import.file,
          tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });'))
        ),
        bsModal(
          ns("modalmanual"), "Define Inputs Manually", "proceed", size = "large",
          tagList(uiOutput(ns("defineInputs")), hr(), footer),
          tags$head(tags$style(paste0("#", ns("modalmanual")," .modal-footer{display:none}")))
        )
      )
    ),
    column(8, DT::dataTableOutput(ns('content')))
  )
}

modelManualExploration.server <- function(input, output, session, DOE, listmodels) {
  
  # Update output types for the visualization only if the surrogate models are updated
  Yinfos <- reactiveValues(int.ids=NULL, control.ids=NULL, const.ids=NULL, visu.ids=NULL, nY=NULL)
  observeEvent(listmodels$selected$id, {
    idSurrogate <- as.numeric(which(!is.na(listmodels$selected$id)))
    Yinfos$int.ids <- DOE$Yinfos$int.ids
    Yinfos$control.ids <- DOE$Yinfos$control.ids
    Yinfos$const.ids <- DOE$Yinfos$const.ids
    Yinfos$visu.ids <- c(DOE$Yinfos$int.ids, DOE$Yinfos$control.ids, DOE$Yinfos$const.ids)
    Yinfos$nY <- length(Yinfos$visu.ids)
    Yinfos$surrogate.ids <- intersect(Yinfos$visu.ids, idSurrogate)
    # reset predictions when models are updated
    dataAdd$values <- NULL
    error.msg$file <- NULL
  })
  
  # Functions for Manual Exploration
  dataAdd <- reactiveValues(values = NULL)
  error.msg <- reactiveValues(file = NULL)
  
  observeEvent(input$proceed, {
    choice <- isolate(input$choice)
    if (choice == 1) {
      toggleModal(session, "modal.file", toggle = "open")
    } else {
      toggleModal(session, "modalmanual", toggle = "open")
    }
  })
  
  output$defineInputs <- renderUI({
    req(DOE$nX, DOE$xnames)
    ns <- session$ns

    defInputs <- lapply(1:DOE$nX, function(i) {
      
      fluidRow(
        column(6, textInput(ns(paste0('defName', i)), "Input", DOE$xnamesvisu[i])),
        
        # numeric variable (define UI for bounds)
        if (DOE$Xinfos[[i]]$type == 'numeric'){
          column(6, numericInput(ns(paste0('defpredict', i)), "Input Value", sum(DOE$Xinfos[[i]]$bounds)/2))
          # categorical variable (define UI for nlevels, levels)
        }else{
          column(6, selectInput(ns(paste0('defpredict', i)), "Input Value", choices = DOE$Xinfos[[i]]$levels))
        }
      )
    })
    
    defInputs
    
  })
  
  observeEvent(input$save, {
    newdata <- get.newData.from.input(DOE, input, Yinfos)
    if (is.null(dataAdd$values)) {
      dataAdd$values <- newdata
    } else {
      dataAdd$values <- rbind(dataAdd$values, newdata)
    }
    toggleModal(session, "modalmanual", toggle = "close")
  })
  
  observeEvent(input$close, {
    toggleModal(session, "modalmanual", toggle = "close")
  })
  
  observeEvent(input$file$datapath, {
    validation.header <- check.header(DOE, input$file$datapath, input$separator, input$decimal)
    if (validation.header$valid){
      newData <- get.new.data.from.file(DOE, input$file$datapath, input$separator, input$decimal)
      validation.newData <- check.new.data(DOE$nX, DOE$Xinfos, newData)
      if (validation.newData$valid){
        newData <- cbind(newData, matrix(NA, nrow = nrow(newData), ncol = Yinfos$nY))
        colnames(newData)[DOE$nX + (1:Yinfos$nY)] <- DOE$ynames[Yinfos$visu.ids]
        dataAdd$values <- newData
        error.msg$file <- NULL
      }else{
        error.msg$file <- validation.newData$error.msg
      }
    }else{
      error.msg$file <- validation.header$error.msg
    }
  })
  
  output$error.file <- renderUI({
    req(error.msg$file)
    list(h4(strong("Error !")), 
         HTML(paste(paste(error.msg$file, collapse = '<br/>'), '<br/> <br/>')))
  })
  
  observeEvent(input$compute, {
    req(listmodels$finalpredfun, dataAdd$values)
    dimx <- DOE$nX
    dimy <- Yinfos$nY
    Ypred <- computeYpredForDataAdd(DOE, listmodels$finalpredfun, dataAdd$values, Yinfos)
    dataAdd$values[,(dimx + 1):(dimx + dimy)] <- Ypred
  })
  
  output$content <- DT::renderDataTable({
    req(dataAdd$values)
    dimd <- ncol(dataAdd$values)
    df <- dataAdd$values
    colnames(df) <- c(DOE$xnamesvisu,DOE$ynamesvisu[Yinfos$visu.ids])
    DT::datatable(
      df, escape = FALSE,
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip', 
        buttons = list(list(extend = 'colvis', columns = 1:dimd)), 
        scrollX = TRUE,scrollY = 400,scroller = TRUE,
        fixedColumns = TRUE
    ))
  })
  
  output$download <- downloadHandler(
    filename = 'SurrogatePrediction.csv',
    content = function(con) {
      df <- dataAdd$values
      colnames(df) <- c(DOE$xnamesmenu, DOE$ynamesmenu[Yinfos$visu.ids])
      write.table(x = df, file=con, row.names = F, col.names = T, sep = ",", quote = FALSE)
    }
  )
}
