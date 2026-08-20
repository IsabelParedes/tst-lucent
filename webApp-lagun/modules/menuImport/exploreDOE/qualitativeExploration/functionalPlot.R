source("modules/shared/dynamicSelectpicker.R", local = TRUE)

###############################
#  Define client user interface
###############################

functionalPlot.ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(2,dynamicSelectpicker.ui(ns("chooseFunc"))),
      column(2,dynamicSelectpicker.ui(ns("chooseDim"))),
      column(2,dynamicSelectpicker.ui(ns("chooseObs"))),
      column(2,dynamicSelectpicker.ui(ns("chooseColor"))),
      column(1,switchInput(ns("showExpData"), 
                          label = "Exp data", 
                          value = TRUE,
                          disabled = TRUE)),
      column(1,switchInput(ns("showErrBars"), 
                          label = "Error bars"))
    ),
    uiOutput(ns('plotFunc'))
  )
}

#####################
# Define server logic
#####################

functionalPlot.server <- function(input, output, session, DOE, window.dimension) {
  ns <- session$ns

  # Plot dimensions for better visualization
  dimplot <- reactiveValues(reg.height=NULL)
  observe({
    dimplot$reg.height <- 0.8*window.dimension$height
  })
  
  choicesYFunc <- reactive({
    req(DOE$Fnames, DOE$Yinfos)
    l <- list()
    
    if (length(DOE$Yinfos$func.ids)>0){
      l[["Simulation Outputs"]] <- as.list(DOE$Fnames)
    } 
    
    return(l)
  })
  
  choicesObs <- reactive({
    req(DOE$Y, funcName())
    
    l <- list()
    
    if(is.null(DOE$OFtot)){
      l <- seq(nrow(DOE$Y))
    }else{
      dfTot <- as.data.frame(DOE$OFtot)
      l <- sort(dfTot$OFtotal, index.return = TRUE)$ix
    }
      
    return(l)
  })
  
  selectedObs <- reactive({
    req(choicesObs())
    
    sampleLength <- ifelse(length(choicesObs())>=50, 50, length(choicesObs()))
    
    return(choicesObs()[1:sampleLength])
    
  })
  
  choicesDim <- reactive({
    req(funcName())
    
    l <- list()
    
    if (!grepl("experimental", funcName(), fixed=TRUE)){
      funcNameId <- match(funcName(), DOE$Fnames)
      l <- colnames(DOE$discF[[funcNameId]])
      
      if(!is.null(DOE$Z)){
        l <- c(l, paste0("experimental_", funcName()))
      }
      
    }else{
      simuFuncName <- stringr::str_split(funcName(), pattern = "_", n = 2)[[1]][2]
      funcNameId <- match(simuFuncName, DOE$Fnames)
      l <- c(colnames(DOE$discZ[[funcNameId]]))
    }
    
    return(l)
  })
  
  choicesColor <- reactive({
    req(funcName())
    
    l <- "Simulation"
    
    if(!is.null(DOE$OFtot)){
      l <- c(l, colnames(DOE$OF), colnames(DOE$OFtot))
    }
    
    return(l)
    
  })
  
  
  funcName <- callModule(dynamicSelectpicker.server, "chooseFunc", label.title = "Y-axis", choices = choicesYFunc,
                      selected = choicesYFunc()[1], multiple = FALSE, livesearch = TRUE)
  
  funcObs <- callModule(dynamicSelectpicker.server, "chooseObs", label.title = "Simulation number", choices = choicesObs,
                      selected = selectedObs, multiple = TRUE, livesearch = TRUE)
  
  funcDim <- callModule(dynamicSelectpicker.server, "chooseDim", label.title = "X-axis", choices = choicesDim,
                      selected = choicesDim()[1], multiple = FALSE, livesearch = TRUE)
  
  funcColor <- callModule(dynamicSelectpicker.server, "chooseColor", label.title = "Color by", choices = choicesColor,
                        selected = choicesColor()[1], multiple = FALSE, livesearch = TRUE)

  output$functionalPlot <- renderPlotly({
    req(DOE$Y, funcDim(), funcName(), funcObs(), funcColor(),
        cancelOutput = TRUE)
    
    funcNameId <- match(funcName(), DOE$Fnames)
    
    dims <- c(colnames(DOE$discF[[funcNameId]]), 
              paste("experimental", funcName(), sep = "_"))
    
    req(funcDim() %in% dims, cancelOutput = TRUE)
    
    if(!is.null(DOE$Z)){
      updateSwitchInput(session,
                        "showExpData",
                        disabled = FALSE)
    }
    
    isExperimentalDim <- grepl("experimental", funcDim(), fixed=TRUE)
    err <- c()
    expData <- NULL
    showExp <- input$showExpData & !is.null(DOE$Z)
    showErr <- input$showErrBars
    
    y <- t(DOE$Y[funcObs(), DOE$idF[[funcNameId]]])
    rownames(y) <- NULL
    
    if (length(funcObs())==1){
      colnames(y) <- funcName()
    }else{
      colnames(y) <- paste(funcName(), funcObs(), sep = "_")
    }
    
    if(!showExp & !isExperimentalDim){
      updateSwitchInput(session, 
                        "showErrBars",
                        value = FALSE,
                        disabled = TRUE)
    }else{
      updateSwitchInput(session, 
                        "showErrBars",
                        disabled = FALSE)
    }
    
    
    if(!isExperimentalDim){
      x <- DOE$discF[[funcNameId]][funcDim()]
    }else{
      x <- t(DOE$Z[, DOE$idZY[[funcNameId]]])
      rownames(x) <- NULL
      
      updateSwitchInput(session,
                        "showExpData",
                        disabled = TRUE,
                        value = FALSE)
    }
    
    
    colnames(x) <- "dim"
    x <- as.data.frame(x)
    
    if(showExp){
      expData <- t(DOE$Z[, DOE$idZY[[funcNameId]]])
      expData <- cbind(expData, paste0("experimental_", funcName()))
      rownames(expData) <- NULL
      colnames(expData) <- c("values", "names")
      expData <- cbind(x, expData)
      
      emptyOF <- data.frame(matrix(ncol = length(DOE$OF)+1, nrow = length(x$dim)))
      colnames(emptyOF) <- c(colnames(DOE$OF), colnames(DOE$OFtot))
      expData <- cbind(expData, emptyOF)
    }
    
    d <- cbind(x, y)
    d <- tidyr::gather(d, "names", "values", -dim)
    
    errx <- list()
    erry <- list()
    if (showErr){
      err <- t(DOE$sigZ[, DOE$idZY[[funcNameId]]])
      if (!isExperimentalDim)
        erry <- list(array = err, color = "red")
      else
        errx <- list(array = err, color = "red")
    }
    
    hoverText <- sapply(
      seq_len(length(DOE$discF[[funcNameId]][,1])), 
      function(i) paste(
        paste0("<b>", colnames(DOE$discF[[funcNameId]]), "</b>"),
        " : ",
        DOE$discF[[funcNameId]][i,],
        collapse="\n"))
    
    h <- rep(hoverText, length(funcObs()))
    h <- paste(paste0("<b>", d$names, "</b>"),
               paste0("<b>", funcName(), "</b> : ", d$values), 
               h, sep = "\n")
    
    if(!is.null(DOE$Z)){
      filteredDOEOF <- cbind(DOE$OF[funcObs(),], OFtotal = DOE$OFtot[funcObs(),])
      filteredDOEOF <- filteredDOEOF[rep(seq_len(nrow(filteredDOEOF)), each=DOE$nF[funcNameId]), ]
      
      hOF <- sapply(
        seq_len(length(filteredDOEOF[,1])),
        function(i) paste(
          paste0("<b>", colnames(filteredDOEOF), "</b>"),
          " : ",
          filteredDOEOF[i,],
          collapse="\n"
        )
      )
      
      h <- paste(h, hOF, sep = "\n")
      
      d <- cbind(d, filteredDOEOF)
    }

    if(funcColor() == "Simulation"){
      colorBy <- "names"
      colorPalette <- "Set1"
      traceMode <- ifelse(isExperimentalDim, "markers", "lines+markers")
    }else{
      colorBy <- funcColor()
      colorPalette <- NULL
      traceMode <- "markers"
    }
    
    p <- plot_ly(data = d, 
                 x = ~dim, 
                 y = ~values, 
                 color = as.formula(paste0("~", colorBy)),
                 colors = colorPalette,
                 type = "scatter",
                 mode = traceMode,
                 error_x = ~errx,
                 text = ~h,
                 hoverinfo = "text",
                 showlegend = FALSE)
    
    
    if (showExp & !isExperimentalDim){

      markerStyle <- list(color = "red",
                          size = 20,
                          symbol = "square",
                          line = list(
                            color = "#31ABFA",
                            width = 3
                          ))
      
      p <- p %>% add_trace(data = expData,
                           x = ~dim,
                           y = ~values,
                           type = "scatter",
                           mode = "markers",
                           marker = markerStyle,
                           error_y = ~erry,
                           text = ~paste(paste0("<b>", expData$names, "</b>"), 
                                         paste0("<b>", "y = ", "</b>", expData$values),
                                         hoverText, 
                                         sep = "\n"),
                           hoverinfo = "text",
                           inherit = FALSE)
    }
    
    if (isExperimentalDim){
      p <- p %>% add_segments(x = min(d$dim), xend = max(d$dim), 
                              y = min(d$dim), yend = max(d$dim), 
                              name = "Ref", color = I("black"),
                              error_x = NULL,
                              text = NULL)
      
      xAxisList <- list(title = paste0("experimental_", DOE$Fnamesvisu[funcNameId]),
                        scaleanchor = "y",
                        scaleratio = 1)
      
    }else{
      xAxisList <- list(title = funcDim())
    }

    p <- p %>% layout(yaxis = list(title = DOE$Fnamesvisu[funcNameId]),
                      xaxis = xAxisList,
                      hoverlabel = list(align = "left"))
    
    
    
    return(p)
  })
  
  output$plotFunc <- renderUI({
    req(dimplot$reg.height, length(DOE$Yinfos$func.ids)>0)
    
    plotlyOutput(ns("functionalPlot"), 
                 width = "100%", 
                 height=paste0(dimplot$reg.height,"px")) %>% withSpinner()
  })
  
}
