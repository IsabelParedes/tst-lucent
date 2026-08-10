#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module modelQuantitativeExploration
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/menuExplore/exploreModel/modelQualitativeExploration.R", local = TRUE)

getShrinkedRange <- function(data, rangeFactor) {
  oldMin <- min(data)
  oldMax <- max(data)
  oldRange <- oldMax - oldMin
  newMin <- oldMin - rangeFactor*oldRange
  newMax <- oldMax + rangeFactor*oldRange
  list(min = newMin, max = newMax)
}

# FIX : refactor this plotting function
plotExplo <- function(DOE, predfun, yname, xname1, xname2, remainingX, ncontours, ftemp, visuname) {
  dimx <- DOE$nX
  numy <- which(DOE$ynamesmenu == yname)
  id1 <- which(DOE$xnamesmenu == xname1)
  id2 <- which(DOE$xnamesmenu == xname2)
  bounds <- DOE$Xinfos[[id1]]$bounds
  notid <- remainingX$ids
  valnotid <- remainingX$values
  length_notid <- length(notid)
  nx <- 50
  if (DOE$adapt.visu){
    margin=list(b = -1, l = -1)
  }else{
    margin=NULL
  }
  Ytype <- DOE$Yinfos$type[numy]

  if (xname2 == "None" | (xname1 == xname2)) {
    dxx <- as.data.frame(matrix(NA, nrow = nx, ncol = dimx))
    colnames(dxx) <- DOE$xnames
    dxx[, id1] <- seq(bounds[1], bounds[2], length.out = nx)
    if (length(notid) > 0){
      dxx[, notid] <- do.call(cbind.data.frame, lapply(1:length(notid),function(i) rep(valnotid[[i]],nx)))
    }
    yy <- predfun(dxx, numy)
    df <- data.frame(x = dxx[,id1], y = yy)
    
    if (Ytype == 'numeric'){
      df <- as.data.frame(apply(df, 2, function(col){
        as.numeric(as.character(col))
      }))
      yRange <- getShrinkedRange(yy, ftemp)
      p <- layout(
        plot_ly(df, x = ~x, y = ~y, mode = "lines", type = "scatter", height = 400),
        title = "Section Plot", 
        xaxis = list(title = DOE$xnamesvisu[id1]), 
        yaxis = list(title = DOE$ynamesvisu[numy], range = yRange), margin=margin
      )
    }
    
    if (Ytype == 'categorical'){
      p <- layout(
        plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = 'markers', height = 400),
        title = "Section Plot", 
        xaxis = list(title = DOE$xnamesvisu[id1]), 
        yaxis = list(title = DOE$ynamesvisu[numy]), margin=margin
      )
    }
    
  } else {
    dxx <- as.data.frame(matrix(NA, nrow = nx*nx, ncol = dimx))
    colnames(dxx) <- DOE$xnames
    bounds.id2 <- DOE$Xinfos[[id2]]$bounds
    xseq <- seq(bounds[1], bounds[2], length.out = nx)
    yseq <- seq(bounds.id2[1], bounds.id2[2], length.out = nx)
    xidtemp <- expand.grid(xseq,yseq)
    dxx[, c(id1, id2)] <- as.matrix(xidtemp)
    if (length(notid) > 0){
      dxx[, notid] <- do.call(cbind.data.frame, lapply(1:length(notid),function(i) rep(valnotid[[i]],nx)))
    }
    yy <- matrix(predfun(dxx, numy), ncol = 1)
    yRange <- getShrinkedRange(yy, ftemp)
    zz <- matrix(yy, nrow = nx, byrow = TRUE)
    if (visuname == "Contour Plot"){
      p <- layout(
        plot_ly(
          z = zz, x = xseq, y = yseq, type = "contour", height = 400,
          colorbar = list(title = DOE$ynamesvisu[numy]),
          zmin = yRange$min, 
          zmax = yRange$max, 
          ncontours = ncontours
        ),
        title = "Contour Plot", 
        xaxis = list(title = DOE$xnamesvisu[id1]), 
        yaxis = list(title = DOE$xnamesvisu[id2]), margin = margin
      )
    }else{
      if (visuname == "3D Surface Plot"){
        p <- layout(
          plot_ly(
            z = zz, x = xseq, y = yseq, type = "surface", height = 400,
            colorbar = list(title = DOE$ynamesvisu[numy])
          ),
          title = "3D Surface Plot",
          scene = list(
            xaxis = list(title = DOE$xnamesvisu[id1]),
            yaxis = list(title = DOE$xnamesvisu[id2]),
            zaxis = list(title = DOE$ynamesvisu[numy], range=c(yRange$min,yRange$max)))
        )
      }else{
        p <- NULL
      }
    }
  }

  return(p)
}

modelQuantitativeExploration.ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns('tabs'), type = "tabs",
                tabPanel(h4("Regression plot - One by One"),
                         tagList(
                           br(),
                           fluidRow(
                             column(
                               4,
                               dynamicSelect.ui(ns("chooseX1")),
                               dynamicSelect.ui(ns("chooseX2"))
                             ),
                             column(
                               4,
                               dynamicSelect.ui(ns("chooseY"))
                             ),
                             column(
                               4,
                               dynamicSelect.ui(ns("chooseVisu"))
                             )
                           ),
                           fluidRow(
                             column(
                               4,
                               uiOutput(ns("sliders"))
                             ),
                             column(
                               8,
                               plotlyOutput(ns("plotExplo"))%>% withSpinner()
                             )
                           )
                         )
                ),
                tabPanel(h4("Regression plot with smoothing - All in One"),
                         tagList(
                           br(),
                           fluidRow(
                             column(4,dynamicSelectpicker.ui(ns("chooseYsmooth"))),
                             column(4,dynamicSelectpicker.ui(ns("chooseXsmooth"))),
                             column(4,uiOutput(ns('ui.treillis')),align="right")
                           ),
                           uiOutput(ns('ui.smooth')),
                           tags$script(paste0('$( "#', ns('chooseXsmooth'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseXsmoothclosed'),'", 1, {priority: "event"}); });')),
                           tags$script(paste0('$( "#', ns('chooseXsmooth'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXsmoothclosed'),'", 1, {priority: "event"}); });')),
                           tags$script(paste0('$( "#', ns('chooseYsmooth'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseYsmoothclosed'),'", 1, {priority: "event"}); });')),
                           tags$script(paste0('$( "#', ns('chooseYsmooth'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYsmoothclosed'),'", 1, {priority: "event"}); });'))
                         )
                ),
                tabPanel(h4("Regression plot with smoothing - All in One (Summary)"), 
                         tagList(
                           br(),
                           fluidRow(
                             column(4,dynamicSelectpicker.ui(ns("chooseYsmoothsummary"))),
                             column(4,dynamicSelectpicker.ui(ns("chooseXsmoothsummary"))),
                             column(4,"")
                           ),
                           uiOutput(ns('ui.smooth.summary')),
                           tags$script(paste0('$( "#', ns('chooseXsmoothsummary'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseXsmoothsummaryclosed'),'", 1, {priority: "event"}); });')),
                           tags$script(paste0('$( "#', ns('chooseXsmoothsummary'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXsmoothsummaryclosed'),'", 1, {priority: "event"}); });')),
                           tags$script(paste0('$( "#', ns('chooseYsmoothsummary'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseYsmoothsummaryclosed'),'", 1, {priority: "event"}); });')),
                           tags$script(paste0('$( "#', ns('chooseYsmoothsummary'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYsmoothsummaryclosed'),'", 1, {priority: "event"}); });'))
                         )
                )
    )
  )
}

modelQuantitativeExploration.server <- function(input, output, session, DOE, listmodels, window.dimension, settings) {
  
  ns <- session$ns
  
  # Plot dimensions for better visualization
  dimplot <- reactiveValues(smooth.width=NULL, smooth.height=NULL)
  observe({
    dimplot$smooth.width <- 0.95*window.dimension$width
    dimplot$smooth.height <- 0.75*window.dimension$height
  })
  
  # Update output types for the visualization only if the surrogate models are updated
  Yinfos <- reactiveValues(int.ids=NULL, control.ids=NULL, const.ids=NULL, visu.ids=NULL, nY=NULL)
  observeEvent(list(listmodels$bestQ2loo$id, DOE$nY), {
    
    YwithSelectedModel <- seq(DOE$nY)
    
    if (!is.null(listmodels$selected))
      YwithSelectedModel <- YwithSelectedModel[sapply(listmodels$selected$id, function(x) !is.na(x[1]))]

    Yinfos$int.ids <- intersect(DOE$Yinfos$int.ids, YwithSelectedModel)
    Yinfos$control.ids <- intersect(DOE$Yinfos$control.ids, YwithSelectedModel)
    Yinfos$const.ids <- intersect(DOE$Yinfos$const.ids, YwithSelectedModel)
    Yinfos$visu.ids <- c(Yinfos$int.ids, Yinfos$control.ids, Yinfos$const.ids)
    Yinfos$nY <- length(Yinfos$visu.ids)
  })
  
  # Regression tabpanel
  
  choicesX1 <- reactive({
    req(DOE$Xinfos)
    unlist(sapply(DOE$Xinfos, function(var){
      if (var$type == 'numeric'){var$namemenu}
    }))
  })
  xname1 <- callModule(dynamicSelect.server, "chooseX1", label = "Input 1", choicesX1)
  
  choicesX2 <- reactive({
    req(DOE$Xinfos, yname(), yname() %in% DOE$ynamesmenu)
    Ytype <- DOE$Yinfos$type[which(DOE$ynamesmenu == yname())]
    if (Ytype == 'numeric'){
      c("None", choicesX1())
    }else{
      "None"
    }
  })
  xname2 <- callModule(dynamicSelect.server, "chooseX2", label = "Input 2", choicesX2)
  
  choicesY <- reactive({
    req(DOE$ynamesmenu,Yinfos)
    l <- list()
    if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[Yinfos$int.ids])
    if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[Yinfos$control.ids])
    if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[Yinfos$const.ids])
    return(l)
  })
  
  yname <- callModule(
    dynamicSelectpicker.server, "chooseY", label.title = "Choose Output to Visualize", choices = choicesY,
    multiple = FALSE, livesearch = TRUE, selected = DOE$ynamesmenu[Yinfos$int.ids[1]]
  )

  choicesVisu <- reactive({
    req(xname2())
    if (xname2() == "None" | (xname1() == xname2())){
      choices <- "Regression Plot"
    }else{
      choices <- c("Contour Plot", "3D Surface Plot")
    }
    return(choices)
  })
  visuname <- callModule(dynamicSelect.server, "chooseVisu", label = "Choose Visualization Type", choicesVisu)
  
  remainingX.ids <- reactive({
    req(DOE$xnamesmenu, xname1(), xname2())
    selected <- c(xname1(), xname2())
    remaining <- which(!(DOE$xnamesmenu %in% selected))
    remaining
  })
  
  output$sliders <- renderUI({
    ns <- session$ns
    sliders <- lapply(DOE$Xinfos[remainingX.ids()], function(Xinfos, nsteps, nsignif) {
      if (Xinfos$type == 'numeric'){
        sliderInput(
          inputId = ns(paste0('Scolumn', Xinfos$name)),
          label = Xinfos$namemenu,
          min = signif(Xinfos$bounds[1], nsignif),
          max = signif(Xinfos$bounds[2], nsignif),
          value = signif(sum(Xinfos$bounds)/2, nsignif),
          step = signif(diff(Xinfos$bounds)/nsteps, nsignif),
          sep = "")
      }else{
        levels <- unlist(Xinfos$levels)
        selectInput(
          inputId = ns(paste0('Scolumn', Xinfos$name)),
          label = Xinfos$namemenu,
          choices = levels,
          selected = levels[1]
        )
      }
    }, nsteps = settings$nsteps, nsignif = settings$nsignif)
    sliders  
  })
  
  remainingX <- reactive({
    xnames <- sapply(remainingX.ids(), function(x) {
      DOE$xnames[x]
    })
    values <- lapply(DOE$Xinfos[remainingX.ids()], function(Xinfos) {
      input[[paste0("Scolumn", Xinfos$name)]]
    })
    list(ids = remainingX.ids(), values = values, xnames = xnames)
  })
  
  output$raw <-  renderPrint({
    remainingX()
  })
  
  output$plotExplo <- renderPlotly({
    # FIX : bad call to predfun generates error, delay reactivity
    req(listmodels$finalpredfun, length(unlist(remainingX()$values)) == length(remainingX()$ids),
        yname(), xname1(), xname2(), visuname(), cancelOutput = TRUE)
    plotExplo(
      DOE, listmodels$finalpredfun, yname(),  xname1(), xname2(), remainingX(), settings$ncontours, settings$ftemp, visuname()
    )
  })
  
  
  # Regression with smoothing tabpanel
  plotsmooth <- reactiveValues(sample = NULL, sample.smooth = NULL, sample.smooth.se = NULL)
  
  Xcat <- reactive({
    req(DOE$Xinfos)
    cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
    return(DOE$xnamesmenu[cat])
  })
  
  choicesX <- reactive({
    req(DOE$xnamesmenu)
    return(DOE$xnamesmenu)
  })
  
  xnamesmooth <- callModule(
    dynamicSelectpicker.server, "chooseXsmooth", label.title =  "Choose Input(s) to Visualize", choices = choicesX,
    selected = choicesX(), livesearch = TRUE
  )
  
  ynamesmooth <- callModule(
    dynamicSelectpicker.server, "chooseYsmooth", label.title = "Choose Output(s) to Visualize", choices = choicesY,
    multiple = TRUE, livesearch = TRUE, selected = DOE$ynamesmenu[Yinfos$int.ids]
  )
  
  selectedAllInOne <- reactiveValues(X = NULL, Y = NULL, nX = NULL, nY = NULL)
  
  treillisInput <- reactiveValues(ncol = NULL, nrow = NULL)
  
  observeEvent(input$chooseXsmoothclosed,{
    selectedAllInOne$nX <- length(xnamesmooth())
    
    if(is.null(selectedAllInOne$X)){
      selectedAllInOne$X <- xnamesmooth()
    }else{
      if(!identical(xnamesmooth(), selectedAllInOne$X)){
        selectedAllInOne$X <- xnamesmooth()
      }
    }
    
    if(!is.null(treillisInput$ncol)){
      if(treillisInput$ncol > selectedAllInOne$nX){
        treillisInput$ncol <- selectedAllInOne$nX
      }
    }
    
  })
  
  observeEvent(input$chooseYsmoothclosed,{
    selectedAllInOne$nY <- length(ynamesmooth())
    
    if(is.null(selectedAllInOne$Y)){
      selectedAllInOne$Y <- ynamesmooth()
    }else{
      if(!identical(ynamesmooth(), selectedAllInOne$Y)){
        selectedAllInOne$Y <- ynamesmooth()
      }
    }
    
    if(!is.null(treillisInput$nrow)){
      if(treillisInput$nrow > selectedAllInOne$nY){
        treillisInput$nrow <- selectedAllInOne$nY
      }
    }
  })
  
  
  # Number of rows and columns in the treillis
  
  currentview <- reactiveValues(idcurrentCol=1,idcurrentRow=1,maxRow=NULL,maxCol=NULL)
  
  observeEvent(c(input$chooseXsmoothclosed, input$chooseYsmoothclosed, treillisInput$nrow, treillisInput$ncol),{
    req(selectedAllInOne$nX, selectedAllInOne$nY)
      currentview$maxRow <- ceiling(selectedAllInOne$nY/treillisInput$nrow)
      currentview$maxCol <- ceiling(selectedAllInOne$nX/treillisInput$ncol)
  })
  
  observeEvent(c(input$chooseXsmoothclosed,input$chooseYsmoothclosed),{
    # Reset current view
    currentview$idcurrentCol <- 1
    currentview$idcurrentRow <- 1
  })
  
  observeEvent(input$nrow,{
    # Reset current view
    currentview$idcurrentRow <- 1
    
    if(!is.null(treillisInput$nrow)){
      if(is.na(input$nrow)){
        #Nothing
      }else if(input$nrow <= selectedAllInOne$nY){
        if(treillisInput$nrow != input$nrow){
          treillisInput$nrow <- input$nrow
        }
      }else{
        treillisInput$nrow <- selectedAllInOne$nY
      }
    }
  })
  
  observeEvent(input$ncol,{
    # Reset current view
    currentview$idcurrentCol <- 1
    
    if(!is.null(treillisInput$ncol)){
      if(is.na(input$ncol)){
        #Nothing
      }else if(input$ncol <= selectedAllInOne$nX){
        if(treillisInput$ncol != input$ncol){
          treillisInput$ncol <- input$ncol
        }
      }else{
        treillisInput$ncol <- selectedAllInOne$nX
      }
    }
  })

  # Navigate in the treillis

  observe({
    req(currentview$idcurrentCol,currentview$idcurrentRow,currentview$maxCol,currentview$maxRow)
    if (currentview$idcurrentCol>1){
      enableActionButton(ns("goPrevious"),session)
    }else{
      disableActionButton(ns("goPrevious"),session)
    }
    if (currentview$idcurrentCol<currentview$maxCol){
      enableActionButton(ns("goNext"),session)
    }else{
      disableActionButton(ns("goNext"),session)
    }
    if (currentview$idcurrentRow>1){
      enableActionButton(ns("goUp"),session)
    }else{
      disableActionButton(ns("goUp"),session)
    }
    if (currentview$idcurrentRow<currentview$maxRow){
      enableActionButton(ns("goDown"),session)
    }else{
      disableActionButton(ns("goDown"),session)
    }
  })

  observeEvent(input$goPrevious,{
    if (currentview$idcurrentCol>1){
      currentview$idcurrentCol <- currentview$idcurrentCol - 1
    }
  })
  observeEvent(input$goNext,{
    req(currentview$maxCol)
    if (currentview$idcurrentCol<currentview$maxCol){
      currentview$idcurrentCol <- currentview$idcurrentCol + 1
    }
  })
  observeEvent(input$goUp,{
    if (currentview$idcurrentRow>1){
      currentview$idcurrentRow <- currentview$idcurrentRow - 1
    }
  })
  observeEvent(input$goDown,{
    req(currentview$maxRow)
    if (currentview$idcurrentRow<currentview$maxRow){
      currentview$idcurrentRow <- currentview$idcurrentRow + 1
    }
  })
  
  #Set to NULL if the selected model changes to recompute
  observeEvent(listmodels$selected$id, {
    plotsmooth$sample <- NULL
    plotsmooth$sample.smooth <- NULL
  })
  
  output$ui.treillis <- renderUI({
    req(selectedAllInOne$nY, selectedAllInOne$nX)
      
    if(is.null(treillisInput$nrow) & is.null(treillisInput$ncol)){
      treillisInput$nrow <- min(3, selectedAllInOne$nY)
      treillisInput$ncol <- min(5, selectedAllInOne$nX)
    }
    
      fluidRow(
        column(2,numericInput(ns("nrow"), "Rows", value = treillisInput$nrow, min = 1, max = selectedAllInOne$nY)),
        column(2,numericInput(ns("ncol"), "Columns", value = treillisInput$ncol, min = 1, max = selectedAllInOne$nX)),
        column(2,actionButton(ns("goPrevious"), "",icon=icon("angle-double-left"), class = "btn-primary", width='100%'), style = "margin-top: 25px;"),
        column(2,actionButton(ns("goNext"), "",icon=icon("angle-double-right"), class = "btn-primary", width='100%'), style = "margin-top: 25px;"),
        column(2,actionButton(ns("goDown"), "",icon=icon("angle-double-down"), class = "btn-primary", width='100%'), style = "margin-top: 25px;"),
        column(2,actionButton(ns("goUp"), "",icon=icon("angle-double-up"), class = "btn-primary", width='100%'), style = "margin-top: 25px;")
      )
  })
  
  output$ui.smooth <- renderUI({
    req(dimplot$smooth.width,dimplot$smooth.height)
    plotlyOutput(ns("plot.smooth"), width = paste0(dimplot$smooth.width,"px"), height = paste0(dimplot$smooth.height,"px"))%>% withSpinner()
  })

  output$plot.smooth <- renderPlotly({
    req(DOE$XY,selectedAllInOne$nY, selectedAllInOne$nX, selectedAllInOne$X, selectedAllInOne$Y,treillisInput$ncol, treillisInput$nrow, currentview$idcurrentCol,currentview$idcurrentRow, currentview$maxRow, currentview$maxCol, !is.null(Xcat()), listmodels$selected$id, cancelOutput = TRUE)
    if (is.null(plotsmooth$sample)){
      showModal(modalDialog(HTML(paste(
        "Generating smoothing graphs for all outputs and inputs.",
        "Depending on the number of inputs/outputs the computation may take a while.",
        "If you close this window it is not advised to navigate in other panels until the computation is done.",
        "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
        size = 'l')
      )
      dimY <- Yinfos$nY
      idYcat <- intersect(which(DOE$Yinfos$type == 'categorical'), DOE$Yinfos$surrogate.ids)
      idYnum <- intersect(setdiff(1:DOE$nY, idYcat), DOE$Yinfos$surrogate.ids)
      idYcat <- intersect(idYcat, Yinfos$visu.ids)
      idYnum <- intersect(idYnum, Yinfos$visu.ids)
      nYcat <- length(idYcat)
      nYnum <- length(idYnum)
      callback <- function(i) {
        incProgress(1/dimY, detail = paste("Computing Prediction for Output", i, "/", dimY))
      }
      if (nYnum > 0){
        withProgress(message = 'Propagating through Surrogates...', value = 0, {
          df <- getParcoordsData(DOE, settings$nobsparcoords, listmodels$finalpredfun, DOE$Xinfos, Yinfos, callback, idYnum)
        })
        plotsmooth$sample <- df
      }
      if (nYcat > 0){
        XvisuCat <- sampleInputs(DOE, settings$nobsparcoords, DOE$Xinfos)
        YvisuCat <- list()
        withProgress(message = 'Propagating through Surrogates...', value = 0, {
          for (j in idYcat){
            YvisuCat[[j]] <- predict.metamodel(listmodels$models[[as.numeric(listmodels$selected$id[j])]][[j]],
                                               XvisuCat, computesd = FALSE, computeProba = TRUE)$mean
            callback(j)
          }
        })
        plotsmooth$sampleXcat <- XvisuCat
        plotsmooth$sampleYcat <- YvisuCat
      }
      removeModal()
    }
    isolate({

      xynames <- c(DOE$xnames,DOE$ynames)
      xynamesvisu <- c(DOE$xnamesvisu,DOE$ynamesvisu)
      dfnames <- xynames[c(xnamesmooth(),ynamesmooth())]
      dfnamesmenu <- c(xnamesmooth(),ynamesmooth())
      dfnamesvisu <- xynamesvisu[c(xnamesmooth(),ynamesmooth())]
      if (!is.null(plotsmooth$sample)){
        df <- plotsmooth$sample[,dfnames]
      }
      if (!is.null(plotsmooth$sampleYcat)){
        dfXcat <- plotsmooth$sampleXcat[xynames[xnamesmooth()]]
        Yproba <- plotsmooth$sampleYcat[as.numeric(sapply(ynamesmooth(), function(x){which(x == DOE$ynamesmenu)}))]
      }
    })
    
    adapt.visu <- DOE$adapt.visu
    if (adapt.visu){
      margin=list(b = -1, l = -1)
    }else{
      margin=NULL
    }
    
    nY <- selectedAllInOne$nY
    nX <- selectedAllInOne$nX
    
    idX <- (1+treillisInput$ncol*(currentview$idcurrentCol-1)):(treillisInput$ncol*currentview$idcurrentCol)
    idY <- (nX+1+treillisInput$nrow*(currentview$idcurrentRow-1)):(nX+min(nY, treillisInput$nrow*currentview$idcurrentRow))
    
    # Create all subplots for treillis
    plots <- outer(idX, idY, Vectorize(function(i,j) {
      if (i > nX | j > (nX+nY)){
        return(plotly_empty(type="scatter", mode = "markers"))
      }else{
        Ytype <- DOE$Yinfos$type[which(DOE$ynames == dfnames[j])]
        if (Ytype == 'numeric'){
          if (dfnamesmenu[i]%in%Xcat()){
            dftemp <- df[,c(dfnames[i],dfnames[j])]
            colnames(dftemp) <- c('x','y')
            dftemp[,'x'] <- as.factor(dftemp[,'x'])
            nx <- length(levels(dftemp$x))
            p <- plot_ly(dftemp,x = ~as.numeric(x), y = ~y, 
                         split = ~x, type = 'violin', box = list(visible = T), showlegend = FALSE)%>%
              layout(xaxis=list(title = dfnamesvisu[i],tickvals=1:nx,ticktext=levels(dftemp$x)),
                     yaxis=list(title = dfnamesvisu[j]), margin = margin)
          }else{
            lmfit <- lm(paste0("`", dfnames[j],"`~`",dfnames[i], "`"),data=df, x = TRUE) #
            lmsort <- sort(lmfit$x[,2], index.return = TRUE)
            lmxplot <- lmsort$x
            lmyplot <- lmfit$fitted.values[lmsort$ix]
            sfit <- loess(paste0("`", dfnames[j],"`~`",dfnames[i], "`"), data = df)
            sxplot <- unique(lmxplot)
            syplot <- predict(sfit,sxplot)
            sdf <- data.frame(x=sxplot,y=syplot)
            colnames(sdf) <- c(dfnames[i],dfnames[j])
            p <- plot_ly(sdf, x = as.formula(paste0("~`",dfnames[i], "`")), y = as.formula(paste0("~`",dfnames[j], "`")), type="scatter", mode = "lines",showlegend = FALSE,line = list(color = 'rgb(205, 12, 24)'))%>%
              layout(xaxis=list(title=dfnamesvisu[i]), yaxis=list(title=dfnamesvisu[j]), margin = margin)
          }
        }
        if (Ytype == 'categorical'){
          Ylevels <- levels(DOE$Y[as.numeric(sapply(ynamesmooth(), function(x){which(x == DOE$ynamesmenu)}))][, j - nX])
          if (dfnamesmenu[i]%in%Xcat()){
            dfproba <- cbind(dfXcat[, i, drop = FALSE], Yproba[[j - nX]])
            sdf <- data.frame(x = as.factor(dfproba[,1]), y = as.factor(Ylevels[apply(dfproba[,2:ncol(dfproba)], 1, which.max)]))
            colnames(sdf) <- c('x','y')
            nx <- length(levels(sdf$x))
            ny <- length(levels(sdf$y))
            p <- plot_ly(sdf,x = ~jitter(as.numeric(x)), y = ~jitter(as.numeric(y)), 
                    type = 'scatter', mode = 'markers',showlegend = FALSE, alpha=0.7)%>%
              layout(xaxis=list(title = dfnamesvisu[i],tickvals=1:nx,ticktext=levels(sdf$x)),
                     yaxis=list(title = dfnamesvisu[j],tickvals=1:ny,ticktext=levels(sdf$y)))
          }else{
            dfproba <- cbind(dfXcat[, i, drop = FALSE], Yproba[[j - nX]])
            colnames(dfproba) <- c(dfnames[i], paste0(dfnames[j], Ylevels))
            sxplot <- sort(dfproba[, 1])
            syplot <- sapply(1:length(Ylevels), function(k){
              sfit <- loess(paste0("`", colnames(dfproba)[k + 1], "`~`", dfnames[i]), "`", data = dfproba)
              predict(sfit, sxplot)
            })
            sdf <- data.frame(x = sxplot, y = Ylevels[apply(syplot, 1, which.max)])
            colnames(sdf) <- c(dfnames[i],dfnames[j])
            colnames(sdf) <- c('x','y')
            sdf[,'y'] <- as.factor(sdf[,'y'])
            ny <- length(levels(sdf$y))
            p <- plot_ly(sdf, x = ~x, y = ~as.numeric(y),
                         split = ~y, type = 'scatter', mode = "markers", showlegend = FALSE)%>%
              layout(xaxis=list(title = dfnamesvisu[i]), 
                     yaxis=list(title = dfnamesvisu[j]),tickvals=1:ny,ticktext=levels(sdf$y), margin = margin)
          }
        }
        return(p)
      }}, SIMPLIFY = FALSE))
    return(plotly::subplot(plots, nrows = length(idY), shareX = TRUE, shareY = TRUE, widths=rep(1/treillisInput$ncol,treillisInput$ncol)))
  })
  
  
  # Regression with smoothing tabpanel - Summary
  
  
  xnamesmooth.summary <- callModule(
    dynamicSelectpicker.server, "chooseXsmoothsummary", label.title =  "Choose Input(s) to Visualize", choices = choicesX1,
    selected = choicesX1()[1:min(10,length(choicesX1()))], livesearch = TRUE
  )
  
  ynamesmooth.summary <- callModule(
    dynamicSelectpicker.server, "chooseYsmoothsummary", label.title = "Choose Output to Visualize", choices = choicesY,
    multiple = FALSE, livesearch = TRUE, selected = choicesY()[1]
  )
  
  output$ui.smooth.summary <- renderUI({
    req(dimplot$smooth.width,dimplot$smooth.height)
    plotlyOutput(ns("plot.smooth.summary"), width = paste0(dimplot$smooth.width,"px"), height = paste0(dimplot$smooth.height,"px"))%>% withSpinner()
  })
  
  output$plot.smooth.summary <- renderPlotly({
    req(input$chooseXsmoothsummaryclosed, input$chooseYsmoothsummaryclosed, isolate(xnamesmooth.summary()),
        isolate(ynamesmooth.summary()), listmodels$selected$id, cancelOutput = TRUE)
    isolate({
      if (is.null(plotsmooth$sample)){
        showModal(modalDialog(HTML(paste(
          "Generating smoothing graphs for all outputs and inputs.",
          "Depending on the number of inputs/outputs the computation may take a while.", 
          "If you close this window it is not advised to navigate in other panels until the computation is done.",
          "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
          size = 'l')
        )
        dimY <- Yinfos$nY
        idYcat <- intersect(which(DOE$Yinfos$type == 'categorical'), DOE$Yinfos$surrogate.ids)
        idYnum <- intersect(setdiff(1:DOE$nY, idYcat), DOE$Yinfos$surrogate.ids)
        idYcat <- intersect(idYcat, Yinfos$visu.ids)
        idYnum <- intersect(idYnum, Yinfos$visu.ids)
        nYcat <- length(idYcat)
        nYnum <- length(idYnum)
        callback <- function(i) {
          incProgress(1/dimY, detail = paste("Computing Prediction for Output", i, "/", dimY))
        }
        if (nYnum > 0){
          withProgress(message = 'Propagating through Surrogates...', value = 0, {
            df <- getParcoordsData(DOE, settings$nobsparcoords, listmodels$finalpredfun, DOE$Xinfos, Yinfos, callback, idYnum)
          })
          plotsmooth$sample <- df
        }
        if (nYcat > 0){
          XvisuCat <- sampleInputs(DOE, settings$nobsparcoords, DOE$Xinfos)
          YvisuCat <- list()
          withProgress(message = 'Propagating through Surrogates...', value = 0, {
            for (j in idYcat){
              YvisuCat[[j]] <- predict.metamodel(listmodels$models[[as.numeric(listmodels$selected$id[j])]][[j]],
                                                 XvisuCat, computesd = FALSE, computeProba = TRUE)$mean
              callback(j)
            }
          })
          plotsmooth$sampleXcat <- XvisuCat
          plotsmooth$sampleYcat <- YvisuCat
        }
        removeModal()
      }
      if (is.null(plotsmooth$sample.smooth)){
        showModal(modalDialog(HTML(paste(
          "Generating smoothing graphs for all outputs and inputs.",
          "Depending on the number of inputs/outputs the computation may take a while.", 
          "If you close this window it is not advised to navigate in other panels until the computation is done.",
          "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
          size = 'l')
        )
        dimX <- length(choicesX1())
        namesY <- DOE$ynames[unlist(choicesY())]
        idY <- which(DOE$ynames %in% namesY)
        namesYmenu <- unlist(choicesY())
        dimY <- length(namesY)
        namesX1 <- DOE$xnames[choicesX1()]
        if (!is.null(plotsmooth$sample)){
          df <- plotsmooth$sample
        }
        if (!is.null(plotsmooth$sampleYcat)){
          dfXcat <- plotsmooth$sampleXcat
          Yproba <- plotsmooth$sampleYcat
        }
        smat <- array(NA,c(11,dimX,dimY))
        semat <- array(NA,c(11,dimX,dimY))
        callback <- function(i) {
          incProgress(1/dimY, detail = paste("Computing Smoothing for Output", i,"/",dimY))
        }
        withProgress(message = 'Smoothing Surrogates...', value = 0, {
          for (j in 1:dimY){
            Ytype <- DOE$Yinfos$type[idY[j]]
            if (Ytype == 'numeric'){
              for (i in 1:dimX){
                ltemp <- loess(as.formula(paste0("`", namesY[j], "`~`", namesX1[i], "`")),df) 
                ptemp <- predict(ltemp,newdata = quantile(df[, namesX1[i]], seq(0, 1, 0.1)), se=TRUE)
                smat[,i,j] <- ptemp$fit
                semat[,i,j] <- ptemp$se.fit
              }
            }
            if (Ytype == 'categorical'){
              for (i in 1:dimX){
                dfproba <- cbind(dfXcat[, namesX1[i], drop = FALSE], Yproba[[idY[j]]])
                Ylevels <- levels(DOE$Y[, idY[j]])
                colnames(dfproba) <- c(namesX1[i], paste0(namesY[j], Ylevels))
                sxplot <- quantile(dfproba[, namesX1[i]], seq(0,1,0.1))
                syplot <- sapply(1:length(Ylevels), function(k){
                  sfit <- loess(paste0("`", colnames(dfproba)[k + 1], "`~`", namesX1[i], "`"), data = dfproba)
                  predict(sfit, sxplot)
                })
                ptemp <- Ylevels[apply(syplot, 1, which.max)]
                smat[,i,j] <- ptemp
                semat[,i,j] <- rep(NA, length(ptemp))
              }
            }
            callback(j)
          }
        })
        dimnames(smat) <- list(NULL,choicesX1(),namesYmenu)
        dimnames(semat) <- list(NULL,choicesX1(),namesYmenu)
        plotsmooth$sample.smooth <- smat
        plotsmooth$sample.smooth.se <- semat
        removeModal()
      }
      df.fit <- plotsmooth$sample.smooth[,xnamesmooth.summary(),ynamesmooth.summary()]
      df.se <- plotsmooth$sample.smooth.se[,xnamesmooth.summary(),ynamesmooth.summary()]
      df <- as.data.frame(melt(df.fit))
      df$sd <- 2*melt(df.se)$value
      Yind <- which(ynamesmooth.summary() == DOE$ynamesmenu)
      Ytype <- DOE$Yinfos$type[Yind]
      if (Ytype == 'numeric'){
        p <- plot_ly(df, x=~Var1, y=~value, color=~Var2, type="scatter", mode="markers+lines", 
                     error_y = ~list(array = sd)) %>% add_trace(x = 1:11, y = rep(mean(df$value),11), type="scatter",
                     mode = "lines", name = "Mean" ,line = list(color = 'rgba(0, 0, 0, 1)'), inherit = FALSE)
        p <- layout(p,title="", xaxis = list(title = "Inputs", tickvals=1:11, ticktext = paste0("Q", seq(0,100,10),"%")),
                    yaxis = list(title = ynamesmooth.summary()))
      }
      if (Ytype == 'categorical'){
        df$value <- factor(df$value, levels = levels(DOE$Y[, Yind]))
        p <- plot_ly(df, x=~Var1, y=~jitter(as.numeric(value)), color=~Var2, type="scatter", mode="markers", jitter = 0.1)
        Ylevels <- levels(df$value)
        p <- layout(p, title="", xaxis=list(title="Inputs", tickvals=1:11, ticktext = paste0("Q", seq(0,100,10),"%")),
                    yaxis = list(title = ynamesmooth.summary(), tickvals=1:length(Ylevels), ticktext = Ylevels), margin = margin)
        
      }
      return(p)
    })
  })
  
}
