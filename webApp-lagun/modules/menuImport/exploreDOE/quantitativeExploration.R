#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module quantitativeExploration
source("modules/shared/dynamicSelect.R", local = TRUE)

# FIX reactivy of MLtype -> plotML, plotly keep the old layout

computeHSICDCOR <- function(Y, X, nrepML, callback) {
  dimx <- ncol(X)
  dimy <- ncol(Y)
  nobs <- nrow(Y)
  MLtempd <- array(0,dim = c(nrepML,dimx,dimy))
  
  nsamples <- ceiling(0.8*nobs)
  samples <- matrix(NA,nsamples,nrepML)
  for (r in 1:nrepML){
    samples[,r] <- sample(nobs, size = nsamples, replace = TRUE)
  }
  for (j in 1:dimy){
    for (i in 1:dimx){
      for (r in 1:nrepML){
        idsample <- samples[,r]
        idnotna <- idsample[!is.na(X[idsample,i]) & !is.na(Y[idsample,j])]
        if (length(idnotna)){
          MLtempd[r,i,j] <- dcor(X[idnotna,i,drop=FALSE],Y[idnotna,j,drop=FALSE])
        }else{
          MLtempd[r,i,j] <- NA
        }
      }
    }
    callback(j)
  }
  return(list(DCOR = MLtempd))
}

quantitativeExploration.ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(type = "tabs",
              tabPanel(h4("Correlation"),
                       tagList(
                         br(),
                         uiOutput(ns('ui.cor'))
                       ), value=ns("COR")
              ),
              tabPanel(h4("Generalized Correlation"), 
                       tagList(
                         br(),
                         uiOutput(ns('ui.hsicdcor'))
                       ), value=ns("DCOR")
              ),
              tabPanel(h4("Rules"), 
                       tagList(
                         br(),
                         uiOutput(ns('ui.sirus'))
                       ), value=ns("sirus")
              ),
              tabPanel(h4("Download Results"), 
                       tagList(
                         br(),
                         uiOutput(ns('ui.download'))
                       ), value = ns("DWLD")
              ), id = ns("clickedtab")
  )
}

quantitativeExploration.server <- function(input, output, session, DOE, window.dimension, settings) {
  
  ns <- session$ns
  
  # Plot dimensions for better visualization
  dimplot <- reactiveValues(cor.width=NULL, cor.height=NULL, dcor.width=NULL, dcor.height=NULL)
  
  observeEvent(c(input$chooseXcorclosed,input$chooseYcorclosed),{
    isolate({
      nvarX <- length(xnamecor())
      nvarY <- length(ynamecor())
    })
    dimplot$cor.width <- max(50*nvarX,0.95*window.dimension$width)
    dimplot$cor.height <- max(50*nvarY,0.8*window.dimension$height)
  })
  
  observeEvent(c(input$chooseXhsicdcorclosed,input$chooseYhsicdcorclosed),{
    isolate({
      nvarX <- length(xnamehsicdcor())
      nvarY <- length(ynamehsicdcor())
    })
    dimplot$dcor.width <- max(50*nvarX,0.95*window.dimension$width)
    dimplot$dcor.height <- max(50*nvarY,0.8*window.dimension$height)
  })
  
  ML <- reactiveValues(HSIC = NULL, DCOR = NULL, DCOR.idclicked = NULL, COR = NULL, COR.idclickedx = NULL, COR.idclickedy = NULL)
  
  Xcat <- reactive({
    req(DOE$Xinfos)
    cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
    return(cat)
  })
  
  Ycat <- reactive({
    req(DOE$Yinfos)
    cat <- which(DOE$Yinfos$type == 'categorical')
    return(cat)
  })
  
  
  
  # Correlation tabpanel
  
  # reset correlation and generalized correlation when DOE is updated
  observeEvent(list(DOE$Xinfos, DOE$Yinfos, DOE$XY), {
    ML$COR <- NULL
    ML$DCOR <- NULL
  })
  
  choicesXcor <- reactive({
    req(DOE$Xinfos,DOE$xnamesmenu, !is.null(Xcat()), DOE$ynamesmenu, DOE$Yinfos)
    # Keep only numerical inputs
    l <- list()
    # Inputs
    idnum <- setdiff(1:DOE$nX, Xcat())
    if (length(idnum)>0) l[["Active Inputs"]] <- as.list(DOE$xnamesmenu[idnum])
    # Outputs
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[setdiff(DOE$Yinfos$int.ids, Ycat())])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[setdiff(DOE$Yinfos$control.ids, Ycat())])
    if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[setdiff(DOE$Yinfos$const.ids, Ycat())])
    return(l)
  })
  
  xnamecor <- callModule(dynamicSelectpicker.server, "chooseXcor", label.title = "X-axis", choices = choicesXcor,
                              multiple = TRUE, livesearch = TRUE, selected = DOE$xnamesmenu)
  
  choicesYcor <- reactive({
    req(DOE$Xinfos,DOE$xnamesmenu, !is.null(Xcat()), DOE$ynamesmenu, DOE$Yinfos)
    # Keep only numerical inputs
    l <- list()
    # Inputs
    idnum <- setdiff(1:DOE$nX, Xcat())
    if (length(idnum)>0) l[["Active Inputs"]] <- as.list(DOE$xnamesmenu[idnum])
    # Outputs
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[setdiff(DOE$Yinfos$int.ids, Ycat())])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[setdiff(DOE$Yinfos$control.ids, Ycat())])
    if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[setdiff(DOE$Yinfos$const.ids, Ycat())])
    return(l)
  })
  
  ynamecor <- callModule(dynamicSelectpicker.server, "chooseYcor", label.title = "Y-axis", choices = choicesYcor, 
                      selected = DOE$ynamesmenu[DOE$Yinfos$int.ids], multiple = TRUE, livesearch = TRUE)

  observe({
    # Initialize dimensions with default choices
    req(window.dimension$width, window.dimension$height)
    nvarX <- DOE$nX
    nvarY <- length(DOE$Yinfos$int.ids)
    dimplot$cor.width <- max(50*nvarX,0.95*window.dimension$width)
    dimplot$cor.height <- max(50*nvarY,0.8*window.dimension$height)
    dimplot$dcor.width <- max(50*nvarX,0.95*window.dimension$width)
    dimplot$dcor.height <- max(50*nvarY,0.8*window.dimension$height)
  })
  
  output$ui.cor <- renderUI({
    req(DOE$XY,dimplot$cor.width,dimplot$cor.height)
    if (is.null(ML$COR)){
      if (length(unlist(choicesYcor())) > 0){
        fluidRow(column(12,actionButton(ns("computecor"),label=HTML(paste("Compute","Correlations",sep="<br>")), class = "btn-warning")), align="center")
      }else{
        HTML('<h4> Not available for categorical outputs.')
      }
    }else{
      tagList(
        fluidRow(
          column(4,dynamicSelect.ui(ns("chooseYcor"))),
          column(4,dynamicSelect.ui(ns("chooseXcor"))),
          column(4,""),
          tags$script(paste0('$( "#', ns('chooseXcor'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseXcorclosed'),'", 1, {priority: "event"}); });')),
          tags$script(paste0('$( "#', ns('chooseXcor'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXcorclosed'),'", 1, {priority: "event"}); });')),
          tags$script(paste0('$( "#', ns('chooseYcor'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseYcorclosed'),'", 1, {priority: "event"}); });')),
          tags$script(paste0('$( "#', ns('chooseYcor'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYcorclosed'),'", 1, {priority: "event"}); });'))
        ),
        br(),
        fluidRow(
          column(12,
                 plotlyOutput(ns("plot.correl"), width = paste0(dimplot$cor.width,"px"), height = paste0(dimplot$cor.height,"px"))%>% withSpinner(),
                 style = paste0("overflow-x: scroll;overflow-y: scroll;max-height: ",window.dimension$height,"px;max-width: ",window.dimension$width,"px;")
          )
        )
      )
    }
  })

  observeEvent(input$computecor,{
    req(DOE$Yinfos, DOE$xnames, !is.null(Xcat()), !is.null(Ycat()), length(unlist(choicesYcor())) > 0)
    xnames <- DOE$xnames[setdiff(1:DOE$nX, Xcat())]
    ynames <- DOE$ynames[setdiff(c(DOE$Yinfos$int.ids,DOE$Yinfos$control.ids,DOE$Yinfos$const.ids), Ycat())]
    data <- cbind(DOE$Y[,ynames,drop=FALSE], DOE$X[,xnames,drop=FALSE])
    ML$COR <- cor(data, use="pairwise.complete.obs")
    xynamesmenu <- c(names(ynames), names(xnames))
    dimnames(ML$COR) <- list(xynamesmenu, xynamesmenu)
  })
  
  plotcor <- reactiveValues(generated=FALSE)
  output$plot.correl <- renderPlotly({
    req(ML$COR,input$chooseXcorclosed,input$chooseYcorclosed, cancelOutput = TRUE)
    if(!is.null(isolate(xnamecor())) & !is.null(isolate(ynamecor()))){
      isolate({
        DOEXYNamesVisu <- c(DOE$ynamesvisu, DOE$xnamesvisu)
        cc <- ML$COR[ynamecor(), xnamecor(), drop=FALSE]
        text.mat <- matrix("Click me !", nrow(cc), ncol(cc))
        plot_ly(source = "heatplotCOR") %>%
          add_heatmap(
            x = xnamecor(), 
            y = ynamecor(), 
            z = cc, colors = "RdBu", text = text.mat,   
            type = "heatmap", zauto = FALSE, zmin = -1, zmax = 1,reversescale=TRUE
          )%>%
          layout(xaxis = list(title = "",ticktext = DOEXYNamesVisu[xnamecor()], tickmode="array", tickvals = 0:length(xnamecor())), 
                 yaxis = list(title = "",ticktext = DOEXYNamesVisu[ynamecor()], tickmode="array", tickvals = 0:length(ynamecor())))
      })
    }
  })
  
  observe({
    req(ML$COR,input$chooseXcorclosed,input$chooseYcorclosed)
    click_event <- event_data("plotly_click", source = "heatplotCOR")
    runjs("Shiny.setInputValue('plotly_click-heatplotCOR', null);")
    if (length(click_event)) {
      idx <- click_event$x
      idy <- click_event$y
      ML$COR.idclickedx <- idx
      ML$COR.idclickedy <- idy
      showModal(modalDialog(
        title = paste("Correlation between ",idy,"and",idx),
        plotlyOutput(ns("scatterplotCOR")),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  output$scatterplotCOR <- renderPlotly({
    req(ML$COR.idclickedx,ML$COR.idclickedy, cancelOutput = TRUE)
    DOEXYNames <- c(DOE$ynames, DOE$xnames)
    DOEXYNamesVisu <- c(DOE$ynamesvisu, DOE$xnamesvisu)
    df <- DOE$XY[,c(DOEXYNames[ML$COR.idclickedx], DOEXYNames[ML$COR.idclickedy])]
    # Remove NAs
    idok <- apply(!is.na(df),1,all)
    df <- df[idok,]
    colnames(df) <- c("x","y")
    yhat <- fitted(lm(y ~ x, data = df))
    if (DOE$adapt.visu){
      margin=list(b = -1, l = -1)
    }else{
      margin=NULL
    }
    p <- plot_ly(df, x = ~x) %>%
      add_markers(y = ~y) %>%
      add_lines(y = ~yhat) %>%
      layout(title = paste("Correlation =",signif(ML$COR[ML$COR.idclickedy, ML$COR.idclickedx],4)),
             xaxis = list(title = DOEXYNamesVisu[ML$COR.idclickedx]), 
             yaxis = list(title = DOEXYNamesVisu[ML$COR.idclickedy]), 
             showlegend = FALSE, margin = margin)
    return(p)
  })
  
  # Generalized Correlation tabpanel
  
  xnamehsicdcor <- callModule(dynamicSelectpicker.server, "chooseXhsicdcor", label.title = "X-axis", choices = choicesXcor,
                         multiple = TRUE, livesearch = TRUE, selected = DOE$xnamesmenu)
  
  ynamehsicdcor <- callModule(dynamicSelectpicker.server, "chooseYhsicdcor", label.title = "Y-axis", choices = choicesYcor, 
                         selected = DOE$ynamesmenu[DOE$Yinfos$int.ids], multiple = TRUE, livesearch = TRUE)
  
  output$ui.hsicdcor <- renderUI({
    req(DOE$XY,dimplot$dcor.width,dimplot$dcor.height)
    if (is.null(ML$DCOR)){
      if (length(unlist(choicesYcor())) > 0){
        fluidRow(column(12,actionButton(ns("computedcor"),label=HTML(paste("Compute","Generalized Correlations",sep="<br>")), class = "btn-warning")), align="center")
      }else{
        HTML('<h4> Not available for categorical outputs.')
      }
    }else{
    tagList(
      fluidRow(
        column(4,dynamicSelect.ui(ns("chooseYhsicdcor"))),
        column(4,dynamicSelect.ui(ns("chooseXhsicdcor"))),
        column(4,""),
        tags$script(paste0('$( "#', ns('chooseXhsicdcor'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseXhsicdcorclosed'),'", 1, {priority: "event"}); });')),
        tags$script(paste0('$( "#', ns('chooseXhsicdcor'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXhsicdcorclosed'),'", 1, {priority: "event"}); });')),
        tags$script(paste0('$( "#', ns('chooseYhsicdcor'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseYhsicdcorclosed'),'", 1, {priority: "event"}); });')),
        tags$script(paste0('$( "#', ns('chooseYhsicdcor'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYhsicdcorclosed'),'", 1, {priority: "event"}); });'))
      ),
      br(),
      fluidRow(
        column(12,
               plotlyOutput(ns("plot.hsicdcor"), width = paste0(dimplot$dcor.width,"px"), height = paste0(dimplot$dcor.height,"px"))%>% withSpinner(),
               style = paste0("overflow-x: scroll;overflow-y: scroll;max-height: ",window.dimension$height,"px;max-width: ",window.dimension$width,"px;")
        )
      )
    )
    }
  })
  
  observeEvent(input$computedcor,{
    req(DOE$Yinfos, DOE$xnames, !is.null(Xcat()), !is.null(Ycat()), length(unlist(choicesYcor())) > 0)
    showModal(modalDialog(HTML(paste(
      "Computing generalized correlations between all outputs and inputs (except categorical ones).",
      "Depending on the number of inputs/outputs the computation may take a while.", 
      "If you close this window it is not advised to navigate in other panels until the computation is done.",
      "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
      size = 'l')
    )
    nVar <- length(c(setdiff(1:DOE$nX, Xcat()), setdiff(c(DOE$Yinfos$int.ids, DOE$Yinfos$control.ids, DOE$Yinfos$const.ids), Ycat())))
    nrepML <- settings$nrepML
    dataDCOR <- cbind(DOE$X[, setdiff(1:DOE$nX, Xcat()), drop=FALSE],
                      DOE$Y[, setdiff(c(DOE$Yinfos$int.ids, DOE$Yinfos$control.ids, DOE$Yinfos$const.ids), Ycat()), drop=FALSE]
                      )
    callback <- function(r) {
      incProgress(1/nVar, detail = paste("Variable", r,"/",nVar)) 
    }
    withProgress(message = 'Computing Generalized Correlations...', value = 0, {
      MLtemp <- computeHSICDCOR(dataDCOR, dataDCOR, nrepML, callback)
    })
    ML$DCOR <- MLtemp$DCOR
    xynames <- c(DOE$xnames[setdiff(1:DOE$nX, Xcat())],
                 DOE$ynames[setdiff(c(DOE$Yinfos$int.ids, DOE$Yinfos$control.ids, DOE$Yinfos$const.ids), Ycat())])
    xynamesmenu <- names(xynames)
    dimnames(ML$DCOR) <- list(NULL, xynamesmenu, xynamesmenu)
    removeModal()
  })
  
  plothsicdcor <- reactiveValues(generated=FALSE)
  output$plot.hsicdcor <- renderPlotly({
    req(ML$DCOR,input$chooseXhsicdcorclosed,input$chooseYhsicdcorclosed, cancelOutput = TRUE)
    if(!is.null(isolate(xnamehsicdcor())) & !is.null(isolate(ynamehsicdcor()))){
      isolate({
        DOEXYNamesVisu <- c(DOE$ynamesvisu, DOE$xnamesvisu)
        cc <- t(apply(ML$DCOR[, xnamehsicdcor(), ynamehsicdcor(), drop=FALSE], c(2,3), median))
        text.mat <- matrix("Click me !", nrow(cc), ncol(cc))
        plot_ly(source = "heatplotDCOR") %>%
          add_heatmap(
            x = xnamehsicdcor(), 
            y = ynamehsicdcor(), 
            z = cc, colors = "RdBu", text = text.mat,   
            type = "heatmap", zauto = FALSE, zmin = -1, zmax = 1,reversescale=TRUE
          )%>%
          layout(xaxis = list(title = "",ticktext = DOEXYNamesVisu[xnamehsicdcor()], tickmode="array", tickvals = 0:length(xnamehsicdcor())), 
                 yaxis = list(title = "",ticktext = DOEXYNamesVisu[ynamehsicdcor()], tickmode="array", tickvals = 0:length(ynamehsicdcor())))
      })
    }
  })
  
  observe({
    req(ML$DCOR,input$chooseXhsicdcorclosed,input$chooseYhsicdcorclosed)
    click_event <- event_data("plotly_click", source = "heatplotDCOR")
    runjs("Shiny.setInputValue('plotly_click-heatplotDCOR', null);")
    if (length(click_event)) {
      idy <- click_event$y
      ML$DCOR.idclicked <- idy
      showModal(modalDialog(
        title = paste("Distance Correlation for ",idy),
        plotlyOutput(ns("boxplotDCOR")),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  output$boxplotDCOR <- renderPlotly({
    req(ML$DCOR.idclicked, cancelOutput = TRUE)
    DOEXYNamesVisu <- c(DOE$ynamesvisu, DOE$xnamesvisu)
    df <- ML$DCOR[, xnamehsicdcor(), ML$DCOR.idclicked]
    df <- melt(df)
    p <- plot_ly(df, x = ~Var2, y = ~value, type = "box")%>%
      layout(title = "Distance Correlation", 
             xaxis = list(title = "Inputs"), 
             yaxis = list(title = DOEXYNamesVisu[ML$DCOR.idclicked], range=c(0,1)))
    return(p)
  })
  
  
  # sirus tabpanel
  
  choicesXsirus <- reactive({
    req(DOE$Xinfos, DOE$xnamesmenu, DOE$idon)
    idon <- DOE$idon
    idoff <- setdiff(1:DOE$nX,idon)
    candidateson <- DOE$xnamesmenu[idon]
    candidatesoff <- DOE$xnamesmenu[idoff]
    l <- list()
    if (length(idon)>0) l[["Active Inputs"]] <- as.list(candidateson)
    if (length(idoff)>0) l[["Non-Active Inputs"]] <- as.list(candidatesoff)
    return(l)
  })
  
  choicesYsirus <- reactive({
    req(DOE$ynamesmenu, DOE$Yinfos)
    l <- list()
    # Outputs
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
    if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
    return(l)
  })
  
  xnamesirus <- callModule(dynamicSelectpicker.server, "chooseXsirus", label.title = "Select Inputs X", choices = choicesXsirus,
                              multiple = TRUE, livesearch = TRUE, selected = DOE$xnamesmenu[DOE$idon])
  
  ynamesirus <- callModule(dynamicSelectpicker.server, "chooseYsirus", label.title = "Select Output Y", choices = choicesYsirus, 
                           multiple = FALSE, livesearch = TRUE, selected = NULL)
  
  sirusModel <- reactiveValues(ruleList = NULL, plotCVerror = NULL, plotCVstab = NULL)
  
  output$UIchooseYsirus <- renderUI({
    classif <- FALSE
    if (!is.null(ynamesirus())){
      yind <- which(DOE$ynamesmenu == ynamesirus())
      if (DOE$Yinfos$type[[yind]] == 'categorical'){
        classif <- TRUE
        Ylevels <- levels(DOE$Y[, yind])
      }
    }
   if (classif) {
     selectInput(ns('selectYlevel'), label = "Select output Level", choices = Ylevels)
   }else{
     NULL
   }
  })
  
  output$ui.sirus <- renderUI({
    req(DOE$XY, DOE$Yinfos)
    if (length(unlist(choicesYsirus())) > 0){
      tagList(
        fluidRow(
          column(3, dynamicSelect.ui(ns("chooseYsirus")), uiOutput(ns("UIchooseYsirus"))),
          column(3, dynamicSelect.ui(ns("chooseXsirus"))),
          column(5, actionButton(ns("computesirus"), label=HTML(paste("Compute SIRUS", "Rule List", sep="<br>")),
                                 width = '80%', class = "btn-warning"), offset = 1)
        ),
        fluidRow(
          column(12, DT::dataTableOutput(ns('sirusRuleList')))
        ),
        hr(),
        fluidRow(
          column(1, ""),
          column(5, plotOutput(ns('sirusPlotError'))),
          column(5, plotOutput(ns('sirusPlotStab'))),
          column(1, "")
        )
      )
    }else{
      HTML('<h4> Not available for categorical outputs.')
    }
  })
  
  observeEvent(input$computesirus, {
    req(DOE$XY, ynamesirus(), xnamesirus())
    if (length(sirusModel$ruleList) == 0){
      sirusModel$ruleList <- as.list(rep(NA, DOE$nY))
      sirusModel$plotCVerror <- as.list(rep(NA, DOE$nY))
      sirusModel$plotCVstab <- as.list(rep(NA, DOE$nY))
    }
    yind <- which(DOE$ynamesmenu == ynamesirus())
    classif <- DOE$Yinfos$type[[yind]] == 'categorical'
    if (classif){
      y <- rep(0, nrow(DOE$Y))
      y[DOE$Y[, yind] == input$selectYlevel] <- 1
      p0criterion <- 'p0.pred'
    }else{
      y <- DOE$Y[, yind]
      p0criterion <- 'p0.stab'
    }
    data <- DOE$X
    for (j in Xcat()){
      data[, j] <- as.factor(data[, j])
    }
    xind <- as.numeric(sapply(xnamesirus(), function(xname){which(DOE$xnamesmenu == xname)}))
    data <- data[, xind, drop=F]
    if (anyNA(data) | anyNA(y)){
      showModal(modalDialog(HTML("SIRUS does not handle missing values."),
                            title = "Warning", size = 'l'))
    }else{
      withProgress(message = 'Fitting SIRUS...', value = 0.2, {
        sirusCVgrid <- suppressWarnings(sirus.cv(data = data, y = y))
        plotCV <- sirus.plot.cv(sirusCVgrid)
        sirusModel$plotCVerror[[yind]] <- plotCV$error
        sirusModel$plotCVstab[[yind]] <- plotCV$stab
        validTuning <- max(sirusCVgrid$error.grid.p0$num.rules.mean) >= 2
        if (validTuning){
          sirusm <- suppressWarnings(sirus.fit(data = data, y = y, verbose = F, p0 = as.numeric(sirusCVgrid[p0criterion])))
          sirusModel$ruleList[[yind]] <- sirus.print(sirusm)
        }
      })
      if (validTuning){
        if (classif){
          ruleList <- sirusModel$ruleList[[yind]]
          ruleList[1] <- gsub('class 1', paste('class', input$selectYlevel), ruleList[1])
          ruleOutput <- paste0('P(', ynamesirus(), ' = ', input$selectYlevel, ') =', collapse = '')
        }else{
          ruleList <- sirusModel$ruleList[[yind]][,2]
          ruleList[1] <- gsub('y', ynamesirus(), ruleList[1])
          ruleOutput <- paste0(ynamesirus(), ' =', collapse = '')
        }
        ruleList[-1] <- sapply(ruleList[-1], function(rule){
          ruleSplit <- strsplit(rule, 'then')[[1]]
          ruleThen <- strsplit(ruleSplit[2], 'else')[[1]]
          ruleThen <- paste(ruleOutput, ruleThen, sep = '')
          ruleThen <- paste0(ruleThen, collapse = 'else ')
          rule <- paste(ruleSplit[1], ruleThen, sep = 'then ')
        })
        if (classif){
          sirusModel$ruleList[[yind]] <- ruleList
        }else{
          sirusModel$ruleList[[yind]][,2] <- ruleList
        }
      }
    }
  })
  
  output$sirusRuleList <- DT::renderDataTable({
    req(sirusModel$ruleList, ynamesirus())
    isolate({
      yind <- which(DOE$ynamesmenu == ynamesirus())
      if (!anyNA(sirusModel$ruleList[[yind]])){
        dfrules <- as.data.frame(sirusModel$ruleList[[yind]])
        if (ncol(dfrules) == 2){dfnames <- c('Weights', 'Rules')}else{dfnames <- c('Rules')}
        colnames(dfrules) <- dfnames
        row.names(dfrules)[1] <- 'Summary'
        row.names(dfrules)[2:nrow(dfrules)] <- paste('Rule', 1:(nrow(dfrules) - 1))
        DT::datatable(
          dfrules,
          extensions = c('FixedColumns', 'Scroller'),
          options = list(
            dom = 'Brtip', ordering = FALSE,
            buttons = list(list(extend = 'colvis', columns = 1:ncol(dfrules))),
            scrollX = TRUE, scrollY = 400, scroller = TRUE, fixedColumns = TRUE
          )
        ) %>% 
          formatStyle(
            0, target = "row",
            fontWeight = styleEqual(c("Summary"), c("bold"))
          )
      }
    })
  })
  
  output$sirusPlotError <- renderPlot({
    req(sirusModel$plotCVerror, ynamesirus())
    yind <- which(DOE$ynamesmenu == ynamesirus())
    sirusModel$plotCVerror[[yind]]
  })
  
  output$sirusPlotStab <- renderPlot({
    req(sirusModel$plotCVstab, ynamesirus())
    yind <- which(DOE$ynamesmenu == ynamesirus())
    sirusModel$plotCVstab[[yind]]
  })
  
  
  # Download tabpanel
  
  output$ui.download <- renderUI({
    req(any(!is.null(ML$COR),!is.null(ML$DCOR)))
    ui <- tagList()
    if (!is.null(ML$COR)){
      ui[[1]] <- downloadButton(ns("downloadCOR"), label = "Download Correlations")
      if (!is.null(ML$DCOR)){
        ui[[2]] <- downloadButton(ns("downloadDCOR"), label = "Download Generalized Correlations")
      }
    }else{
      ui[[1]] <- downloadButton(ns("downloadDCOR"), label = "Download Generalized Correlations")
    }
    return(ui)
  })
  
  output$downloadCOR <- downloadHandler(
    filename = 'MLCOR.csv',
    content = function(con) {
      write.csv(x = ML$COR, file = con, row.names = T, quote = F)
    }
  )
  output$downloadDCOR <- downloadHandler(
    filename = 'MLDCOR.csv',
    content = function(con) {
      df <- t(apply(ML$DCOR,c(2,3),median))
      write.csv(x = df, file = con, row.names = T, quote = F)
    }
  )
  
  return(ML)
}
