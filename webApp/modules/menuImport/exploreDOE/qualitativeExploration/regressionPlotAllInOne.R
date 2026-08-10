source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)

###############################
#  Define client user interface
###############################

regressionPlotAllInOne.ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    uiOutput(ns('plot.allinone'))
  )
}

#####################
# Define server logic
#####################

regressionPlotAllInOne.server <- function(input, output, session, DOE, window.dimension) {
  ns <- session$ns

  # Plot dimensions for better visualization
  dimplot <- reactiveValues(scatter.width=NULL, scatter.height=NULL)
  
  Xcat <- reactive({
    req(DOE$Xinfos)
    cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
    return(DOE$xnamesmenu[cat])
  })
  
  Ycat <- reactive({
    req(DOE$Yinfos)
    cat <- which(DOE$Yinfos$type == 'categorical')
    return(DOE$ynamesmenu[cat])
  })
  
  choicesXallinone <- reactive({
    req(DOE$Xinfos,DOE$xnamesmenu, !is.null(Xcat()))
    l <- list()
    l[["Active"]] <- as.list(DOE$xnamesmenu)
    return(l)
  })
  
  xnameallinone <- callModule(dynamicSelectpicker.server, "chooseXallinone", label.title = "Choose Inputs", choices = choicesXallinone,
                              multiple = TRUE, livesearch = TRUE, selected = DOE$xnamesmenu)
  
  choicesYallinone <- reactive({
    req(DOE$xnamesmenu, DOE$ynamesmenu, DOE$Yinfos)
    l <- list()
    # Outputs first
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
    if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
    if (length(DOE$Yinfos$status.ids)>0) l[["Status Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$status.ids])
    return(l)
  })
  
  ynameallinone <- callModule(dynamicSelectpicker.server, "chooseYallinone", label.title = "Choose Outputs", choices = choicesYallinone,
                              multiple = TRUE, livesearch = TRUE, selected = DOE$ynamesmenu[DOE$Yinfos$int.ids])
  

  selectedAllInOne <- reactiveValues(X = NULL, Y = NULL, nX = NULL, nY = NULL)
  
  treillisInput <- reactiveValues(ncol = NULL, nrow = NULL)
  
  observeEvent(input$chooseXallclosed,{
    selectedAllInOne$nX <- length(xnameallinone())
    
    if(is.null(selectedAllInOne$X)){
      selectedAllInOne$X <- xnameallinone()
    }else{
      if(!identical(xnameallinone(), selectedAllInOne$X)){
        selectedAllInOne$X <- xnameallinone()
      }
    }
    
    if(!is.null(treillisInput$ncol)){
      if(treillisInput$ncol > selectedAllInOne$nX){
        treillisInput$ncol <- selectedAllInOne$nX
      }
    }
    
  })

  observeEvent(input$chooseYallclosed,{
    selectedAllInOne$nY <- length(ynameallinone())
    
    if(is.null(selectedAllInOne$Y)){
      selectedAllInOne$Y <- ynameallinone()
    }else{
      if(!identical(xnameallinone(), selectedAllInOne$Y)){
        selectedAllInOne$Y <- ynameallinone()
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
  
  observeEvent(c(input$chooseXallclosed,input$chooseYallclosed,treillisInput$nrow,treillisInput$ncol),{
    req(selectedAllInOne$nX, selectedAllInOne$nY)
      currentview$maxRow <- ceiling(selectedAllInOne$nY/treillisInput$nrow)
      currentview$maxCol <- ceiling(selectedAllInOne$nX/treillisInput$ncol)
  })
  
  observeEvent(c(input$chooseXallclosed,input$chooseYallclosed),{
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
  
  output$plot.allinone <- renderUI({
    req(DOE$XY)
    tagList(
      fluidRow(
        column(4,dynamicSelect.ui(ns("chooseYallinone"))),
        column(4,dynamicSelect.ui(ns("chooseXallinone"))),
        column(4,uiOutput(ns('ui.treillis')),align="right"),
        tags$script(paste0('$( "#', ns('chooseXallinone'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseXallclosed'),'", 1, {priority: "event"}); });')),
        tags$script(paste0('$( "#', ns('chooseXallinone'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXallclosed'),'", 1, {priority: "event"}); });')),
        tags$script(paste0('$( "#', ns('chooseYallinone'), '-select" ).on( "loaded.bs.select", function() { Shiny.onInputChange("',ns('chooseYallclosed'),'", 1, {priority: "event"}); });')),
        tags$script(paste0('$( "#', ns('chooseYallinone'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYallclosed'),'", 1, {priority: "event"}); });'))
      ),
      br(),
      fluidRow(
        column(12,uiOutput(ns("plot.allinone.inside")))
      )
    )
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
  
  observe({
    dimplot$scatter.width <- 0.95*window.dimension$width
    dimplot$scatter.height <- 0.75*window.dimension$height
  })
  
  output$plot.allinone.inside <- renderUI({
    req(dimplot$scatter.width,dimplot$scatter.height)
    plotlyOutput(ns("plotallinone.treillis"), width = paste0(dimplot$scatter.width,"px"), height = paste0(dimplot$scatter.height,"px"))%>% withSpinner()
  })
  
  output$plotallinone.treillis <- renderPlotly({
    req(DOE$XY, selectedAllInOne$nX, selectedAllInOne$nY, selectedAllInOne$X, selectedAllInOne$Y, treillisInput$ncol,
        treillisInput$nrow, currentview$idcurrentCol,currentview$idcurrentRow, currentview$maxRow, currentview$maxCol,
        isolate(!is.null(Xcat())), cancelOutput = TRUE)
    isolate({
      xynames <- c(DOE$xnames, DOE$ynames)
      xynamesvisu <- c(DOE$xnamesvisu, DOE$ynamesvisu)
      xynamesmenu <- c(DOE$xnamesmenu, DOE$ynamesmenu)
      xynamesallinone <- c(xnameallinone(), ynameallinone())
      req(all(xynamesallinone %in% xynamesmenu))
      dfnames <- xynames[xynamesallinone]
      dfnamesmenu <- xynamesallinone
      dfnamesvisu <- xynamesvisu[xynamesallinone]
      df <- DOE$XY[,dfnames]
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
    plots <- outer(idX,idY,Vectorize(function(i,j) {
      validPlot <- TRUE
      if (i>nX | j>(nX+nY) | j <= nX){
        validPlot <- FALSE
      }else{
        # check non-NA rows
        if (all(apply(df[, c(i,j)], 1, anyNA))){
          validPlot <- FALSE
        }
      }
      if (!validPlot){
        return(plotly_empty(type="scatter",mode = "markers"))
      }else{
        if (dfnamesmenu[j] %in% Ycat() & dfnamesmenu[i] %in% Xcat()){
          # categorical output and categorical input
          dftemp <- df[,c(dfnames[i],dfnames[j])]
          colnames(dftemp) <- c('x','y')
          dftemp[,'x'] <- as.factor(dftemp[,'x'])
          dftemp[,'y'] <- as.factor(dftemp[,'y'])
          nx <- length(levels(dftemp$x))
          ny <- length(levels(dftemp$y))
          p <- plot_ly(dftemp,x = ~jitter(as.numeric(x)), y = ~jitter(as.numeric(y)), color = ~y, 
                       type = 'scatter', mode = 'markers',showlegend = FALSE)%>%
            layout(xaxis=list(title = dfnamesvisu[i],tickvals=1:nx,ticktext=levels(dftemp$x)),
                   yaxis=list(title = dfnamesvisu[j],tickvals=1:ny,ticktext=levels(dftemp$y)), margin = margin)
        }
        if (!(dfnamesmenu[j] %in% Ycat()) & dfnamesmenu[i] %in% Xcat()){
          # numeric output and categorical input
          dftemp <- df[,c(dfnames[i],dfnames[j])]
          colnames(dftemp) <- c('x','y')
          dftemp[,'x'] <- as.factor(dftemp[,'x'])
          nx <- length(levels(dftemp$x))
          p <- plot_ly(dftemp,x = ~as.numeric(x), y = ~y, 
                       split = ~x, type = 'violin', box = list(visible = T),
                       points = 'all', jitter = 0.3, showlegend = FALSE)%>%
                  layout(xaxis=list(title = dfnamesvisu[i],tickvals=1:nx,ticktext=levels(dftemp$x)),
                         yaxis=list(title = dfnamesvisu[j]), margin = margin)
        }
        if (dfnamesmenu[j] %in% Ycat() & !(dfnamesmenu[i] %in% Xcat())){
          # categorical output and numeric input
          dftemp <- df[,c(dfnames[i],dfnames[j])]
          colnames(dftemp) <- c('x','y')
          dftemp[,'y'] <- as.factor(dftemp[,'y'])
          ny <- length(levels(dftemp$y))
          p <- plot_ly(dftemp, x = ~x, y = ~as.numeric(y),
                       split = ~y, type = 'scatter', mode = "markers", showlegend = FALSE)%>%
               layout(xaxis=list(title = dfnamesvisu[i]), 
                      yaxis=list(title = dfnamesvisu[j]),tickvals=1:ny,ticktext=levels(dftemp$y), margin = margin)
        }
        if (!(dfnamesmenu[j] %in% Ycat()) & !(dfnamesmenu[i] %in% Xcat())) {
          # numeric output and input (smoothing)
          lmfit <- lm(paste0("`", dfnames[j], "`~`", dfnames[i], "`"),data=df, x = TRUE)
          lmsort <- sort(lmfit$x[,2], index.return = TRUE)
          lmxplot <- lmsort$x
          lmyplot <- lmfit$fitted.values[lmsort$ix]
          
          p <- plot_ly(df, x = as.formula(paste0("~`", dfnames[i], "`")), 
                       y = as.formula(paste0("~`", dfnames[j], "`")),showlegend = FALSE, type="scatter", 
                       mode = "markers",marker = list(size = 6,color = 'rgb(31, 119, 180)'))%>%
            add_trace(x = lmxplot, y = lmyplot, type="scatter", mode = "lines",inherit = FALSE,showlegend = FALSE,line = list(color = 'rgb(255, 127, 14)'))%>%
            layout(xaxis=list(title=dfnamesvisu[i]), yaxis=list(title=dfnamesvisu[j]), margin = margin)
          
          out <- tryCatch({
            sfit <- loess(paste0("`", dfnames[j], "`~`", dfnames[i], "`"), data = df)
            sxplot <- unique(lmxplot)
            syplot <- predict(sfit,sxplot)
            p <- p %>% add_trace(x = sxplot, y = syplot, type="scatter", 
                                 mode = "lines",inherit = FALSE,
                                 showlegend = FALSE, 
                                 line = list(color = 'rgb(205, 12, 24)'))
          },
          error = function(err_msg){
            message(paste("Smoothing failed:", paste0("`", dfnames[j], "`~`", dfnames[i], "`")))
          }
          )
        }
        return(p)
      }}, SIMPLIFY = FALSE))
    return(plotly::subplot(plots, nrows = length(idY), shareX = TRUE, shareY = TRUE, widths=rep(1/treillisInput$ncol,treillisInput$ncol)))
  })
  
}
