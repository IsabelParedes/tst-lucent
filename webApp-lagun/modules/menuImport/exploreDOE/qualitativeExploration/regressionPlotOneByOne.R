source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)

plot.regression <- function(df, xname, yname, colorname, sizename, markname, catnames, showhist, xnamevisu, ynamevisu, colnamevisu, adapt.visu) {
  var.sel <- c(yname, xname)
  df.names <- c('y', 'x')
  idok <- !is.na(df[,xname]) & !is.na(df[,yname])
  
  if (adapt.visu){
    margin=list(b = -1, l = -1)
  }else{
    margin=NULL
  }
  
  if (is.na(sizename)){
    szn <- NULL
  }else{
    szn <- as.formula(paste0("~`",sizename, "`"))
  }
  if (is.na(markname)){
    mark <- NULL
  }else{
    mark <- as.formula(paste0("~`",markname, "`"))
  }
  
  # Remove NAs
  if (sum(idok)>0){
    if (is.element(xname,catnames)){
      # X-axis is a categorical input or a status output
      df <- df[idok,]
      if (!is.element(yname,catnames)){
        # Y-axis is a continuous variable
        # Violon plot, without color
        df <- df[,var.sel]
        colnames(df) <- df.names
        df <- df[order(df[,'x']),]
        df[,'x'] <- as.factor(df[,'x'])
        if (showhist){
          # Subplot with histograms below and on the left
          p <- subplot(
            plot_ly(data = df, y = ~y, type = "histogram", name=paste("Hist",yname)),
            plot_ly(df,x = ~x, y = ~y,split = ~x,type = 'violin',
                    box = list(visible = T),points = 'all',jitter = 0.3),
            plotly_empty(),
            plot_ly(data = df, x = ~x, type = "histogram", name=paste("Hist",xname)),
            nrows = 2, heights = c(0.8, 0.2), widths = c(0.2, 0.8), margin = 0,
            shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
          )
          p <- layout(p,title="", xaxis=list(visible=FALSE), xaxis2=list(visible=TRUE,title=xnamevisu), yaxis=list(title=ynamevisu), margin=margin)
        }else{
          # No histogram, juste a violin plot
          p <- plot_ly(df,x = ~x, y = ~y,split = ~x,type = 'violin',
                       box = list(visible = T),points = 'all',jitter = 0.3)
          p <- layout(p,title="", xaxis=list(title=xnamevisu), yaxis=list(title=ynamevisu), margin=margin)
        }
      }else{
        # Y-axis is also a categorical input or a status output
        # Jittered scatter plot, possibly with color
        nameplot <- "Jittered Scatter"
        if (!is.na(colorname)){
          # Color will be given by a variable
          var.sel <- c(var.sel, colorname)
          df.names <- c(df.names, colorname)
          if (is.element(colorname,catnames)){
            # If color is from a categorical variable, name is changed for nicer legend
            df[,colorname] <- paste(colorname,"=",as.factor(df[,colorname]))
          }
          clr <- as.formula(paste0("~`",colorname, "`"))
          nameplot <- NULL
        }else{
          clr <- NULL
        }
        if (!is.na(sizename)){
          # Size will be given by a variable
          var.sel <- c(var.sel, sizename)
          df.names <- c(df.names, sizename)
          nameplot <- NULL
        }
        if (!is.na(markname)){
          # Size will be given by a variable
          var.sel <- c(var.sel, markname)
          df.names <- c(df.names, markname)
          df[,markname] <- as.factor(df[,markname])
          nameplot <- NULL
        }
        df <- df[,var.sel]
        colnames(df) <- df.names
        df[,'x'] <- as.factor(df[,'x'])
        df[,'y'] <- as.factor(df[,'y'])
        nx <- length(levels(df$x))
        ny <- length(levels(df$y))
        if (showhist){
          # Subplot with histograms below and on the left
          p <- subplot(
            plot_ly(data = df, y = ~as.numeric(y), type = "histogram", name=paste("Hist",yname)),
            plot_ly(df,x = ~jitter(as.numeric(x)), y = ~jitter(as.numeric(y)), color = clr, size = szn, symbol = mark, type = 'scatter', mode = "markers", name=nameplot),
            plotly_empty(),
            plot_ly(data = df, x = ~as.numeric(x), type = "histogram", name=paste("Hist",xname)),
            nrows = 2, heights = c(0.8, 0.2), widths = c(0.2, 0.8), margin = 0,
            shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
          )
          p <- layout(p,title="", xaxis=list(visible=FALSE), xaxis2=list(visible=TRUE,title=xnamevisu,tickvals=1:nx,ticktext=levels(df$x)), 
                      yaxis=list(title=ynamevisu,tickvals=1:ny,ticktext=levels(df$y)), margin=margin)
        }else{
          # No histogram, just a jittered scatter plot
          p <- plot_ly(df,x = ~jitter(as.numeric(x)), y = ~jitter(as.numeric(y)), color = clr, size = szn, symbol = mark, type = 'scatter', mode = "markers", name=nameplot)
          p <- layout(p,title="", xaxis=list(title=xnamevisu,tickvals=1:nx,ticktext=levels(df$x)), yaxis=list(title=ynamevisu,tickvals=1:ny,ticktext=levels(df$y)), margin=margin)
        }
        if (!is.null(clr)){
          p <- p %>% colorbar(title = colnamevisu)
        }
      }
    }else{
      # X coordinate is a continuous variable
      df <- df[idok,]
      if (is.element(yname,catnames)){
        # Y-axis is a categorical variable
        df <- df[,var.sel]
        colnames(df) <- df.names
        df <- df[order(df[,'y']),]
        df[,'y'] <- as.factor(df[,'y'])
        if (showhist){
          # Subplot with histograms below and on the left
          p <- subplot(
            plot_ly(data = df, y = ~y, type = "histogram", name=paste("Hist",yname)),
            plot_ly(df,x = ~x, y = ~y, split = ~y, type = 'scatter', mode = 'markers', showlegend = FALSE),
            plotly_empty(),
            plot_ly(data = df, x = ~x, type = "histogram", name=paste("Hist",xname)),
            nrows = 2, heights = c(0.8, 0.2), widths = c(0.2, 0.8), margin = 0,
            shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
          )
          p <- layout(p,title="", xaxis=list(visible=FALSE), xaxis2=list(visible=TRUE,title=xnamevisu), yaxis=list(title=ynamevisu), margin=margin)
        }else{
          # No histogram, just a scatter plot
          p <- plot_ly(df, x = ~x, y = ~y, split = ~y, type = 'scatter', mode = 'markers', showlegend = FALSE)
          p <- layout(p,title="", xaxis=list(title=xnamevisu), yaxis=list(title=ynamevisu), margin=margin)
        }
      }else{
        # Y-axis is also a continuous variable
        nameplot <- "Scatter"
        if (!is.na(colorname)){
          # Color will be given by a variable
          var.sel <- c(var.sel, colorname)
          df.names <- c(df.names, colorname)
          if (is.element(colorname,catnames)){
            # If color is from a categorical variable, name is changed for nicer legend
            df[,colorname] <- paste(colorname,"=",as.factor(df[,colorname]))
            nameplot <- NULL
          }
          clr <- as.formula(paste0("~`",colorname, "`"))
        }else{
          clr <- NULL
        }
        if (!is.na(sizename)){
          # Size will be given by a variable
          var.sel <- c(var.sel, sizename)
          df.names <- c(df.names, sizename)
        }
        if (!is.na(markname)){
          # Symbol will be given by a variable
          var.sel <- c(var.sel, markname)
          df.names <- c(df.names, markname)
          df[,markname] <- as.factor(df[,markname])
          nameplot <- NULL
        }
        df <- df[,var.sel]
        colnames(df) <- df.names
        df <- df[order(df[,'x']),]
        fit <- loess(y ~ x, data = df)
        dfit <- data.frame(x = df$x, y = predict(fit))
        if (showhist){
          # Subplot with histograms below and on the left
          p <- subplot(
            plot_ly(data = df, y = ~y, type = "histogram", name=paste("Hist",yname)),
            plot_ly(data = df, x = ~x, y = ~y, mode = "markers", color = clr, size = szn, symbol = mark, type="scatter", name=nameplot, marker = list(color = "#636EFA"))%>%add_trace(x = dfit$x, y = dfit$y, type="scatter", mode = "lines", name = "Smooth",line = list(color = 'rgba(255,127,14,1)'),inherit = FALSE),
            plotly_empty(),
            plot_ly(data = df, x = ~x, type = "histogram", name=paste("Hist",xname)),
            nrows = 2, heights = c(0.8, 0.2), widths = c(0.2, 0.8), margin = 0,
            shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
          )
          p <- layout(p,title="", xaxis=list(visible=FALSE), xaxis2=list(visible=TRUE,title=xnamevisu), yaxis=list(title=ynamevisu), margin=margin)
        }else{
          # No histogram, juste a scatter plot
          p <- plot_ly(df, x = ~x, y = ~y, mode = "markers", color = clr, size = szn, symbol = mark, type="scatter", name=nameplot)%>%add_trace(x = dfit$x, y = dfit$y, type="scatter", mode = "lines", name = "Smooth",line = list(color = 'rgba(255,127,14,1)'),inherit = FALSE)
          p <- layout(p,title="", xaxis=list(title=xnamevisu), yaxis=list(title=ynamevisu), margin=margin)
        }
        if (!is.null(clr)){
          p <- p %>% colorbar(title = colnamevisu)
        }
      }
    }
  }else{
    # Only NAs
    p <- NULL
  }
  return(p)
}

###############################
#  Define client user interface
###############################

regressionPlotOneByOne.ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(2,dynamicSelect.ui(ns("chooseY"))),
      column(2,dynamicSelect.ui(ns("chooseX"))),
      column(2,dynamicSelect.ui(ns("chooseRegColor"))),
      column(2,dynamicSelect.ui(ns("chooseRegSize"))),
      column(2,dynamicSelect.ui(ns("chooseRegMark"))),
      column(2,switchInput(ns("showhist"), label = "Show Histograms"),align="right")
    ),
    uiOutput(ns('plot.reg'))
  )
}

#####################
# Define server logic
#####################

regressionPlotOneByOne.server <- function(input, output, session, DOE, window.dimension) {
  ns <- session$ns

  # Plot dimensions for better visualization
  dimplot <- reactiveValues(reg.height=NULL)
  observe({
    dimplot$reg.height <- 0.8*window.dimension$height
  })
  
  # Regression plot - One by One
  choicesX <- reactive({
    req(DOE$xnamesmenu, DOE$ynamesmenu,DOE$Yinfos)
    l <- list()
    # Inputs first
    l[["Active Inputs"]] <- as.list(DOE$xnamesmenu)
    # Then outputs
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
    if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
    if (length(DOE$Yinfos$status.ids)>0) l[["Status Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$status.ids])
    return(l)
  })
  
  xname <- callModule(dynamicSelectpicker.server, "chooseX", label.title = "X-axis", choices = choicesX,
                      multiple = FALSE, selected = choicesX()[1], livesearch = TRUE)
  
  Xcat <- reactive({
    req(DOE$Xinfos)
    cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
    return(DOE$xnamesmenu[cat])
  })
  
  choicesY <- reactive({
    req(DOE$xnamesmenu, DOE$ynamesmenu, DOE$Yinfos)
    l <- list()
    # Outputs first
    if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
    if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
    if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
    if (length(DOE$Yinfos$status.ids)>0) l[["Status Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$status.ids])
    # Then inputs
    l[["Active Inputs"]] <- as.list(DOE$xnamesmenu)
    return(l)
  })
  
  yname <- callModule(dynamicSelectpicker.server, "chooseY", label.title = "Y-axis", choices = choicesY,
                      selected = choicesY()[1], multiple = FALSE, livesearch = TRUE)
  
  choicesRegColor <- reactive({
    req(DOE$xnamesmenu, DOE$ynamesmenu, DOE$Yinfos, xname(), yname(), !is.null(Xcat()))
    l <- list()
    l[["None"]] <- as.list('None')
    if (!xor(xname() %in% c(Xcat(), DOE$ynamesmenu[DOE$Yinfos$type == "categorical"]), 
             yname() %in% c(Xcat(), DOE$ynamesmenu[DOE$Yinfos$type == "categorical"]))){
      # Colors are not activated if only one axis is categorical (i.e. for violin plots)
      # Inputs first
      l[["Active Inputs"]] <- as.list(DOE$xnamesmenu)
      # Then outputs
      if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
      if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
      if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
      if (length(DOE$Yinfos$status.ids)>0) l[["Status Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$status.ids])
    }
    return(l)
  })
  colorname <- callModule(dynamicSelectpicker.server, "chooseRegColor", label.title = "Color",
                          choices = choicesRegColor, multiple = FALSE, selected = choicesRegColor()[1], livesearch = TRUE)
  
  choicesRegSize<- reactive({
    req(DOE$xnamesmenu, DOE$ynamesmenu,DOE$Yinfos, xname(), yname(), !is.null(Xcat()))
    l <- list()
    l[["None"]] <- as.list('None')
    if (!xor(xname() %in% c(Xcat(), DOE$ynamesmenu[DOE$Yinfos$type == "categorical"]),
             yname() %in% c(Xcat(), DOE$ynamesmenu[DOE$Yinfos$type == "categorical"]))){
      # Size is not activated if only one axis is categorical (i.e. for violin plots)
      # Can only choose from continuous variables
      cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
      num <- setdiff(1:DOE$nX, cat)
      # Inputs first
      if (length(num)>0) l[["Active Inputs"]] <- as.list(DOE$xnamesmenu[num])
      # Then outputs
      if (length(DOE$Yinfos$int.ids)>0) l[["Interest Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
      if (length(DOE$Yinfos$control.ids)>0) l[["Control Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
      if (length(DOE$Yinfos$const.ids)>0) l[["Constant Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
    }
    return(l)
  })
  sizename <- callModule(dynamicSelectpicker.server, "chooseRegSize", label.title = "Size",
                         choices = choicesRegSize, multiple = FALSE, selected = choicesRegSize()[1], livesearch = TRUE)
  
  choicesRegMark <- reactive({
    req(DOE$xnamesmenu, DOE$ynamesmenu,DOE$Yinfos, xname(), yname(), !is.null(Xcat()))
    l <- list()
    l[["None"]] <- as.list('None')
    if (!xor(xname() %in% c(Xcat(), DOE$ynamesmenu[DOE$Yinfos$type == "categorical"]),
             yname() %in% c(Xcat(), DOE$ynamesmenu[DOE$Yinfos$type == "categorical"]))){
      # Markers are not activated if only one axis is categorical (i.e. for violin plots)
      # Can only choose from categorical variables
      cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
      # Inputs first
      if (length(cat)>0) l[["Active Inputs"]] <- as.list(DOE$xnamesmenu[cat])
      # Then outputs
      if (sum(DOE$Yinfos$type == "categorical") > 0) l[["Categorical Outputs"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$type == "categorical"])
    }
    return(l)
  })
  markname <- callModule(dynamicSelectpicker.server, "chooseRegMark", label.title = "Mark",
                         choices = choicesRegMark, multiple = FALSE, selected = choicesRegMark()[1], livesearch = TRUE)
  
  output$regression <- renderPlotly({
    req(DOE$XY, xname(), yname(), colorname(), sizename(), markname(), !is.null(Xcat()), !is.null(input$showhist), cancelOutput = TRUE)
    xynames <- c(DOE$xnames, DOE$ynames, DOE$Fnames)
    xynamesmenu <- c(DOE$xnamesmenu, DOE$ynamesmenu, DOE$Fnamesmenu)
    req(all(c(xname(), yname()) %in% xynamesmenu))
    xynamesvisu <- c(DOE$xnamesvisu, DOE$ynamesvisu, DOE$Fnamesvisu)
    plot.regression(DOE$XY, xname = xynames[xname()], yname = xynames[yname()], xynames[colorname()], xynames[sizename()],xynames[markname()], 
                    c(DOE$xnames[Xcat()],DOE$ynames[DOE$Yinfos$type == 'categorical']),input$showhist,xynamesvisu[xname()],xynamesvisu[yname()],xynamesvisu[colorname()],DOE$adapt.visu)
  })
  
  output$plot.reg <- renderUI({
    req(dimplot$reg.height)
    plotlyOutput(ns("regression"), width = "100%", height=paste0(dimplot$reg.height,"px"))%>% withSpinner()
  })
  
}
