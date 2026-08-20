###############################
#  Define client user interface
###############################

inputsPlot.ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("plotoptim.dynui"))
  )
}

#####################
# Define server logic
#####################

inputsPlot.server <- function(input, output, session, define, data.plot) {
  ns <- session$ns

  output$plotoptim.dynui <- renderUI({
    req(define$COformulation$idO, cancelOutput = TRUE)
    tl <- tagList(
      fluidRow(
        column(12, plotlyOutput(ns("inputsPlot"))),
      )
    )
  })

  output$inputsPlot <- renderPlotly({
    req(data.plot$dataX, cancelOutput = TRUE)

    p <- plot_ly()
    for(i in seq_len(ncol(data.plot$dataX))) {
      var <- define$Xinfos$Xinfos[[i]]
      name <- colnames(data.plot$dataX)[i]
      x <- data.plot$rowNames
      text <- as.vector(data.plot$dataX[, i]) # y axis values, not normalized (used for tooltip)
      y <- text # y axis values, to be normalized
      if (var$type == "numeric") {
        lb <- var$bounds[1]
        ub <- var$bounds[2]
        y <- as.vector(as.numeric(data.plot$dataX[, i]) - lb) / (ub - lb) # normalized
      }
      if (var$type == "categorical") {
        catMap <- list()
        for (j in seq_len(var$nlevels)) {
          catMap[[toString(var$levels[[j]])]] <- j
        }
        lb <- 1
        ub <- var$nlevels
        y <- unlist(lapply(data.plot$dataX[, i], function(row) {
          (catMap[[toString(row)]] - 1) / (var$nlevels - 1) # normalized
        }))
      }
      p <- add_trace(
        p,
        x = x,
        y = y,
        text = text,
        name = name,
        hovertemplate = paste0(name, " = %{text} at iteration %{x} <extra></extra>"),
        type = "scatter", 
        mode = "lines+markers"
      )
    }
    p <- layout(
      p,
      xaxis = list(
        title = "Iteration_Simulation",
        categoryorder = "array",
        categoryarray = data.plot$rowNames
      ),
      yaxis = list(
        title = "Normalized Inputs",
        zeroline = FALSE
      )
    )
    return(p)
  })
}
