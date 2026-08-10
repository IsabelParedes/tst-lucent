respect.constraints <- function(Y, signconstr, thconstr) {
  return(apply(Y, 1, function(x) {
    x * signconstr > signconstr * thconstr
  }))
}

###############################
#  Define client user interface
###############################

resultsPlot.ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(4, dynamicSelectpicker.ui(ns("chooseYobj1"))),
      column(4, dynamicSelectpicker.ui(ns("chooseYobj2"))),
      column(4, "")
    ),
    uiOutput(ns("plotoptim.dynui"))
  )
}

#####################
# Define server logic
#####################

resultsPlot.server <- function(input, output, session, define, data.plot) {
  ns <- session$ns

  output$plotoptim.dynui <- renderUI({
    req(define$COformulation$idO, cancelOutput = TRUE)
    if (length(define$COformulation$idO) == 1) {
      if (length(define$COformulation$idC) > 1) {
        tl <- tagList(
          fluidRow(
            column(12, plotlyOutput(ns("plotObj"))),
          ),
          fluidRow(
            column(12, plotlyOutput(ns("plotConstr")))
          )
        )
      } else {
        tl <- tagList(
          fluidRow(
            column(12, plotlyOutput(ns("plotObj"))),
          )
        )
      }
    } else {
      if (length(define$COformulation$idC) > 1) {
        tl <- tagList(
          # fluidRow(
          #   column(4,dynamicSelectpicker.ui(ns("chooseYobj1"))),
          #   column(4,dynamicSelectpicker.ui(ns("chooseYobj2"))),
          #   column(4,"")
          # ),
          fluidRow(
            column(12, plotlyOutput(ns("plotBiObj"))),
          ),
          fluidRow(
            column(12, plotlyOutput(ns("plotConstr")))
          )
        )
      } else {
        tl <- tagList(
          # fluidRow(
          #   column(4,dynamicSelectpicker.ui(ns("chooseYobj1"))),
          #   column(4,dynamicSelectpicker.ui(ns("chooseYobj2"))),
          #   column(4,"")
          # ),
          fluidRow(
            column(12, plotlyOutput(ns("plotBiObj"))),
          )
        )
      }
    }
  })

  output$plotObj <- renderPlotly({
    # This is just an initialization
    req(define$COformulation$idO, !is.null(data.plot$dataY), cancelOutput = TRUE)
    isolate({
      p <- plot_ly()

      yRowIndexes <- as.numeric(rownames(data.plot$dataY))
      nconstr <- length(define$COformulation$idC)
      idobj1 <- define$COformulation$idO
      y1 <- data.plot$dataY[, idobj1]
      yTitle1 <- colnames(data.plot$dataY[, idobj1, drop = F])
      if (nconstr > 0) {
        rnew <- respect.constraints(data.plot$dataY[, define$COformulation$idC, drop = FALSE], define$COformulation$COsign, define$COformulation$COt)
        if (nconstr > 1) {
          allrnew <- apply(rnew, 2, all)
        } else {
          allrnew <- rnew
        }
        ind0 <- which(allrnew)
        if (length(ind0) > 0) {
          p <- add_trace(p,
            x = data.plot$rowNames[ind0],
            y = y1[ind0],
            name = "Feasible",
            type = "scatter",
            mode = "markers",
            marker = list(color = toRGB("green"), symbol = "circle"),
            hovertemplate = "Run: %{x}, Result: %{y:.2e} <extra></extra>"
          )

          p <- layout(p,
            xaxis = list(
              title = "Iteration_Simulation",
              categoryorder = "array",
              categoryarray = data.plot$rowNames
            ),
            yaxis = list(title = yTitle1),
            title = "Objective",
            legend = list(orientation = "h")
          )
        }
        ind1 <- which(!allrnew)
        if (length(ind1) > 0) {
          p <- add_trace(p,
            x = data.plot$rowNames[ind1],
            y = y1[ind1],
            name = "Non Feasible",
            type = "scatter",
            mode = "markers",
            marker = list(color = toRGB("red"), symbol = "circle"),
            hovertemplate = "Run: %{x}, Result: %{y:.2e} <extra></extra>",
            inherit = FALSE
          )

          p <- layout(p,
            xaxis = list(
              title = "Iteration_Simulation",
              categoryorder = "array",
              categoryarray = data.plot$rowNames
            ),
            yaxis = list(
              title = yTitle1,
              zeroline = FALSE
            ),
            title = "Objective",
            legend = list(orientation = "h")
          )
        }
      } else {
        naFilter <- !is.na(y1)
        p <- add_trace(p,
          x = data.plot$rowNames[naFilter],
          y = y1[naFilter],
          name = "",
          type = "scatter",
          mode = "markers",
          marker = list(symbol = "circle"),
          hovertemplate = "Run: %{x}, Result: %{y:.2e} <extra></extra>"
        )
        p <- layout(p,
          xaxis = list(
            title = "Iteration_Simulation",
            categoryorder = "array",
            categoryarray = data.plot$rowNames
          ),
          yaxis = list(
            title = yTitle1,
            zeroline = FALSE
          ),
          title = "Objective",
          legend = list(orientation = "h")
        )
      }

    })
    return(p)
  })

  output$plotConstr <- renderPlotly({
    # This is just an initialization
    req(define$COformulation$idO, !is.null(data.plot$dataY), define$COformulation$idC, !is.null(data.plot$rowNames), cancelOutput = TRUE)
    isolate({
      yRowIndexes <- as.numeric(rownames(data.plot$dataY))
      yRowIndexMax <- max(yRowIndexes)
      cValues <- data.plot$dataY[, define$COformulation$idC, drop = FALSE]
      r <- matrix(NA, length(define$COformulation$idC), yRowIndexMax)
      rownames(r) <- colnames(cValues)
      r[, yRowIndexes] <- ifelse(
        respect.constraints(cValues, define$COformulation$COsign, define$COformulation$COt),
        1,
        0
      )

      # Hack to emulate a categorical color scale
      colors <- c("red", "green")
      if (all(r == 1, na.rm = T)) {
        colors <- c("green", "green")
      }
      else if (all(r == 0, na.rm = T)) {
        colors <- c("red", "red")
      }

      tt <- define$COformulation$thresholds
      texty <- paste0(colnames(tt), tt)
      textz <- matrix("", nrow(r), ncol(r))
      textz[r == 0] <- "Non-Feasible"
      textz[r == 1] <- "Feasible"

      plot_ly(source = "heatplotConstr") %>%
        add_heatmap(
          z = r,
          x = data.plot$rowNames[1:yRowIndexMax],
          y = 1:length(define$COformulation$idC),
          colors = colors,
          type = "heatmap",
          zauto = FALSE,
          zmin = 0,
          zmax = 1,
          showscale = FALSE,
          hoverinfo = "x+y+text",
          hoverongaps = FALSE,
          text = textz
        ) %>%
        layout(
          xaxis = list(
            title = "Iteration_Simulation",
            categoryorder = "array",
            categoryarray = data.plot$rowNames,
            showgrid = FALSE
          ),
          yaxis = list(
            title = "Constraint Satisfaction",
            showgrid = FALSE,
            tickvals = 1:length(define$COformulation$idC),
            ticktext = texty
          ),
          showlegend = TRUE,
          title = "Constraints"
        )
    })
  })

  # For now we restrict ourselves to bi-objective optimization (limitation from constrained Define)
  choicesY1 <- reactive({
    req(!is.null(data.plot$dataY), define$COformulation$idO)
    l <- NULL
    if (length(define$COformulation$idO) > 1) {
      l <- colnames(data.plot$dataY)[define$COformulation$idO[1]]
    }
    return(l)
  })
  choicesY2 <- reactive({
    req(!is.null(data.plot$dataY), define$COformulation$idO)
    l <- NULL
    if (length(define$COformulation$idO) > 1) {
      l <- colnames(data.plot$dataY)[define$COformulation$idO[2]]
    }
    return(l)
  })

  ynameBiObj1 <- callModule(
    dynamicSelect.server, "chooseYobj1",
    label = "Choose Objective 1 to Visualize", choices = choicesY1, selected.ind = 1
  )
  ynameBiObj2 <- callModule(
    dynamicSelect.server, "chooseYobj2",
    label = "Choose Objective 2 to Visualize", choices = choicesY2, selected.ind = 1
  )

  output$plotBiObj <- renderPlotly({
    req(define$COformulation$idO, !is.null(data.plot$dataY), ynameBiObj1(), ynameBiObj2(), cancelOutput = TRUE)

    p <- plot_ly()

    idobj1 <- which(colnames(data.plot$dataY) == ynameBiObj1())
    idobj2 <- which(colnames(data.plot$dataY) == ynameBiObj2())
    y1 <- data.plot$dataY[, idobj1]
    y2 <- data.plot$dataY[, idobj2]
    yTitle1 <- colnames(data.plot$dataY[, idobj1, drop = F])
    yTitle2 <- colnames(data.plot$dataY[, idobj2, drop = F])

    nconstr <- length(define$COformulation$idC)
    if (nconstr > 0) {
      rnew <- respect.constraints(data.plot$dataY[, define$COformulation$idC, drop = FALSE], define$COformulation$COsign, define$COformulation$COt)
      if (nconstr > 1) {
        allrnew <- apply(rnew, 2, all)
      } else {
        allrnew <- rnew
      }
      ind0 <- which(allrnew)
      if (length(ind0) > 0) {
        p <- add_trace(p,
          x = y1[ind0],
          y = y2[ind0],
          text = data.plot$rowNames[ind0],
          name = "Feasible",
          type = "scatter",
          mode = "markers",
          marker = list(color = toRGB("green"), symbol = "circle"),
          hovertemplate = paste0(yTitle1, ": %{x:.2e}, ", yTitle2, ": %{y:.2e} <extra>Run: %{text}</extra>")
        )
        p <- layout(p,
          xaxis = list(title = yTitle1),
          yaxis = list(title = yTitle2),
          title = "Objectives",
          legend = list(orientation = "h")
        )
      }
      ind1 <- which(!allrnew)
      if (length(ind1) > 0) {
        p <- add_trace(p,
          x = y1[ind1],
          y = y2[ind1],
          text = data.plot$rowNames[ind1],
          name = "Non Feasible",
          type = "scatter",
          mode = "markers",
          marker = list(color = toRGB("red"), symbol = "circle"),
          hovertemplate = paste0(yTitle1, ": %{x:.2e}, ", yTitle2, ": %{y:.2e} <extra>Run: %{text}</extra>"),
          inherit = FALSE
        )
        p <- layout(p,
          xaxis = list(title = yTitle1, zeroline = FALSE),
          yaxis = list(title = yTitle2, zeroline = FALSE),
          title = "Objectives",
          legend = list(orientation = "h")
        )
      }
    } else {
      p <- add_trace(p,
        y = y1,
        x = y2,
        text = data.plot$rowNames,
        name = "",
        type = "scatter",
        mode = "markers",
        marker = list(symbol = "circle"),
        hovertemplate = paste0(yTitle1, ": %{x:.2e}, ", yTitle2, ": %{y:.2e} <extra>Run: %{text}</extra>")
      )
      p <- layout(
        p,
        xaxis = list(title = yTitle1, zeroline = FALSE),
        yaxis = list(title = yTitle2, zeroline = FALSE),
        title = "Objectives"
      )
    }
    return(p)
  })

}
