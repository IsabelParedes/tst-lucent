library(shiny)
library(bslib)
library(thematic)
library(plotly)
library(ggplot2)

thematic_shiny(font = "auto")

# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- setdiff(names(iris), "Species")

ui <- page_sidebar(
  input_dark_mode(mode = "dark"),
  title = "Iris k-means clustering",
  sidebar = sidebar(
    selectInput("xcol", "X Variable", vars),
    selectInput("ycol", "Y Variable", vars, selected = vars[[2]]),
    numericInput("clusters", "Cluster count", 3, min = 1, max = 9)
  ),
  layout_columns(
    card(card_header("plotly"), plotlyOutput("plot1")),
    card(card_header("ggplot2"), plotOutput("plot2"))
  )
)

server <- function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  plotData <- reactive({
    df <- selectedData()
    km <- clusters()
    xcol <- input$xcol
    ycol <- input$ycol

    df$cluster <- factor(km$cluster)
    centers <- as.data.frame(km$centers)
    colnames(centers) <- c(xcol, ycol)

    list(df = df, centers = centers, xcol = xcol, ycol = ycol)
  })

  output$plot1 <- renderPlotly({
    d <- plotData()
    df <- d$df
    centers <- d$centers
    xcol <- d$xcol
    ycol <- d$ycol

    plot_ly(
      df,
      x = df[[xcol]],
      y = df[[ycol]],
      color = df$cluster,
      type = "scatter",
      mode = "markers",
      marker = list(size = 10)
    ) |>
      add_markers(
        data = centers,
        x = centers[[xcol]],
        y = centers[[ycol]],
        inherit = FALSE,
        name = "Centers",
        marker = list(symbol = "x", size = 14, color = "black", line = list(width = 2))
      ) |>
      layout(
        xaxis = list(title = xcol),
        yaxis = list(title = ycol)
      )
  })

  output$plot2 <- renderPlot({
    d <- plotData()
    df <- d$df
    centers <- d$centers
    xcol <- d$xcol
    ycol <- d$ycol

    ggplot(df, aes(.data[[xcol]], .data[[ycol]], color = cluster)) +
      geom_point(size = 3) +
      geom_point(
        data = centers,
        aes(.data[[xcol]], .data[[ycol]]),
        color = "black",
        shape = 4,
        size = 4,
        stroke = 1.5,
        inherit.aes = FALSE
      ) +
      labs(x = xcol, y = ycol) +
      theme(legend.position = "right")
  })

}

shinyApp(ui, server)
