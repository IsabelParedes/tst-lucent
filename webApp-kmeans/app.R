library(shiny)
library(bslib)
library(thematic)
library(plotly)

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
  card(plotlyOutput("plot1"))
)

server <- function(input, output, session) {

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlotly({
    df <- selectedData()
    km <- clusters()
    xcol <- input$xcol
    ycol <- input$ycol

    df$cluster <- factor(km$cluster)
    centers <- as.data.frame(km$centers)
    colnames(centers) <- c(xcol, ycol)

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

}

shinyApp(ui, server)
