library(shiny)
library(bslib)
library(thematic)
library(shinyWidgets)
library(shinyjs)
library(scatterPlotMatrix)

thematic_shiny(font = "auto")

# Minimal reproduction of Lagun Prepare DOE → Generate DOE plot path:
# - gear (dropdownButton) appears after data exists
# - pickerInputs are created later via renderUI into uiOutput("…-select")
# - page-load JS binds $("#…-select").on("loaded.bs.select", …) before those
#   pickers exist (Lagun visualizeDOE::getJsPickerEvent)
# - scatterPlotMatrix is gated on selection$num from those picker events

CONTINUOUS_CS <- c(
  "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool",
  "Rainbow", "CubehelixDefault", "Blues", "Greens", "Greys",
  "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
  "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn",
  "YlOrBr", "YlOrRd"
)

CATEGORIAL_CS <- c("Category10", "Accent", "Dark2", "Paired", "Set1")

numeric_vars <- setdiff(names(iris), "Species")

# Same binding style as Lagun visualizeDOE::getJsPickerEvent.
# Use HTML() for the script body: htmltools escapes && to &amp;&amp; otherwise.
getJsPickerEvent <- function(pickerID, shinyInputID) {
  pickerLoaded <- paste0(
    '$("#', pickerID, '-select").on("loaded.bs.select", function() {',
    'Shiny.setInputValue("', shinyInputID, '", 1, {priority: "event"});',
    "});"
  )
  pickerHidden <- paste0(
    '$("#', pickerID, '-select").on("hidden.bs.select", function() {',
    'Shiny.setInputValue("', shinyInputID, '", 1, {priority: "event"});',
    "});"
  )
  paste(pickerLoaded, pickerHidden, sep = "\n")
}

ui <- fluidPage(
  theme = bs_theme(version = 3, bootswatch = "darkly"),
  useShinyjs(),
  titlePanel("Lagun-style scatterPlotMatrix repro"),
  fluidRow(
    column(
      4,
      actionButton("goDOE", "Generate DOE", class = "btn-primary")
    )
  ),
  hr(),
  # Plot chrome is always in the DOM (as in Lagun); hide/show via shinyjs by id.
  fluidRow(
    align = "center",
    column(
      2,
      br(),
      dropdownButton(
        inputId = "advancedSettings",
        tags$h4("Mouse Mode"),
        radioButtons(
          "mouseMode",
          label = "Set the type of mouse interactions",
          choices = c("tooltip", "filter", "zoom"),
          selected = "tooltip",
          inline = TRUE
        ),
        tags$h4("Palette Colors"),
        selectInput(
          "choose.palette.num",
          "Choose Palette for Numeric Columns:",
          choices = CONTINUOUS_CS,
          selected = CONTINUOUS_CS[1]
        ),
        selectInput(
          "choose.palette.cat",
          "Choose Palette for Categorical Columns:",
          choices = CATEGORIAL_CS,
          selected = CATEGORIAL_CS[1]
        ),
        hr(),
        tags$h4("Representation"),
        selectInput(
          "corrPlotType",
          "Correlation Plot Type:",
          choices = list("Text" = "Text", "AbsText" = "AbsText"),
          selected = "Text"
        ),
        selectInput(
          "corrPlotCs",
          "Choose Palette for Correlation Plot:",
          choices = CONTINUOUS_CS,
          selected = "RdBu"
        ),
        selectInput(
          "distribType",
          "Distribution:",
          choices = list("Histogram" = 2, "Density Plot" = 1),
          selected = 1
        ),
        circle = TRUE,
        icon = icon("cog"),
        status = "primary",
        right = FALSE,
        tooltip = tooltipOptions(title = "Click for advanced settings")
      )
    ),
    column(4, uiOutput("numericSelection-select")),
    column(4, uiOutput("categoricalSelection-select"))
  ),
  fluidRow(
    align = "center",
    column(
      12,
      scatterPlotMatrixOutput("scatterPlotMatrix", height = "800px")
    )
  ),
  tags$script(HTML(
    paste(
      getJsPickerEvent("numericSelection", "numSelectionClosed"),
      getJsPickerEvent("categoricalSelection", "catSelectionClosed"),
      sep = "\n"
    )
  ))
)

server <- function(input, output, session) {
  doe <- reactiveVal(NULL)
  selection <- reactiveValues(num = NULL, cat = NULL)

  # Lagun pattern: gear in DOM, hide until DOE exists (do not wrap with hidden())
  shinyjs::hide("advancedSettings")

  observeEvent(input$goDOE, {
    selection$num <- NULL
    selection$cat <- NULL
    doe(within(iris, {
      Species <- as.character(Species)
    }))
    shinyjs::show("advancedSettings")
  })

  observe({
    if (is.null(doe())) {
      shinyjs::hide("advancedSettings")
    }
  })

  output[["numericSelection-select"]] <- renderUI({
    req(doe())
    pickerInput(
      inputId = "numericSelection-choice",
      label = "Numeric variables",
      choices = numeric_vars,
      selected = numeric_vars,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        style = "btn-primary",
        `live-search` = TRUE
      )
    )
  })

  output[["categoricalSelection-select"]] <- renderUI({
    req(doe())
    cats <- paste("Species", unique(doe()$Species), sep = "|")
    names(cats) <- unique(doe()$Species)
    pickerInput(
      inputId = "categoricalSelection-choice",
      label = "Categorical variables",
      choices = list(Species = as.list(cats)),
      selected = cats,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        style = "btn-primary",
        `live-search` = TRUE
      )
    )
  })

  observeEvent(input$numSelectionClosed, {
    choice <- input[["numericSelection-choice"]]
    if (is.null(selection$num)) {
      selection$num <- choice
    } else if (!identical(choice, selection$num)) {
      selection$num <- choice
    }
  })

  observeEvent(input$catSelectionClosed, {
    choice <- input[["categoricalSelection-choice"]]
    if (is.null(selection$cat)) {
      selection$cat <- choice
    } else if (!identical(choice, selection$cat)) {
      selection$cat <- choice
    }
  })

  output$scatterPlotMatrix <- renderScatterPlotMatrix({
    req(doe(), selection$num)

    df <- as.data.frame(doe())
    if (!is.null(selection$cat) && length(selection$cat) > 0) {
      df$group <- df$Species
    }

    categorical <- lapply(colnames(df), function(col) {
      if (identical(col, "group")) {
        as.list(unique(df$group))
      } else {
        NULL
      }
    })

    kept <- colnames(df) %in% selection$num
    z_axis <- if (!is.null(df$group)) "group" else NULL

    scatterPlotMatrix(
      data = df,
      keptColumns = kept,
      zAxisDim = z_axis,
      cutoffs = NULL,
      controlWidgets = NULL,
      distribType = as.numeric(isolate(input$distribType)),
      categorical = categorical,
      corrPlotType = as.character(isolate(input$corrPlotType)),
      corrPlotCS = as.character(isolate(input$corrPlotCs)),
      continuousCS = as.character(isolate(input$choose.palette.num)),
      categoricalCS = as.character(isolate(input$choose.palette.cat)),
      cssRules = list(".jitterZone" = "fill: white"),
      plotProperties = list(
        noCatColor = "#1F78B4",
        point = list(alpha = 0.8, radius = 5)
      ),
      slidersPosition = list(dimCount = min(3L, sum(kept))),
      eventInputId = "myPlotEvent"
    )
  })

  observeEvent(input$mouseMode, {
    scatterPlotMatrix::changeMouseMode("scatterPlotMatrix", input$mouseMode)
  })
  observeEvent(input$corrPlotType, {
    scatterPlotMatrix::setCorrPlotType("scatterPlotMatrix", input$corrPlotType)
  })
  observeEvent(input$corrPlotCs, {
    scatterPlotMatrix::setCorrPlotCS("scatterPlotMatrix", input$corrPlotCs)
  })
  observeEvent(input$distribType, {
    scatterPlotMatrix::setDistribType("scatterPlotMatrix", input$distribType)
  })
  observeEvent(input$choose.palette.num, {
    scatterPlotMatrix::setContinuousColorScale(
      "scatterPlotMatrix",
      input$choose.palette.num
    )
  })
  observeEvent(input$choose.palette.cat, {
    scatterPlotMatrix::setCategoricalColorScale(
      "scatterPlotMatrix",
      input$choose.palette.cat
    )
  })
}

shinyApp(ui, server)
