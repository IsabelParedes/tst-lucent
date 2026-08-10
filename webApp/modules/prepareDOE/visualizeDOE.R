#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

source("modules/shared/dynamicSelectpicker.R", local = TRUE)
source("modules/shared/spmExport.R", local = TRUE)


CONTINUOUS_CS <- c(
  "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool",
  "Rainbow", "CubehelixDefault", "Blues", "Greens", "Greys",
  "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
  "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn",
  "YlOrBr", "YlOrRd"
)

CATEGORIAL_CS <- c("Category10", "Accent", "Dark2", "Paired", "Set1")

# Returns a list which associates to each categorical variable a list which associates to each level of this categorical variable a string representation 'cat|level'
buildCategoricalSelection <- function(data, categoricalVariables, mapNames,
                                      separator = "|") {
  choiceList <- list()

  for (i in seq_len(length(categoricalVariables))) {
    currentCategorical <- categoricalVariables[i]
    currentCategoricalMenu <-
      mapNames[mapNames[, "names"] == currentCategorical, "menu"]
    categoricalLevels <- levels(as.factor(data[[currentCategorical]]))
    categories <-
      paste(currentCategorical, categoricalLevels, sep = separator)
    names(categories) <- categoricalLevels
    choiceList[[currentCategoricalMenu]] <- as.list(categories)
  }

  return(choiceList)
}

getJsPickerEvent <- function(pickerID, shinyInputID) {
  pickerLoaded <-
    paste0(
      '$("#', pickerID, '-select").on("loaded.bs.select", function() {',
      'Shiny.setInputValue("', shinyInputID, '", 1, {priority: "event"});',
      "});"
    )

  pickerHidden <-
    paste0(
      '$("#', pickerID, '-select").on("hidden.bs.select", function() {',
      'Shiny.setInputValue("', shinyInputID, '", 1, {priority: "event"});',
      "});"
    )

  return(paste(pickerLoaded, pickerHidden, sep = "\n"))
}


visualizeDOEUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$style(
      HTML("
            #fluidrow {
              margin-left: auto;
              margin-right: auto;
              width: 1000px;
              text-align: left;
            }
           ")
    ),
    tagList(
      fluidRow(
        align = "center",
        id = "fluidrow",
        column(2, br(),
          dropdownButton(
            inputId = ns("advancedSettings"),
            tags$h4("Mouse Mode"),
            radioButtons(ns("mouseMode"),
              label = "Set the type of mouse interactions",
              choices = c("tooltip", "filter", "zoom"),
              selected = "tooltip", inline = TRUE
            ),
            tags$h4("Palette Colors"),
            selectInput(ns("choose.palette.num"),
              "Choose Palette for Numeric Columns:",
              choices = CONTINUOUS_CS,
              selected = CONTINUOUS_CS[1]
            ),
            selectInput(ns("choose.palette.cat"),
              "Choose Palette for Categorical Columns:",
              choices = CATEGORIAL_CS,
              selected = CATEGORIAL_CS[1]
            ),
            hr(),
            tags$h4("Represention"),
            selectInput(ns("corrPlotType"),
              "Correlation Plot Type:",
              choices = list("Text" = "Text", "AbsText" = "AbsText"),
              selected = "Text"
            ),
            selectInput(ns("corrPlotCs"),
              "Choose Palette for Correlation Plot:",
              choices = CONTINUOUS_CS,
              selected = CONTINUOUS_CS[22] # RdBu
            ),
            selectInput(ns("distribType"),
              "Distribution:",
              choices = list("Histogram" = 2, "Density Plot" = 1),
              selected = 1
            ),
            hr(),
            tags$h4("Export"),
            spmExport.ui(ns("spmExport")),
            circle = TRUE,
            icon = icon("cog"), status = "primary", right = FALSE,
            tooltip = tooltipOptions(title = "Click for advanced settings")
          ),
          align = "center"
        ),
        column(4, dynamicSelectpicker.ui(ns("numericSelection"))),
        column(4, dynamicSelectpicker.ui(ns("categoricalSelection")))
      ),
      fluidRow(
        align = "center",
        column(
          12,
          scatterPlotMatrixOutput(ns("scatterPlotMatrix"),
            height = "1000px"
          )
        )
      ),
      tags$script(
        getJsPickerEvent(ns("numericSelection"), ns("numSelectionClosed")),
        getJsPickerEvent(ns("categoricalSelection"), ns("catSelectionClosed"))
      )
    )
  )
}


visualizeDOEServer <- function(id, dataUsingFactors, numericVariables,
                               categoricalVariables, mapNames) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      selection <- reactiveValues(num = NULL, cat = NULL)

      data <- reactive({
        req(dataUsingFactors())

        # Replace factor levels by character representations
        data <- dataUsingFactors()

        for (i in seq_len(length(categoricalVariables()))) {
          currentCategorical <- categoricalVariables()[i]
          data[[currentCategorical]] <- as.character(data[[currentCategorical]])
        }

        return(data)
      })

      observe({
        if (is.null(dataUsingFactors())) {
          shinyjs::hide("advancedSettings")
        } else {
          shinyjs::show("advancedSettings")
        }
      })

      observeEvent(numericVariables(), {
        selection$num <- NULL
        selection$cat <- NULL
      })


      categoricalList <- reactive({
        req(categoricalVariables(), mapNames())

        choiceList <-
          buildCategoricalSelection(data(), categoricalVariables(), mapNames())

        return(choiceList)
      })

      selected <- reactive({
        unlist(categoricalList())
      })


      categoricalSelection <- callModule(dynamicSelectpicker.server,
        "categoricalSelection",
        label.title = "Categorical variables",
        choices = categoricalList,
        multiple = TRUE,
        selected = selected,
        livesearch = TRUE
      )


      numericList <- reactive({
        req(numericVariables(), mapNames())

        choiceList <- as.list(numericVariables())
        names(choiceList) <-
          mapNames()[mapNames()[, "names"] %in% numericVariables(), "menu"]

        return(choiceList)
      })


      numericSelection <- callModule(dynamicSelectpicker.server,
        "numericSelection",
        label.title = "Numeric variables",
        choices = numericList,
        multiple = TRUE,
        selected = numericList,
        livesearch = TRUE
      )

      observeEvent(input$numSelectionClosed, {
        if (is.null(selection$num)) {
          selection$num <- numericSelection()
        } else {
          if (!identical(numericSelection(), selection$num)) {
            selection$num <- numericSelection()
          }
        }
      })

      observeEvent(input$catSelectionClosed, {
        if (is.null(selection$cat)) {
          selection$cat <- categoricalSelection()
        } else {
          if (!identical(categoricalSelection(), selection$cat)) {
            selection$cat <- categoricalSelection()
          }
        }
      })

      datavisu <- reactiveVal(NULL)

      cutoffs <- reactive({
        if (length(filteredCategoricalVariables()) == 0) {
          return(NULL)
        }
        groupCategories <- groupColumnCategories()
        toKeepIndexes <- Filter(
          function(i) {
            splittedCat <- unlist(strsplit(groupCategories[[i]], ", "))
            splittedCat <- gsub(".*=", "", splittedCat)
            toSearch <- sapply(seq_len(length(splittedCat)), function(catIndex) {
              paste(filteredCategoricalVariables()[catIndex], splittedCat[catIndex], sep = "|")
            })
            setequal(intersect(unlist(selection$cat), toSearch), toSearch)
          },
          seq_len(length(groupCategories))
        )
        xyCutoffs <- sapply(toKeepIndexes, function(i) {
          list(list(NULL, c(i - 1 - 1 / 8, i - 1 + 1 / 8)))
        })
        list(list(xDim = isolate(numericVariables())[1], yDim = "group", xyCutoffs = xyCutoffs))
      })

      # Represents a additionnal column for the data, where values correspond to the aggregation of each value of categorical variables (with the form 'catVarName1=catVarValue1, catVarName2=catVarValue2, etc.')
      groupColumn <- reactive({
        filteredCatVarList <- filteredCategoricalVariables()
        req(filteredCatVarList)
        df <- data()[filteredCatVarList]
        sapply(
          seq_len(nrow(df)),
          function(i) paste(names(df), df[i, ], sep = "=", collapse = ", ")
        )
      })

      # Returns categorical variables which have at least one level selected
      filteredCategoricalVariables <- reactive({
        if (is.null(selection$cat)) {
          return(c())
        }
        Filter(function(name) any(startsWith(selection$cat, name)), categoricalVariables())
      })

      # Returns the levels of the additionnal column 'group' (witch have the form 'catVarName1=catVarValue1, catVarName2=catVarValue2, etc.')
      groupColumnCategories <- reactive({
        req(groupColumn())
        as.list(levels(as.factor(groupColumn())))
      })

      output$scatterPlotMatrix <- renderScatterPlotMatrix({
        req(data(), mapNames(), selection$num)

        data <- as.data.frame(data())
        if (length(filteredCategoricalVariables()) != 0) {
          data$group <- groupColumn()
        }
        datavisu(data)

        categorical <- lapply(seq_len(ncol(datavisu())), function(icol) {
          if (colnames(datavisu())[icol] == "group") {
            return(as.list(levels(as.factor(datavisu()[, icol]))))
          }
          return(NULL)
        })

        keptCols <- colnames(datavisu()) %in% selection$num

        scatterPlotMatrix(
          data = datavisu(),
          keptColumns = keptCols,
          zAxisDim = unlist(ifelse(length(filteredCategoricalVariables()) == 0, list(NULL), "group")),
          cutoffs = isolate(cutoffs()),
          controlWidgets = NULL,
          distribType = as.numeric(isolate(input$distribType)),
          categorical = categorical,
          corrPlotType = as.character(isolate(input$corrPlotType)),
          corrPlotCS = as.character(isolate(input$corrPlotCs)),
          continuousCS = as.character(isolate(input$choose.palette.num)),
          categoricalCS = as.character(isolate(input$choose.palette.cat)),
          cssRules = list(
            ".jitterZone" = "fill: white"
          ),
          plotProperties = list(
            noCatColor = "#1F78B4",
            point = list(
              alpha = 0.8,
              radius = 5
            )
          ),
          slidersPosition = list(
            dimCount = 3
          ),
          eventInputId = ns("myPlotEvent")
        )
      })

      observe({
        scatterPlotMatrix::changeMouseMode(
          ns("scatterPlotMatrix"),
          input$mouseMode
        )
      })

      # If 'corrPlotType' has been changed ...
      observeEvent(input$corrPlotType, {
        scatterPlotMatrix::setCorrPlotType(
          ns("scatterPlotMatrix"),
          input$corrPlotType
        )
      })

      # If 'corrPlotCs' has been changed ...
      observeEvent(input$corrPlotCs, {
        scatterPlotMatrix::setCorrPlotCS(
          ns("scatterPlotMatrix"),
          input$corrPlotCs
        )
      })

      # If 'distribType' has been changed ...
      observeEvent(input$distribType, {
        scatterPlotMatrix::setDistribType(
          ns("scatterPlotMatrix"),
          input$distribType
        )
      })

      # If 'linearRegressionCB' or 'loessCB' have been changed ...
      observe({
        linearFlag <- ifelse(input$linearRegressionCB, 1, 0)
        loessFlag <- ifelse(input$loessCB, 2, 0)
        scatterPlotMatrix::setRegressionType(
          ns("scatterPlotMatrix"),
          linearFlag + loessFlag
        )
      })

      # If continuous palette has been changed ...
      observeEvent(input$choose.palette.num, {
        scatterPlotMatrix::setContinuousColorScale(
          ns("scatterPlotMatrix"),
          input$choose.palette.num
        )
      })

      # If categorical palette has been changed ...
      observeEvent(input$choose.palette.cat, {
        scatterPlotMatrix::setCategoricalColorScale(
          ns("scatterPlotMatrix"),
          input$choose.palette.cat
        )
      })

      callModule(spmExport.server,
        "spmExport",
        scatterPlotMatrixId = ns("scatterPlotMatrix"),
        datavisu = datavisu
      )

      observeEvent(input$myPlotEvent, {
        if (!is.null(input$myPlotEvent) && input$myPlotEvent$type == "zAxisChange") {
          zDim <- input$myPlotEvent$value
          if (is.null(zDim)) {
            scatterPlotMatrix::setZAxis(ns("scatterPlotMatrix"), "group")
          }
        }
      })
    }
  )
}
