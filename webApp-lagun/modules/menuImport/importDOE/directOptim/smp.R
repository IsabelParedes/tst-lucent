source("modules/shared/spmExport.R", local = TRUE)
source("modules/shared/pcpExport.R", local = TRUE)

SIMULATION_COLUMN_NAME <- "Simulations"

CONTINUOUS_CS <- c(
  "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool",
  "Rainbow", "CubehelixDefault", "Blues", "Greens", "Greys",
  "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
  "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn",
  "YlOrBr", "YlOrRd"
)

DEFAULT_CONT_CS <- CONTINUOUS_CS[1]

CATEGORIAL_CS <- c("Category10", "Accent", "Dark2", "Paired", "Set1")

DEFAULT_CAT_CS <- CATEGORIAL_CS[1]

ARRANGE_METHODS <- c("fromLeft", "fromRight", "fromBoth", "fromNone")

DEFAULT_ARRANGE_METHOD <- ARRANGE_METHODS[2]

###############################
#  Define client user interface
###############################

spm.ui <- function(id) {
  ns <- NS(id)

  modalContent <- tagList(
    uiOutput(ns("zoomContent")),
    fluidRow(
      column(4, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                             width = '100%'), offset = 4)
    )
  )
  
  tagList(
    uiOutput(ns("plotoptim.dynui")),
    # Modal dialog behind the 'Zoom' button
    bsModal(ns("zoomModal"), "Parallel coordinate plot - Scatter Plot Matrix - Selected Points", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("zoomModal")," .modal-footer{display:none}",
                                        " .modal-lg{width: 20%}"))))
  )
}

#####################
# Define server logic
#####################

spm.server <- function(input, output, session, define, data.plot) {

  ns <- session$ns

  observeEvent(input$layout, {
    if (input$layout == "Vertical") {
      shinyjs::runjs(paste0(
        "$('#", ns("pcpspm"), ">.scatterPlotMatrix').attr('align', 'center');",
        "$('#", ns("pcpspm"), "').css('display', 'block');",
        "$('#", ns("pcpspm"), ">.parallelPlot').css('width', '100%');",
        "$('#", ns("pcpspm"), ">.scatterPlotMatrix').css('width', '100%').trigger('shown');"
      ))
      shinyjs::runjs(paste0(
        "$('#", ns("zoomPcpspm"), ">.scatterPlotMatrix').attr('align', 'center');",
        "$('#", ns("zoomPcpspm"), "').css('display', 'block');",
        "$('#", ns("zoomPcpspm"), ">.parallelPlot').css('width', '100%');",
        "$('#", ns("zoomPcpspm"), ">.scatterPlotMatrix').css('width', '100%').trigger('shown');"
      ))
    }
    if (input$layout == "Horizontal") {
      shinyjs::runjs(paste0(
        "$('#", ns("pcpspm"), "').css('display', 'flex');",
        "$('#", ns("pcpspm"), ">.parallelPlot').css('width', '55%');",
        "$('#", ns("pcpspm"), ">.scatterPlotMatrix').css('width', '45%').trigger('shown');"
      ))
      shinyjs::runjs(paste0(
        "$('#", ns("zoomPcpspm"), "').css('display', 'flex');",
        "$('#", ns("zoomPcpspm"), ">.parallelPlot').css('width', '55%');",
        "$('#", ns("zoomPcpspm"), ">.scatterPlotMatrix').css('width', '45%').trigger('shown');"
      ))
    }
  })
  
  available.columns <- reactiveVal(NULL)
  selected.columns <- reactiveVal(NULL)

  output$plotoptim.dynui <- renderUI({
    req(define$COformulation$idO, cancelOutput = TRUE)
    tl <- tagList(
      useShinyjs(),
      br(),
      fluidRow(
        align = "left",
        column(1, br(),
          dropdownButton(
            radioGroupButtons(
              inputId = ns("layout"),
              label = tags$h4("Layout"), 
              choices = c("Vertical", "Horizontal"),
              status = "primary"
            ),
            tags$h4("Palette Colors"),
            selectInput(ns("choose.palette.num"),
              "Choose Palette for Numeric Columns",
              choices = CONTINUOUS_CS,
              selected = DEFAULT_CONT_CS
            ),
            selectInput(ns("choose.palette.cat"),
              "Choose Palette for Categorical Columns",
              choices = CATEGORIAL_CS,
              selected = DEFAULT_CAT_CS
            ),
            hr(),
            tags$h3("Parallel Coordinate Plot"),
            selectInput(
              ns("arrange.method"),
              "Arrange Method in Category Boxes",
              choices = ARRANGE_METHODS,
              selected = DEFAULT_ARRANGE_METHOD
            ),
            pcpExport.ui(ns("pcpExport")),
            hr(),
            tags$h3("Scatter Plot Matrix"),
            radioButtons(ns("mouseMode"),
              label = "Type of mouse interactions",
              choices = c("tooltip", "filter", "zoom"),
              selected = "tooltip", inline = TRUE
            ),
            spmExport.ui(ns("spmExport")),
            circle = TRUE,
            icon = icon("gear"), status = "primary", right = FALSE,
            tooltip = tooltipOptions(title = "Click for advanced settings")
          ), align="right"
        ),
        column(2, br(), actionButton(ns("zoom.button"), label = "Plot Selected Points", class = "btn-primary", width = "100%"), align="left"),
        column(9, dynamicSelectpicker.ui(ns("columnSelection")), align="left")
      ),
      div(id = ns("pcpspm"),
        parallelPlotOutput(ns("parcoords")),
        scatterPlotMatrixOutput(ns("spm"), height = "800px")
      ),
      tags$script(paste0('$( "#', ns('columnSelection'), '-select" ).on( "loaded.bs.select", function() { console.log("columnSelection loaded"); Shiny.onInputChange("', ns('columnSelectionClosed'), '", 1, {priority: "event"}); });')),
      tags$script(paste0('$( "#', ns('columnSelection'), '-select" ).on( "hidden.bs.select", function() { console.log("columnSelection hidden"); Shiny.onInputChange("', ns('columnSelectionClosed'), '", 1, {priority: "event"}); });'))
    )
  })

  dataIXY <- reactive({
    req(data.plot$dataX, data.plot$dataY, nrow(data.plot$dataY) > 0)

    yids <- define$COformulation$idO
    if (length(define$COformulation$idC) > 0) {
      yids <- c(yids, define$COformulation$idC)
    }
    if (isTRUE(define$COformulation$isInversion)) {
      yids <- define$COformulation$idC
    }

    dataY <- data.plot$dataY[, yids, drop = F]

    simulationsCol <- cbind(as.numeric(rownames(data.plot$dataY)))
    colnames(simulationsCol) <- SIMULATION_COLUMN_NAME

    ixy <- cbind(
      simulationsCol,
      head(data.plot$dataX, nrow(dataY)),
      dataY
    )
    rownames(ixy) <- data.plot$rowNames
    return(ixy)
  })

  initSpmDone <- FALSE
  initPcpDone <- FALSE

  observeEvent(c(selected.columns, dataIXY()), {
    if (initSpmDone) {
      # If spm has been built, send it a message to retrieve its current configuration
      scatterPlotMatrix::getPlotConfig(
        ns("spm"),
        ns("ConfigForSpmUpdate")
      )
    }
    else {
      categorical <- c(
        list(NULL),
        sapply(define$Xinfos$Xinfos, function(x) {if ("categorical" %in% x$type) return(unlist(x$levels)) else return(NULL)}),
        rep(list(NULL), ncol(dataIXY()) - 1 - length(define$Xinfos$Xinfos))
      )

      spmConfig(list(
        categorical = categorical,
        zAxisDim = SIMULATION_COLUMN_NAME,
        corrPlotType = "Empty",
        distribType = 2,
        regressionType = 0,
        rotateTitle = FALSE,
        cssRules = list(
          ".jitterZone" = "fill: white"
        ),
        plotProperties = list(
          noCatColor = "#1F78B4",
          point = list(
            alpha = 0.8,
            radius = 4
          )
        ),
        slidersPosition = list(
          dimCount = min(6, ncol(dataIXY()))
        ),
        continuousCS = DEFAULT_CONT_CS,
        categoricalCS = DEFAULT_CAT_CS
      ))
      askForSpmUpdate(askForSpmUpdate() + 1)
    }

    if (initPcpDone) {
      # If pcp has been built, send it a message to retrieve its current configuration
      parallelPlot::getPlotConfig(
        ns("parcoords"),
        ns("ConfigForPcpUpdate")
      )
    }
    else {
      categorical <- c(
        list(NULL),
        sapply(define$Xinfos$Xinfos, function(x) {if ("categorical" %in% x$type) return(unlist(x$levels)) else return(NULL)}),
        rep(list(NULL), ncol(dataIXY()) - 1 - length(define$Xinfos$Xinfos))
      )

      pcpConfig(list(
        categorical = categorical,
        categoriesRep = "EquallySpacedLines",
        refColumnDim = SIMULATION_COLUMN_NAME,
        rotateTitle = FALSE,
        arrangeMethod = DEFAULT_ARRANGE_METHOD,
        continuousCS = DEFAULT_CONT_CS,
        categoricalCS = DEFAULT_CAT_CS
      ))
      askForPcpUpdate(askForPcpUpdate() + 1)
    }
  })

  spmConfig <- reactiveVal(NULL)
  askForSpmUpdate <- reactiveVal(0)

  pcpConfig <- reactiveVal(NULL)
  askForPcpUpdate <- reactiveVal(0)

  observeEvent(input$ConfigForSpmUpdate, {
    spmConfig(input$ConfigForSpmUpdate)
    askForSpmUpdate(askForSpmUpdate() + 1)
  })

  output$spm <- renderScatterPlotMatrix({
    askForSpmUpdate()
    req(isolate(dataIXY()), cancelOutput = TRUE)
    initSpmDone <<- TRUE

    keptCols <- colnames(isolate(dataIXY())) %in% selected.columns()
    if (!is.null(keptCols) && length(which(keptCols)) == 0) {
      # If no column selected, keep all columns
      keptCols <- rep(TRUE, length(keptCols))
    }

    config <- isolate(spmConfig())
    scatterPlotMatrix(
      data = isolate(dataIXY()),
      controlWidgets = NULL,
      categorical = config$categorical,
      inputColumns = config$inputColumns,
      cutoffs = config$cutoffs,
      keptColumns = keptCols,
      zAxisDim = config$zAxisDim,
      distribType = as.numeric(config$distribType),
      regressionType = as.numeric(config$regressionType),
      corrPlotType = config$corrPlotType,
      corrPlotCS = config$corrPlotCS,
      rotateTitle = config$rotateTitle,
      columnLabels = config$columnLabels,
      continuousCS = config$continuousCS,
      categoricalCS = config$categoricalCS,
      cssRules = config$cssRules,
      plotProperties = config$plotProperties,
      slidersPosition = config$slidersPosition,
      eventInputId = ns("spmEvent")
    )
  })

  observeEvent(input$ConfigForPcpUpdate, {
    pcpConfig(input$ConfigForPcpUpdate)
    askForPcpUpdate(askForPcpUpdate() + 1)
  })

  output$parcoords <- renderParallelPlot({
    askForPcpUpdate()
    req(isolate(dataIXY()), cancelOutput = TRUE)
    initPcpDone <<- TRUE

    keptCols <- colnames(isolate(dataIXY())) %in% selected.columns()
    if (!is.null(keptCols) && length(which(keptCols)) == 0) {
      # If no column selected, keep all columns
      keptCols <- rep(TRUE, length(keptCols))
    }

    config <- isolate(pcpConfig())
    parallelPlot(
      data = isolate(dataIXY()),
      categorical = config$categorical,
      categoriesRep = config$categoriesRep,
      arrangeMethod = config$arrangeMethod,
      inputColumns = config$inputColumns,
      keptColumns = keptCols,
      histoVisibility = config$histoVisibility,
      invertedAxes = config$invertedAxes,
      cutoffs = config$cutoffs,
      refRowIndex = config$refRowIndex,
      refColumnDim = config$refColumnDim,
      rotateTitle = config$rotateTitle,
      columnLabels = config$columnLabels,
      continuousCS = config$continuousCS,
      categoricalCS = config$categoricalCS,
      controlWidgets = NULL,
      cssRules = config$cssRules,
      sliderPosition = config$sliderPosition,
      eventInputId = ns("pcpEvent")
    )
  })

  observe({
    req(!is.null(data.plot$dataX), !is.null(data.plot$dataY))

    yids <- define$COformulation$idO
    if (length(define$COformulation$idC) > 0) {
      yids <- c(yids, define$COformulation$idC)
    }

    availableColumns <- c(
      SIMULATION_COLUMN_NAME,
      colnames(data.plot$dataX),
      colnames(data.plot$dataY[, yids, drop = F])
    )

    if (is.null(available.columns())) {
      available.columns(availableColumns)
    } else {
      if (!identical(columnSelection(), available.columns())) {
        available.columns(availableColumns)
      }
    }
  })

  columnSelection <- callModule(dynamicSelectpicker.server,
    "columnSelection",
    label.title = "Displayed Columns",
    choices = available.columns,
    multiple = TRUE,
    selected = available.columns,
    livesearch = TRUE
  )

  observeEvent(input$columnSelectionClosed, {
    if (is.null(selected.columns())) {
      selected.columns(columnSelection())
    } else {
      if (!identical(columnSelection(), selected.columns())) {
        selected.columns(columnSelection())
      }
    }
  })

  observe({
    scatterPlotMatrix::changeMouseMode(ns("spm"), input$mouseMode)
  })

  observeEvent(input$spmEvent, {
    if (input$spmEvent$type == "zAxisChange") {
      parallelPlot::setRefColumnDim(ns("parcoords"), input$spmEvent$value)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "refColumnDimChange") {
      scatterPlotMatrix::setZAxis(ns("spm"), input$pcpEvent$value$refColumnDim)
    }
  })

  observeEvent(input$spmEvent, {
    if (input$spmEvent$type == "hlPointEvent") {
      parallelPlot::highlightRow(ns("parcoords"), input$spmEvent$value$pointIndex)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "hlRowEvent") {
      scatterPlotMatrix::highlightPoint(ns("spm"), input$pcpEvent$value$rowIndex)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "cutoffChange" && !input$pcpEvent$value$adjusting) {
      ppCutoffs <- input$pcpEvent$value$cutoffs

      updatedDim <- input$pcpEvent$value$updatedDim
      if (ppCutoffs[updatedDim] == "NULL") {
        ppCutoffs[updatedDim] <- list(NULL)
      }

      setSpmCutoffsFromPP(ns("spm"), ppCutoffs)

      selectedTraces <<- unlist(input$pcpEvent$value$selectedTraces)
    }
  })

  appendPPCutoff <- function(ppCutoff, curCutoff, categories) {
    if (is.null(categories)) {
      return(append(ppCutoff, curCutoff))
    }
    else {
      keptCategories <- categories
      if (!is.null(curCutoff)) {
        sorted <- sort(unlist(curCutoff)) + 1
        if (ceiling(sorted[1]) <= floor(sorted[2])) {
          keptCategories <- categories[ceiling(sorted[1]):floor(sorted[2])]
        }
      }
      return(union(ppCutoff, list(keptCategories)))
    }
  }

  observeEvent(input$spmEvent, {
    req(input$spmEvent)
    if (input$spmEvent$type == "cutoffChange" && !input$spmEvent$value$adjusting) {
      spmCutoffs <- input$spmEvent$value$cutoffs
      ppCutoffs <- NULL
      if (!is.null(spmCutoffs)) {
        dimNames <- colnames(dataIXY())
        ppCutoffs <- list()
        for (dimName in dimNames) {
          ppCutoffs[dimName] <- list(NULL)
        }
        for (i in seq_along(spmCutoffs)) {
          xDim <- spmCutoffs[[i]]$xDim
          if (!is.vector(ppCutoffs[[xDim]])) {
            ppCutoffs[[xDim]] <- vector()
          }

          yDim <- spmCutoffs[[i]]$yDim
          if (!is.vector(ppCutoffs[[yDim]])) {
            ppCutoffs[[yDim]] <- vector()
          }

          for (xyCutoff in spmCutoffs[[i]]$xyCutoffs) {
            ppCutoffs[[xDim]] <- appendPPCutoff(ppCutoffs[[xDim]], xyCutoff[1], levels(dataIXY()[[which(dimNames == xDim)]]))
            ppCutoffs[[yDim]] <- appendPPCutoff(ppCutoffs[[yDim]], xyCutoff[2], levels(dataIXY()[[which(dimNames == yDim)]]))
          }
        }
      }
      parallelPlot::setCutoffs(ns("parcoords"), ppCutoffs)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "cutoffChange" && !input$pcpEvent$value$adjusting) {
      ppCutoffs <- input$pcpEvent$value$cutoffs

      updatedDim <- input$pcpEvent$value$updatedDim
      if (ppCutoffs[updatedDim] == "NULL") {
        ppCutoffs[updatedDim] <- list(NULL)
      }

      setSpmCutoffsFromPP(ns("spm"), ppCutoffs)
    }
  })

  setSpmCutoffsFromPP <- function(spmId, ppCutoffs) {
    spmCutoffs <- NULL
    if (is.list(ppCutoffs)) {
      categorical <- isolate(pcpConfig()$categorical)
      dimNames <- colnames(dataIXY())
      spmCutoffs <- vector()
      for (dimName in names(ppCutoffs)) {
        ppCutoff <- ppCutoffs[[dimName]]
        if (!is.null(ppCutoff)) {
          spCutoff <- list(xDim = dimName, yDim = dimName)
          if (!is.null(categorical[[which(dimNames == dimName)]])) {
            ppCutoff <- Filter(function(e) { return(e %in% unique(dataIXY()[[dimName]]))}, ppCutoff) # Workaround (bug in 'spm.setCutoffs' when a category is not used in data)
            categories <- categorical[[which(dimNames == dimName)]]
            spCutoff$xyCutoffs <- sapply(ppCutoff, function(cat) {
              catIndex <- which(cat == categories)
              list(list(NULL, c(catIndex - 1 - 1 / 8, catIndex - 1 + 1 / 8)))
            })
          }
          else {
            xyCutoffs <- list()
            for (cutoff in ppCutoff) {
              xyCutoffs <- append(xyCutoffs, list(list(NULL, rev(cutoff))))
            }
            spCutoff$xyCutoffs <- xyCutoffs
          }
          spmCutoffs <- append(spmCutoffs, list(spCutoff))
        }
      }
    }
    scatterPlotMatrix::setCutoffs(spmId, spmCutoffs)
  }

  # If continuous palette has been changed ...
  observeEvent(input$choose.palette.num, {
    scatterPlotMatrix::setContinuousColorScale(ns("spm"), input$choose.palette.num)
    parallelPlot::setContinuousColorScale(ns("parcoords"), input$choose.palette.num)
  })

  # If categorical palette has been changed ...
  observeEvent(input$choose.palette.cat, {
    scatterPlotMatrix::setCategoricalColorScale(ns("spm"), input$choose.palette.cat)
    parallelPlot::setCategoricalColorScale(ns("parcoords"), input$choose.palette.cat)
  })

  # If arrange method has been changed ...
  observeEvent(input$arrange.method, {
    parallelPlot::setArrangeMethod(ns("parcoords"), input$arrange.method)
  })

  callModule(pcpExport.server,
    "pcpExport",
    parallelPlotId = ns("parcoords"),
    datavisu = dataIXY
  )

  callModule(spmExport.server,
    "spmExport",
    scatterPlotMatrixId = ns("spm"),
    datavisu = dataIXY
  )

  observeEvent(input$zoom.button, {
    parallelPlot::getPlotConfig(
      ns("parcoords"),
      ns("configForPcpZoom")
    )
    scatterPlotMatrix::getPlotConfig(
      ns("spm"),
      ns("configForSpmZoom")
    )
    toggleModal(session, "zoomModal", toggle = "open")
  })
  
  observeEvent(input$close, {
    toggleModal(session, "zoomModal", toggle = "close")
  })
  
  output$zoomContent <- renderUI({
    tagList(
      div(id = ns("zoomPcpspm"),
        parallelPlotOutput(ns("zoomParcoords")),
        scatterPlotMatrixOutput(ns("zoomSpm"), height = "800px")
      )
    )
  })
  
  output$zoomParcoords <- renderParallelPlot({
    req(input$configForPcpZoom, cancelOutput = TRUE)
    if (is.null(selectedTraces)) {
      data <- dataIXY()
    }
    else {
      data <- dataIXY()[selectedTraces,]
    }
    parallelPlot(
      data = data,
      categorical = input$configForPcpZoom$categorical,
      categoriesRep = input$configForPcpZoom$categoriesRep,
      arrangeMethod = input$configForPcpZoom$arrangeMethod,
      inputColumns = input$configForPcpZoom$inputColumns,
      keptColumns = input$configForPcpZoom$keptColumns,
      histoVisibility = input$configForPcpZoom$histoVisibility,
      invertedAxes = input$configForPcpZoom$invertedAxes,
      refRowIndex = input$configForPcpZoom$refRowIndex,
      refColumnDim = input$configForPcpZoom$refColumnDim,
      rotateTitle = input$configForPcpZoom$rotateTitle,
      columnLabels = input$configForPcpZoom$columnLabels,
      continuousCS = input$configForPcpZoom$continuousCS,
      categoricalCS = input$configForPcpZoom$categoricalCS,
      controlWidgets = NULL,
      cssRules = input$configForPcpZoom$cssRules,
      sliderPosition = input$configForPcpZoom$sliderPosition,
      eventInputId = ns("zoomPcpEvent")
    )
  })

  output$zoomSpm <- renderScatterPlotMatrix({
    req(input$configForSpmZoom, cancelOutput = TRUE)
    if (is.null(selectedTraces)) {
      data <- dataIXY()
    }
    else {
      data <- dataIXY()[selectedTraces,]
    }
    scatterPlotMatrix(
      data = data,
      controlWidgets = NULL,
      categorical = input$configForSpmZoom$categorical,
      inputColumns = input$configForSpmZoom$inputColumns,
      keptColumns = input$configForSpmZoom$keptColumns,
      zAxisDim = input$configForSpmZoom$zAxisDim,
      distribType = as.numeric(input$configForSpmZoom$distribType),
      regressionType = as.numeric(input$configForSpmZoom$regressionType),
      corrPlotType = input$configForSpmZoom$corrPlotType,
      corrPlotCS = input$configForSpmZoom$corrPlotCS,
      rotateTitle = input$configForSpmZoom$rotateTitle,
      columnLabels = input$configForSpmZoom$columnLabels,
      continuousCS = input$configForSpmZoom$continuousCS,
      categoricalCS = input$configForSpmZoom$categoricalCS,
      mouseMode = input$configForSpmZoom$mouseMode,
      cssRules = input$configForSpmZoom$cssRules,
      plotProperties = input$configForSpmZoom$plotProperties,
      slidersPosition = input$configForSpmZoom$slidersPosition,
      eventInputId = ns("zoomSpmEvent")
    )
  })

  observeEvent(input$zoomSpmEvent, {
    if (input$zoomSpmEvent$type == "zAxisChange") {
      parallelPlot::setRefColumnDim(ns("zoomParcoords"), input$zoomSpmEvent$value)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$zoomPcpEvent)
    if (input$zoomPcpEvent$type == "refColumnDimChange") {
      scatterPlotMatrix::setZAxis(ns("zoomSpm"), input$zoomPcpEvent$value$refColumnDim)
    }
  })

  observeEvent(input$zoomSpmEvent, {
    if (input$zoomSpmEvent$type == "hlPointEvent") {
      parallelPlot::highlightRow(ns("zoomParcoords"), input$zoomSpmEvent$value$pointIndex)
    }
  })

  observeEvent(input$zoomPcpEvent, {
    req(input$zoomPcpEvent)
    if (input$zoomPcpEvent$type == "hlRowEvent") {
      scatterPlotMatrix::highlightPoint(ns("zoomSpm"), input$zoomPcpEvent$value$rowIndex)
    }
  })

  observeEvent(input$zoomPcpEvent, {
    req(input$zoomPcpEvent)
    if (input$zoomPcpEvent$type == "cutoffChange" && !input$zoomPcpEvent$value$adjusting) {
      ppCutoffs <- input$zoomPcpEvent$value$cutoffs

      updatedDim <- input$zoomPcpEvent$value$updatedDim
      if (ppCutoffs[updatedDim] == "NULL") {
        ppCutoffs[updatedDim] <- list(NULL)
      }

      setSpmCutoffsFromPP(ns("zoomSpm"), ppCutoffs)
    }
  })
}
	