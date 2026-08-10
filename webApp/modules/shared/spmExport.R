#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module spmExport
spmExport.ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("panel"))
}

spmExport.server <- function(input, output, session, scatterPlotMatrixId, datavisu) {
  ns <- session$ns
  
  spmForHtmlExport <- NULL
  spmAsListForJsonExport <- NULL

  output$panel <- renderUI({
    tagList(
      fluidRow(
        column(12,
          actionGroupButtons(
            inputIds = c(ns("exportSpmAsHtmlButton"), ns("exportAsJsonSpmButton")),
            labels = list("Export As HTML", "Export As JSON"),
            status = "primary"
          ),
          align = "center"
        )
      ),
      fluidRow(
        column(6,
          downloadButton(ns("associatedSpmAsHtmlButton"), label = "Export As HTML", class = "btn-primary"),
          align = "center",
        ),
        column(6,
          downloadButton(ns("associatedSpmAsJsonButton"), label = "Export As JSON", class = "btn-primary"),
          align = "center"
        ),
        style = "visibility: hidden;"
      )
    )
  })
  
  # If 'exportSpmAsHtmlButton' has been clicked, send a 'getPlotConfig' message
  observeEvent(input$exportSpmAsHtmlButton, {
    scatterPlotMatrix::getPlotConfig(
      scatterPlotMatrixId,
      ns("SpmConfigForHtmlExport")
    )
  })

  # When the 'SpmConfigForHtmlExport' reactive input is changed,
  # create a scatterPlotMatrix and save it by emulating a
  # click on 'associatedSpmAsHtmlButton'
  observeEvent(input$SpmConfigForHtmlExport, {
    spmForHtmlExport <<- scatterPlotMatrix(
      data = datavisu(),
      controlWidgets = NULL,
      categorical = input$SpmConfigForHtmlExport$categorical,
      inputColumns = input$SpmConfigForHtmlExport$inputColumns,
      cutoffs = input$SpmConfigForHtmlExport$cutoffs,
      keptColumns = input$SpmConfigForHtmlExport$keptColumns,
      zAxisDim = input$SpmConfigForHtmlExport$zAxisDim,
      distribType = as.numeric(input$SpmConfigForHtmlExport$distribType),
      regressionType = as.numeric(input$SpmConfigForHtmlExport$regressionType),
      corrPlotType = input$SpmConfigForHtmlExport$corrPlotType,
      corrPlotCS = input$SpmConfigForHtmlExport$corrPlotCS,
      rotateTitle = input$SpmConfigForHtmlExport$rotateTitle,
      columnLabels = input$SpmConfigForHtmlExport$columnLabels,
      continuousCS = input$SpmConfigForHtmlExport$continuousCS,
      categoricalCS = input$SpmConfigForHtmlExport$categoricalCS,
      mouseMode = input$SpmConfigForHtmlExport$mouseMode,
      cssRules = input$SpmConfigForHtmlExport$cssRules,
      plotProperties = input$SpmConfigForHtmlExport$plotProperties,
      slidersPosition = input$SpmConfigForHtmlExport$slidersPosition
    )
    shinyjs::runjs(paste0(
      "document.getElementById('",
      ns("associatedSpmAsHtmlButton"),
      "').click();"
    ))
  })

  # if a click on 'associatedSpmAsHtmlButton' occured,
  # save 'spmForHtmlExport' widget (previously created)
  output$associatedSpmAsHtmlButton <- downloadHandler(
    filename = function() {
      # To determine if pandoc is available, try a fake 'saveWidget'
      saveStatus <- try({
        saveWidget(spmForHtmlExport, tempfile(pattern = "file", tmpdir = tempdir(), fileext = "html"))
      })
      if (inherits(saveStatus, "try-error")) {
        # If the fake 'saveWidget' has failed, maybe pandoc is not available, so 'selfcontained' cannot be used, a zip file will be used instead
        paste("scatterPlotMatrix-", Sys.Date(), ".zip", sep = "")
      }
      else {
        paste("scatterPlotMatrix-", Sys.Date(), ".html", sep = "")
      }
    },
    content = function(tmpContentFile) {
      saveStatus <- try({
        saveWidget(spmForHtmlExport, tmpContentFile)
      })
      # If 'saveWidget' failed, try again with 'selfcontained = FALSE'
      if (inherits(saveStatus, "try-error")) {
        showModal(modalDialog(HTML(paste('Exporting the plot as a single self-contained HTML file failed.',
                                        'Trying to export as a ZIP file (containing an HTML file with external resources placed in an adjacent directory).',
                                        sep = '<br/>')), title = "Warning", size = 'l'))
        
        #  Since the first 'saveWidget' failed, 'tmpContentFile' should have a 'zip' extension
        tmpHtmlFile <- paste0(substr(tmpContentFile, 1, nchar(tmpContentFile) - 4), ".html")
        saveWidget(spmForHtmlExport, tmpHtmlFile, selfcontained = FALSE)
        wdToRestore <- getwd()
        setwd(dirname(tmpHtmlFile))
        extDir <- paste0(substr(tmpHtmlFile, 1, nchar(tmpHtmlFile) - 5), "_files")
        extDirList <- dir(basename(extDir), full.names = TRUE, recursive = TRUE)
        unlink(tmpContentFile)
        zip(tmpContentFile, c(basename(tmpHtmlFile), extDirList))
        setwd(wdToRestore)
      }
    }
  )

  # If 'exportAsJsonSpmButton' has been clicked, send a 'getPlotConfig' message
  observeEvent(input$exportAsJsonSpmButton, {
    scatterPlotMatrix::getPlotConfig(
      scatterPlotMatrixId,
      ns("spmConfigForJsonExport")
    )
  })

  # When the 'spmConfigForJsonExport' reactive input is changed,
  # create a scatterPlotMatrix and save it by emulating a
  # click on 'associatedSpmAsJsonButton'
  observeEvent(input$spmConfigForJsonExport, {
    spmAsListForJsonExport <<- list(
      data = datavisu(),
      controlWidgets = NULL,
      categorical = input$spmConfigForJsonExport$categorical,
      inputColumns = input$spmConfigForJsonExport$inputColumns,
      cutoffs = input$spmConfigForJsonExport$cutoffs,
      keptColumns = input$spmConfigForJsonExport$keptColumns,
      zAxisDim = input$spmConfigForJsonExport$zAxisDim,
      distribType = as.numeric(input$spmConfigForJsonExport$distribType),
      regressionType = as.numeric(input$spmConfigForJsonExport$regressionType),
      corrPlotType = input$spmConfigForJsonExport$corrPlotType,
      corrPlotCS = input$spmConfigForJsonExport$corrPlotCS,
      rotateTitle = input$spmConfigForJsonExport$rotateTitle,
      columnLabels = input$spmConfigForJsonExport$columnLabels,
      continuousCS = input$spmConfigForJsonExport$continuousCS,
      categoricalCS = input$spmConfigForJsonExport$categoricalCS,
      mouseMode = input$spmConfigForJsonExport$mouseMode,
      cssRules = input$spmConfigForJsonExport$cssRules,
      plotProperties = input$spmConfigForJsonExport$plotProperties,
      slidersPosition = input$spmConfigForJsonExport$slidersPosition
    )
    shinyjs::runjs(paste0(
      "document.getElementById('",
      ns("associatedSpmAsJsonButton"),
      "').click();"
    ))
  })

  # if a click on 'associatedSpmAsJsonButton' occured,
  # save 'spmAsListForJsonExport' (previously created)
  output$associatedSpmAsJsonButton <- downloadHandler(
    filename = function() {
      paste("scatterPlotMatrix-", Sys.Date(), ".json", sep = "")
    },
    content = function(tmpContentFile) {
      jsonlite::write_json(spmAsListForJsonExport, dataframe = "columns", pretty = TRUE, tmpContentFile)
    }
  )

}
