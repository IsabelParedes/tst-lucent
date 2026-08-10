#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module pcpExport
pcpExport.ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("panel"))
}

pcpExport.server <- function(input, output, session, parallelPlotId, datavisu) {
  ns <- session$ns
  
  pcpForHtmlExport <- NULL
  pcpAsListForJsonExport <- NULL

  output$panel <- renderUI({
    tagList(
      fluidRow(
        column(12,
          actionGroupButtons(
            inputIds = c(ns("exportPcpAsHtmlButton"), ns("exportAsJsonPcpButton")),
            labels = list("Export As HTML", "Export As JSON"),
            status = "primary"
          ),
          align = "center"
        )
      ),
      fluidRow(
        column(6,
          downloadButton(ns("associatedPcpAsHtmlButton"), label = "Export As HTML", class = "btn-primary"),
          align = "center",
        ),
        column(6,
          downloadButton(ns("associatedPcpAsJsonButton"), label = "Export As JSON", class = "btn-primary"),
          align = "center"
        ),
        style = "visibility: hidden;"
      )
    )
  })
  
  # If 'exportPcpAsHtmlButton' has been clicked, send a 'getPlotConfig' message
  observeEvent(input$exportPcpAsHtmlButton, {
    parallelPlot::getPlotConfig(
      parallelPlotId,
      ns("pcpConfigForHtmlExport")
    )
  })

  # When the 'pcpConfigForHtmlExport' reactive input is changed,
  # create a parallelPlot and save it by emulating a
  # click on 'associatedPcpAsHtmlButton'
  observeEvent(input$pcpConfigForHtmlExport, {
    pcpForHtmlExport <<- parallelPlot(
      data = datavisu(),
      categorical = input$pcpConfigForHtmlExport$categorical,
      categoriesRep = input$pcpConfigForHtmlExport$categoriesRep,
      arrangeMethod = input$pcpConfigForHtmlExport$arrangeMethod,
      inputColumns = input$pcpConfigForHtmlExport$inputColumns,
      keptColumns = input$pcpConfigForHtmlExport$keptColumns,
      histoVisibility = input$pcpConfigForHtmlExport$histoVisibility,
      invertedAxes = input$pcpConfigForHtmlExport$invertedAxes,
      cutoffs = input$pcpConfigForHtmlExport$cutoffs,
      refRowIndex = input$pcpConfigForHtmlExport$refRowIndex,
      refColumnDim = input$pcpConfigForHtmlExport$refColumnDim,
      rotateTitle = input$pcpConfigForHtmlExport$rotateTitle,
      columnLabels = input$pcpConfigForHtmlExport$columnLabels,
      continuousCS = input$pcpConfigForHtmlExport$continuousCS,
      categoricalCS = input$pcpConfigForHtmlExport$categoricalCS,
      controlWidgets = NULL,
      cssRules = input$pcpConfigForHtmlExport$cssRules,
      sliderPosition = input$pcpConfigForHtmlExport$sliderPosition
    )
    shinyjs::runjs(paste0(
      "document.getElementById('",
      ns("associatedPcpAsHtmlButton"),
      "').click();"
    ))
  })

  # if a click on 'associatedPcpAsHtmlButton' occured,
  # save 'pcpForHtmlExport' widget (previously created)
  output$associatedPcpAsHtmlButton <- downloadHandler(
    filename = function() {
      # To determine if pandoc is available, try a fake 'saveWidget'
      saveStatus <- try({
        saveWidget(pcpForHtmlExport, tempfile(pattern = "file", tmpdir = tempdir(), fileext = "html"))
      })
      if (inherits(saveStatus, "try-error")) {
        # If the fake 'saveWidget' has failed, maybe pandoc is not available, so 'selfcontained' cannot be used, a zip file will be used instead
        paste("parallelPlot-", Sys.Date(), ".zip", sep = "")
      }
      else {
        paste("parallelPlot-", Sys.Date(), ".html", sep = "")
      }
    },
    content = function(tmpContentFile) {
      saveStatus <- try({
        saveWidget(pcpForHtmlExport, tmpContentFile)
      })
      # If 'saveWidget' failed, try again with 'selfcontained = FALSE'
      if (inherits(saveStatus, "try-error")) {
        showModal(modalDialog(HTML(paste('Exporting the plot as a single self-contained HTML file failed.',
                                        'Trying to export as a ZIP file (containing an HTML file with external resources placed in an adjacent directory).',
                                        sep = '<br/>')), title = "Warning", size = 'l'))
        
        #  Since the first 'saveWidget' failed, 'tmpContentFile' should have a 'zip' extension
        tmpHtmlFile <- paste0(substr(tmpContentFile, 1, nchar(tmpContentFile) - 4), ".html")
        saveWidget(pcpForHtmlExport, tmpHtmlFile, selfcontained = FALSE)
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

  # If 'exportAsJsonPcpButton' has been clicked, send a 'getPlotConfig' message
  observeEvent(input$exportAsJsonPcpButton, {
    parallelPlot::getPlotConfig(
      parallelPlotId,
      ns("pcpConfigForJsonExport")
    )
  })

  # When the 'pcpConfigForJsonExport' reactive input is changed,
  # create a parallelPlot and save it by emulating a
  # click on 'associatedPcpAsJsonButton'
  observeEvent(input$pcpConfigForJsonExport, {
    pcpAsListForJsonExport <<- list(
      data = datavisu(),
      categorical = input$pcpConfigForJsonExport$categorical,
      categoriesRep = input$pcpConfigForJsonExport$categoriesRep,
      arrangeMethod = input$pcpConfigForJsonExport$arrangeMethod,
      inputColumns = input$pcpConfigForJsonExport$inputColumns,
      keptColumns = input$pcpConfigForJsonExport$keptColumns,
      histoVisibility = input$pcpConfigForJsonExport$histoVisibility,
      invertedAxes = input$pcpConfigForJsonExport$invertedAxes,
      cutoffs = input$pcpConfigForJsonExport$cutoffs,
      refRowIndex = input$pcpConfigForJsonExport$refRowIndex,
      refColumnDim = input$pcpConfigForJsonExport$refColumnDim,
      rotateTitle = input$pcpConfigForJsonExport$rotateTitle,
      columnLabels = input$pcpConfigForJsonExport$columnLabels,
      continuousCS = input$pcpConfigForJsonExport$continuousCS,
      categoricalCS = input$pcpConfigForJsonExport$categoricalCS,
      controlWidgets = NULL,
      cssRules = input$pcpConfigForJsonExport$cssRules,
      sliderPosition = input$pcpConfigForJsonExport$sliderPosition
    )
    shinyjs::runjs(paste0(
      "document.getElementById('",
      ns("associatedPcpAsJsonButton"),
      "').click();"
    ))
  })

  # if a click on 'associatedPcpAsJsonButton' occured,
  # save 'pcpAsListForJsonExport' (previously created)
  output$associatedPcpAsJsonButton <- downloadHandler(
    filename = function() {
      paste("parallelPlot-", Sys.Date(), ".json", sep = "")
    },
    content = function(tmpContentFile) {
      jsonlite::write_json(pcpAsListForJsonExport, dataframe = "columns", pretty = TRUE, tmpContentFile)
    }
  )

}
