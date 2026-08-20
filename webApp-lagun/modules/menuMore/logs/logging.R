#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module logging
logging.ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
        pickerInput(
          inputId = ns("logSelector"),
          label = "Displayed log",
          choices = rownames(logger$orderedInfos()),
          options = pickerOptions(
            style = "btn-primary"
          )
        )
    ),
    fluidRow(
      verbatimTextOutput(ns("logs"))
    )
  )
}

logging.server <- function(input, output, session) {

  selectedLogMTime <- NULL # Used to determine if the log currently displayed should be updated

  previousSortedLogs <- NULL # Used to determine if the list of logs currently displayed should be updated

  sortedLogs <- reactive({
    # Check log list every 10 seconds (except the first time)
    millis <- ifelse(is.null(previousSortedLogs), 1000, 10000)
    invalidateLater(millis, session)
    return(rownames(logger$orderedInfos()))
  })
  
  observe({
    # If log list has changed, update the log selector
    if (is.null(previousSortedLogs) || toString(previousSortedLogs) != toString(sortedLogs())) {
      previousSortedLogs <<- sortedLogs()
      selectedLogMTime <<- NULL
      updatePickerInput(
        session = session,
        inputId = "logSelector",
        choices = sortedLogs()
      )
    }
  })

  output$logs <- renderPrint({
    req(input$logSelector, file.exists(input$logSelector))

    # Check last log modification time every 1 second
    invalidateLater(1000, session)
    
    # If last log modification time is unchanged, leave unchanged the output 
    mTime <- toString(file.info(input$logSelector)$mtime[1])
    req(is.null(selectedLogMTime) || mTime != selectedLogMTime, cancelOutput = T)
    selectedLogMTime <<- mTime

    return(cat(paste(readLines(input$logSelector), collapse = "\n")))
  })
}
