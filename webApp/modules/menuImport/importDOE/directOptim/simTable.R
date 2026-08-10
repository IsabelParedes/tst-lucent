###############################
#  Define client user interface
###############################

simTable.ui <- function(id) {
  ns <- NS(id)

  tagList(
    DTOutput(ns("simulationsView"))
  )
}

#####################
# Define server logic
#####################

simTable.server <- function(input, output, session, define, data.plot, launcherRuns, advance.simu) {
  ns <- session$ns

  simTable <- reactive({
    req(length(data.plot$runRefs) != 0, cancelOutput = TRUE)
    tableRows <- NULL
    for (runIndex in seq_len(length(data.plot$runRefs))) {
        runIdAsString <- data.plot$runRefs[[runIndex]]$runIdAsString
        # Trick to have a natural ordering of the 'Directory' column: 
        # use runId, convert it later as a directory name in a custom renderer
        dirMatrix <- matrix(as.numeric(runIdAsString), dimnames = list(runIndex, "Directory"))
        status <- launcherRuns[[runIdAsString]]$status
        status <- ifelse(is.null(status), " ", status)
        statusMatrix <- matrix(status, dimnames = list(runIndex, "Status"))

        xValues <- data.plot$dataX[runIndex, ]

        yids <- define$COformulation$idO
        if (length(define$COformulation$idC) > 0) {
          yids <- c(yids, define$COformulation$idC)
        }
        if (isTRUE(define$COformulation$isInversion)) {
          yids <- define$COformulation$idC
        }
        
        yValues <- data.plot$dataY[runIndex, yids, drop = F]

        # cat("******* statusMatrix *******")
        # write.table(statusMatrix)
        # cat("******* xValues *******")
        # write.table(xValues)
        # cat("******* yValues *******")
        # write.table(yValues)

        curRow <- cbind(dirMatrix, statusMatrix, xValues, yValues)
        rownames(curRow) <- data.plot$rowNames[runIndex]
        if (is.null(tableRows)) {
          tableRows <- curRow
        }
        else {
          colnames(tableRows) <- colnames(curRow)
          tableRows <- rbind(tableRows, curRow)
        }
    }

    tableRows
  })

  throttle.simTable <- throttle(simTable, 1000)

  yColNames <- NULL

  initDone <- function(simTable) {
    newYColNames <- colnames(simTable)[(3 + ncol(data.plot$dataX)):ncol(simTable)]
    return(
      !is.null(yColNames) && 
      length(newYColNames) == length(yColNames) &&
      all(newYColNames == yColNames)
    )
  }
  
  output$simulationsView = DT::renderDT({
    simTable <- throttle.simTable()
    # If 'initDone' is FALSE, update DT rendering
    # (else DT rendering is done through the 'dtProxy')
    req(simTable, !initDone(simTable), cancelOutput = TRUE)
    yColNames <<- colnames(simTable)[(3 + ncol(data.plot$dataX)):ncol(simTable)]
    DT::datatable(
      simTable,
      extensions = c("FixedColumns","Scroller"),
      options = list(
          dom = "t",
          columnDefs = list(
            # To the 'Directory' column, set a custom renderer which converts received 'runId' as a directory name
            list(targets = 1, render = JS(
                "function(data, type, row, meta) {
                    return (type == 'sort') ? data : 'run' + (data + 1);
                 }"
              )
            ),
            list(targets = (2 + ncol(data.plot$dataX)):ncol(simTable), render = JS(
                "function(data, type, row, meta) {
                    return (type == 'display' && row[2] == 'ended' && data == null) ? '<b>Not a number</b>' : data;
                 }"
              )
            )
          ),
          scrollX = T, scrollY = 400, scroller = TRUE, fixedColumns = list(leftColumns = 2),
          autoWidth = F
      )
    )
  })

  dtProxy = dataTableProxy("simulationsView")
  observe({
    simTable <- throttle.simTable()
    req(simTable, initDone(simTable), cancelOutput = TRUE)
    replaceData(dtProxy, simTable, resetPaging = FALSE)
  })
}
