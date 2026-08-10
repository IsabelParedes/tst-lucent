#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

computeOF <- function(ofData, yValues) {
  objFunc <- list()
  if (ofData$norm == "L2") {
    objFunc$OF <- sapply(seq_along(ofData$idZ), function(j) {
      z <- ofData$Z[, ofData$idZ[[j]], drop = F]
      sigZ <- ofData$sigZ[ofData$idZ[[j]]]
      y <- yValues[ofData$idZY[[j]]]
      sum((y - z)^2 / sigZ^2)
    })
  } else if (ofData$norm == "L1") {
    objFunc$OF <- sapply(seq_along(ofData$idZ), function(j) {
      z <- ofData$Z[, ofData$idZ[[j]], drop = F]
      sigZ <- ofData$sigZ[ofData$idZ[[j]]]
      y <- yValues[ofData$idZY[[j]]]
      sum(abs(y - z) / sigZ)
    })
  }
  
  objFunc$OFtot <- objFunc$OF %*% ofData$weights
  objFunc$OF <- as.data.frame(rbind(objFunc$OF))
  objFunc$OFtot <- as.data.frame(rbind(objFunc$OFtot))
  colnames(objFunc$OF) <- paste0("OF", seq_along(ofData$idZ))
  colnames(objFunc$OFtot) <- "OFtotal"

  return(objFunc)
}

defineObjective.ui <- function(id) {
  ns <- NS(id)
  
  modalContent <- tagList(
    uiOutput(ns("weights")),
    fluidRow(
      column(3, actionButton(ns("save"), label = "Save and Close", class = "btn-warning",
                             width = '100%'), offset = 2),
      column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                             width = '100%'), offset = 2)
    )
  )
  
  tagList(
    fluidRow(
      column(1, br(), 
             actionButton(ns("changeWeights"), label = HTML(paste("Change", "OF Weights", sep = '<br>')),
                             class = "btn-primary", width = '100%')),
      column(11, 
        fluidRow(
          column(12, DT::dataTableOutput(ns('tableOFStat'))),
          column(12, DT::dataTableOutput(ns('tableOF')))
        )
      )
    ),
    bsModal(ns("modalWeights"), "Change Weights", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modalWeights")," .modal-footer{display:none}",
                                        " .modal-lg{width: 20%}"))))
  )
}


defineObjective.server <- function(input, output, session, DOE, xpData, persistence, settings) {
  
  ns <- session$ns
  
  objFunc <- reactiveValues(OF = NULL, norm = "L2", weights = NULL, OFtot = NULL, weightsTemp = NULL)

  selectedNorm <- callModule(dynamicSelectpicker.server, "chooseNorm", label.title = "Norm", choices = reactive(c("L1", "L2")),
                         selected = "L2", multiple = FALSE, livesearch = TRUE)
  
  output$weights <- renderUI({
    tagList(
      fluidRow(
        column(2,
               dynamicSelectpicker.ui(ns("chooseNorm")),
               lapply(1:length(DOE$nF), function(j){
                 numericInput(ns(paste0("weightOF", j)), label = paste0("Weight OF", j), value = 1)
               }), offset = 5
        )
      ), hr()
    )
  })
  
  observeEvent(input$changeWeights, {
    toggleModal(session, "modalWeights", toggle = "open")
  })
  
  observe({
    req(length(DOE$nF) > 0, length(objFunc$weights) != length(DOE$nF))
    objFunc$weightsTemp <- rep(1, length(DOE$nF))
    objFunc$weights <- objFunc$weightsTemp
  })
  
  observe({
    req(length(DOE$nF) > 0)
    lapply(1:length(DOE$nF), function(j){
      observeEvent(input[[paste0("weightOF", j)]], {
        req(input[[paste0("weightOF", j)]])
        objFunc$weightsTemp[j] <- input[[paste0("weightOF", j)]]
      })
    })
  })

  observeEvent(input$save, {
    objFunc$weights <- objFunc$weightsTemp
    objFunc$norm <- selectedNorm()
    toggleModal(session, "modalWeights", toggle = "close")
  })
  
  observeEvent(input$close, {
    toggleModal(session, "modalWeights", toggle = "close")
  })
  
  # Compute OF for all points if weights, Z, sigZ or normalization type have changed
  observeEvent(list(objFunc$weights, xpData$Z, xpData$sigZ, objFunc$norm), {
    req(DOE$XY, xpData$Z, objFunc$norm, objFunc$weights, xpData$idZ, xpData$idZY)

    ofData <- list(
      norm = objFunc$norm,
      idZY = DOE$idF,
      Z = xpData$Z,
      idZ = xpData$idZ,
      sigZ = xpData$sigZ,
      weights = objFunc$weights
    )

    OFtot <- c()
	  OF <- c()
    for (i in seq_len(nrow(DOE$Y))) {
      ofRes <- computeOF(ofData, DOE$Y[i, ])
      OFtot <- rbind(OFtot, unlist(ofRes$OFtot))
      OF <- rbind(OF, unlist(ofRes$OF))
    }
    objFunc$OFtot <- as.data.frame(OFtot)
    objFunc$OF <- as.data.frame(OF)
  })
  
  # When DOE$Y has changed, compute OF only for points having results and not yet processed
  observeEvent(DOE$Y, {
    req(DOE$XY, xpData$Z, objFunc$norm, objFunc$weights, xpData$idZ, xpData$idZY)

    ofData <- list(
      norm = objFunc$norm,
      idZY = xpData$idZY,
      Z = xpData$Z,
      idZ = xpData$idZ,
      sigZ = xpData$sigZ,
      weights = objFunc$weights
    )

    ofResNaN <- list()
    ofResNaN$OF <- rbind(rep(NA_real_, length(ofData$idZ)))
    ofResNaN$OFtot <- rbind(NA_real_)
    colnames(ofResNaN$OF) <- paste0("OF", seq_along(ofData$idZ))
    colnames(ofResNaN$OFtot) <- "OFtotal"

    OFtot <- c()
	  OF <- c()
    for (i in seq_len(nrow(DOE$Y))) {
      # if Y is not yet available, set OF to NaN
      if (is.na(DOE$Y[i, 1])) { # maybe we should check all elements (not only the first one)
        OFtot <- rbind(OFtot, ofResNaN$OFtot)
        OF <- rbind(OF, ofResNaN$OF)
      }
      else {
        if (is.null(objFunc$OFtot[i,]) || is.na(objFunc$OFtot[i,])) {
          ofRes <- computeOF(ofData, DOE$Y[i, ])
          OFtot <- rbind(OFtot, unlist(ofRes$OFtot))
          OF <- rbind(OF, unlist(ofRes$OF))
        }
        else {
          OFtot <- rbind(OFtot, objFunc$OFtot[i,])
          OF <- rbind(OF, objFunc$OF[i,])
        }
      }
    }
    objFunc$OFtot <- as.data.frame(OFtot)
    objFunc$OF <- as.data.frame(OF)
  })
  
  observeEvent(list(xpData$Z, xpData$sigZ), {
    if (is.null(xpData$Z)){
      objFunc$OF <- NULL
      objFunc$OFtot <- NULL
    }
  })
  
  ofStatTable <- reactive({
    req(objFunc$OF, objFunc$OFtot)

    dfOF <- as.data.frame(cbind(objFunc$OF, objFunc$OFtot))
    dfBottom <- apply(dfOF, 2, function(of) {
      ofWithoutNaN <- of[!is.na(of)]
      if (length(ofWithoutNaN) == 0) {
        return(c(NaN, NaN, NaN))
      }
      c(min(ofWithoutNaN), mean(ofWithoutNaN), max(ofWithoutNaN))
    })
    df <- rbind(c(rep(objFunc$norm, ncol(objFunc$OF)), NA), c(objFunc$weights, NA), dfBottom)
    rownames(df) <- c("Norm", "Weight", "Min", "Mean", "Max")

    return(df)
  })

  output$tableOFStat <- DT::renderDataTable({
    req(length(ofStatTable()) != 0)

    DT::datatable(
      ofStatTable(), escape = FALSE,
      extensions = c("FixedColumns", "Scroller"),
      options = list(
        dom = "t",
        columnDefs = list(
          list(targets = seq_len(ncol(ofStatTable())), className = "dt-right", render = JS(
              "function(data, type, row, meta) {
                  if (row[0] == 'Norm' || row[0] == 'Weight') {
                    return data;
                  }
                  return (type == 'display' && data == null) ? 'Not a number' : data;
               }"
            )
          )
        ),
        scrollX = TRUE, scrollY = 200, scroller = TRUE, fixedColumns = TRUE
      ), selection = "single") %>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual(c("Norm", "Weight", "Min", "Mean", "Max"), rep("bold", 5))
      )
  })
  
  ofTable <- reactive({
    req(objFunc$OF, objFunc$OFtot)

    df <- as.data.frame(cbind(objFunc$OF, objFunc$OFtot))
    rownames(df) <- seq_len(nrow(objFunc$OF))

    return(df)
  })

  output$tableOF <- DT::renderDataTable({
    req(length(ofTable() != 0), cancelOutput = TRUE)

    dt <- DT::datatable(
      ofTable(), escape = FALSE, 
      extensions = c("FixedColumns", "Scroller"), filter = "top",
      options = list(
        dom = "rtip",
        columnDefs = list(
          list(orderable = TRUE, targets = 0), # Add row header ordering
          list(targets = seq_len(ncol(ofTable())), render = JS(
              "function(data, type, row, meta) {
                  return (type == 'display' && data == null) ? 'Not a number' : data;
               }"
            )
          )
        ),
        scrollX = TRUE, scrollY = 400, scroller = TRUE, fixedColumns = TRUE
      ), selection = "single") %>%
      formatStyle(
        0,
        target = "row"
      )
      dt$x$data[[1]] <- as.numeric(dt$x$data[[1]]) # useful to have a good row header ordering
      dt
  })
  
  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "defineObjective-clean") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      objFunc$norm = NULL
      objFunc$weightsTemp <- NULL
      objFunc$weights <- objFunc$weightsTemp
      objFunc$OF = NULL
      objFunc$OFtot = NULL
      progressToNextStep(persistence)
    }
    else if (
      persistence$updatingStep == "defineObjective-menuImport" &&
      grepl("nav-menuImport-defineCalibration-defineObjective", ns("bidon"))
    ) {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      if (!is.null(persistence$loadedStudy$calibration)) {
        objFunc$weightsTemp <- persistence$loadedStudy$calibration$weights
        objFunc$weights <- objFunc$weightsTemp
        objFunc$norm <- persistence$loadedStudy$calibration$norm
        selectedNorm(objFunc$norm)
      }
      progressToNextStep(persistence)
    }
    else if (
      persistence$updatingStep == "defineObjective-directOptim" &&
      grepl("nav-menuImport-importDOE-directOptim-defineCalibration-defineObjective", ns("bidon"))
    ) {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      if (!is.null(persistence$loadedStudy$directOptim$calibration)) {
        objFunc$weightsTemp <- persistence$loadedStudy$directOptim$calibration$weights
        objFunc$weights <- objFunc$weightsTemp
        objFunc$norm <- persistence$loadedStudy$directOptim$calibration$norm
        selectedNorm(objFunc$norm)
      }
      progressToNextStep(persistence)
    }
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  return(objFunc)
  
}
