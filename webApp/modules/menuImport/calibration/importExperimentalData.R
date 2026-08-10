#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

checkXPdata <- function(data, DOE, ynames){

  errorMessage <- list()
  nrowValid <- 1 <= nrow(data) & nrow(data) <= 2
  if (!nrowValid){
    nrowMessage <- 'Invalid number of rows in imported file. The first row must be the experimental data. 
                    The second optional row provides the standard deviation of each observation.'
    errorMessage <- c(errorMessage, list(nrowMessage))
  }
  ncolValid <- ncol(data) == length(DOE$Yinfos$func.ids)
  if (!ncolValid){
    ncolMessage <- 'Invalid number of columns in imported file. The number of columns must match the number
                    of fonctional output columns in the imported DOE.'
    errorMessage <- c(errorMessage, list(ncolMessage))
  }
  typeValid <- all(sapply(data, is.numeric)) & !anyNA(data)
  if (!typeValid){
    typeMessage <- 'Invalid data type. Data must take only numeric values.'
    errorMessage <- c(errorMessage, list(typeMessage))
  }
  varianceValid <- TRUE
  if (typeValid & nrow(data) == 2){
    varianceValid <- all(data[2,] > 0)
    if (!varianceValid){
      varianceMessage <- 'Invalid standard deviations. Standard deviations must take positive values.'
      errorMessage <- c(errorMessage, list(varianceMessage))
    }
  }
  dataValid <- nrowValid & ncolValid & typeValid & varianceValid
  
  namesValid <- all(ynames == DOE$ynamesmenu[DOE$Yinfos$func.ids])
  
  return(list(dataValid = dataValid, errorMessage = errorMessage, namesValid = namesValid))
  
}

importExperimentalData.ui <- function(id) {
  ns <- NS(id)
  
  tagList(
          fluidRow(
            column(8, fileInput(ns('file'), 'Select File', accept = c('.txt','.dat','.csv'))),
            column(4, "")
          ),
          uiOutput(ns("sepdec.dynui")),
          uiOutput(ns("preview.dynui")),
          DT::dataTableOutput(ns('headerdf'))
  )

}


importExperimentalData.server <- function(input, output, session, DOE, persistence, settings) {

  
  ns <- session$ns
  
  file.to.load <- reactiveValues(datapath = NULL)
  
  xpData <- reactiveValues(Z = NULL, sigZ = NULL, nZ = NULL, idZ = NULL, idZY = NULL, discZ = NULL, zFileName = NULL)
  
  
  observeEvent(input$file,{
    file.to.load$datapath <- input$file$datapath
  })
  
  
  # Once we have the file path, load it and try to autodetect separator, header and decimal
  
  header <- reactiveValues(bool = TRUE)
  separator <- reactiveValues(char= ",")
  decimal <- reactiveValues(char = ".")
  firstguessfile <- reactiveValues(finished = FALSE)
  
  observe({
    req(file.to.load$datapath)
    # Investigate the second line (to prevent a false detection if there is a header with points in variable names)
    line2 <- readLines(file.to.load$datapath, n = 2)
    if (length(line2) > 0){
      if (length(line2) >= 2){numLine <- 2}else{numLine <- 1}
      # Try all possible separators
      count.comma <- stri_count_fixed(line2, ",")[numLine]
      count.semicolon <- stri_count_fixed(line2, ";")[numLine]
      count.tab <- stri_count_fixed(line2, "\t")[numLine]
      if (count.semicolon > 0){
        separator$char <- ";"
        if (count.comma > 0){
          decimal$char <- ","
        }else{
          decimal$char <- "."
        }
      }else{
        if (count.tab > 0){
          separator$char <- "\t"
          if (count.comma > 0){
            decimal$char <- ","
          }else{
            decimal$char <- "."
          }
        }else{
          separator$char <- ","
          decimal$char <- "."
        }
        }
      # Then use the separator to detect if there is a header
      line1 <- readLines(file.to.load$datapath, n = 1)
      xynames <- unlist(strsplit(line1, separator$char))
      xynames <- gsub(paste0('[',decimal$char,']'), '.',  xynames)
      header$bool <- suppressWarnings(all(is.na(as.numeric(xynames))))
    }else{
      header$bool <- F
    }
    firstguessfile$finished <- TRUE
  })
  
  # Initialize separator and decimal UI with first guess
  # Separator and decimal UI now longer accessible once import is confirmed
  output$sepdec.dynui <- renderUI({
    req(firstguessfile$finished)
    tagList(
      hr(),
      h5("Header, Separator and Decimal have been auto-detected. Please change values if not correct."),
      fluidRow(
        column(4, radioButtons(ns("separator"), "Separator",
                               choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"), selected=separator$char)),
        column(4, radioButtons(ns("decimal"), "Decimal",
                               choices = list(". (point)" = ".", ", (comma)" = ","), selected=decimal$char))
      ),
      hr()
      )
  })
  
  # Now read file with appropriate settings
  file.data <- reactive({
    req(firstguessfile$finished, file.to.load$datapath, input$separator != input$decimal)
    # consistency check
    head.lines.consist <- length(unique(lapply(readLines(file.to.load$datapath, n = 2), function(line){
      stringi::stri_count(line, fixed = input$separator)
    })))
    if (head.lines.consist == 1){
      df <- read.csv(file.to.load$datapath, header = header$bool, sep = input$separator,
                     dec = input$decimal, check.names = F)
      df.temp <- df
      colnames(df.temp) <- NULL
      if (anyDuplicated(df.temp) > 0) {
        toggleModal(session, "modalduplicate", toggle = "open")
      }
    }else{
      df <- NULL
    }
    return(df)
  })
  
  # Show a file preview to check the loading
  output$preview.dynui <- renderUI({
    req(file.data())
    tagList(
      fluidRow(
        column(6,
               h4(paste0("File Data Preview: Observations = ",nrow(file.data()),", Variables = ",ncol(file.data())))
        ),
        column(6,XYnamesChange.ui(ns("change.names")))
      )
    )
  })
  
  initialXYnames <- reactiveValues(xynames = NULL)
  observeEvent(file.data(), {
    req(file.data(),!is.null(header$bool), separator$char)
    print("Read File for Names")
    # Read again file.data to get exaxt headers (without R formatting)
    if (header$bool){
      line1 <- readLines(file.to.load$datapath, n = 1)
      initialXYnames$xynames <- unlist(strsplit(line1, separator$char))
    }
  })
  newXYnames <- callModule(XYnamesChange.server, "change.names", initialXYnames)
  
  XYdata <- reactive({
    doe <- isolate(reactiveValuesToList(DOE))
    req(file.data(), doe$Yinfos, length(doe$nF) > 0)
    XPdataValid <- checkXPdata(file.data(), doe, initialXYnames$xynames)
    if (XPdataValid$dataValid){
      if (!XPdataValid$namesValid){
        showModal(modalDialog(HTML(paste('Experimental data names do not match the imported DOE names.',
                                         'Since the data dimensions are correct, column indices are used for experimental
                                         and simulated data matching.',
                                         sep = '<br/>')), title = "Warning", size = 'l'))
      }
      return(file.data())
    }else{
      xpData$Z <- NULL
      xpData$sigZ <- NULL
      xpData$nZ <- NULL 
      xpData$idZ <- xpData$idZY <- NULL
      xpData$discZ <- NULL
      xpData$zFileName <- NULL
      showModal(modalDialog(HTML(paste(XPdataValid$errorMessage, collapse = '<br/>')), title = "Error", size = 'l'))
      return(NULL)
    }
  })
  
  observeEvent(DOE$Yinfos, {
    DOE$Z <- NULL
    DOE$sigZ <- NULL
    DOE$nZ <- NULL 
    DOE$idZ <- DOE$idZY <- NULL
    DOE$discZ <- NULL
    
    xpData$Z <- NULL
    xpData$sigZ <- NULL
    xpData$nZ <- NULL 
    xpData$idZ <- xpData$idZY <- NULL
    xpData$discZ <- NULL
    xpData$zFileName <- NULL
    
    file.to.load$datapath <- NULL
    initialXYnames$xynames <- NULL
  })
  
  output$headerdf <- DT::renderDataTable({
    req(xpData$Z, xpData$sigZ, !is.null(header$bool))
    print("Table generated")
    d <- rbind(xpData$Z, xpData$sigZ)
    rownames(d) <- c('Data', 'Standard Deviation')
    dimd <- ncol(d)
    if (header$bool & !is.null(newXYnames$namesvisu)){
      colnames(d) <- newXYnames$namesvisu
    }
    DT::datatable(
      t(d), escape = FALSE,
      extensions = c('FixedColumns','Scroller'), filter = 'top',
      options = list(
        dom = 'Brtip',
        scrollX = TRUE, scroller = TRUE, scrollY = 400
      ))
  })
  
  
  
  # Update xpData with uploaded file
  observeEvent(XYdata(), {
    req(XYdata())
    xpData$Z <- XYdata()[1,,drop=F]
    sig0 <- rep(1, ncol(xpData$Z))
    if (nrow(XYdata()) > 1){
      xpData$sigZ <- XYdata()[2,,drop=F]
    }else{
      xpData$sigZ <- sig0
    }

    Fheader <- strsplit(colnames(XYdata()), '@')
    if (!all(unlist(lapply(Fheader, function(header) length(header) == 2)))) {
      Fheader <- strsplit(DOE$ynamesmenu[DOE$Yinfos$func.ids], '@')
    }
    Fnamesraw <- unlist(sapply(Fheader, function(x) { if (length(x) > 1) x[1] else "" }))
    Fnames <- Fnamesmenu <- Fnamesvisu <- unique(unlist(sapply(Fheader, function(x) { if (length(x) > 1) x[1] else NULL })))
    xpData$idZ <- lapply(Fnames, function(name){
      which(Fnamesraw == name)
    })
    xpData$nZ <- sapply(xpData$idZ, length)
    xpData$discZ <- lapply(1:length(Fnames), function(j){
      df <- sapply(Fheader[xpData$idZ[[j]]], function(x){as.numeric(x[2])})
      df <- as.data.frame(df)
      colnames(df) <- paste0('t', j)
      return(df)
    })
    if (is.null(xpData$idZY)) {
      xpData$idZY <- xpData$idZ
    }

    xpData$zFileName <- input$file$name
  })
  
  ##################################################################
  # LOADED STUDY
  
  observeEvent(persistence$updatingStep, {
    if (
      persistence$updatingStep == "importExperimentalData-menuImport" &&
      grepl("nav-menuImport-defineCalibration-importXPdata", ns("bidon"))
    ) {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      if (!is.null(persistence$loadedStudy$calibration)) {
        xpData$Z <- persistence$loadedStudy$calibration$Z
        xpData$sigZ <- persistence$loadedStudy$calibration$sigZ
        xpData$nZ <- persistence$loadedStudy$calibration$nF
        xpData$idZ <- persistence$loadedStudy$calibration$idZ
        xpData$idZY <- persistence$loadedStudy$calibration$idZY
        xpData$discZ <- persistence$loadedStudy$calibration$discZ
        xpData$zFileName <- persistence$loadedStudy$calibration$zFileName
      }
      progressToNextStep(persistence)
    }
    else if (
      persistence$updatingStep == "importExperimentalData-directoptim" &&
      grepl("nav-menuImport-importDOE-directOptim-defineCalibration-importXPdata", ns("bidon"))
    ) {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      if (!is.null(persistence$loadedStudy$directOptim$calibration)) {
        xpData$Z <- persistence$loadedStudy$directOptim$calibration$Z
        xpData$sigZ <- persistence$loadedStudy$directOptim$calibration$sigZ
        xpData$nZ <- persistence$loadedStudy$directOptim$calibration$nF
        xpData$idZ <- persistence$loadedStudy$directOptim$calibration$idZ
        xpData$idZY <- persistence$loadedStudy$directOptim$calibration$idZY
        xpData$discZ <- persistence$loadedStudy$directOptim$calibration$discZ
        xpData$zFileName <- persistence$loadedStudy$directOptim$calibration$zFileName
      }
      progressToNextStep(persistence)
    }
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  return(xpData)
  
}
