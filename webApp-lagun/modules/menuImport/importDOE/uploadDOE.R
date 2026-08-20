#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module uploadDOE
source("modules/shared/XinfosChange.R", local = TRUE)
source("modules/shared/XactiveChange.R", local = TRUE)
source("modules/shared/YinfosChange.R", local = TRUE)
source("modules/shared/XYnamesChange.R", local = TRUE)
source("modules/shared/missingValChange.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/prepareDOE/evaluateDOE.R", local = TRUE)
source("modules/prepareDOE/visualizeDOE.R", local = TRUE)
source("modules/menuImport/importDOE/compositeFunction.R", local = TRUE)
source("modules/menuImport/calibration/importDiscretization.R", local = TRUE)


my.summary <- function(x,...){
  x <- as.numeric(x[!is.na(x)])
  c(Mean=mean(x, ...),
    Sd=sd(x, ...),
    Median=median(x, ...),
    Min=min(x, ...),
    Max=max(x,...),
    Qvar=abs((quantile(x,probs=0.75,...)-quantile(x,probs=0.25,...))/(quantile(x,probs=0.75,...)+quantile(x,probs=0.25,...))))
}

compute.summary <- function(Y,my.summary,Yinfos){
  # Compute stats on all outputs except status ones
  nYtot <- length(Yinfos$all.ids)
  ns <- length(my.summary(1))
  s <- matrix(NA,ns+1,nYtot)
  idok <- c(Yinfos$int.ids,Yinfos$const.ids,Yinfos$control.ids, Yinfos$func.ids)
  idok.num <- intersect(idok, which(Yinfos$type == 'numeric'))
  idok.cat <- intersect(idok, which(Yinfos$type == 'categorical'))
  s[2:(ns+1),idok.num] <- apply(Y[,idok.num,drop=FALSE], 2, my.summary, na.rm=TRUE)
  s <- signif(s,4)
  if (length(idok.cat) > 0){
    s[1,idok.cat] <- sapply(idok.cat, function(id){
      paste0(unique(as.vector(Y[,id])), collapse = ',')
    })
  }
  return(s)
}

checkValid.XYData <- function(XY, nX) {
  if (is.null(XY)) {
    print("XYdata is NULL")
    return(FALSE)
  }
  if (!is.data.frame(XY)) {
    print("XYdata not a dataframe")
    return(FALSE)
  }
  if (nX > ncol(XY)) {
    print("Wrong number of inputs")
    return(FALSE)
  }
  return(TRUE)
}

get.XYdata <- function(XY, nX, header, firstcol,namesvisu,namesmenu) {
  
  if (firstcol){
    XY <- XY[,-1,drop=FALSE]
    xnamesvisu <- namesvisu[2:(nX+1)]
    xnamesmenu <- namesmenu[2:(nX+1)]
  }else{
    xnamesvisu <- namesvisu[1:nX]
    xnamesmenu <- namesmenu[1:nX]
  }
  nY <- ncol(XY) - nX
  xynames <- colnames(XY)
  ynames <- NULL
  if (header) {
    xnames <- xynames[1:nX]
    if (nY > 0) {ynames <- xynames[(nX + 1):(nX + nY)]}
    if (firstcol){
      xnamesvisu <- namesvisu[2:(nX+1)]
      xnamesmenu <- namesmenu[2:(nX+1)]
      if (nY > 0) {ynamesvisu <- namesvisu[(nX + 2):(nX + 1 + nY)];ynamesmenu <- namesmenu[(nX + 2):(nX + 1 + nY)]}
    }else{
      xnamesvisu <- namesvisu[1:nX]
      xnamesmenu <- namesmenu[1:nX]
      if (nY > 0) {ynamesvisu <- namesvisu[(nX + 1):(nX + nY)];ynamesmenu <- namesmenu[(nX + 1):(nX + nY)]}
    }
  } else {
    xnames <- paste0("X", 1:nX)
    xnamesvisu <- xnamesmenu <- xnames
    if (nY > 0) {ynames <- paste0("Output", 1:nY);ynamesvisu <- ynamesmenu <- ynames}
    colnames(XY) <- c(xnames, ynames)
  }
  if (nY == 0){ynamesvisu <- ynamesmenu <- ynames <- NULL}
  X <- XY[, xnames, drop = F]
  Y <- XY[, ynames, drop = F]
  nobs <- nrow(XY)
  list(XY = XY, X = X, Y = Y, nobs = nobs, nX = nX, nY = nY, xnames = xnames, ynames = ynames, 
       xnamesvisu = xnamesvisu, ynamesvisu = ynamesvisu,xnamesmenu = xnamesmenu, ynamesmenu = ynamesmenu)
  
}

get.Xinfos.col <- function(ind, XDOE, xnames, xnamesvisu, xnamesmenu, nvalues){
  
  name <- xnames[ind]
  namevisu <- xnamesvisu[ind]
  namemenu <- xnamesmenu[ind]
  DOE.col <- XDOE[,ind]
  DOE.col <- DOE.col[!is.na(DOE.col)]
  uniqueCol <- unique(DOE.col)
  is.num <- is.numeric(DOE.col) && (length(uniqueCol) == length(DOE.col) || length(uniqueCol) >= nvalues )
  is.cst <- length(uniqueCol) == 1
  
  if (is.num){
    type <- 'numeric'
    bounds <- c(min(DOE.col), max(DOE.col))
    nlevels <- NA
    levels <- NA
  }else{
    if (is.cst){type = 'constant'}else{type <- 'categorical'}
    bounds <- c(NA,NA)
    levels <- as.vector(uniqueCol)
    nlevels <- length(levels)
  }
  return(list(name = name, namevisu = namevisu, namemenu = namemenu, type = type, bounds = bounds, nlevels = nlevels, levels = levels))
  
}

get.Yinfos.col <- function(ind, YDOE, nvalues, threshold.var, ynamesmenu){
  
  DOE.col <- YDOE[,ind]
  DOE.col <- DOE.col[!is.na(DOE.col)]
  uniqueCol <- unique(DOE.col)
  is.num <- is.numeric(DOE.col) && (length(uniqueCol) == length(DOE.col) || length(uniqueCol) >= nvalues )
  is.func <- grepl('@', ynamesmenu[ind])
  
  if(is.num){
    type <- "numeric"
  }else{
    type <- "categorical"
  }

  if (is.func){
    group <- "Functional"
  }else{
    if (!is.num){
      # Factor output, by default considered as an output of interest
      group <- "Interest"
    }else{
      Q1 <- quantile(DOE.col,probs=0.25)
      Q3 <- quantile(DOE.col,probs=0.75)
      Qvariation <- abs((Q3-Q1)/(Q3+Q1))
      if (is.na(Qvariation)){
        group <- "Constant"
      }else{
        if (Qvariation < threshold.var){
          # Not a lot of variation in the output, considered constant
          group <- "Constant"
        }else{
          group <- "Interest"
        }
      }
    }
  }
  
  return(list(group = group, type = type))
  
}

uploadDOE.ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    uiOutput(outputId = ns("import.dynui"))
  )
}

uploadDOE.server <- function(input, output, session, doeProblemDef, persistence, settings) {
  
  ns <- session$ns
  
  displayui <- reactiveValues(mode="tabset")
  displaytabs <- reactiveValues(inputs=FALSE,outputs=FALSE,finish=FALSE,finishinputs=FALSE,ids=c(1))
  finishedtabs <- reactiveValues(inputs=FALSE,outputs=FALSE,finish=FALSE)
  
  
  # Forced to render the tabset panel manually because show and hidetab 
  # do not work inside the renderUI
  namestabsui <- c(ns("tabimport"),ns("tabinputs"),ns("taboutputs"),ns("tabfinish"),ns("tabfinishinputs"))
  tabsui <- list()
  tabsui[[1]] <- tabPanel(h4("Import Data"),
                          tagList(
                            br(),
                            uiOutput(outputId = ns("tabimport.dynui"))
                          ),
                          value =ns("tabimport")
  )
  tabsui[[2]] <- tabPanel(h4("Define Inputs"),
                          tagList(
                            br(),
                            uiOutput(outputId = ns("tabinputs.dynui")),
                            XinfosChange.ui.preview(ns("bounds"))
                          ),
                          value =ns("tabinputs")
  )
  tabsui[[3]] <- tabPanel(h4("Define Outputs"),
                          tagList(
                            br(),
                            uiOutput(outputId = ns("taboutputs.dynui")),
                            tagList(
                              fluidRow(
                                column(4, h4("Current Outputs")),
                                column(8, "")
                              ),
                              DT::dataTableOutput(ns('preview'))
                            )
                          ),
                          value =ns("taboutputs")
  )
  tabsui[[4]] <- tabPanel(h4("Finish"),
                          tagList(
                            br(),
                            uiOutput(outputId = ns("tabfinish.dynui"))
                          ),
                          value =ns("tabfinish")
  )
  tabsui[[5]] <- tabPanel(h4("Finish"),
                          tagList(
                            br(),
                            uiOutput(outputId = ns("tabfinishinputs.dynui"))
                          ),
                          value =ns("tabfinishinputs")
  )
  
  # Main panel which switches according to displayui$mode
  # between the import welcome page, the import tabsetpanel and the final view
  output$import.dynui <- renderUI({
    req(!is.null(displayui$mode),displaytabs$ids)
    if (displayui$mode=="tabset"){
      t <- tagList(
        do.call(tabsetPanel,c(tabsui[displaytabs$ids],selected=namestabsui[rev(displaytabs$ids)[1]])),
        br(),
        hr(),
        fluidRow(
          column(4,""),
          column(4, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                                 width = '100%')),
          column(4,"")
        ),
        bsModal(
          ns("modalduplicate"), "Information", NULL, size = "large",
          uiOutput(outputId = ns("alertduplicate"))
        ),
        bsModal(
          ns("modalDuplicateColnames"), h3("Error!"), NULL, size = "large",
          uiOutput(outputId = ns("alertDuplicateColnames"))
        ),
        bsModal(
          ns("modalNAcolumns"), h3("Error!"), NULL, size = "large",
          uiOutput(outputId = ns("alertNAcolumns"))
        ),
        bsModal(
          ns("modalBlockNA"), h3("Error!"), NULL, size = "large",
          uiOutput(outputId = ns("alertBlockNA"))
        ),
        bsModal(
          ns("modalFusedDOE"), h3("Warning!"), NULL, size = "large",
          uiOutput(outputId = ns("alertFusedDOE"))
        )
      )
    }
    if (displayui$mode=="finalpreview"){
      if(!anyNA(DOE$X)) 
        compositeButton <- actionButton(ns("addCompositeOutput"), 
                                        label = "Composite Output", 
                                        class = "btn-primary")
      else
        compositeButton <- disabled(actionButton(ns("addCompositeOutput"),
                                                 label = "Composite Output",
                                                 class = "btn-primary"))
      t <- tagList(
        useShinyjs(),
        br(),
        br(),
        fluidRow(
          column(6,wellPanel(
            tagList(
              fluidRow(
                column(2,""),
                column(4,h4("Input Summary")),
                column(4,XactiveChange.ui(ns("inputactivate2"),label="Change Input Activation"),
                       h5("Here you can decide which inputs will be used in surrogate models."), align="center"),
                column(4,"")
              ),
              hr(),
              XinfosChange.ui.preview(ns("bounds2"), simple = TRUE),
              br(),
              actionButton(ns("gocheckDOE"), "Analyze DOE", class = "btn-info",width='80%'),
              bsModal(ns("modalcheckDOE"), "Analyze DOE", NULL, size = "large",
                      tabsetPanel(
                        tabPanel("Visualize", visualizeDOEUI(id = ns("plotVisualize"))),
                        tabPanel("Evaluate", evaluateDOE.ui(id = ns("evaluateDOE")))
                      )
              )
            ),
            style = "background: white"),align="center"),
          column(6,wellPanel(
            tagList(
              fluidRow(
                column(4,h4("Output Summary")),
                column(4,YinfosChange.ui(ns("outtype2"),label="Change Output Groups"),
                       h5("Here you can change the outputs types (interest, control, status, constant, functional)."), align = "center"),
                tags$head(tags$style(HTML(paste0("#", ns("modalComposite"), " .modal-body {min-height: 500px; max-height: 500px; overflow-y: auto;}")))),
                column(4, compositeButton,
                       h5("Here you can create new outputs by composing existing outputs"), align = "center"),
                bsModal(ns("modalComposite"), "Composite Output", NULL, size = "large",
                        compositeFunctionUI(id = ns("compositeOutput")))
              ),
              hr(),
              DT::dataTableOutput(ns('preview2'))
            ),
            style = "background: white"),align="center")
        ),
        hr(),
        uiOutput(outputId = ns("DTsettings.dynui")),
        hr(),
        fluidRow(
          DT::dataTableOutput(ns('DTcontents'))
        )
      )
    }
    if (displayui$mode=="finalpreviewinputs"){
      t <- tagList(
        tagList(
          fluidRow(
            column(2,""),
            column(4,h4("Input Summary")),
            column(4,XactiveChange.ui(ns("inputactivate2"),label="Change Input Activation"),
                   h5("Here you can decide which inputs will be used in surrogate models."), align="center"),
            column(4,"")
          ),
          hr(),
          XinfosChange.ui.preview(ns("bounds2"), simple = TRUE),
          br(),
          fluidRow(
            column(12,actionButton(ns("gocheckDOE"), "Analyze DOE", class = "btn-info",width='80%'),align="center")
          ),
          bsModal(ns("modalcheckDOE"), "Analyze DOE", NULL, size = "large",
            tabsetPanel(
              tabPanel("Visualize", visualizeDOEUI(id = ns("plotVisualize"))),
              tabPanel("Evaluate", evaluateDOE.ui(id = ns("evaluateDOE")))
            )
          )
        )
      )
    }
    return(t)
  })
  
  # Active tabsetpanel display
  observeEvent(input$OpenImport, {
    displayui$mode <- "tabset"
  })
  
  # Events to trigger new tabpanels
  observeEvent(displaytabs$inputs,{
    if (displaytabs$inputs){
      displaytabs$ids <- c(1,2)
    }
  })
  observeEvent(displaytabs$outputs,{
    if (displaytabs$outputs){
      displaytabs$ids <- c(1,2,3)
    }
  })
  observeEvent(displaytabs$finish,{
    if (displaytabs$finish){
      displaytabs$ids <- c(1,2,3,4)
    }
  })
  observeEvent(displaytabs$finishinputs,{
    if (displaytabs$finishinputs){
      displaytabs$ids <- c(1,2,5)
    }
  })
  
  observeEvent(input$importconfirm,{
    typeDetectionValue(input$nvalues)
    displaytabs$inputs <- TRUE
  })
  observeEvent(input$inputsconfirm,{
    if (DOEtemp$nY > 0){
      displaytabs$outputs <- TRUE
    }else{
      displaytabs$finishinputs <- TRUE
    }
  })
  observeEvent(input$outputsconfirm,{
    YcatInd <- which(DOE$Yinfos$type == 'categorical')
    for (j in YcatInd){
      DOE$XY[, DOE$nX + j] <- DOE$Y[, j] <- as.factor(DOE$Y[, j])
    }
    YnumInd <- which(DOE$Yinfos$type == 'numeric')
    for (j in YnumInd){
      DOE$XY[, DOE$nX + j] <- DOE$Y[, j] <- as.numeric(DOE$Y[, j])
    }
    
    filteredColnames <- initialXYnames$xynames[!initialXYnames$xynames %in% DOE$Fnames]
    dup_cols <- duplicated(filteredColnames)
    NAcolumns <- sapply(DOE$XY, allNA)
    if (anyNA(DOE$X)){
      XIndexNA <- is.na(DOE$X)
      YIndexNA <- is.na(DOE$Y)
      XIndexNA <- unique(XIndexNA)
      YIndexNA <- unique(YIndexNA)
      blockAllNA <- any(apply(XIndexNA, 1, all)) | any(apply(YIndexNA, 1, all))
    }else{
      blockAllNA <- FALSE
    }
    
    if (any(dup_cols)){
      output$alertDuplicateColnames <- renderUI({
        tagList(
          h4(paste("The following column names have duplicates:", 
                   paste0(filteredColnames[dup_cols], 
                          collapse = ", "))),
          h4("This causes issues with the visualizations, and leads to unexpected behaviours."),
          h4("You may now restart the app and load a file with different column names."),
          br(),
          h4("Note that duplicates are only allowed for functional outputs."),
        )
      })
      
      toggleModal(session, "modalDuplicateColnames", toggle = "open")
    }else if (length(DOE$nF) > 0){
      # check functional outputs
      anyCatFunc <- length(intersect(YcatInd, DOE$Yinfos$func.ids)) > 0
      if (anyCatFunc){
        showModal(modalDialog(HTML(
          "Functional categorical outputs are not handled.")
          , title = "Error !")
        )
      }
      discFValid <- all(sapply(1:length(DOE$nF), function(k){
        t <- DOE$discF[[k]]
        !anyNA(apply(t, 1, as.numeric)) && nrow(t) == DOE$nF[k]
      }))
      if (!discFValid){
        showModal(modalDialog(HTML(
          "Discretization of functional outputs is missing or not valid. <br>
          Please use @ in all output headers or load discretization files. <br>
          For each output, the discretization must take numeric values and match the length of loaded data.")
          , title = "Error!")
        )
      }
      if (!anyCatFunc && discFValid){
        displaytabs$finish <- TRUE
      }
    }else if (any(NAcolumns)){
      output$alertNAcolumns <- renderUI({
        tagList(
          h4(paste("The following columns contain only missing values: ", 
                   paste0(c(DOE$xnamesmenu, DOE$ynamesmenu)[NAcolumns], collapse = ", "))),
          h4("You may now restart the app and load a file without empty columns.")
        )
      })
      toggleModal(session, "modalNAcolumns", toggle = "open")
    }else if (blockAllNA){
      output$alertBlockNA <- renderUI({
        tagList(
          h4("Fused DOE detected. Some blocks contains only missing values for all inputs or all outputs."),
          h4("You may now restart the app and load a file with an appropriate missing value structure.")
        )
      })
      toggleModal(session, "modalBlockNA", toggle = "open")
    }else {
      displaytabs$finish <- TRUE
    }
  })
  
  ##################################################################
  # Import Tab
  
  output$tabimport.dynui <- renderUI({
    tagList(
      uiOutput(ns("upload.dynui")),
      uiOutput(ns("sepdec.dynui")),
      uiOutput(ns("preview.dynui"))
    )
  })
  
  # Upload now longer possible once import is confirmed
  output$upload.dynui <- renderUI({
    req(!displaytabs$inputs)
    if (finishedtabs$inputs){
      t <- tagList(fluidRow(
        column(8,fileInput(ns('file'), 'Select File', accept = c('.txt','.dat','.csv'))),
        column(3,""),
        column(1,actionButton(ns("importconfirm"),label=HTML(paste("Confirm","Import",sep="<br>")), icon=icon("step-forward"), class = "btn-primary"), align="right")
      ))
    }else{
      t <- tagList(fluidRow(
        column(8,fileInput(ns('file'), 'Select File', accept = c('.txt','.dat','.csv'))),
        column(4,"")
      ))
    }
    return(t)
  })
  
  file.to.load <- reactiveValues(datapath = NULL)
  
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
    # Try all possible separators
    count.comma <- stri_count_fixed(line2, ",")[2]
    count.semicolon <- stri_count_fixed(line2, ";")[2]
    count.tab <- stri_count_fixed(line2, "\t")[2]
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
    firstguessfile$finished <- TRUE
  })
  
  # Initialize separator and decimal UI with first guess
  # Separator and decimal UI now longer accessible once import is confirmed
  
  typeDetectionValue <- reactiveVal(5)
  
  output$sepdec.dynui <- renderUI({
    req(!displaytabs$inputs,firstguessfile$finished)
    tagList(
      hr(),
      h5("Header, Separator and Decimal have been auto-detected. Please change values if not correct."),
      fluidRow(
        column(4, radioButtons(ns("separator"), "Separator",
                               choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"), selected=separator$char)),
        column(4, radioButtons(ns("decimal"), "Decimal",
                               choices = list(". (point)" = ".", ", (comma)" = ","), selected=decimal$char)),
        column(4,
               numericInput(ns('nvalues'), label = "Type detection", value =  5),
               h6('Min. nb. of distinct values for numeric variables.
                  Auto-defined as categorical if lower.'), align="right")
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
                     dec = input$decimal)
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
  
  # Detect when loading is finished (confirmation button can appear)
  observe({
    req(file.data())
    finishedtabs$inputs <- TRUE
  })
  
  # Alert the user if there are duplicate rows
  output$alertduplicate <- renderUI({
    tagList(
      h3("Identical simulations have been detected in the DOE."),
      h3("This will cause failures in some algorithms."),
      h3("We strongly suggest you clean your file and reload it."),
      DT::dataTableOutput(ns('DTcontentsdupli'))
    )
  })
  
  output$DTcontentsdupli <- DT::renderDataTable({
    dinit <- file.data()
    d <- dinit[duplicated(dinit),]
    dimd <- ncol(d)
    DT::datatable(
      d,
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip',
        buttons = list(list(extend = 'colvis', columns = 1:dimd)),
        scrollX = TRUE,scrollY = 400,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  # Show a file preview to check the loading
  output$preview.dynui <- renderUI({
    req(finishedtabs$inputs)
    tagList(
      fluidRow(
        column(6,
               h4(paste0("File Data Preview: Observations = ",nrow(file.data()),", Variables = ",ncol(file.data())))
        ),
        column(6,XYnamesChange.ui(ns("change.names")))
      ),
      DT::dataTableOutput(ns('headerdf')),
      br(),
      hr()
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
  
  output$headerdf <- DT::renderDataTable({
    req(file.data(),!is.null(header$bool),finishedtabs$inputs)
    print("Table generated")
    d <- file.data()[1:min(5,nrow(file.data())),]
    dimd <- ncol(d)
    if (header$bool & !is.null(newXYnames$namesvisu)){
      colnames(d) <- newXYnames$namesvisu
    }
    DT::datatable(
      d, escape = FALSE,
      extensions = c('FixedColumns','Scroller'),filter = 'top',
      options = list(
        dom = 'Brtip',
        scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  ##################################################################
  # Inputs Tab
  # A temporary DOE object is filled once the tab is opened
  
  DOEtemp <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL, Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL, 
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL, adapt.visu = FALSE,
    idref = NULL, idon = NULL,
    discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesvisu = NULL
  )
  DOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL, Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL, 
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL, adapt.visu = FALSE,
    idref = NULL, idon = NULL,
    discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesvisu = NULL
  )
  
  XYdata <- reactive({
    req(displaytabs$inputs, input$nX > 0, !is.null(input$nobs.firstcol))
    if (checkValid.XYData(file.data(), input$nX)){
      get.XYdata(file.data(), input$nX, header$bool, input$nobs.firstcol,newXYnames$namesvisu,newXYnames$namesmenu)
    }
  })
  
  observeEvent(XYdata(), {
    req(XYdata())
    print("updating DOE : new Xopt")
    print(paste(
      XYdata()$nobs,
      paste(XYdata()$xnames, collapse = ","),
      paste(XYdata()$ynames, collapse = ",")
    ))
    DOEtemp$Xopt <- XYdata()$X
    DOEtemp$XY <- XYdata()$XY
    DOEtemp$X <- XYdata()$X
    DOEtemp$Y <- XYdata()$Y
    DOEtemp$nobs <- XYdata()$nobs
    DOEtemp$nX <- XYdata()$nX
    DOEtemp$nY <- XYdata()$nY
    DOEtemp$xnames <- XYdata()$xnames
    DOEtemp$ynames <- XYdata()$ynames
    DOEtemp$xnamesvisu <- XYdata()$xnamesvisu
    DOEtemp$ynamesvisu <- XYdata()$ynamesvisu
    DOEtemp$xnamesmenu <- XYdata()$xnamesmenu
    DOEtemp$ynamesmenu <- XYdata()$ynamesmenu
    DOEtemp$adapt.visu <- newXYnames$adapt.visu
  })
  
  initialXinfos = reactiveValues(nX = NULL, Xinfos = NULL)
  # initialize Xinfos with input data
  observe( {
    req(input$nX)
    hideFeedback("nX")
    if (input$nX < 1) {
      showFeedbackWarning(
        inputId = "nX",
        text = "Should be greater than 0"
      )
    }
    if (is.null(XYdata()$XY) || !is.data.frame(XYdata()$XY) || XYdata()$nX > ncol(XYdata()$XY)) {
      showFeedbackWarning(
        inputId = "nX",
        text = "Inconsistent with number of imported data"
      )
    }
    req(checkValid.XYData(XYdata()$XY, XYdata()$nX), input$decimal, typeDetectionValue())
    initialXinfos$nX <- XYdata()$nX
    if (isTruthy(initialXinfos$nX > 0)){
      initialXinfos$Xinfos <- lapply(1:initialXinfos$nX, get.Xinfos.col,
                                     XDOE = XYdata()$X, xnames = XYdata()$xnames, xnamesvisu = XYdata()$xnamesvisu, xnamesmenu = XYdata()$xnamesmenu, nvalues = typeDetectionValue())
      types <- lapply(initialXinfos$Xinfos, function(xinfo) xinfo$type)
      categoricalCount <-  length(which(types == "categorical"))
      if (categoricalCount > 0) {
        showFeedback(
          inputId = "nX",
          icon = shiny::icon("ok", lib = "glyphicon"),
          text = paste(categoricalCount, "categorical input(s) detected")
        )
      }
    }else{
      initialXinfos$Xinfos <- NULL
    }
  })
  
  Xinfos <- callModule(XinfosChange.server, "bounds", initialXinfos, data = DOEtemp, 
                       nvalues = typeDetectionValue())
  
  # Reordering if asked by Xinfos
  observeEvent(Xinfos$Xinfos, {
    req(Xinfos$Xinfos)
    # # Update DOEtemp info if Xinfos changes
    xnames <- sapply(Xinfos$Xinfos, function(Xinfo){Xinfo$name})
    xnamesvisu <- sapply(Xinfos$Xinfos, function(Xinfo){Xinfo$namevisu})
    xnamesmenu <- sapply(Xinfos$Xinfos, function(Xinfo){Xinfo$namemenu})
    reorder.bool <- FALSE
    if (!is.null(DOEtemp$xnames)){
      reorder.bool <- !all(xnames == DOEtemp$xnames) & length(intersect(xnames, colnames(DOEtemp$XY))) == DOEtemp$nX
    }
    # reorder inputs / outputs in final dataframe
    if (reorder.bool){
      # Reordering cannot happen if we only load inputs with no header, in this case
      # we just get the names from Xinfos and apply them so we don't do anything here
      if (DOEtemp$nY!=0){
        xynames <- c(DOEtemp$xnames, DOEtemp$ynames)
        DOEtemp$ynames <- xynames[!xynames %in% xnames]
        DOEtemp$X <- DOEtemp$XY[xnames]
        DOEtemp$Y <- DOEtemp$XY[DOEtemp$ynames]
        DOEtemp$XY <- cbind(DOEtemp$X, DOEtemp$Y)
        xynamesvisu <- c(DOEtemp$xnamesvisu, DOEtemp$ynamesvisu)
        DOEtemp$ynamesvisu <- xynamesvisu[!xynames %in% xnames]
        xynamesmenu <- c(DOEtemp$xnamesmenu, DOEtemp$ynamesmenu)
        DOEtemp$ynamesmenu <- xynamesmenu[!xynames %in% xnames]
      }
    }
    print("updating DOE : new bounds")
    DOEtemp$Xinfos <- Xinfos$Xinfos
    DOEtemp$xnames <- xnames
    colnames(DOEtemp$XY)[1:DOEtemp$nX] <- DOEtemp$xnames
    DOEtemp$xnamesvisu <- xnamesvisu
    DOEtemp$xnamesmenu <- xnamesmenu
  })
  
  output$tabinputs.dynui <- renderUI({
    req(!displaytabs$outputs)
    tagList(
      fluidRow(
        column(2,numericInput(ns("nX"), "Number of Inputs", 1, min = 1)),
        column(2,switchInput(ns("nobs.firstcol"), value = F, label = "1stCol = ObsNb",size = "mini")),
        column(2,
               XinfosChange.ui(ns("bounds"),label="Change Input Settings"),
               h5("Here you can change the inputs types, as well as their bounds (if numerical) and their levels (if categorical)."), align="center"),
        column(4, ""),
        column(1,actionButton(ns("inputsdismiss"),label=HTML(paste("Dismiss","Inputs",sep="<br>")), icon=icon("step-backward"), class = "btn-warning"), align="right"),
        column(1,actionButton(ns("inputsconfirm"),label=HTML(paste("Confirm","Inputs",sep="<br>")), icon=icon("step-forward"), class = "btn-primary"), align="right")
      ),
      hr()
    )
  })
  
  output$alertFusedDOE <- renderUI({
    inputNA <- sapply(DOE$X, anyNA)
    HTML(paste(
      "Missing values are detected in input data for: ", paste0(DOE$xnamesmenu[inputNA], collapse = ', '),
      "Missing values are NOT handled for inputs, excepted for fused DOE (see documentation).", sep = '<br/>')
    )
  })

  observeEvent(input$inputsconfirm, {
    DOE$Xinfos <- DOEtemp$Xinfos
    DOE$Xopt <- DOEtemp$X
    DOE$XY <- DOEtemp$XY
    DOE$X <- DOEtemp$X
    DOE$Y <- DOEtemp$Y
    DOE$nobs <- DOEtemp$nobs
    DOE$nX <- DOEtemp$nX
    DOE$nY <- DOEtemp$nY
    DOE$xnames <- DOEtemp$xnames
    DOE$ynames <- DOEtemp$ynames
    DOE$xnamesvisu <- DOEtemp$xnamesvisu
    DOE$ynamesvisu <- DOEtemp$ynamesvisu
    DOE$xnamesmenu <- DOEtemp$xnamesmenu
    DOE$ynamesmenu <- DOEtemp$ynamesmenu
    DOE$adapt.visu <- DOEtemp$adapt.visu
    
    # Warning if NA are detected in input data
    inputNA <- sapply(DOE$X, anyNA)
    if (any(inputNA)){
      toggleModal(session, "modalFusedDOE", toggle = "open")
    }
  })

  
  observeEvent(input$inputsdismiss,{
    # Reinitialize tabsetpanel
    displaytabs$inputs <- FALSE
    # Reinitialize UI
    displaytabs$ids <- c(1)
    # Reset Xinfos and Yinfos
    initialXinfos$nX <- NULL
    initialXinfos$Xinfos <- NULL
    Xinfos$nX <- NULL
    Xinfos$Xinfos <- NULL
    # Reinitialize temporary DOE
    DOEtemp$Xopt <- NULL
    DOEtemp$XY <- NULL
    DOEtemp$X <- NULL
    DOEtemp$Y <- NULL
    DOEtemp$nobs <- NULL
    DOEtemp$nX <- NULL
    DOEtemp$nY <- NULL
    DOEtemp$xnames <- NULL
    DOEtemp$ynames <- NULL
    DOEtemp$xnamesvisu <- NULL
    DOEtemp$ynamesvisu <- NULL
    DOEtemp$xnamesmenu <- NULL
    DOEtemp$ynamesmenu <- NULL
    DOEtemp$Xinfos <- NULL
    DOEtemp$idon <- NULL
    DOEtemp$Yinfos <- NULL
    DOEtemp$nYsurrogate <- NULL
    # Reinitialize DOE
    DOE$Xopt <- NULL
    DOE$XY <- NULL
    DOE$X <- NULL
    DOE$Y <- NULL
    DOE$nobs <- NULL
    DOE$nX <- NULL
    DOE$nY <- NULL
    DOE$xnames <- NULL
    DOE$ynames <- NULL
    DOE$xnamesvisu <- NULL
    DOE$ynamesvisu <- NULL
    DOE$xnamesmenu <- NULL
    DOE$ynamesmenu <- NULL
    DOE$Xinfos <- NULL
    DOE$idon <- NULL
    DOE$Yinfos <- NULL
    DOE$nYsurrogate <- NULL
    DOE$discF <- NULL
    DOE$nF <- NULL
    DOE$idF <- NULL
    DOE$Fnames <- NULL
    DOE$Fnamesvisu <- NULL
  })
  
  ##################################################################
  # Outputs Tab
  
  initialYinfos <- reactiveValues(ynames = NULL, ynamesvisu = NULL, ynamesmenu = NULL, nY = NULL, Yinfos = NULL)
  
  # initialize Yinfos with output data once inputs are confirmed
  observeEvent(input$inputsconfirm, {
    isolate({
    # Pre-identify the class of each output
    nY <- DOEtemp$nY
    if (isTruthy(nY > 0)){
      initialYinfos$ynames <- DOEtemp$ynames
      initialYinfos$ynamesvisu <- DOEtemp$ynamesvisu
      initialYinfos$ynamesmenu <- DOEtemp$ynamesmenu
      newYinfos <-  lapply(1:nY, get.Yinfos.col, YDOE = DOEtemp$Y, nvalues = typeDetectionValue(), threshold.var = 0.01,
                           ynamesmenu = DOEtemp$ynamesmenu)
      res.ids <- sapply(newYinfos, function(x){x$group})
      initialYinfos$Yinfos$all.ids <- res.ids
      initialYinfos$Yinfos$int.ids <- which(res.ids=="Interest")
      initialYinfos$Yinfos$control.ids <- NULL
      initialYinfos$Yinfos$status.ids <- which(res.ids=="Status")
      initialYinfos$Yinfos$const.ids <- which(res.ids=="Constant")
      initialYinfos$Yinfos$func.ids <- which(res.ids=="Functional")
      initialYinfos$Yinfos$surrogate.ids <- c(initialYinfos$Yinfos$int.ids, initialYinfos$Yinfos$control.ids)
      initialYinfos$Yinfos$type <- sapply(newYinfos, function(x){x$type})
      initialYinfos$nY <- length(initialYinfos$Yinfos$surrogate.ids)
    }
    })
  })
  
  Yinfos <- callModule(YinfosChange.server, "outtype", initialYinfos, FALSE)
  
  # Update DOE info if Yinfos changes
  observeEvent(Yinfos$Yinfos, {
    req(Yinfos$Yinfos)
    print("updating output group")
    DOE$Yinfos <- Yinfos$Yinfos
    DOE$nYsurrogate <- Yinfos$nY
  })
  
  # Missing values
  initialNA <- reactiveValues(ynames = NULL, val= NULL, nY = NULL)
  storedNA <- reactiveValues(val = NULL, index = NULL)
  
  # Build NA defaults with Yinfos
  observeEvent(Yinfos$ynames, {
    initialNA$ynames <- Yinfos$ynames
    initialNA$nY <- length(Yinfos$ynames)
    initialNA$val <- rep("NA", initialNA$nY)
    storedNA$val <- initialNA$val
    storedNA$index <- lapply(DOE$Y, is.na)
  })
  
  missingVal <- callModule(missingValChange.server, "missval", initialNA)
  
  # Update DOE outputs with missing val
  observeEvent(missingVal$val, {
    req(DOE$Y, DOEtemp$Y, missingVal$val)
    lapply(1:DOE$nY, function(j){
      if (missingVal$val[[j]] != storedNA$val[[j]]){
        if (storedNA$val[[j]] == "NA"){
          DOE$Y[storedNA$index[[j]], j] <- NA        
        }else{
          DOE$Y[storedNA$index[[j]], j] <- storedNA$val[j]
        }
        if (missingVal$val[[j]] == "NA"){
          storedNA$index[[j]] <- is.na(DOE$Y[, j])
        }else{
          NAindex <- DOE$Y[, j] == missingVal$val[j]
          NAindex[is.na(NAindex)] <- FALSE
          storedNA$index[[j]] <- NAindex
        }
        DOE$Y[storedNA$index[[j]], j] <- NA
        storedNA$val[j] <- missingVal$val[j]
      }
    })
    consitentWithType <- sapply(which(DOE$Yinfos$type == "numeric"),  function(j){
      Z <- DOE$Y[!storedNA$index[[j]], j]
      anyNA(as.numeric(Z[!is.na(Z)]))
    })
    if (any(consitentWithType)){
      showModal(modalDialog(HTML(paste(
        "The defined missing values are NOT compatible with the numeric type of outputs: ", 
        paste0(DOE$ynamesmenu[consitentWithType], collapse = ', '),
        "NA have overwritten non-numeric values for these outputs.", sep = '<br/>')), title = "Warning",
        size = 'l')
      )
    }
  })
  
  importDiscF <- callModule(importDiscretization.server, "disc", DOE)
  
  # update fonctional data structures
  observeEvent(Yinfos$Yinfos$func.ids, {

    if (length(Yinfos$Yinfos$func.ids) > 0){
      # compute functional info
      Fheader <- strsplit(DOE$ynamesmenu, '@')
      Fnamesraw <- unlist(sapply(Fheader, function(x) { if (length(x) > 1) x[1] else "" }))
      Fnames <- Fnamesmenu <- Fnamesvisu <- unique(unlist(sapply(Fheader, function(x) { if (length(x) > 1) x[1] else NULL })))
      idF <- lapply(Fnames, function(name){
        which(Fnamesraw == name)
      })
      nF <- sapply(idF, length)
      discF <- lapply(1:length(Fnames), function(j){
        df <- sapply(Fheader[idF[[j]]], function(x){as.numeric(x[2])})
        df <- as.data.frame(df)
        colnames(df) <- paste0('t', j)
        return(df)
      })
    }else{
      discF <- NULL
      nF <- NULL
      idF <- NULL
      Fnames <- NULL
    }
    
    # store functional info in DOE
    DOE$discF <- discF
    DOE$nF <- nF
    DOE$idF <- idF
    DOE$Fnames <- Fnames
    
  })
  
  observeEvent(importDiscF$discF, {
    DOE$discF <- importDiscF$discF
  })
  
  initialFnames <- reactiveValues(Fnames = NULL)
  observeEvent(DOE$Fnames, {
    initialFnames$xynames <- DOE$Fnames
  })
  newFnames <- callModule(XYnamesChange.server, "changeFnames", initialFnames)
  observeEvent(list(newFnames$namesmenu, newFnames$namesvisu), {
    DOE$Fnamesvisu <- newFnames$namesvisu
  })

  
  # View Yinfos
  output$preview <- DT::renderDataTable({
    req(Yinfos$Yinfos,DOE$Y)
    s <- compute.summary(DOE$Y,my.summary,Yinfos$Yinfos)
    s <- rbind(matrix(c(Yinfos$Yinfos$all.ids,Yinfos$Yinfos$type), nrow=2, byrow = T),s)
    df <- as.data.frame(s)
    colnames(df) <- Yinfos$ynamesvisu
    rownames(df) <- c("Group", "Type", "Categories", "Mean","Sd","Median","Min","Max","Quantile Var.")
    ncolumns <- ncol(df)
    # Abbreviate character strings (useful if many levels)
    xx <- data.frame(lapply(df,as.character),stringsAsFactors = FALSE)
    xx[is.na(xx)] <- "NA"
    rownames(xx) <- rownames(df)
    colnames(xx) <- colnames(df)    
    DT::datatable(
      xx, escape = FALSE, 
      extensions = c('FixedColumns','Scroller'),
      options = list(
        dom = 't', ordering=F,
        pageLength = 2, scrollX = TRUE,scroller = TRUE, fixedColumns = TRUE,
        columnDefs = list(list(
          targets = 1:ncolumns,
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 12 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
            "}")))
      ))
  })
  
  output$taboutputs.dynui <- renderUI({
    if (!displaytabs$finish){
      t <- tagList(
        fluidRow(
          column(2, missingValChange.ui(ns("missval"),HTML(paste("Define","Missing Values",sep="<br>"))), align = "center"),
          column(2, actionButton(ns("changeTypeOutput"), label = HTML(paste("Change","Output Types",sep="<br>")), class = "btn-primary"),
                 h5("Here you can change the outputs types (numeric, categorical)."), align="center"),
          bsModal(
            ns("modalChangeTypeOutput"), "Change Output Types", NULL,
            tagList(
              br(),
              br(),
              fluidRow(
                column(6,
                       uiOutput(ns("chooseOutNumericUI"))),
                column(6,
                       uiOutput(ns("chooseOutCategoricalUI")))
              ),
              br(),
              hr(),
              uiOutput(ns("errorSaveModalChangeTypeOutput")),
              br(),
              fluidRow(
                column(3, actionButton(ns("saveModalChangeTypeOutput"), label = "Save and Close", class = "btn-warning",
                                       width = '100%'), offset = 2),
                column(3, actionButton(ns("closeModalChangeTypeOutput"), label = "Dismiss", class = "btn-secondary",
                                       width = '100%'), offset = 2)
              )
            ),
            size = "large",
            tags$head(
              tags$style(
                paste0("#", 
                       ns("modalChangeTypeOutput"),
                       " .modal-footer{display:none}")))
          ),
          column(2,YinfosChange.ui(ns("outtype"),label=HTML(paste("Change","Output Groups",sep="<br>"))),
                 h5("Here you can change the outputs groups (interest, control, status, constant)."), align = "center"),
          importDiscretization.ui(ns("disc")),
          column(2,""),
          column(1,actionButton(ns("outputsdismiss"),label=HTML(paste("Dismiss","Outputs",sep="<br>")), icon=icon("step-backward"), class = "btn-warning"), align="right"),
          column(1,actionButton(ns("outputsconfirm"),label=HTML(paste("Confirm","Outputs",sep="<br>")), icon=icon("step-forward"), class = "btn-primary"), align = "right")
        ),
        hr(),
        fluidRow(
          column(2,h4(htmlOutput(ns("info.textNbOutputs")))),
          column(2,h4(htmlOutput(ns("info.textint")))),
          column(2,h4(textOutput(ns("info.textcontrol")))),
          column(2,h4(textOutput(ns("info.textstatus")))),
          column(2,h4(textOutput(ns("info.textconst")))),
          column(2,h4(textOutput(ns("info.textfunc"))))
        ),
        hr()
      )
    }else{
      t <- NULL
    }
    return(t)
  })
  
  choicesYchangeTypeOutput <- reactive({
    req(initialYinfos$ynamesmenu)
    return(initialYinfos$ynamesmenu)
  })
  
  output$chooseOutNumericUI <- renderUI({
    req(choicesYchangeTypeOutput(),isolate(Yinfos$Yinfos))
    isolate({
      numSelection <- initialYinfos$ynamesmenu[which(Yinfos$Yinfos$type=="numeric")]
      pickerInput(ns("chooseOutNumeric"), label = "Choose Numeric Outputs", 
                  choices = choicesYchangeTypeOutput(), selected = numSelection, multiple = TRUE,
                  options = list(`selected-text-format` = "count > 3",
                                 style = "btn-primary",`live-search` = TRUE))
    })
  })
  
  output$chooseOutCategoricalUI <- renderUI({
    req(choicesYchangeTypeOutput(),isolate(Yinfos$Yinfos))
    isolate({
      catSelection <- initialYinfos$ynamesmenu[which(Yinfos$Yinfos$type=="categorical")]
      pickerInput(ns("chooseOutCategorical"), label = "Choose Categorical Outputs", 
                  choices = choicesYchangeTypeOutput(), selected = catSelection, multiple = TRUE,
                  options = list(`selected-text-format` = "count > 3",
                                 style = "btn-primary",`live-search`= TRUE))
    })
  })
  
  observe({
    ynamesmenu <- isolate(initialYinfos$ynamesmenu)
    # Detect change in numeric
    if (!is.null(input$chooseOutNumeric)) {
      new <- ynamesmenu[!ynamesmenu %in% input$chooseOutNumeric]
    }
    else {
      new <- ynamesmenu
    }
    # Update categorical
    isolate({
      if (toString(new) != toString(input$chooseOutCategorical)) {
        updatePickerInput(session, inputId = "chooseOutCategorical", selected = new)
      }
    })
  })
  
  observe({
    ynamesmenu <- isolate(initialYinfos$ynamesmenu)
    # Detect change in categorical
    if (!is.null(input$chooseOutCategorical)) {
      new <- ynamesmenu[!ynamesmenu %in% input$chooseOutCategorical]
    }
    else {
      new <- ynamesmenu
    }
    # Update numeric
    isolate({
      if (toString(new) != toString(input$chooseOutNumeric)) {
        updatePickerInput(session, inputId = "chooseOutNumeric", selected = new)
      }
    })
  })
  
  observeEvent(input$changeTypeOutput, {
    toggleModal(session, "modalChangeTypeOutput", toggle = "open")
  })
  
  observeEvent(input$saveModalChangeTypeOutput, {
    
    # Verifiy consistency

    notConsistent = list(num=NULL, cat=NULL, nb=FALSE)
    err <- NULL

    for(cat in input$chooseOutCategorical){
      catDf <- DOE$ynames[which(DOE$ynamesmenu == cat)]
      if (length(unique(DOE$Y[[catDf]]))==length(DOE$Y[[catDf]])){
        notConsistent$cat <- c(notConsistent$cat, cat)
      }
    }
    
    for(num in input$chooseOutNumeric){
      numDf <- DOE$ynames[which(DOE$ynamesmenu == num)]
      if(any(is.na(as.numeric(DOE$Y[!is.na(DOE$Y[, numDf]), numDf])))){
        notConsistent$num <- c(notConsistent$num, num)
      }
    }
    
    if (length(initialYinfos$Yinfos$type) != 
        (length(input$chooseOutCategorical) + length(input$chooseOutNumeric)))
      notConsistent$nb <- TRUE
    
    if(length(notConsistent$num)==0 & 
       length(notConsistent$cat)==0 &
       !notConsistent$nb){
      newTypes <- rep("numeric", length(initialYinfos$Yinfos$type))
      newTypes[initialYinfos$ynamesmenu %in% input$chooseOutCategorical] <- "categorical"
      DOE$Yinfos$type <- newTypes
      Yinfos$Yinfos$type <- newTypes
      initialYinfos$ynames <- Yinfos$ynames
      initialYinfos$ynamesvisu <- Yinfos$ynamesvisu
      initialYinfos$ynamesmenu <- Yinfos$ynamesmenu
      initialYinfos$nY <- Yinfos$nY
      initialYinfos$Yinfos <- Yinfos$Yinfos
      toggleModal(session, "modalChangeTypeOutput", toggle = "close")
    }else{
      
      err <- "Error:"
      if (length(notConsistent$num)>0){
        err <- paste(err, 
                     paste("The following output don't seem numeric:", 
                           paste0(notConsistent$num, collapse = ", ")), 
                     sep = "<br>")
      }
      
      if (length(notConsistent$cat)>0){
        err <- paste(err, 
                     paste("The following output don't seem categorical:", 
                           paste0(notConsistent$cat, collapse = ", ")), 
                     sep = "<br>")
      }
      
      if (notConsistent$nb){
        err <- paste(err, "Make sure to assign a type to each output", sep = "<br>")
      }
    }
    
    output$errorSaveModalChangeTypeOutput <- renderUI({
      return(h4(HTML(err)))
    })
    
  })
  
  observeEvent(input$closeModalChangeTypeOutput, {
    
    numSelection <- initialYinfos$ynamesmenu[which(Yinfos$Yinfos$type=="numeric")]
    catSelection <- initialYinfos$ynamesmenu[which(Yinfos$Yinfos$type=="categorical")]
    
    updatePickerInput(session, 
                      inputId = "chooseOutNumeric", 
                      selected = numSelection)
    updatePickerInput(session, 
                      inputId = "chooseOutCategorical", 
                      selected = catSelection)
    
    toggleModal(session, "modalChangeTypeOutput", toggle = "close")
  })
  
  output$info.textint <- renderText({
    req(DOE$Yinfos)
    nint <- length(DOE$Yinfos$int.ids)
    nfunc <- length(DOE$Yinfos$func.ids)
    if (nint==0 & nfunc==0){
      font <- 'red'
    }else{
      font <- 'black'
    }
    formatedFont <- sprintf('<font color="%s">%s</font>',font,paste0("Interest: ",nint))
  })
  output$info.textcontrol <- renderText({
    req(DOE$Yinfos)
    ncontrol <- length(DOE$Yinfos$control.ids)
    paste0("Control: ",ncontrol)
  })
  output$info.textstatus <- renderText({
    req(DOE$Yinfos)
    nstatus <- length(DOE$Yinfos$status.ids)
    paste0("Status: ",nstatus)
  })
  output$info.textconst <- renderText({
    req(DOE$Yinfos)
    nconst <- length(DOE$Yinfos$const.ids)
    paste0("Constant: ",nconst)
  })
  output$info.textfunc <- renderText({
    req(DOE$Yinfos)
    nfunc <- length(DOE$nF)
    paste0("Functional: ", nfunc)
  })
  
  output$info.textNbOutputs <- renderText({
    req(DOE$Yinfos)
    nint <- length(DOE$Yinfos$int.ids)
    ncontrol <- length(DOE$Yinfos$control.ids)
    nconst <- length(DOE$Yinfos$const.ids)
    nstatus <- length(DOE$Yinfos$status.ids)
    nfunc <- length(DOE$nF)
    ntotal <- sum(nint, ncontrol, nconst, nstatus, nfunc)
    paste0("Number of Outputs: ", ntotal)
  })
  
  
  observeEvent(input$outputsdismiss,{
    # Reinitialize tabsetpanel
    displaytabs$outputs <- FALSE
    # Reinitialize UI
    displaytabs$ids <- c(1,2)
    # Reset Yinfos
    initialYinfos$ynames <- NULL
    initialYinfos$ynamesvisu <- NULL
    initialYinfos$ynamesmenu <- NULL
    initialYinfos$nY <- NULL
    initialYinfos$Yinfos <- NULL
    Yinfos$ynames <- NULL
    Yinfos$ynamesvisu <- NULL
    Yinfos$ynamesmenu <- NULL
    Yinfos$nY <- NULL
    Yinfos$Yinfos <- NULL
  })
  
  ##################################################################
  # Finish Tab
  
  DOEfinal <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL, Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL, 
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL, adapt.visu = FALSE,
    idref = NULL, idon = NULL, compositeInfos = NULL,
    discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesvisu = NULL
  )
  
  observeEvent(input$finishconfirm,{
    displayui$mode <- "finalpreview"
    # Fill final DOE object
    print("Confirmed final DOE")
    DOEfinal$Xopt <- DOE$Xopt
    DOEfinal$XY <- DOE$XY
    DOEfinal$X <- DOE$X
    DOEfinal$Y <- DOE$Y
    DOEfinal$nobs <- DOE$nobs
    DOEfinal$nX <- DOE$nX
    DOEfinal$nY <- DOE$nY
    DOEfinal$xnames <- DOE$xnames
    DOEfinal$ynames <- DOE$ynames
    DOEfinal$xnamesvisu <- DOE$xnamesvisu
    DOEfinal$ynamesvisu <- DOE$ynamesvisu
    DOEfinal$xnamesmenu <- DOE$xnamesmenu
    DOEfinal$ynamesmenu <- DOE$ynamesmenu
    DOEfinal$adapt.visu <- DOE$adapt.visu
    DOEfinal$Xinfos <- DOE$Xinfos
    DOEfinal$idon <- DOE$idon
    DOEfinal$Yinfos <- DOE$Yinfos
    DOEfinal$nYsurrogate <- DOE$nYsurrogate
    DOEfinal$discF <- DOE$discF
    DOEfinal$nF <- DOE$nF
    DOEfinal$idF <- DOE$idF
    DOEfinal$Fnames <- DOE$Fnames
    DOEfinal$Fnamesvisu <- DOE$Fnamesvisu

    persistence$autoSavingCount <- persistence$autoSavingCount + 1
    persistence$autoSavingCaller <- "uploadDOE-finishconfirm"
  })
  
  observeEvent(input$finishdismiss,{
    # Reinitialize tabsetpanel
    displaytabs$finish <- FALSE
    # Reinitialize UI
    displaytabs$ids <- c(1,2,3)
  })
  
  output$tabfinish.dynui <- renderUI({
    Yinfos <- DOE$Yinfos
    nint <- length(Yinfos$int.ids)
    ncontrol <- length(Yinfos$control.ids)
    nconst <- length(Yinfos$const.ids)
    nstatus <- length(Yinfos$status.ids)
    nfunc <- length(DOE$nF)
    ntotal <- sum(nint, ncontrol, nconst, nstatus, nfunc)
    tagList(
      fluidRow(
        column(10,""),
        column(1,actionButton(ns("finishdismiss"),label=HTML(paste("Previous","Screen",sep="<br>")), icon=icon("step-backward"), class = "btn-warning"), align="right"),
        column(1,actionButton(ns("finishconfirm"),label=HTML(paste("Final","Confirmation",sep="<br>")), icon=icon("step-forward"), class = "btn-primary"), align="right")
      ),
      h4("Import Summary"),
      h4(paste0("Observations: ",DOE$nobs)),
      h4(paste0("Inputs: ",DOE$nX)),
      h4(paste0("Outputs: ", ntotal, " (Interest: ", nint, ", Control: ", ncontrol,", Constant: ", nconst,", Status: ", nstatus, 
                ", Functional: ", nfunc, ")"))
    )
  })
  
  ##################################################################
  # Finish Inputs Tab

  observeEvent(input$finishinputsconfirm,{
    displayui$mode <- "finalpreviewinputs"
    # Fill final DOE object
    print("Confirmed final DOE inputs")
    DOEfinal$Xopt <- DOE$Xopt
    DOEfinal$X <- DOE$X
    DOEfinal$nobs <- DOE$nobs
    DOEfinal$nX <- DOE$nX
    DOEfinal$xnames <- DOE$xnames
    DOEfinal$xnamesvisu <- DOE$xnamesvisu
    DOEfinal$xnamesmenu <- DOE$xnamesmenu
    DOEfinal$Xinfos <- DOE$Xinfos
    DOEfinal$idon <- DOE$idon

    persistence$autoSavingCount <- persistence$autoSavingCount + 1
    persistence$autoSavingCaller <- "uploadDOE-finalpreviewinputs"
  })
  
  observeEvent(input$finishinputsdismiss,{
    # Reinitialize tabsetpanel
    displaytabs$finishinputs <- FALSE
    # Reinitialize UI
    displaytabs$ids <- c(1,2)
  })
  
  output$tabfinishinputs.dynui <- renderUI({
    tagList(
      fluidRow(
        column(10,""),
        column(1,actionButton(ns("finishinputsdismiss"),label=HTML(paste("Previous","Screen",sep="<br>")), icon=icon("step-backward"), class = "btn-warning"), align="right"),
        column(1,actionButton(ns("finishinputsconfirm"),label=HTML(paste("Final","Confirmation",sep="<br>")), icon=icon("step-forward"), class = "btn-primary"), align="right")
      ),
      h4("Import Summary"),
      h4(paste0("Observations: ",DOE$nobs)),
      h4(paste0("Inputs: ",DOE$nX))
    )
  })
  
  ##################################################################
  # Dismiss
  
  observeEvent(input$close,{
    # Reinitialize tabsetpanel
    displaytabs$inputs <- FALSE
    displaytabs$outputs <- FALSE
    displaytabs$finish <- FALSE
    finishedtabs$inputs <- FALSE
    finishedtabs$outputs <- FALSE
    finishedtabs$finish <- FALSE
    # Reinitialize file info
    file.to.load$datapath <- NULL
    header$bool <- TRUE
    separator$char <- ","
    decimal$char <- "."
    firstguessfile$finished <-  FALSE
    file.data <- NULL
    # Reinitialize UI
    displayui$mode <- "tabset"
    displaytabs$ids <- c(1)
    # Reset Xinfos and Yinfos
    initialXinfos$nX <- NULL
    initialXinfos$Xinfos <- NULL
    Xinfos$nX <- NULL
    Xinfos$Xinfos <- NULL
    initialYinfos$ynames <- NULL
    initialYinfos$ynamesvisu <- NULL
    initialYinfos$ynamesmenu <- NULL
    initialYinfos$nY <- NULL
    initialYinfos$Yinfos <- NULL
    Yinfos$ynames <- NULL
    Yinfos$nY <- NULL
    Yinfos$Yinfos <- NULL
    # Reinitialize temporary DOE
    DOEtemp$Xopt <- NULL
    DOEtemp$XY <- NULL
    DOEtemp$X <- NULL
    DOEtemp$Y <- NULL
    DOEtemp$nobs <- NULL
    DOEtemp$nX <- NULL
    DOEtemp$nY <- NULL
    DOEtemp$xnames <- NULL
    DOEtemp$ynames <- NULL
    DOEtemp$xnamesvisu <- NULL
    DOEtemp$ynamesvisu <- NULL
    DOEtemp$xnamesmenu <- NULL
    DOEtemp$ynamesmenu <- NULL
    DOEtemp$adapt.visu <- FALSE
    DOEtemp$Xinfos <- NULL
    DOEtemp$idon <- NULL
    DOEtemp$Yinfos <- NULL
    DOEtemp$nYsurrogate <- NULL
    DOEtemp$discF <- NULL
    DOEtemp$nF <- NULL
    DOEtemp$idF <- NULL
    DOEtemp$Fnames <- NULL
    DOEtemp$Fnamesvisu <- NULL
    # Reinitialize temporary DOE
    DOE$Xopt <- NULL
    DOE$XY <- NULL
    DOE$X <- NULL
    DOE$Y <- NULL
    DOE$nobs <- NULL
    DOE$nX <- NULL
    DOE$nY <- NULL
    DOE$xnames <- NULL
    DOE$ynames <- NULL
    DOE$xnamesvisu <- NULL
    DOE$ynamesvisu <- NULL
    DOE$xnamesmenu <- NULL
    DOE$ynamesmenu <- NULL
    DOE$adapt.visu <- FALSE
    DOE$Xinfos <- NULL
    DOE$idon <- NULL
    DOE$Yinfos <- NULL
    DOE$nYsurrogate <- NULL
    DOE$discF <- NULL
    DOE$nF <- NULL
    DOE$idF <- NULL
    DOE$Fnames <- NULL
    DOE$Fnamesvisu <- NULL
  })
  
  
  
  #### Loaded study ####
  
  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "uploadDOE") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      if (is.null(persistence$loadedStudy$doeProblemDef$choice) || persistence$loadedStudy$doeProblemDef$choice == 1 || persistence$loadedStudy$doeProblemDef$choice == 2) {
        displayui$mode <- ifelse(persistence$loadedStudy$doeProblemDef$choice == 1, "finalpreview", "finalpreviewinputs")
        if (is.null(persistence$loadedStudy$DOE.manual)) { # Old saving doesn't have the field 'DOE.manual' => use field 'DOE'
          DOEXY <- persistence$loadedStudy$DOE
        }
        else {
          DOEXY <- persistence$loadedStudy$DOE.manual
        }
        
        DOEfinal$Xopt <- DOEXY$Xopt
        DOEfinal$Xinfos <- DOEXY$Xinfos
        DOEfinal$Yinfos <- DOEXY$Yinfos
        DOEfinal$XY <- DOEXY$XY
        DOEfinal$X <- DOEXY$X
        DOEfinal$Y <- DOEXY$Y
        DOEfinal$nobs <- DOEXY$nobs
        DOEfinal$nX <- DOEXY$nX
        DOEfinal$nY <- DOEXY$nY
        DOEfinal$xnames <- DOEXY$xnames
        DOEfinal$ynames <- DOEXY$ynames
        DOEfinal$xnamesvisu <- DOEXY$xnamesvisu
        DOEfinal$ynamesvisu <- DOEXY$ynamesvisu
        DOEfinal$xnamesmenu <- DOEXY$xnamesmenu
        DOEfinal$ynamesmenu <- DOEXY$ynamesmenu
        DOEfinal$adapt.visu <- DOEXY$adapt.visu
        DOEfinal$idref <- DOEXY$idref
        DOEfinal$idon <- DOEXY$idon
        DOEfinal$compositeInfos <- DOEXY$compositeInfos
        DOEfinal$discF <- DOEXY$discF
        DOEfinal$nF <- DOEXY$nF
        DOEfinal$idF <- DOEXY$idF
        DOEfinal$Fnames <- DOEXY$Fnames
        DOEfinal$Fnamesvisu <- DOEXY$Fnamesvisu
        
        if (!is.null(DOEXY$OFtot)) {
          # Remove OF from DOE$Y
          
          DOEfinal$nY <- DOEfinal$nY - length(DOEfinal$nF) - 1
          DOEfinal$Y <- DOEfinal$Y[1:DOEfinal$nY]
          DOEfinal$XY <- cbind(DOEfinal$X, DOEfinal$Y)
          DOEfinal$ynames <- DOEfinal$ynames[1:DOEfinal$nY]
          DOEfinal$ynamesmenu <- DOEfinal$ynamesmenu[1:DOEfinal$nY]
          DOEfinal$ynamesvisu <- DOEfinal$ynamesvisu[1:DOEfinal$nY]

          DOEfinal$Yinfos$all.ids <- DOEfinal$Yinfos$all.ids[1:DOEfinal$nY]
          DOEfinal$Yinfos$int.ids <- which(DOEfinal$Yinfos$all.ids == "Interest")
          DOEfinal$Yinfos$surrogate.ids <- c(DOEfinal$Yinfos$int.ids, DOEfinal$Yinfos$control.ids)
          DOEfinal$Yinfos$type <- DOEfinal$Yinfos$type[1:DOEfinal$nY]
        }

        # Update DOE for composites
        
        DOE$nX <- DOEfinal$nX
        DOE$X <- DOEfinal$X
        if (!is.null(DOEfinal$nY)) {
          DOE$nY <- DOEfinal$nY - length(DOEfinal$compositeInfos)
          DOE$Y <- DOEfinal$Y[1:DOE$nY]
          DOE$XY <- c(DOE$X, DOE$Y)
          DOE$ynames <- DOEfinal$ynames[1:DOE$nY]
          DOE$ynamesmenu <- DOEfinal$ynamesmenu[1:DOE$nY]
          DOE$ynamesvisu <- DOEfinal$ynamesvisu[1:DOE$nY]
        }
      }

      progressToNextStep(persistence)
    }
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  
  ##################################################################
  # FINAL PREVIEW
  
  Xinfos2 <- callModule(XinfosChange.server, "bounds2", DOEfinal, data = DOEfinal, nvalues = typeDetectionValue())
  
  Xactivation <- callModule(XactiveChange.server, "inputactivate2", DOEfinal)
  
  # Update DOE info if Xactivation changes
  observeEvent(Xactivation$idon,{
    DOEfinal$idon <- Xactivation$idon
  })
  
  Yinfos2 <- callModule(YinfosChange.server, "outtype2", DOEfinal, TRUE)
  
  # Update DOEfinal info if Yinfos2 changes
  observeEvent(Yinfos2$Yinfos, {
    req(Yinfos2$Yinfos)
    print("updating output type")
    DOEfinal$Yinfos <- Yinfos2$Yinfos
    DOEfinal$nYsurrogate <- Yinfos2$nY
  })
  
  # Preview last Yinfos
  output$preview2 <- DT::renderDataTable({
    req(Yinfos2$Yinfos,DOEfinal$Y, 
        length(Yinfos2$Yinfos$all.ids)==length(Yinfos2$ynamesvisu))
    s <- compute.summary(DOEfinal$Y,my.summary,Yinfos2$Yinfos)
    s <- rbind(matrix(c(Yinfos2$Yinfos$all.ids,Yinfos2$Yinfos$type), nrow=2, byrow = T),s)
    df <- as.data.frame(s)
    colnames(df) <- Yinfos2$ynamesvisu
    rownames(df) <- c("Group","Type","Categories","Mean","Sd","Median","Min","Max","Quantile Var.")
    ncolumns <- ncol(df)
    xx <- data.frame(lapply(df,as.character),stringsAsFactors = FALSE)
    xx[is.na(xx)] <- "NA"
    rownames(xx) <- rownames(df)
    colnames(xx) <- colnames(df)    
    DT::datatable(
      xx, escape = FALSE,
      extensions = c('FixedColumns','Scroller'),
      options = list(
        dom = 't', ordering=F,
        pageLength = 2, scrollX = TRUE,scroller = TRUE, fixedColumns = TRUE,
        columnDefs = list(list(
          targets = 1:ncolumns,
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 12 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
            "}")))
      ))
  })
  
  # Build datatable  for display
  DTsummary <- reactiveValues(stats=NULL)
  observe({
    req(DOEfinal$nX, DOEfinal$Y, DOEfinal$xnames, DOEfinal$ynames, DOEfinal$Yinfos)
    df <- cbind(as.data.frame(matrix(NA,length(my.summary(1))+1,DOEfinal$nX)),compute.summary(DOEfinal$Y, my.summary, DOEfinal$Yinfos))
    colnames(df) <- c(DOEfinal$xnamesvisu,DOEfinal$ynamesvisu)
    DTsummary$stats <- df
  })
  
  choicesY <- reactive({
    req(DOEfinal$ynamesmenu,DOEfinal$Yinfos)
    idYnum <- which(DOE$Yinfos$type == 'numeric')
    c("None", DOEfinal$ynamesmenu[intersect(c(DOEfinal$Yinfos$int.ids, DOEfinal$Yinfos$control.ids), idYnum)])
  })
  ynamecolor <- callModule(dynamicSelect.server, "chooseYcolor", label="Color vs Output", choices = choicesY)
  
  output$DTcontents <- DT::renderDataTable({
    req(DOEfinal$X,DOEfinal$Y,!is.null(ynamecolor()),!is.null(input$DT.summarystats))
    dfinit <- cbind(DOEfinal$X,DOEfinal$Y)
    colnames(dfinit) <- c(DOEfinal$xnamesvisu,DOEfinal$ynamesvisu)
    dimd <- ncol(dfinit)
    nobs <- nrow(dfinit)
    
    if (ynamecolor()!="None"){
      idcolcolor <- which(ynamecolor()==DOEfinal$ynamesmenu)
      if (idcolcolor %in% DOEfinal$Yinfos$status.ids){
        u <- unique(DOEfinal$Y[DOEfinal$ynames[idcolcolor]])
        typecol <- "factor"
        brks <- unlist(lapply(u,as.character))
        clrs <- brewer.pal(nrow(u),"Set1")[1:nrow(u)]
      }else{
        typecol <- "numeric"
        brks <- c(-1e8,quantile(DOEfinal$Y[DOEfinal$ynames[idcolcolor]], probs = seq(0.5, 0.95, length.out=10), na.rm = TRUE))
        clrs <- c("#fff",brewer.pal(11,"RdBu")[11:1])
      }
    }
    
    if (!input$DT.summarystats){
      df <- dfinit
      rnames <- as.character(1:nobs)
      if (ynamecolor()!="None"){
        newcol <- DOEfinal$Y[DOEfinal$ynames[idcolcolor]]
        if (typecol=="factor"){
          newcol <- data.frame(x=unlist(lapply(newcol,as.character)))
        }
        colnames(newcol) <- "colY"
        df <- cbind(df,newcol)
      }
    }else{
      df <- rbind(DTsummary$stats,dfinit)
      rnames <- c("Categories", "Mean","Sd","Median","Min","Max","Quantile Var.", 1:nobs)
      if (ynamecolor()!="None"){
        newcol <- DOEfinal$Y[DOEfinal$ynames[idcolcolor]]
        if (typecol=="factor"){
          newcol <- data.frame(x=unlist(lapply(newcol,as.character)))
        }
        colnames(newcol) <- "colY"
        newcol <- rbind(data.frame(colY = rep(NA, 7)), newcol)
        df <- cbind(df,newcol)
      }
    }
    if (ynamecolor()!="None"){
      dt <- DT::datatable(
        df, rownames = rnames, escape = FALSE, 
        extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
        options = list(columnDefs = list(list(visible=FALSE, targets=dimd+1)),
                       dom = 'Brtip',
                       buttons = list(list(extend = 'colvis', columns = 1:dimd)),
                       scrollX = TRUE,scrollY = 400,scroller = TRUE,fixedColumns = TRUE
        ), selection = 'single')
    }else{
      dt <- DT::datatable(
        df, rownames = rnames, escape = FALSE, 
        extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
        options = list(
          dom = 'Brtip',
          buttons = list(list(extend = 'colvis', columns = 1:dimd)),
          scrollX = TRUE,scrollY = 400,scroller = TRUE,fixedColumns = TRUE
        ), selection = 'single')
    }
    if (ynamecolor()!="None"){
      if (typecol=="numeric"){
        dt <- dt %>%
          formatStyle(
            "colY",
            target = "row",
            backgroundColor = styleInterval(brks, clrs)
          )
      }else{
        dt <- dt %>%
          formatStyle(
            "colY",
            target = "row",
            backgroundColor = styleEqual(brks, clrs)
          )
      }
    }
    if (input$DT.summarystats){
      dt <- dt %>%
        formatStyle(
          0,
          target = "row",
          fontWeight = styleEqual(c("Mean","Sd","Median","Min","Max","Quantile Var."), rep("bold",6))
        )
    }
    return(dt)
  })
  
  # Select reference point, if any
  observeEvent(input$set_ref,{
    if (is.null(input$DTcontents_rows_selected)){
      showModal(modalDialog(HTML(paste(
        "Please select a row.")), title = "Warning",
        size = 'l')
      )
    }else{
      if (!input$DT.summarystats){
        DOEfinal$idref <- input$DTcontents_rows_selected
      }else{
        nstat <- length(my.summary(1))
        if (input$DTcontents_rows_selected > nstat){
          DOEfinal$idref <- input$DTcontents_rows_selected - nstat
        }else{
          showModal(modalDialog(HTML(paste(
            "Please select a row with an observation and not a summary stat.")), title = "Warning",
            size = 'l')
          )
        }
      }
    }
  })
  
  output$DTsettings.dynui <- renderUI({
    if (is.null(DOEfinal$idref)){
      title_button <- HTML(paste("Set Reference Point","Please select a row",sep="<br>"))
    }else{
      title_button <- HTML(paste("Set Reference Point",paste("Currently selected:",DOEfinal$idref),sep="<br>"))
    }
    fluidRow(
      column(2,""),
      column(2,
             actionButton(ns("set_ref"), title_button, class = "btn-primary"), align = "center"
      ),
      column(2,""),
      column(2,
             dynamicSelect.ui(ns("chooseYcolor")), align = "center"
      ),
      column(2,
             switchInput(ns("DT.summarystats"), value = F, label = "Display Summary Stats",size = "mini"), align="right"
      )
    )
  })
  
  var.num <- reactiveValues(names = NULL)
  var.cat <- reactiveValues(names = NULL)
  mapNames <- reactiveValues(df = NULL)
  
  # Possibility to evaluate DOE quality 
  observeEvent(input$gocheckDOE, {
    req(DOEfinal$Xopt)
    
    if (is.null(var.num$names)){
      var.num$names <- unlist(sapply(DOEfinal$Xinfos, function(var.info){
        if (var.info$type == 'numeric'){
          var.info$name
        }else{NULL}
      }))
    }
    
    if (is.null(var.cat$names)){
      var.cat$names <-  unlist(sapply(DOEfinal$Xinfos, function(var.info){
        if (var.info$type == 'categorical'){
          var.info$name
        }else{NULL}
      }))
    }
    
    if (is.null(mapNames$df)){
      mapNames$df <- data.frame(DOEfinal$xnames, DOEfinal$xnamesmenu, 
                                DOEfinal$xnamesvisu)
      names(mapNames$df) <- c("names", "menu", "visu")
    }

    toggleModal(session, "modalcheckDOE", toggle = "open")
  })
  
  observeEvent(input$addCompositeOutput, {
    toggleModal(session, "modalComposite", toggle = "open")
  })
  
  callModule(evaluateDOE.server, "evaluateDOE", DOEfinal, settings)
  
  visualizeDOEServer("plotVisualize",
                     data = reactive(DOEfinal$Xopt),
                     numericVariables = reactive(var.num$names),
                     categoricalVariables = reactive(var.cat$names),
                     mapNames = reactive(mapNames$df))
  
  compositeInfos <- compositeFunctionServer("compositeOutput", DOEfinal)
  
  observeEvent(compositeInfos$CInfos, {
    req(compositeInfos$CInfos, Yinfos2$Yinfos)

    nC <- length(compositeInfos$CInfos)
    namesC <- sapply(compositeInfos$CInfos, function(x){x$name})
    idC <- sapply(compositeInfos$CInfos, function(x){x$id})
    typeC <- sapply(compositeInfos$CInfos, function(x){x$type})
    modelModeC <- sapply(compositeInfos$CInfos, function(x){x$modelMode})
    YC <- lapply(compositeInfos$CInfos, function(x){x$dfNewCol})
    YC <- do.call('cbind', YC)

    DOEfinal$nY <- DOE$nY + nC
    DOEfinal$nYsurrogate <- DOE$nYsurrogate + nC
    if (nC > 0){
      DOEfinal$Y <- cbind(DOE$Y, YC)
      DOEfinal$XY <- cbind(DOE$XY, YC)
      DOEfinal$ynames <- c(DOE$ynames, namesC)
      DOEfinal$ynamesmenu <- c(DOE$ynamesmenu, namesC)
      DOEfinal$ynamesvisu <- c(DOE$ynamesvisu, paste(namesC,
                                                   paste0(
                                                     "(Composite - ",
                                                     modelModeC,
                                                     " mode)"), sep="<br>"))
      DOEfinal$Yinfos$all.ids <- c(Yinfos2$Yinfos$all.ids[1:DOE$nY], rep("Interest", nC))
      DOEfinal$Yinfos$int.ids <- c(Yinfos2$Yinfos$int.ids[which(Yinfos2$Yinfos$int.ids <= DOE$nY)], idC)
      DOEfinal$Yinfos$surrogate.ids <- c(Yinfos2$Yinfos$surrogate.ids[which(Yinfos2$Yinfos$surrogate.ids <= DOE$nY)], idC)
      DOEfinal$Yinfos$type <- c(Yinfos2$Yinfos$type[1:DOE$nY], typeC)
    }else{
      DOEfinal$Y <- DOE$Y
      DOEfinal$XY <- DOE$XY
      DOEfinal$ynames <- DOE$ynames
      DOEfinal$ynamesmenu <- DOE$ynamesmenu
      DOEfinal$ynamesvisu <- DOE$ynamesvisu
      DOEfinal$Yinfos$all.ids <- Yinfos2$Yinfos$all.ids[1:DOE$nY]
      DOEfinal$Yinfos$int.ids <- Yinfos2$Yinfos$int.ids[which(Yinfos2$Yinfos$int.ids <= DOE$nY)]
      DOEfinal$Yinfos$surrogate.ids <- Yinfos2$Yinfos$surrogate.ids[which(Yinfos2$Yinfos$surrogate.ids <= DOE$nY)]
      DOEfinal$Yinfos$type <- Yinfos2$Yinfos$type[1:DOE$nY]
    }
    DOEfinal$compositeInfos <- compositeInfos$CInfos
    
    persistence$autoSavingCount <- persistence$autoSavingCount + 1
    persistence$autoSavingCaller <- "uploadDOE-compositeInfos$CInfos-changed"
    
  })
  
  return(DOEfinal)
}
