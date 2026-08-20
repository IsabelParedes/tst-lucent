#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module distributionfitting
source("modules/menuExplore/UQModel/testfunctions.R", local = TRUE)


tableUQ <- matrix(c(
  "N", "U", "E", "G", "LN", "B", "TN","Est","KDE",
  "norm","unif","exp","gamma","lnorm","beta", "truncnorm","estimated","kde",
  "Normal","Uniform","Exponential","Gamma","LogNormal","Beta", "Truncated Normal","Estimated","KDE",
  "Mean=","Lower=","Lambda=","a=","Mean=","alpha=", "Mean=","","",
  "Std=","Upper=","","b=","Std=","beta=", "Std=","","",
  "", "", "", "", "", "", "a","","",
  "", "", "", "", "", "", "b","",""
), ncol = 7)
nameDensNS <- c("N","E","G","LN","U")
nameDens <- c("Normal","Exponential","Gamma","LogNormal","Uniform")
nameDensMASS <- c("normal","exponential","gamma","log-normal","unif")
nameCDFgof <- c("pnorm","pexp","pgamma","plnorm","punif")
namePDF <- c("dnorm","dexp","dgamma","dlnorm","dunif")
paramPDF <- c(2,1,2,2,2)
names(paramPDF) <- nameDens
nameCDFINV <- c("qnorm","qexp","qgamma","qlnorm","qunif")
nbparamDens <- c(2,1,2,2,2)
maxparamDens <- max(nbparamDens)
ndistr <- length(nameDens)
idDistrnonneg <- c(2,3,4)
nameTests <- c("Kolmogorov-Smirnov","Anderson-Darling","Cramer-Von Mises")
nameFuncTests <- c("ks.test","ad.test","cvm.test")
nTest <- length(nameTests)

ComputeGOF <- function(nameTest,sample,distr,fit,resample){
  # fit always contains estimated parameters
  switch(nameTest,
         "Kolmogorov-Smirnov"={
           if (resample){
             res <-ks.test.rep(sample,distr,fit)
           }else{
             res <-ks.test.one(sample,distr,fit)
           }
         },
         "Anderson-Darling"={
           if (resample){
             res <- ad.test.rep(sample,distr,fit)
           }else{
             res <- ad.test.one(sample,distr,fit)
           }
         },
         "Cramer-Von Mises"={
           if (resample){
             res <- cvm.test.rep(sample,distr,fit)
           }else{
             res <- cvm.test.one(sample,distr,fit)
           }
         })
  return(res)
}

distributionfitting.ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('ui.allpanels'))%>%withSpinner()
}

distributionfitting.server <- function(input, output, session, UQparams, listCopulas, DOE) {
  
  ns <- session$ns
  
  # --------------------
  # Data structures
  # --------------------
  
  data.to.fit.marginals <- reactiveValues(data = NULL)
  data.to.fit.copulas <- reactiveValues(data = NULL)
  TempEstimDist <- reactiveValues(idall=NULL, idcat=NULL, idnum=NULL, listUQparams=NULL, all.res.test=NULL, idbest=NULL)
  FinalEstimDist <- reactiveValues(UQparams=NULL, selection.marginals="unconfirmed", listCopulas=NULL, selection.copulas="unconfirmed")
  CopulaType <- reactiveValues(ids=NULL, mode=NULL, groups=NULL, unique.groups=NULL)
  
  # we reinitialize everything when UQparams is changed
  observeEvent(UQparams$UQparams, {
    data.to.fit.marginals$data <- NULL
    if (any(UQparams$estimated)){
      # Create empty structure to contain further final selected estimated distributions
      UQparams.temp <- UQparams$UQparams[UQparams$estimated]
      # Identify indices
      idall <- which(UQparams$estimated)
      idcat <- which(sapply(idall,function(i) DOE$Xinfos[[i]]$type=="categorical"))
      idnum <- setdiff(1:length(idall),idcat)
      TempEstimDist$idall <- idall
      TempEstimDist$idcat <- idcat
      TempEstimDist$idnum <- idnum
      TempEstimDist$listUQparams <- NULL
      TempEstimDist$all.res.test <- NULL
      TempEstimDist$idbest <- NULL
    }
    FinalEstimDist$UQparams <- UQparams$UQparams
    FinalEstimDist$selection.marginals <- "unconfirmed"
  })
  
  # we reinitialize everything when listCopulas is changed
  observeEvent(listCopulas$listCopulas, {
    data.to.fit.copulas$data <- NULL
    sampleCopulaFit$sample <- NULL
    # Parse listCopulas to detect which fitting mode is asked
    # (i.e. known or unknown groups)
    if (all(!listCopulas$listCopulas$inputs)){
      # No copula defined
      CopulaType$mode <- "No estimation"
    }else{
      if (any(listCopulas$listCopulas$groups=="Estimated")){
        # We have to estimate the groups and then each copula
        CopulaType$mode <- "Unknown groups"
        CopulaType$ids <- listCopulas$listCopulas$inputs
        CopulaType$groups <- listCopulas$listCopulas$groups
      }else{
        # The groups have been defined
        group.estimate <- listCopulas$listCopulas$typeCopulas=="Estimated"
        if (any(group.estimate)){
          # For some groups he wave to estimate copulas
          id.estimate <- listCopulas$listCopulas$groups %in% as.character(which(group.estimate))
          CopulaType$ids <- id.estimate
          CopulaType$mode <- "Known groups"
          CopulaType$groups <- listCopulas$listCopulas$groups[id.estimate]
        }else{
          # All groups have already their own copula defined
          CopulaType$mode <- "No estimation"
        }
      }
    }
    FinalEstimDist$listCopulas <- listCopulas$listCopulas
    FinalEstimDist$selection.copulas <- "unconfirmed"
  })
  
  # --------------------
  # UI panels
  # --------------------
  
  output$ui.allpanels <- renderUI({
    if ((is.null(UQparams$estimated) | all(!UQparams$estimated)) & CopulaType$mode=="No estimation"){
      mode <- "No fit"
    }else{
      if (is.null(UQparams$estimated) | all(!UQparams$estimated)){
        mode <- "Copula fit only"
      }else{
        if (CopulaType$mode=="No estimation"){
          mode <- "Marginal fit only"
        }else{
          mode <- "Marginal and copula fit"
        }
      }
    }
    t <- switch(mode,
                "No fit"={
                  tagList(
                    br(),
                    h4("No marginal distribution to fit (set some input parameters to 'Estimated' if yout want to fit their distribution) 
                       or copula to estimate (set some copulas to 'Estimated')"),
                    br()
                  )
                },
                "Marginal fit only"={
                  tabsetPanel(
                    id = ns("tabFittingMarginal"),
                    type = "tabs",
                              tabPanel(h4("Data to Fit Marginals"),
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.data.marginals'))
                                       )
                              ),
                              tabPanel(h4("Distribution Fitting (marginals)"), 
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.fit'))
                                       )
                              ),
                              tabPanel(h4("Select Distributions (marginals)"), 
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.select.marginals'))
                                       )
                              )
                  )
                },
                "Copula fit only"={
                  tabsetPanel(
                    id = ns("tabFittingCopula"),
                    type = "tabs",
                              tabPanel(h4("Data to Fit Copulas"),
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.data.copulas'))
                                       )
                              ),
                              tabPanel(h4("Distribution Fitting (copulas)"), 
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.fitcopula'))
                                       )
                              )
                  )
                },
                "Marginal and copula fit"={
                  tabsetPanel(
                    id = ns("tabFittingMarginalCopula"),
                    type = "tabs",
                              tabPanel(h4("Data to Fit Marginals"),
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.data.marginals'))
                                       )
                              ),
                              tabPanel(h4("Distribution Fitting (marginals)"), 
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.fit'))
                                       )
                              ),
                              tabPanel(h4("Select Distributions (marginals)"), 
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.select.marginals'))
                                       )
                              ),
                              tabPanel(h4("Data to Fit Copulas"),
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.data.copulas'))
                                       )
                              ),
                              tabPanel(h4("Distribution Fitting (copula)"), 
                                       tagList(
                                         br(),
                                         uiOutput(ns('ui.fitcopula'))
                                       )
                              )
                  )
                })
    return(t)
  })
  
  output$ui.data.marginals <- renderUI({
    tagList(
      uiOutput(ns("upload.marginals.dynui")),
      uiOutput(ns("sepdec.marginals.dynui")),
      uiOutput(ns("preview.marginals.dynui"))
    )
  })
  
  output$ui.data.copulas <- renderUI({
    tagList(
      uiOutput(ns("upload.copulas.dynui")),
      uiOutput(ns("sepdec.copulas.dynui")),
      uiOutput(ns("preview.copulas.dynui"))
    )
  })
  
  output$upload.marginals.dynui <- renderUI({
    text.estimated <- paste0("Input parameters to be estimated: ",paste(DOE$xnamesmenu[UQparams$estimated],collapse=", "))
    text.file <- "To proceed you must load a file with observations from these input parameters (with their name in the header) or use the observations 
    from the imported DOE."
    if (is.null(DOE$X)){
      t <- tagList(fluidRow(
        column(6,fileInput(ns('file.marginals'), 'Select File', accept = c('.txt','.dat','.csv')),
               tags$script(paste0('$( "#', ns('file.marginals'), '" ).on( "click", function() { this.value = null; });'))),
        column(6,"")
      ))
    }else{
      t <- tagList(fluidRow(
        column(6,fileInput(ns('file.marginals'), 'Select File', accept = c('.txt','.dat','.csv')),
               tags$script(paste0('$( "#', ns('file.marginals'), '" ).on( "click", function() { this.value = null; });'))),
        column(6,actionButton(ns("useDOEX.marginals"),label=HTML(paste("Use Data From","Imported DOE",sep="<br>")), class = "btn-primary"))
      ))
    }
    return(tagList(h3(text.estimated),h5(text.file),br(),br(),t))
  })
  
  output$upload.copulas.dynui <- renderUI({
    text.estimated <- paste0("Input parameters involved in copulas to be estimated: ",paste(DOE$xnamesmenu[CopulaType$ids],collapse=", "))
    text.file <- "To proceed you must load a file with observations from these input parameters (with their name in the header) or use the observations 
    from the imported DOE."
    if (is.null(DOE$X)){
      t <- tagList(fluidRow(
        column(6,fileInput(ns('file.copulas'), 'Select File', accept = c('.txt','.dat','.csv')),
               tags$script(paste0('$( "#', ns('file.copulas'), '" ).on( "click", function() { this.value = null; });'))),
        column(6,"")
      ))
    }else{
      t <- tagList(fluidRow(
        column(6,fileInput(ns('file.copulas'), 'Select File', accept = c('.txt','.dat','.csv')),
               tags$script(paste0('$( "#', ns('file.copulas'), '" ).on( "click", function() { this.value = null; });'))),
        column(6,actionButton(ns("useDOEX.copulas"),label=HTML(paste("Use Data From","Imported DOE",sep="<br>")), class = "btn-primary"))
      ))
    }
    return(tagList(h3(text.estimated),h5(text.file),br(),br(),t))
  })
  
  header.marginals <- reactiveValues(bool = TRUE)
  separator.marginals <- reactiveValues(char= ",")
  decimal.marginals <- reactiveValues(char = ".")
  firstguessfile.marginals <- reactiveValues(finished = FALSE)
  
  header.copulas <- reactiveValues(bool = TRUE)
  separator.copulas <- reactiveValues(char= ",")
  decimal.copulas <- reactiveValues(char = ".")
  firstguessfile.copulas <- reactiveValues(finished = FALSE)
  
  observeEvent(input$file.marginals,{
    # Investigate the second line (to prevent a false detection if there is a header with points in variable names)
    line2 <- readLines(input$file.marginals$datapath, n = 2)
    # Try all possible separators
    count.comma <- stri_count_fixed(line2, ",")[2]
    count.semicolon <- stri_count_fixed(line2, ";")[2]
    count.tab <- stri_count_fixed(line2, "\t")[2]
    if (count.semicolon > 0){
      separator.marginals$char <- ";"
      if (count.comma > 0){
        decimal.marginals$char <- ","
      }else{
        decimal.marginals$char <- "."
      }
    }else{
      if (count.tab > 0){
        separator.marginals$char <- "\t"
        if (count.comma > 0){
          decimal.marginals$char <- ","
        }else{
          decimal.marginals$char <- "."
        }
      }else{
        separator.marginals$char <- ","
        decimal.marginals$char <- "."
      }
    }
    # Then use the separator to detect if there is a header
    line1 <- readLines(input$file.marginals$datapath, n = 1)
    xynames <- unlist(strsplit(line1, separator.marginals$char))
    xynames <- gsub(paste0('[',decimal.marginals$char,']'), '.',  xynames)
    header.marginals$bool <- suppressWarnings(all(is.na(as.numeric(xynames))))
    firstguessfile.marginals$finished <- TRUE
  })
  
  observeEvent(input$file.copulas,{
    # Investigate the second line (to prevent a false detection if there is a header with points in variable names)
    line2 <- readLines(input$file.copulas$datapath, n = 2)
    # Try all possible separators
    count.comma <- stri_count_fixed(line2, ",")[2]
    count.semicolon <- stri_count_fixed(line2, ";")[2]
    count.tab <- stri_count_fixed(line2, "\t")[2]
    if (count.semicolon > 0){
      separator.copulas$char <- ";"
      if (count.comma > 0){
        decimal.copulas$char <- ","
      }else{
        decimal.copulas$char <- "."
      }
    }else{
      if (count.tab > 0){
        separator.copulas$char <- "\t"
        if (count.comma > 0){
          decimal.copulas$char <- ","
        }else{
          decimal.copulas$char <- "."
        }
      }else{
        separator.copulas$char <- ","
        decimal.copulas$char <- "."
      }
    }
    # Then use the separator to detect if there is a header
    line1 <- readLines(input$file.copulas$datapath, n = 1)
    xynames <- unlist(strsplit(line1, separator.copulas$char))
    xynames <- gsub(paste0('[',decimal.copulas$char,']'), '.',  xynames)
    header.copulas$bool <- suppressWarnings(all(is.na(as.numeric(xynames))))
    firstguessfile.copulas$finished <- TRUE
  })
  
  output$sepdec.marginals.dynui <- renderUI({
    req(firstguessfile.marginals$finished)
    tagList(
      hr(),
      h5("Header, Separator and Decimal have been auto-detected. Please change values if not correct."),
      fluidRow(
        column(4, radioButtons(ns("separator.marginals"), "Separator",
                               choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"), selected=separator.marginals$char)),
        column(4, radioButtons(ns("decimal.marginals"), "Decimal",
                               choices = list(". (point)" = ".", ", (comma)" = ","), selected=decimal.marginals$char)),
        column(4,"")
      ),
      hr()
    )
  })
  
  output$sepdec.copulas.dynui <- renderUI({
    req(firstguessfile.copulas$finished)
    tagList(
      hr(),
      h5("Header, Separator and Decimal have been auto-detected. Please change values if not correct."),
      fluidRow(
        column(4, radioButtons(ns("separator.copulas"), "Separator",
                               choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"), selected=separator.copulas$char)),
        column(4, radioButtons(ns("decimal.copulas"), "Decimal",
                               choices = list(". (point)" = ".", ", (comma)" = ","), selected=decimal.copulas$char)),
        column(4,"")
      ),
      hr()
    )
  })
  
  file.data.marginals <- reactive({
    req(firstguessfile.marginals$finished, input$file.marginals$datapath, input$separator.marginals != input$decimal.marginals)
    # consistency check
    head.lines.consist <- length(unique(lapply(readLines(input$file.marginals$datapath, n = 2), function(line){
      stringi::stri_count(line, fixed = input$separator.marginals)
    })))
    if (head.lines.consist == 1){
      df <- read.csv(input$file.marginals$datapath, header = header.marginals$bool, sep = input$separator.marginals,
                     dec = input$decimal.marginals)
    }else{
      df <- NULL
    }
    return(df)
  })
  
  file.data.copulas <- reactive({
    req(firstguessfile.copulas$finished, input$file.copulas$datapath, input$separator.copulas != input$decimal.copulas)
    # consistency check
    head.lines.consist <- length(unique(lapply(readLines(input$file.copulas$datapath, n = 2), function(line){
      stringi::stri_count(line, fixed = input$separator.copulas)
    })))
    if (head.lines.consist == 1){
      df <- read.csv(input$file.copulas$datapath, header = header.copulas$bool, sep = input$separator.copulas,
                     dec = input$decimal.copulas)
    }else{
      df <- NULL
    }
    return(df)
  })
  
  observeEvent(file.data.marginals(),{
    paramsinheader <- DOE$xnames[UQparams$estimated] %in% colnames(file.data.marginals())
    if (all(paramsinheader)){
      cnames <- DOE$xnames[UQparams$estimated]
      data.to.fit.marginals$data <- file.data.marginals()[cnames]
      colnames(data.to.fit.marginals$data) <- DOE$xnamesmenu[UQparams$estimated]
      showModal(modalDialog(HTML(
        "You can now proceed to the next tab to fit the distributions with your file.")
        , title = "Loading done !")
      )
    }else{
      showModal(modalDialog(HTML(
        "Some input parameters to be estimated were not detected in the header of the file.")
        , title = "Warning !")
      )
    }
  })
  
  observeEvent(file.data.copulas(),{
    paramsinheader <- DOE$xnames[CopulaType$ids] %in% colnames(file.data.copulas())
    if (all(paramsinheader)){
      cnames <- DOE$xnames[CopulaType$ids]
      data.to.fit.copulas$data <- file.data.copulas()[cnames]
      colnames(data.to.fit.copulas$data) <- DOE$xnamesmenu[CopulaType$ids]
      showModal(modalDialog(HTML(
        "You can now proceed to the next tab to fit the copulas with your file.")
        , title = "Loading done !")
      )
    }else{
      showModal(modalDialog(HTML(
        "Some copulas to be estimated are related to inputs which were not detected in the header of the file.")
        , title = "Warning !")
      )
    }
  })
  
  observeEvent(input$useDOEX.marginals,{
    data.to.fit.marginals$data <- DOE$X[,UQparams$estimated,drop=FALSE]
    colnames(data.to.fit.marginals$data) <- DOE$xnamesmenu[UQparams$estimated]
    showModal(modalDialog(HTML(
      "You can now proceed to the next tab to fit the distributions with your data.")
      , title = "Loading done !")
    )
  })
  
  observeEvent(input$useDOEX.copulas,{
    data.to.fit.copulas$data <- DOE$X[,CopulaType$ids]
    colnames(data.to.fit.copulas$data) <- DOE$xnamesmenu[CopulaType$ids]
    showModal(modalDialog(HTML(
      "You can now proceed to the next tab to fit the copulas with your data.")
      , title = "Loading done !")
    )
  })
  
  output$preview.marginals.dynui <- renderUI({
    req(data.to.fit.marginals$data)
    tagList(
      fluidRow(
        column(6,
               h4(paste0("Data Preview: Observations = ",nrow(data.to.fit.marginals$data),", Variables = ",ncol(data.to.fit.marginals$data)))
        ),
        column(6,"")
      ),
      DT::dataTableOutput(ns('headerdf.marginals')),
      br(),
      hr()
    )
  })
  
  output$preview.copulas.dynui <- renderUI({
    req(data.to.fit.copulas$data)
    tagList(
      fluidRow(
        column(6,
               h4(paste0("Data Preview: Observations = ",nrow(data.to.fit.copulas$data),", Variables = ",ncol(data.to.fit.copulas$data)))
        ),
        column(6,"")
      ),
      DT::dataTableOutput(ns('headerdf.copulas')),
      br(),
      hr()
    )
  })
  
  output$headerdf.marginals <- DT::renderDataTable({
    req(data.to.fit.marginals$data)
    d <- data.to.fit.marginals$data[1:min(5,nrow(data.to.fit.marginals$data)),]
    dimd <- ncol(d)
    DT::datatable(
      d, escape = FALSE,
      extensions = c('FixedColumns','Scroller'),filter = 'top',
      options = list(
        scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  output$headerdf.copulas <- DT::renderDataTable({
    req(data.to.fit.copulas$data)
    d <- data.to.fit.copulas$data[1:min(5,nrow(data.to.fit.copulas$data)),]
    dimd <- ncol(d)
    DT::datatable(
      d, escape = FALSE,
      extensions = c('FixedColumns','Scroller'),filter = 'top',
      options = list(
        scrollX = TRUE,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  output$ui.fit <- renderUI({
    req(data.to.fit.marginals$data)
    tagList(
      fluidRow(
        column(6,
               wellPanel(
                 radioGroupButtons(
                   inputId = ns("choiceFit"),
                   label = "Choose fitting mode",
                   choices = c("Parametric", 
                               "Non-parametric", "Both"),
                   status = "primary",
                   individual = TRUE,
                   checkIcon = list(
                     yes = icon("ok", 
                                lib = "glyphicon"),
                     no = icon("remove",
                               lib = "glyphicon"))
                 ),
                 h5("For parametric, check which distributions to fit and which tests to perform for your dataset on the right."),
                 h5("For non-parametric, a kernel density estimate is used."),
                 h5("For categorical variables, the chosen option does not impact the estimation, which is in all cases obtained by computing the 
                    empirical frequency of all categories in the dataset."),
                 br(),
                 h5(HTML("<b>Launch fitting</b>")),
                 actionButton(ns("goAutoFD"), "Proceed", class="btn-primary")
               )
        ),
        column(6,
               wellPanel(
                 h4("Parametric fitting settings"),
                 br(),
                 fluidRow(
                   column(4,
                          checkboxGroupInput(ns("idDistr"), label = "Distributions to fit", 
                                             choices = list("Normal" = 1, "Exponential" = 2, "Gamma" = 3, "LogNormal" = 4, "Uniform" = 5),
                                             selected = c(1,5))),
                   column(8,
                          checkboxGroupInput(ns("idTest"), label = "Tests to perform", 
                                             choices = list("Kolmogorov-Smirnov" = 1, "Anderson-Darling" = 2, "Cramer-Von Mises" = 3),
                                             selected = c(1,2,3)),
                          switchInput(
                            inputId = ns("resample"),
                            label = "Resample ?",
                            labelWidth = "80px",
                            size = "mini",
                            value = TRUE
                          ),
                          h5("By default all test statistics are computed with resampling in order to 
                           account for the estimation of distribution parameters, however this is computationally intensive."),
                          h5("Resampling can be switched-off but keep in mind that in this case goodness of fit will be over-confident.")
                   )
                 )
               ))
      ),
      fluidRow(
        column(3,
               uiOutput(outputId =ns("chooseYVisu"))
        ),column(3,
                 uiOutput(outputId = ns("chooseTypeVisu"))
        )
      ),
      br(),
      fluidRow(
        uiOutput(outputId = ns("DisplayTypeVisu"))%>%withSpinner()
      ),
      fluidRow(
        column(12,
               DT::dataTableOutput(ns('tableTests'))%>%withSpinner()
        )
      )
    )
  })
  
  output$ui.fitcopula <- renderUI({
    req(data.to.fit.copulas$data)
    if (CopulaType$mode=="Unknown groups"){
      copula.groups.choices <- c("Automatic", "All parameters in same group")
    }else{
      copula.groups.choices <- "Manual (defined in previous tab)"
    }
    if (is.null(sampleCopulaFit$sample)){
      textPlot3 <- textPlot4 <- NULL
    }else{
      textPlot3 <- "Kendall's tau coefficient (estimated copula)"
      textPlot4 <- "Spearman's rho coefficient (estimated copula)"
    }
    tagList(
    fluidRow(
      column(6,
             wellPanel(
               h4("Visual analysis of potential correlations (numeric inputs only)"),
               checkboxInput(ns("doreorder"), "Reorder Variables ?", value = F),
               h6("This will regroup variables into clusters of correlated ones."),
               hr(),
               h4("Select copula to estimate (numeric inputs only)"),
               radioGroupButtons(
                 inputId = ns("choiceCopula"),
                 label = "Choose copula groups",
                 choices = copula.groups.choices,
                 status = "primary",
                 individual = TRUE,
                 checkIcon = list(
                   yes = icon("ok", 
                              lib = "glyphicon"),
                   no = icon("remove",
                             lib = "glyphicon"))
               ),
               h5("'Manual' means that groups have been defined in the previous panel."),
               h5("'Automatic' means that groups are identified by clustering the correlation matrices."),
               h5("'All parameters in same group' means that a joint copula is automatically estimated."),
               br(),
               uiOutput(ns("ui.fitcopula.buttons"))
             )
      ),
      column(3,
             h4("Kendall's tau coefficient (pseudo-obs)"),
             plotOutput(ns("corrPlot1")),
             h4(textPlot3),
             plotOutput(ns("corrPlot3")), align="center"
      ),
      column(3,
             h4("Spearman's rho coefficient (pseudo-obs)"),
             plotOutput(ns("corrPlot2")),
             h4(textPlot4),
             plotOutput(ns("corrPlot4")), align="center"
      )
    ),
    bsModal(ns("modalAutomatic"), "Automatic selection of copula groups", NULL, size = "large", uiOutput(ns("automatic.dynui")),
            tags$head(tags$style(paste0("#", ns("modalAutomatic")," .modal-footer{display:none}",
                                        " .modal-lg{width: 70%}"))))
    )
  })
  
  output$ui.fitcopula.buttons <- renderUI({
    if (FinalEstimDist$selection.copulas=="unconfirmed"){
      t <- tagList(
        fluidRow(
          column(6,
                 h5(HTML("<b>Launch copula fitting</b>")),
                 actionButton(ns("goCopulafit"), "Proceed", class="btn-primary")
          ),
          column(6,""
          )
        )
      )
    }else{
      t <- tagList(
        fluidRow(
          column(6,
                 h5(HTML("<b>Launch copula fitting</b>")),
                 actionButton(ns("goCopulafit"), "Proceed", class="btn-primary")
          ),
          column(6,
                 h5(HTML("<b>Check copula fitting</b>")),
                 actionButton(ns("checkCopulafit"), "Proceed", class="btn-primary"),
                 h5("We generate a random sample following the estimated copulas and compute the correlations.")
          )
        )
      )
    }
    return(t)
  })
  
  output$automatic.dynui <- renderUI({
    tagList(
      fluidRow(
        column(4,
               pickerInput(inputId = ns("choiceCor"), 
                           label = "Choose Correlation Type", 
                           choices = c("Kendall's tau","Spearman's rho"),
                           selected = "Kendall's tau",
                           options = list(style = "btn-primary"), 
                           multiple = FALSE)),
        column(4,
               numericInput(ns("nbclust"), "Number of Clusters", 2, min = 1, max = sum(CopulaType$ids))),
        column(4,
               br(),
               actionButton(ns("buildclusters"), "Launch Clustering", class = "btn-primary", width = '100%'))
      ),
      fluidRow(
        plotlyOutput(ns("plotHclust"))%>% withSpinner()
      ),
      fluidRow(
        column(4,""),
        column(4,actionButton(ns("continueCopulafit"), "Continue to copula fitting", class = "btn-primary", width = '100%')),
        column(4,"")
      )
    )
  })
  
  # -----------------------------------
  # Functions for fitting marginals
  # -----------------------------------
  
  observeEvent(input$goAutoFD, {
    req(data.to.fit.marginals$data,input$choiceFit)
    idall <- TempEstimDist$idall
    idcat <- TempEstimDist$idcat
    idnum <- TempEstimDist$idnum
    all.res.test <- NULL
    idbest <- NULL
    if (length(idcat)){
      # Deal with categorical variables (they are directly stored in FinalEstimDist)
      catUQparams <- UQparams$UQparams[idall[idcat]]
      for (i in 1:length(idcat)){
        data.temp <- data.to.fit.marginals$data[,idcat[i]]
        levels <- catUQparams[[i]]$levels
        nlevels <- length(levels)
        weights <- lapply(1:nlevels,function(i){
          mean(data.temp == levels[i])
        })
        catUQparams[[i]]$typeDistr <- "Cat"
        catUQparams[[i]]$weights <- weights
      }
    }
    idDistr <- as.numeric(input$idDistr)
    nDistr <- length(idDistr)
    idTest <- as.numeric(input$idTest)

    # We have to train all parametric distributions
    withProgress(message = 'Fitting distributions & performing tests',
                 detail = 'This may take a while...',{
                   listUQparams <- lapply(idnum,function(j){
                     ltemp <- NULL
                     if (input$choiceFit!="Non-parametric"){
                       ltemp2 <- lapply(idDistr,function(id){
                         currentsample <- data.to.fit.marginals$data[,j]
                         currentsample <- currentsample[!is.na(currentsample)]
                         if (min(currentsample)<0 & id%in%idDistrnonneg){
                           # There are negative samples so we do not fit 
                           # a distribution supported only on the positive reals
                           fit <- rep(NA,4)
                           names(fit) <- paste0("P",1:4,"Distr")
                           res.test <- matrix(NA,2,3)
                           typeDistr <- tableUQ[which(nameDens[id]==tableUQ[,3]),2]
                           return(c(list(typeDistr=typeDistr,levels=NA,weights=NA,res.test=res.test),fit))
                         }else{
                           # Fit
                           fittemp <- closed.form.estimators(nameCDFgof[id],currentsample)
                           fit <- as.list(fittemp)
                           nfit <- length(fit)
                           fit <- c(fit,rep(NA,4-nfit))
                           names(fit) <- paste0("P",1:4,"Distr")
                           typeDistr <- tableUQ[which(nameDens[id]==tableUQ[,3]),2]
                           # Test, if any
                           if (length(idTest)){
                             res.test <- matrix(NA,2,3)
                             for (k in idTest){
                               res.test.temp <- try(ComputeGOF(nameTests[k],currentsample,nameCDFgof[id],fittemp,input$resample),silent=TRUE)
                               if (!class(res.test.temp)[1]=="try-error"){
                                 if (!is.nan(res.test.temp$statistic)){
                                   res.test[1,k] <- res.test.temp$statistic
                                   res.test[2,k] <- res.test.temp$p.value
                                 }
                               }
                             }
                           }else{
                             res.test <- NULL
                           }
                           return(c(list(typeDistr=typeDistr,levels=NA,weights=NA,res.test=res.test),fit))
                         }
                       })
                       ltemp <- c(ltemp,ltemp2)
                     }
                     if (input$choiceFit!="Parametric"){
                       # Use kde
                       currentsample <- data.to.fit.marginals$data[,j]
                       currentsample <- currentsample[!is.na(currentsample)]
                       ltemp <- c(ltemp,list(list(typeDistr="kde",struct=kde(currentsample),
                                                  P1Distr=NA,P2Distr=NA,P3Distr=NA,P4Distr=NA,levels=NA,weights=NA)))
                     }
                     return(ltemp)
                   })
                 })
    if (input$choiceFit!="Non-parametric"){
      atemp <- lapply(listUQparams,function(l) matrix(unlist(lapply(l,function(v) v$res.test[2,])),nrow=3))
      all.res.test <- array(NA,dim=c(3,length(idnum),nDistr))
      for (i in 1:length(idnum)){
        all.res.test[,i,] <- atemp[[i]]
      }
      all.rank.res <- ndistr - apply(all.res.test,c(1,2),rank,na.last="keep")
      all.rank.res[is.na(all.rank.res)] <- ndistr
      idbest <- numeric(length(idall))
      idbest[idnum] <- apply(apply(all.rank.res,c(1,3),sum,na.rm=TRUE),2,which.min)
    }else{
      idbest <- NULL
    }
    TempEstimDist$listUQparams <- vector('list',length(idall))
    if (length(idcat)){
      TempEstimDist$listUQparams[idcat] <- catUQparams
    }
    TempEstimDist$listUQparams[idnum] <- listUQparams
    TempEstimDist$all.res.test <- all.res.test
    TempEstimDist$idbest <- idbest
    showModal(modalDialog(HTML(
      "After analyzing the results, do not forget to proceed to the next tab to select which distribution to assign to each input.")
      , title = "Fitting finished !")
    )
  })
  
  output$chooseYVisu <- renderUI({
    req(input$goAutoFD, input$goAutoFD >= 1)

    idnum <- TempEstimDist$idnum
    cnames <- DOE$xnamesmenu[TempEstimDist$idall]
    selectInput(ns("VisuY"),
                label = "Choose Variable to Visualize",
                choices = cnames,
                selected = cnames[1])
  })

  output$chooseTypeVisu <- renderUI({
    req(input$VisuY)
    idY <- which(input$VisuY==colnames(data.to.fit.marginals$data))
    if (idY%in%TempEstimDist$idnum){
      choices <- list("Distribution Fit & QQ-Plot","Distribution Fit only","QQ-Plot only")
      selected <- "Distribution Fit & QQ-Plot"
    }else{
      choices <- list("Histogram")
      selected <- "Histogram"
    }
    selectInput(ns("FITvisu"),
                label = "Choose Visualization",
                choices = choices,
                selected = selected)
  })

  output$DisplayTypeVisu <- renderUI({
    req(input$FITvisu)
    
    t <- switch(input$FITvisu,
                "Histogram" = {
                  tagList(fluidRow(
                    column(12,
                           plotlyOutput(ns("plotDhist"))
                    )
                  ))
                },
                "Distribution Fit & QQ-Plot" = {
                  tagList(fluidRow(
                    column(6,
                           plotlyOutput(ns("plotFDhist"))
                    ),
                    column(6,
                           plotlyOutput(ns("plotFDqqplot"))
                    )
                  ))
                },
                "Distribution Fit only" = {
                  tagList(fluidRow(
                    column(12,
                           plotlyOutput(ns("plotFDhist"))
                    )
                  ))
                },
                "QQ-Plot only" = {
                  tagList(fluidRow(
                    column(12,
                           plotlyOutput(ns("plotFDqqplot"))
                    )
                  ))
                })
    return(t)
  })
  
  output$plotDhist <- renderPlotly({
    req(input$goAutoFD,input$VisuY, cancelOutput = TRUE)
    input$FITvisu
    yname <- DOE$xnamesmenu[which(input$VisuY==DOE$xnamesmenu)]
    idY <- which(colnames(data.to.fit.marginals$data)==yname)
    if (idY %in% TempEstimDist$idcat){
      d <- as.factor(data.to.fit.marginals$data[,idY])
      plot_ly(x=d, type = "histogram", histnorm="probability density") %>%
        layout(title="", xaxis=list(title=DOE$xnamesvisu[yname]), yaxis=list(title="Histogram"))
    }else{
      return(NULL)
    }
  })

  output$plotFDhist <- renderPlotly({
    req(input$goAutoFD,input$VisuY, cancelOutput = TRUE)
    input$FITvisu
    yname <- DOE$xnamesmenu[which(input$VisuY==DOE$xnamesmenu)]
    idYall <- which(colnames(data.to.fit.marginals$data)==yname)
    d <- data.to.fit.marginals$data[,idYall]
    if (idYall %in% TempEstimDist$idnum){
      xbounds <- DOE$Xinfos[[TempEstimDist$idall[idYall]]]$bounds
      nseq <- 1000
      xseq <- seq(xbounds[1],xbounds[2],length.out=nseq)
      # Loop on all available distributions
      UQparams.temp <- TempEstimDist$listUQparams[[idYall]]
      nd <- length(UQparams.temp)
      xpdf <- matrix(NA,nrow=nseq,ncol=nd)
      names.pdf <- NULL
      for (i in 1:nd){
        if (UQparams.temp[[i]]$typeDistr=="kde"){
          xpdf[,i] <- predict(UQparams.temp[[i]]$struct,x=xseq)
          names.pdf <- c(names.pdf,"KDE")
        }else{
          nameDist <- tableUQ[which(UQparams.temp[[i]]$typeDistr==tableUQ[,2]),3]
          params <- unname(unlist(UQparams.temp[[i]][paste0("P",1:4,"Distr")]))
          idDist <- which(nameDist==nameDens)
          xpdf[,i] <- do.call(namePDF[idDist],c(list(xseq),params[1:paramPDF[nameDist]]))
          names.pdf <- c(names.pdf,nameDist)
        }
      }
      xpdf <- as.data.frame(xpdf)
      colnames(xpdf) <- names.pdf
      xmeltpdf <- cbind(data.frame(x=rep(xseq,ncol(xpdf))),melt(xpdf))
      dfhist <- data.frame(ys=d)
      plot_ly(xmeltpdf,x=~x,y=~value,color=~as.factor(variable), type = 'scatter', mode = 'lines') %>%
        add_trace(data = dfhist, x = ~ys, type = "histogram", name = "Histogram", histnorm="probability density",marker=list(color='grey'),opacity=0.25,inherit=FALSE) %>%
        layout(title="", xaxis=list(title=DOE$xnamesvisu[yname]), yaxis=list(title="Probability Density Function"))
    }else{
      return(NULL)
    }
  })

  output$plotFDqqplot <- renderPlotly({
    req(input$goAutoFD,input$VisuY, cancelOutput = TRUE)
    input$FITvisu
    yname <- DOE$xnamesmenu[which(input$VisuY==DOE$xnamesmenu)]
    idYall <- which(colnames(data.to.fit.marginals$data)==yname)
    d <- data.to.fit.marginals$data[,idYall]
    if (idYall %in% TempEstimDist$idnum){
      pp <- ppoints(d[!is.na(d)])
      npp <- length(pp)
      empqq <- quantile(d,pp)
      minqq <- min(empqq)
      maxqq <- max(empqq)
      # Loop on all available distributions
      UQparams.temp <- TempEstimDist$listUQparams[[idYall]]
      nd <- length(UQparams.temp)
      thqq <- matrix(NA,nrow=npp,ncol=nd)
      names.pdf <- NULL
      for (i in 1:nd){
        if (UQparams.temp[[i]]$typeDistr=="kde"){
          thqq[,i] <- qkde(p=pp,fhat=UQparams.temp[[i]]$struct)
          names.pdf <- c(names.pdf,"KDE")
        }else{
          nameDist <- tableUQ[which(UQparams.temp[[i]]$typeDistr==tableUQ[,2]),3]
          params <- unname(unlist(UQparams.temp[[i]][paste0("P",1:4,"Distr")]))
          idDist <- which(nameDist==nameDens)
          thqq[,i] <- do.call(nameCDFINV[idDist],c(list(pp),params[1:paramPDF[nameDist]]))
          names.pdf <- c(names.pdf,nameDist)
        }
      }
      thqq <- as.data.frame(thqq)
      colnames(thqq) <- names.pdf
      thqqmelt <- cbind(data.frame(x=rep(empqq,ncol(thqq))),melt(thqq))
      dfline <- data.frame(x=c(minqq,maxqq),y=c(minqq,maxqq))
      plot_ly(thqqmelt,x=~x,y=~value,color=~as.factor(variable),type = 'scatter',mode='lines+markers',showlegend=T) %>%
        add_trace(data=dfline,x = ~x, y = ~y,line=list(color='black'),type = 'scatter',mode='lines', name = "Perfect Fit",inherit=FALSE) %>%
        layout(title="", xaxis=list(title=paste("Empirical Quantiles for",DOE$xnamesvisu[yname])), yaxis=list(title=paste("Theoretical Quantiles for",input$VisuY)))
    }else{
      return(NULL)
    }
  })

  output$tableTests <- DT::renderDataTable({
    req(input$goAutoFD,input$VisuY)
    yname <- DOE$xnamesmenu[which(input$VisuY==DOE$xnamesmenu)]
    idYall <- which(colnames(data.to.fit.marginals$data)==yname)
    if (idYall %in% TempEstimDist$idnum){
      # Loop on all available distributions
      UQparams.temp <- TempEstimDist$listUQparams[[idYall]]
      nd <- length(UQparams.temp)
      pvalues <- NULL
      names.pdf <- NULL
      for (i in 1:nd){
        if (!UQparams.temp[[i]]$typeDistr=="kde"){
          nameDist <- tableUQ[which(UQparams.temp[[i]]$typeDistr==tableUQ[,2]),3]
          pvalues <- rbind(pvalues,UQparams.temp[[i]]$res.test[2,])
          names.pdf <- c(names.pdf,nameDist)
        }
      }
      if (!is.null(names.pdf)){
        df <- signif(pvalues,6)
        dimd <- ncol(df)
        colnames(df) <- nameTests
        rownames(df) <- names.pdf
        limitpval <- 0.05
        dfvec <- c(df)
        dfvec <- dfvec[dfvec>limitpval]
        q <- quantile(dfvec, na.rm=T)
        colorint <- c(limitpval,0.1,0.25,0.5,0.75,0.9,0.95,1)
        nc <- length(colorint)
        colorintRGB <- c("#FFFFFF",brewer.pal(nc,"Blues"))
        if (nc > 1){
          bgcol = styleInterval(colorint,colorintRGB)
        }else{
          bgcol = "#FFFFFF"
        }
        DT::datatable(df, extensions = c('FixedColumns','Scroller','Buttons'),
                      options = list(dom = 'Brtip', buttons = list('copy','csv'), pageLength = 2,
                                     scrollX = TRUE,scroller = TRUE,
                                     fixedColumns = TRUE)) %>%
          formatStyle(colnames(df),backgroundColor = bgcol)
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })
  
  # ------------------------------------
  # Functions for fitting copulas
  # ------------------------------------
  
  output$corrPlot1 <- renderPlot({
    req(!is.null(input$doreorder),data.to.fit.copulas$data)
    d <- pseudo_obs(data.to.fit.copulas$data)
    colnames(d) <- DOE$xnamesvisu[CopulaType$ids]
    val <- cor(d, use = "pairwise.complete.obs",method="kendall")
    if (input$doreorder){
      oo <- "hclust"
    }else{
      oo <- "original"
    }
    corrplot(val, order = oo, tl.srt = 45)
  })
  
  output$corrPlot2 <- renderPlot({
    req(!is.null(input$doreorder),data.to.fit.copulas$data)
    d <- pseudo_obs(data.to.fit.copulas$data)
    colnames(d) <- DOE$xnamesvisu[CopulaType$ids]
    val <- cor(d, use = "pairwise.complete.obs",method="spearman")
    if (input$doreorder){
      oo <- "hclust"
    }else{
      oo <- "original"
    }
    oo <- "original"
    corrplot(val, order = oo, tl.srt = 45)
  })
  
  output$corrPlot3 <- renderPlot({
    req(!is.null(input$doreorder),sampleCopulaFit$sample)
    d <- sampleCopulaFit$sample
    colnames(d) <- DOE$xnamesvisu[CopulaType$ids]
    val <- cor(d, use = "pairwise.complete.obs",method="kendall")
    if (input$doreorder){
      oo <- "hclust"
    }else{
      oo <- "original"
    }
    corrplot(val, order = oo, tl.srt = 45)
  })
  
  output$corrPlot4 <- renderPlot({
    req(!is.null(input$doreorder),sampleCopulaFit$sample)
    d <- sampleCopulaFit$sample
    colnames(d) <- DOE$xnamesvisu[CopulaType$ids]
    val <- cor(d, use = "pairwise.complete.obs",method="spearman")
    if (input$doreorder){
      oo <- "hclust"
    }else{
      oo <- "original"
    }
    oo <- "original"
    corrplot(val, order = oo, tl.srt = 45)
  })
  
  observeEvent(input$goCopulafit,{
    req(data.to.fit.copulas$data,input$choiceCopula)
    switch(input$choiceCopula,
           "All parameters in same group"={
             # A joint copula on all (numeric) parameters
             d <- pseudo_obs(data.to.fit.copulas$data)
             withProgress(message = 'Computing joint copula',
                          detail = 'This may take a while...',{
                            fitted.copula <- vinecop(d)
                          })
             FinalEstimDist$listCopulas$groups[CopulaType$ids] <- rep("1",sum(CopulaType$ids))
             FinalEstimDist$listCopulas$unique.groups <- "1"
             FinalEstimDist$listCopulas$typeCopulas <- "Vine"
             FinalEstimDist$listCopulas$Copulas <- list("1"=fitted.copula)
             FinalEstimDist$selection.copulas <- "confirmed"
           },
           "Manual (defined in previous tab)"={
             groups <- CopulaType$groups
             unique.groups <- unique(groups)
             withProgress(message = 'Computing copula for all groups',detail = 'This may take a while...',{
               for (k in unique.groups){
                 length.group <- sum(groups==k)
                 if (length.group>1){
                   FinalEstimDist$listCopulas$typeCopulas[as.numeric(k)] <- "Vine"
                   dtemp <- pseudo_obs(data.to.fit.copulas$data[,k==groups])
                   FinalEstimDist$listCopulas$Copulas[[as.character(k)]] <- vinecop(dtemp)
                 }
               }
             })
             FinalEstimDist$selection.copulas <- "confirmed"
           },
           "Automatic"={
             toggleModal(session, "modalAutomatic", toggle = "open")
           })
  })
  
  hclustRes <- reactiveValues(hc=NULL,groups=NULL)
  
  observeEvent(input$choiceCor,{
    d <- pseudo_obs(data.to.fit.copulas$data)
    colnames(d) <- DOE$xnamesvisu[CopulaType$ids]
    if (input$choiceCor=="Kendall's tau"){
      corr <- cor(d, use = "pairwise.complete.obs",method="kendall")
    }else{
      corr <- cor(d, use = "pairwise.complete.obs",method="spearman")
    }
    hclustRes$hc <- hclust(as.dist(1 - abs(corr)))
  })
  
  output$plotHclust <- renderPlotly({
    req(hclustRes$hc, cancelOutput = TRUE)
    hclustRes$groups <- cutree(hclustRes$hc, k = input$nbclust)
    dend <- as.dendrogram(hclustRes$hc)
    dend <- color_branches(dend, k = input$nbclust)
    dend <- color_labels(dend, k = input$nbclust)
    g <- ggplot(dend, offset_labels = -0.1) + theme(legend.position='none')
    ggplotly(g)
  })
  
  observeEvent(input$continueCopulafit,{
    req(hclustRes$groups)
    toggleModal(session, "modalAutomatic", toggle = "close")
    groups.temp <- unname(hclustRes$groups)
    # Remove groups with only one variable
    unique.groups.temp <- unique(groups.temp)
    n.unique <- length(unique.groups.temp)
    length.groups <- sapply(unique.groups.temp,function(k) sum(groups.temp==k))
    groups <- groups.temp
    count.group <- 1
    for (k in 1:n.unique){
      if (length.groups[k]==1){
        groups[groups==unique.groups.temp[k]] <- 0
      }else{
        groups[groups==unique.groups.temp[k]] <- count.group
        count.group <- count.group + 1
      }
    }
    groups <- as.character(groups)
    unique.groups <- setdiff(unique(groups),"0")
    FinalEstimDist$listCopulas$groups[CopulaType$ids] <- groups
    FinalEstimDist$listCopulas$unique.groups <- unique.groups
    FinalEstimDist$listCopulas$typeCopulas <- vector('list',length(unique.groups))
    names(FinalEstimDist$listCopulas$typeCopulas) <- unique.groups
    withProgress(message = 'Computing copula for all groups',detail = 'This may take a while...',{
      for (k in unique.groups){
        FinalEstimDist$listCopulas$typeCopulas[as.numeric(k)] <- "Vine"
        dtemp <- pseudo_obs(data.to.fit.copulas$data[,k==groups])
        FinalEstimDist$listCopulas$Copulas[[as.character(k)]] <- vinecop(dtemp)
      }
    })
    FinalEstimDist$selection.copulas <- "confirmed"
  })
  
  sampleCopulaFit <- reactiveValues(sample=NULL)
  
  observeEvent(input$checkCopulafit,{
    # Relaunch automatically if choice has changed
    input$choiceCopula
    names <- DOE$xnamesvisu[CopulaType$ids]
    n <- 1000
    sample <- matrix(NA,n,length(names))
    colnames(sample) <- names
    groups <- FinalEstimDist$listCopulas$groups
    unique.groups <- c(FinalEstimDist$listCopulas$unique.groups,"0")
    for (k in unique.groups){
      id.group <- which(groups[CopulaType$ids]==k)
      if (k=="0"){
        # Independent uniform sample
        sample[,id.group] <- matrix(runif(n*length(id.group)),nrow=n)
      }else{
        # Sample from the group copula
        if (FinalEstimDist$listCopulas$typeCopulas[as.numeric(k)]=="Vine"){
          sample[,id.group] <- rvinecop(n, FinalEstimDist$listCopulas$Copulas[[k]])
        }else{
          sample[,id.group] <- rCopula(n, FinalEstimDist$listCopulas$Copulas[[k]])
        }
      }
    }
    sampleCopulaFit$sample <- sample
  })
  
  # --------------------------------------------------
  # Function for final selection of distributions
  # --------------------------------------------------
  
  output$ui.select.marginals <- renderUI({
    req(data.to.fit.marginals$data,input$goAutoFD)
    cnames <- DOE$xnamesmenu[UQparams$estimated]
    n <- length(cnames)
    t <- tagList(
      fluidRow(
        column(4,
               wellPanel(
                 h4("Please select which estimated distributions must be assigned to each input."),
                 h5("By default we select the KDE estimate (if 'Non-parametric') or the best parametric distribution 
         according to the computed tests (all other cases)."),
                 h5("Once your choise is done, click below to be able to update the first panel with your selection."),
                 br(),
                 actionButton(ns("save"),label="Confirm your selection", class = "btn-primary")
                 )
        ),
        column(8,
               lapply(1:n, function(i) {
                 UQparams.temp <- TempEstimDist$listUQparams[[i]]
                 if (i %in% TempEstimDist$idcat){
                   list.choices <- "Categorical with estimated weights"
                   selected <- NULL
                 }else{
                   choices <- unlist(lapply(UQparams.temp,function(l) l$typeDistr))
                   list.choices <- tableUQ[which(tableUQ[,2] %in% choices),3]
                   if (!is.null(TempEstimDist$idbest[i])){
                     selected <- tableUQ[which(tableUQ[,2]==choices[TempEstimDist$idbest[i]]),3]
                   }else{
                     selected <- NULL
                   }
                 }
                 fluidRow(
                   column(6,
                          textInput(ns(paste0('defName', i)), "Variable", cnames[i])
                   ),
                   column(6,
                          selectInput(ns(paste0('defFit', i)), 
                                      label = "Selected Distribution",
                                      choices = list.choices,
                                      selected = selected)
                   )
                 )
               }))
      )
    )
    return(t)
  })
  
  observeEvent(input$save, {
    disableActionButton(ns("save"),session)
    idall <- TempEstimDist$idall
    UQparams.temp <- lapply(1:isolate(length(idall)), function(i){
      if (i %in% TempEstimDist$idnum){
        selected <- tableUQ[which(tableUQ[,3] %in% input[[paste0('defFit', i)]]),2] 
        id.selected <- which(lapply(TempEstimDist$listUQparams[[i]],function(l) l$typeDistr)==selected)
        return(TempEstimDist$listUQparams[[i]][[id.selected]])
      }else{
        TempEstimDist$listUQparams[[i]]
      }
    })
    FinalEstimDist$UQparams[idall] <- UQparams.temp
    FinalEstimDist$selection.marginals <- "confirmed"
  })
  
  trigger.typeDistr <- reactive({
    req(input$goAutoFD)
    lapply(1:isolate(length(TempEstimDist$idall)), function(i){
      input[[paste0('defFit', i)]]
    })
  })
  
  observeEvent(trigger.typeDistr(), {
    req(input$goAutoFD)
    enableActionButton(ns("save"),session)
  })
  
  return(FinalEstimDist)
}
