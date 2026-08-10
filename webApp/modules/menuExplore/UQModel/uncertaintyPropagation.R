#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module uncertaintyPropagation
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)
source('modules/surrogate/surrogate_functions.R', local = TRUE)

getUQData <- function(DOE, predfun, nsample, UQparams, Yinfos, callback)  {
  dimx <- DOE$nX
  dimy <- Yinfos$nY
  sampleUQ <- generateUQ(UQparams, nsample, DOE)
  YUQ <- as.data.frame(matrix(0, nrow = nsample, ncol = dimy))
  for (j in 1:dimy) {
    YUQ[,j] <- predfun(sampleUQ,Yinfos$visu.ids[j])
    callback(j)
  }
  UQtemp <- cbind(sampleUQ, YUQ)
  colnames(UQtemp) <- c(DOE$xnamesmenu,DOE$ynamesmenu[Yinfos$visu.ids])
  colnames(YUQ) <- DOE$ynames[Yinfos$visu.ids]
  list(UQsample = YUQ, data = UQtemp)
}

getUQStats <- function(DOE, UQsample, Yinfos) {
  Ytypes <- DOE$Yinfos$type[Yinfos$visu.ids]
  dimy <- sum(Ytypes == 'numeric')
  ys <- UQsample[, Ytypes == 'numeric', drop = FALSE]
  df <- matrix(NA,nrow = 5,ncol = dimy)
  df[1,] <- apply(ys,2,mean)
  df[2,] <- apply(ys,2,sd)
  df[3,] <- apply(ys,2,median)
  df[4,] <- apply(ys,2,quantile,probs = 0.25)
  df[5,] <- apply(ys,2,quantile,probs = 0.75)
  df <- as.data.frame(df)
  colnames(df) <- DOE$ynamesvisu[Yinfos$visu.ids][Ytypes == 'numeric']
  rownames(df) <- c("Mean","Standard deviation","Median","Quantile 25%","Quantile 75%")
  return(df)
}

plotUQ <- function(DOE, idy, UQsample, typevisu) {
  yname <- DOE$ynames[idy]
  ynamevisu <- DOE$ynamesvisu[idy]
  ynamemenu <- DOE$ynamesmenu[idy]
  if (DOE$adapt.visu){
    margin=list(b = -1)
  }else{
    margin=NULL
  }
  ys <- UQsample[,yname]
  Ytype <- DOE$Yinfos$type[idy]
  if (Ytype == 'numeric'){
    m <- mean(ys, na.rm = T)
    sig <- sd(ys, na.rm = T)
    dens <- density(ys,na.rm = T)
    nx <- length(dens$x)
    if (typevisu=="Probability Distribution Function"){
      df <- data.frame(x = dens$x,y = dens$y,id = rep("Estimated Probability<br>Density Function",nx))
      df <- rbind(df,data.frame(x = dens$x,y = dnorm(df$x,m,sig),id = rep("Gaussian with<br>same moments",nx)))
      p <- layout(
        plot_ly(df, x = ~x, y = ~y, split = ~id, type = 'scatter', mode = 'lines'),
        title = paste("Uncertainty Propagation for ", ynamemenu), 
        xaxis = list(title = ynamevisu), 
        yaxis = list(title = "Probability"), margin = margin
      )
    }else{
      df <- data.frame(x = dens$x,y = ecdf(ys)(dens$x),id = rep("Estimated Cumulative<br>Density Function",nx))
      df <- rbind(df,data.frame(x = dens$x,y = pnorm(df$x,m,sig),id = rep("Gaussian with<br>same moments",nx)))
      p <- layout(
        plot_ly(df, x = ~x, y = ~y, split = ~id, type = 'scatter', mode = 'lines'),
        title = paste("Uncertainty Propagation for ", ynamemenu), 
        xaxis = list(title = ynamevisu), 
        yaxis = list(title = "Cumulative Probability"), margin = margin
      )
    }
  }
  if (Ytype == 'categorical'){
    p <- layout(
      plot_ly(x = ~ys, type = 'histogram', histnorm = "probability"),
      title = paste("Uncertainty Propagation for ", ynamemenu), 
      xaxis = list(title = ynamevisu), 
      yaxis = list(title = "Probability"), margin = margin
    )
  }
  return(p)
}

getUQDataProba <- function(DOE, predfun, nrep, nsample, UQparams, id, sname, tname, callback)  {
  YUQproba <- matrix(0,nsample,nrep)
  for (r in 1:nrep){
    sampleUQ <- generateUQ(UQparams, nsample, DOE)
    YUQproba[,r] <- predfun(sampleUQ,id)
    callback(r)
  }
  if (sname == "<="){
    idproba <- (YUQproba <= tname)
  }
  if (sname == ">="){
    idproba <- (YUQproba >= tname)
  }
  if (sname == "="){
    idproba <- (YUQproba == tname)
  }
  proba <- apply(idproba,2,mean)
  list(probasample = proba)
}

plotUQproba <- function(yname, sname, tname, UQproba) {
  layout(
    plot_ly(y = UQproba$probasample, type = "box",name=paste(yname,sname,tname)),
    title = "Probability Estimation", 
    xaxis = list(title = ""), 
    yaxis = list(title = "Probability"),
    boxmode = "group"
  )
}

computeAdditionalSimulationsUQ <- function(DOE, nadd, ntest, yname, tname, models, Yinfos, callback) {

  dimx <- DOE$nX
  Xadd <- as.data.frame(matrix(nrow = nadd, ncol = dimx))
  colnames(Xadd) <- colnames(DOE$X)
  Xbounds <- get.bounds(DOE$Xinfos)
  if ("All" %in% yname) {
    idyadd <- Yinfos$visu.ids
  } else {
    idyadd <- which(DOE$ynamesmenu %in% yname)
  }
  nidyadd <- length(idyadd)
  Xtest <- generateXtest(Xbounds,ntest,DOE)
  modelstemp <- vector('list',nidyadd)
  for (i in 1:nidyadd) {
    modelstemp[[i]] <- models[[idyadd[i]]]
  }
  Ytype <- DOE$Yinfos$type[idyadd]
  if (Ytype == 'numeric'){crit <- "ranjan"}
  if (Ytype == 'categorical'){crit <- "local_categorical"}
  # Begin loop on points to add
  Xtemp <- Xtest
  for (a in 1:nadd) {
    nsy <- nrow(Xtemp)
    nextpoint <- onestep.improve.metamodel(modelstemp, Xtemp, criterion = crit, target = tname)
    Xadd[a,] <- nextpoint$Xbest
    Xtemp <- Xtemp[-nextpoint$idbest,]
    constantliar <- matrix(NA, nrow = 1, ncol = nidyadd)
    for (i in 1:nidyadd){
      constantliar[i] <- predict.metamodel(modelstemp[[i]], Xadd[a,], computesd = FALSE)$mean
    }
    
    # Update kriging models
    for (i in 1:nidyadd){
      modelstemp[[i]] <- update.metamodel(modelstemp[[i]],Xadd = Xadd[a,],Yadd = constantliar[i])
    }
    callback(a)
  }
  
  nsimu <- nrow(Xadd)
  rownames(Xadd) <- paste0("Simu", 1:nsimu)
  return(Xadd)
}

uncertaintyPropagation.ui <- function(id) {
  ns <- NS(id)
  
  firstPanel <- wellPanel(
    actionButton(ns("go"), "Propagate Uncertainties",icon = icon("chart-bar"), class = "btn-primary"),
    hr(),
    numericInput(ns("nsample"), "Sample Size", 10000, min = 1),
    downloadButton(ns("download"), "Export UQ propagation", class = "btn-info")
  )
  firstTab <- fluidRow(
    column(
      4,firstPanel
    ),
    column(
      8,
      fluidRow(
        column(6,
               dynamicSelect.ui(ns("chooseY"))
        ),
        column(6,
               selectInput(
                 ns("chooseVisu"), 
                 label = "Choose UQ propagation visualization",
                 choices = c("Probability Distribution Function","Cumulative Distribution Function"),
                 selected = "Probability Distribution Function"
               ))),
      br(),
      plotlyOutput(ns("plot"), height = "600px"),
      DT::dataTableOutput(ns("table"))
    )
  )
  
  secondPanel <- wellPanel(
    fluidRow(
      column(5,
             dynamicSelect.ui(ns("chooseYUQproba"))
      ),
      uiOutput(ns("probaThresholdDef"))
    ),
    br(),
    numericInput(ns("nsampleUQproba"), "Sample Size", 10000, min = 1),
    "It is recommended to choose a sample size at least equal to 100/p if the probability to be estimated is of the order p. If you do not have an estimate of the probability, you can use the UQ propagation above as a rough estimate.",
    br(),
    br(),
    actionButton(ns("goUQproba"), "Compute Probability",icon=icon("chart-bar"), class="btn-primary"),
    checkboxInput(ns("dobootproba"), "Resample ?", value = T),
    br(),
    br(),
    h4("Refine Surrogate Model Near Threshold"),
    conditionalPanel(
            condition = paste0("output['", ns("conditionalRefine"), "']"),
    fluidRow(
      column(6, numericInput(ns("naddproba"), "Number of Additional Simulations", 1, min = 1)),
      uiOutput(ns("tagDOEUI"))
    ),
    fluidRow(
      column(7, disabled(actionButton(ns("generate"), "Generate Additional Simulations",
                             icon = icon("table"), class = "btn-info", width = '100%'))),
      conditionalPanel(condition =  paste0("output['", ns("use_simulator"), "']"),
        column(5, disabled(actionButton(ns("launch.simu"), "Launch Simulations", class = 'btn-primary',
                               icon = icon('cog'), width = '100%')))
      )
    ),
    br(),
    br(),
    downloadButton(ns("downloadproba"), "Export Additional Simulations", class="btn-info")
    ),
    conditionalPanel(
      condition = paste0("output['", ns("conditionalRefine"), "'] == false"),
      fluidRow(
        p("Refinement criteria not available for the selected surrogate model")
      )
    )
  )
  secondTab <- fluidRow(
    column(4,secondPanel),
    column(8,
           plotlyOutput(ns("plotproba")),
           uiOutput(ns("preview"))
    )
  )
  
  tagList(
    tabsetPanel(id = ns('tabs-propagation'), type = "tabs",
                tabPanel(h4("Global Propagation"), value = ns('global'),
                         tagList(
                           br(),
                           firstTab
                         )
                ),
                tabPanel(h4("Probability Estimation"), value = ns('proba'),
                         tagList(
                           br(),
                           secondTab,
                           bsModal(ns("modalLaunchSimu"), "Choose Mode  to Launch Additional Simulations", NULL,
                                   fluidRow(
                                     column(6, h5('Send the additional simulations to the "importDOE" panel where they can be
                                      manually launched.')),
                                     column(6, h5('Automatically launch the additional simulations with the current simulator settings.'))
                                   ),
                                   fluidRow(
                                     column(6, actionButton(ns('simu.manual'), 'Manual', class='btn-primary', width = '100%',
                                                            icon = icon("table"))),
                                     column(6, actionButton(ns('simu.automatic'), 'Automatic', class='btn-warning', width = '100%',
                                                            icon = icon("play-circle")))
                                   )
                           )
                         )
                )
    )
  )
}

uncertaintyPropagation.server <- function(id, DOE, listmodels, doeProblemDef, UQparams, persistence, settings) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns 
      
      output$conditionalRefine <- reactive({
          req(any(!is.na(listmodels$selected$id)),!is.null(ynameproba()))
          refinableModels <- lapply(listmodels$selected$id,function(i){listmodels$withsdmodels[i]})
          refinableselectedoutput <- refinableModels[ynameproba()]
          return(refinableselectedoutput[[1]])
      })
      
      outputOptions(output, "conditionalRefine", suspendWhenHidden = FALSE)

      # Update output types for the visualization only if the surrogate models are updated
      Yinfos <- reactiveValues(int.ids=NULL, control.ids=NULL, const.ids=NULL, visu.ids=NULL, nY=NULL)
      
      use_simulator <- reactive({
        bool <- FALSE
        if (!is.null(doeProblemDef$choice)){
          bool <- (doeProblemDef$choice != 1)
        }
        return(bool)
      })

      observeEvent(list(listmodels$bestQ2loo$id, DOE$nY), {
        
        YwithSelectedModel <- seq(DOE$nY)
        
        if (!is.null(listmodels$selected))
          YwithSelectedModel <- YwithSelectedModel[sapply(listmodels$selected$id, function(x) !is.na(x[1]))]
        
        Yinfos$int.ids <- intersect(DOE$Yinfos$int.ids, YwithSelectedModel)
        Yinfos$control.ids <- intersect(DOE$Yinfos$control.ids, YwithSelectedModel)
        Yinfos$const.ids <- intersect(DOE$Yinfos$const.ids, YwithSelectedModel)
        Yinfos$visu.ids <- c(Yinfos$int.ids, Yinfos$control.ids, Yinfos$const.ids)
        Yinfos$nY <- length(Yinfos$visu.ids)
      })
      
      choicesY <- reactive({
        req(DOE$ynamesmenu,Yinfos)
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[Yinfos$int.ids])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[Yinfos$control.ids])
        if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[Yinfos$const.ids])
        return(l)
      })
      
      yname <- callModule(
        dynamicSelectpicker.server, "chooseY", label.title = "Choose Output to Visualize", choices = choicesY,
        multiple = FALSE, livesearch = TRUE, selected = DOE$ynamesmenu[Yinfos$int.ids[1]]
      )
      
      output$use_simulator <- use_simulator
      outputOptions(output, 'use_simulator', suspendWhenHidden = FALSE)
      
      UQres <- reactiveValues(UQsample = NULL, data = NULL, nsample = NULL)
      
      observeEvent(persistence$updatingStep, {
        if (persistence$updatingStep == "uncertaintyPropagation") {
          logger$print(paste("Loaded study, updating", persistence$updatingStep))
          
          if (!is.null(persistence$loadedStudy$UQres$nsample)) {
            # Update Global Propagation"

            loadedUQres <- persistence$loadedStudy$UQres
            
            UQres$UQsample <- loadedUQres$UQsample
            UQres$data <- loadedUQres$data
            UQres$nsample <- loadedUQres$nsample
            
            updateNumericInput(session, "nsample", value = UQres$nsample)

            # Update Probability Estimation

            loadedUQproba <- persistence$loadedStudy$UQproba
            
            UQproba$probasample <- loadedUQproba$probasample
            UQproba$yname <- loadedUQproba$yname
            UQproba$sign <- loadedUQproba$sign
            UQproba$threshold <- loadedUQproba$threshold
            UQproba$nsample <- loadedUQproba$nsample

            updateNumericInput(session, "nsampleUQproba", value = UQproba$nsample)
          }
          progressToNextStep(persistence)
        }        
      }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
      
      # reset UQ if uncertainty definition is changed
      observeEvent({
        UQparams$UQparams
        UQparams$listCopulas
      }, {
        UQres$UQsample = NULL
        UQres$data = NULL
        UQproba$probasample = NULL
      })
      
      observeEvent(input$go, {
        req(listmodels$finalpredfun, input$nsample)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          dimy <- Yinfos$nY
          callback <- function(i) {
            incProgress(1/dimy, detail = paste("Output",i,"/",dimy))
          }
          withProgress(message = 'Propagating...', value = 0, {
            newUQres <- getUQData(
              DOE, listmodels$finalpredfun, input$nsample, UQparams, Yinfos, callback
            )
          })
          UQres$UQsample <- newUQres$UQsample
          UQres$data <- newUQres$data
          UQres$nsample <- input$nsample
          persistence$autoSavingCount <- persistence$autoSavingCount + 1
          persistence$autoSavingCaller <- "uncertaintyPropagation-go"
        }
      })
      
      output$plot <- renderPlotly({
        req(UQres$UQsample, yname(), cancelOutput = TRUE)
        idy <- which(DOE$ynamesmenu==yname())
        plotUQ(DOE, idy, UQres$UQsample, input$chooseVisu)
      })
      
      output$table <- DT::renderDataTable({
        req(UQres$UQsample, length(colnames(UQres$UQsample))==length(unlist(choicesY())))
        df <- signif(getUQStats(DOE, UQres$UQsample, Yinfos),4)
        dimd <- ncol(df)
        colnames(df) <- DOE$ynamesvisu[Yinfos$visu.ids][DOE$Yinfos$type[Yinfos$visu.ids] == 'numeric']
        DT::datatable(
          df, escape = FALSE,
          extensions = c('FixedColumns','Scroller','Buttons'),
          options = list(
            dom = 'Brtip', 
            buttons = list(list(extend = 'colvis', buttons = c("csv"),
                                text = "Download", columns = 1:dimd)), 
            pageLength = 2, scrollX = TRUE,scroller = TRUE, fixedColumns = TRUE
          ))
      })
      
      output$download <- downloadHandler(
        filename = 'UQpropagation.csv',
        content = function(con) {
          write.table(x = UQres$data, file = con, row.names = F, col.names = T, sep = ",")
        }
      )
      

      selectedYnameProba <- reactive({
        req(DOE$ynamesmenu, Yinfos)
        
        name <- isolate(UQproba$yname)
        if(is.null(name)){
          DOE$ynamesmenu[Yinfos$int.ids[1]]
        }else{
          name
        }
        
      })
      
      ynameproba <- callModule(
        dynamicSelectpicker.server, "chooseYUQproba", label.title = "Choose Output", choices = choicesY,
        multiple = FALSE, livesearch = TRUE, selected = selectedYnameProba()
      )
      
      output$probaThresholdDef <- renderUI({
        req(DOE$Yinfos, ynameproba())
        
        Ytype <- DOE$Yinfos$type[which(ynameproba() == DOE$ynamesmenu)]
        
        if (Ytype == 'numeric'){
          
          if(!is.null(UQproba$nsample)){
            selectedSign <- UQproba$sign
            selectedThresh <- UQproba$threshold
          }else{
            selectedSign <- "<="
            selectedThresh <- 0
          }
          
          signUI <- selectInput(ns("signYUQproba"), label = "",
                                choices = c("<=",">="), selected = selectedSign)
          thresholdUI <- numericInput(ns("threshYUQproba"), "", selectedThresh)
        }
        
        if (Ytype == 'categorical'){
          Ylevels <- unique(DOE$Y[,which(ynameproba() == DOE$ynamesmenu)])
          
          if(!is.null(UQproba$nsample)){
            selectedSign <- UQproba$sign
            selectedThresh <- UQproba$threshold
          }else{
            selectedSign <- "="
            selectedThresh <- Ylevels[1]
          }
          
          signUI <- selectInput(ns("signYUQproba"), label = "",
                                choices = c("="), selected = selectedSign)
          thresholdUI <- selectInput(ns("threshYUQproba"), label = "",
                                     choices = Ylevels, selected = selectedThresh)
        }
        
        tagList(
          column(3, signUI),
          column(4, thresholdUI)
        )
      })
      
      UQproba <- reactiveValues(probasample = NULL, yname = NULL, sign = NULL, threshold = NULL, nsample = NULL)
      
      observeEvent(input$goUQproba, {
        req(listmodels$finalpredfun, input$nsampleUQproba)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          predfun <- listmodels$finalpredfun
          nrep <- if (input$dobootproba)  {settings$nrepGSA} else {1}
          idy <- which(DOE$ynamesmenu==ynameproba())
          
          callback2 <- function(r) {
            incProgress(1/nrep, detail = paste("Resample", r,"/",nrep))
          }
          withProgress(message = 'Propagating...', value = 0, {
            newUQProbares <- getUQDataProba(
              DOE, listmodels$finalpredfun, nrep, input$nsampleUQproba, UQparams, idy, input$signYUQproba, input$threshYUQproba, callback2
            )
          })
          UQproba$probasample <- newUQProbares$probasample
          UQproba$yname <- ynameproba()
          UQproba$sign <- input$signYUQproba
          UQproba$threshold <- input$threshYUQproba
          UQproba$nsample <- input$nsampleUQproba
          
          persistence$autoSavingCount <- persistence$autoSavingCount + 1
          persistence$autoSavingCaller <- "uncertaintyPropagation-goUQproba"
          
        }
      })
      
      
      output$plotproba <- renderPlotly({
        req(UQproba$probasample, ynameproba(), cancelOutput = TRUE)
        plotUQproba(isolate(ynameproba()), isolate(input$signYUQproba), isolate(input$threshYUQproba), UQproba)
      })
      
      simulations = reactiveValues(Xadd = NULL, mode.manual = NULL, mode.automatic = NULL, 
                                   tagDOE = "Refine Threshold 1", nRefine = 1)
      
      observeEvent(listmodels$selected, {
        simulations$Xadd <- NULL
      })
      
      observeEvent(simulations$Xadd, {
        if (!is.null(simulations$Xadd) && nrow(simulations$Xadd) != 0) {
          shinyjs::enable("launch.simu")
        }
        else {
          shinyjs::disable("launch.simu")
        }
      })
      
      observeEvent(input$launch.simu, {
        toggleModal(session, "modalLaunchSimu", toggle = "open")
      })
      
      observeEvent(input$simu.manual, {
        req(simulations$Xadd, input$launch.simu)
        simulations$mode.manual <- input$simu.manual
        simulations$tagDOE <- input$tagDOE
        toggleModal(session, "modalLaunchSimu", toggle = "close")
      })
      
      observeEvent(input$simu.automatic, {
        req(simulations$Xadd, input$launch.simu)
        simulations$mode.automatic <- input$simu.automatic
        simulations$tagDOE <- input$tagDOE
        toggleModal(session, "modalLaunchSimu", toggle = "close")
      })
      
      numDOE <- reactive({
        get.nb.num(DOE$Xinfos) == DOE$nX
      })
      
      observeEvent(list(ynameproba(), listmodels$trainedModels, DOE$Xinfos), {
        if (!is.null(listmodels$trainedModels)) {
          shinyjs::enable("generate")
        }
        else {
          shinyjs::disable("generate")
        }
      })
      
      observeEvent(input$generate, {
        req(ynameproba(), !is.null(listmodels$trainedModels), DOE$Xinfos)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          models <- vector('list',Yinfos$nY)
          models[Yinfos$visu.ids] <- lapply(Yinfos$visu.ids,function (i){
            return(listmodels$models[[listmodels$selected$id[[i]]]][[i]])
          })
          nadd <- input$naddproba
          callback <- function(a) {
            incProgress(1/nadd, detail = paste("Adding", a,"/", nadd))
          }
          withProgress(message = 'Identifying Additional Simulations...', value = 0, {
            Xadd <- try(computeAdditionalSimulationsUQ(
              DOE, nadd, settings$nfaure, ynameproba(), input$threshYUQproba, models, Yinfos, callback
            ),silent=TRUE)
          })
          if (class(Xadd)[1]=="try-error") {
            showModal(modalDialog(HTML("No additional simulations are computed."),
                                  title = "Generation of additional simulations has failed"))
          } else {
            simulations$Xadd <- Xadd
            if (use_simulator()){
              if (simulations$tagDOE == input$tagDOE){
                simulations$tagDOE <- paste("Refine Threshold", simulations$nRefine)
              }else{
                simulations$tagDOE <- input$tagDOE
              }
              simulations$nRefine <- simulations$nRefine + 1
            }
          }
        }
      })
      
      output$tagDOEUI <- renderUI({
        req(use_simulator())
        column(6, textInput(ns("tagDOE"), label = 'Tag DOE Info', value = simulations$tagDOE, width = '100%'))
      })
      
      output$preview <- renderUI({
        req(simulations$Xadd)
        ns <- session$ns
        tagList(
          h4("Proposed Additional Simulations"),
          hr(),
          DT::dataTableOutput(ns("tableproba"))
        )
      })
      
      output$tableproba <- DT::renderDataTable({
        req(simulations$Xadd)
        df <- simulations$Xadd
        dimd <- ncol(df)
        colnames(df) <- DOE$xnamesvisu
        DT::datatable(
          df, escape = FALSE,
          extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
          options = list(
            dom = 'Brtip',
            buttons = list(list(extend = 'colvis', columns = 1:dimd)),
            scrollX = TRUE,scrollY = 200,scroller = TRUE,fixedColumns = TRUE
          ))
      })
      
      
      output$downloadproba <- downloadHandler(
        filename = 'AdditionalSimulations.csv',
        content = function(con) {
          isolate({
            df <- simulations$Xadd
            colnames(df) <- DOE$xnamesmenu
          })
          write.table(x=df, file=con, row.names=F, col.names=T, sep=",")
        }
      )
      
      return(list(simulations = simulations, UQproba = UQproba, UQres = UQres))
    }
  )
}
