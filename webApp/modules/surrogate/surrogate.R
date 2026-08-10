#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module surrogate
source("modules/surrogate/getSurrogatesDescr.R", local = TRUE)
USER_SURROGATES_DIRECTORY <- "modules/surrogatemodels"
source("modules/surrogate/buildsurrogate.R", local = TRUE)
source("modules/surrogate/choosesurrogate.R", local = TRUE)
source("modules/surrogate/refinesurrogate.R", local = TRUE)
source("modules/surrogate/export_surrogate.R", local = TRUE)

BUILTIN_SURROGATE_NAMES <- c("Lasso","Acosso 1","Acosso 2 with all vars","Acosso2 with Acosso 1 vars",
                             "Kriging with constant trend and all vars","Kriging with linear trend and all vars",
                             "Kriging with Lasso trend and all vars","Kriging with Acosso 1 trend and all vars",
                             "Kriging with constant trend and Lasso vars","Kriging with linear trend and Lasso vars",
                             "Kriging with constant trend and Acosso 1 vars","Kriging with Acosso 1 trend and Acosso 1 vars")

surrogate.ui <- function(id) {
  ns <- NS(id)
  tagList(
    bsModal(ns("modalViewDOEsurrogate"), "View Currently Used DOE", NULL,
            DT::dataTableOutput(ns('DTcurrentDOEsurrogate')), size = "large"
    ),
    bsModal(ns("modalconfirm.refresh"), "Confirm DOE Refresh", NULL,
            h4("Refreshing the DOE will delete all the metamodels trained so far."),
            actionButton(ns("surrogate.refresh"), label = "Confirm DOE Refresh", class = "btn-primary",icon = icon("sync"), width = '100%')
    ),
    fluidRow(
      column(2,h4(textOutput(ns("info.text0")))),
      column(2,h4(textOutput(ns("info.text1")))),
      column(2,h4(htmlOutput(ns("info.text2")))),
      uiOutput(ns('infoDOEsurrogate.dynui'))
    ),
    hr(),
    bsCollapse(
      id = ns("collapseSurrogate"),
      multiple = TRUE, open = "Build Surrogate Models",
      bsCollapsePanel(
        "Build Surrogate Models", style = "primary",
        buildsurrogate.ui(id = ns("buildsurrogate"))
      ),
      bsCollapsePanel(
        "Select Final Surrogate Models", style = "primary",
        choosesurrogate.ui(id = ns("choosesurrogate"))
      ),
      bsCollapsePanel(
        "Export Surrogate Models", style = "primary",
        exportSurrogateUI(id = ns("export_surrogate"))
      ),
      bsCollapsePanel(
        "Additional Simulations to Refine Surrogate Models", style = "primary",
        refinesurrogate.ui(id = ns("refinesurrogate"))
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("openPanelSurrogate"),
        label = "Open Panel",
        choices = c("",
                    "Build Surrogate Models",
                    "Select Final Surrogate Models",
                    "Export Surrogate Models",
                    "Additional Simulations to Refine Surrogate Models"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("closePanelSurrogate"),
        label = "Close Panel",
        choices = c("",
                    "Build Surrogate Models",
                    "Select Final Surrogate Models",
                    "Export Surrogate Models",
                    "Additional Simulations to Refine Surrogate Models"),
        selected = ""
      )
    )
  )
}

surrogate.server <- function(input, output, session, DOE, settings, doeProblemDef, surrogate.clicked, advance.importDOE, persistence) {

  ns <- session$ns
  
  asked.surrogate.refresh <- reactiveValues(bool = FALSE)
  observe({
    req(input$surrogate.refresh)
    asked.surrogate.refresh$bool <- (input$surrogate.refresh>0)
  })
  
  currentDOE <- reactiveValues(
    Xopt = NULL,
    Xinfos = NULL, Yinfos = NULL,
    XY = NULL, X = NULL, Y = NULL, nobs = NULL, nX = NULL, nY = NULL, 
    xnames = NULL, ynames = NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL,
    nobs = 0, adapt.visu = FALSE, compositeInfos = NULL, OFtot = NULL
  )
  
  updateCurrentDOE <- reactiveValues(initial = TRUE, launch = FALSE)
  
  observeEvent(input$openPanelSurrogate,{
    updateCollapse(session,
                   "collapseSurrogate",
                   open = input$openPanelSurrogate)
  })
  
  observeEvent(input$closePanelSurrogate,{
    updateCollapse(session,
                   "collapseSurrogate",
                   close = input$closePanelSurrogate)
  })
  
  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice != 1)
    }
    return(bool)
  })

  observeEvent(asked.surrogate.refresh$bool, {
    # refresh DOE when asked by user
    req(DOE$XY)
    asked.surrogate.refresh$bool <- FALSE
    updateCurrentDOE$launch <- TRUE
  })
  observeEvent(surrogate.clicked(), {
    # refresh DOE the first time surrogate tab is clicked when connected to an external simulator
    req(DOE$XY, use_simulator(), updateCurrentDOE$initial)
    updateCurrentDOE$initial <- FALSE
    updateCurrentDOE$launch <- TRUE
  })
  observeEvent(list(DOE$xnames, DOE$ynames, DOE$Xinfos, DOE$idon, DOE$Yinfos), {
    # refresh DOE when names, infos
    req(DOE$XY)
    SurrogateEnv$SurrogateDescrs <- getSurrogateDescr(USER_SURROGATES_DIRECTORY,DOE$Xinfos)$filtered
    SurrogateEnv$AllSurrogateDescrs <- getSurrogateDescr(USER_SURROGATES_DIRECTORY,DOE$Xinfos)$all
    updateCurrentDOE$initial <- TRUE
    updateCurrentDOE$launch <- TRUE
  })
  observeEvent(DOE$OFtot, {
    # refresh DOE when OF definition changes
    req(DOE$OFtot, currentDOE$OFtot)
    newOFtot <- DOE$OFtot[1:currentDOE$nobs,]
    oldOFtot <- currentDOE$OFtot[1:currentDOE$nobs,]

    if (!all(oldOFtot == newOFtot)) {
      updateCurrentDOE$initial <- TRUE
      updateCurrentDOE$launch <- TRUE
    }
  })
  
  observeEvent(updateCurrentDOE$launch, {
    req(DOE$XY)
    if (updateCurrentDOE$launch){
      updateCurDOE()
    }
  })
  
  updateCurDOE <- function() {
      idnotna <- !apply(is.na(DOE$Y),1,all)
      listmodels$currentlyUsed <- which(idnotna)
      currentDOE$Xopt <- DOE$X[idnotna,,drop=FALSE]
      currentDOE$XY <- DOE$XY[idnotna,,drop=FALSE]
      currentDOE$X <- DOE$X[idnotna,,drop=FALSE]
      currentDOE$Y <- DOE$Y[idnotna,,drop=FALSE]
      currentDOE$nobs <- sum(idnotna)
      currentDOE$nX <- DOE$nX
      currentDOE$nY <- DOE$nY
      currentDOE$xnames <- DOE$xnames
      currentDOE$ynames <- DOE$ynames
      currentDOE$xnamesvisu <- DOE$xnamesvisu
      currentDOE$ynamesvisu <- DOE$ynamesvisu
      currentDOE$xnamesmenu <- DOE$xnamesmenu
      currentDOE$ynamesmenu <- DOE$ynamesmenu
      currentDOE$adapt.visu <- DOE$adapt.visu
      currentDOE$Xinfos <- DOE$Xinfos
      currentDOE$nobs <- sum(idnotna)
      currentDOE$Yinfos <- DOE$Yinfos
      currentDOE$nYsurrogate <- DOE$nYsurrogate
      currentDOE$idon <- DOE$idon
      currentDOE$compositeInfos <- DOE$compositeInfos
      currentDOE$OFtot <- DOE$OFtot
      updateCurrentDOE$launch <- FALSE
  }
  

  # Initialize Q2 tables 
  df1 <- data.frame(Type=c("Lasso","Acosso1","Acosso2","Acosso2","Kriging","Kriging","Kriging","Kriging","Kriging","Kriging","Kriging","Kriging"),
                    Var=c(NA,NA,"All",'Acosso1',"All","All","All","All","Lasso","Lasso","Acosso1","Acosso1"),
                    Trend=c(NA,NA,NA,NA,"Constant","Linear","Lasso","Acosso1","Constant","Lasso","Constant","Acosso1"))
  
  SurrogateEnv <- reactiveValues(
    SurrogateDescrs = NULL, # List of surrogates 
    SurrogateFileName = NULL, # Identifies surrogate in use (prediction, build, update)
  )

  SurrogateDescrs <- function() {
    return(SurrogateEnv$SurrogateDescrs)
  }

  SurrogateNames <- function() {
    choices <- unlist(lapply(seq_len(length(SurrogateDescrs())), function(i) {
      SurrogateDescrs()[[i]]$name
    }))
    return(choices)
  }

  SurrogateWithRefine <- function() {
    SdList <- unlist(lapply(seq_len(length(SurrogateDescrs())), function(i) {
      return(isTRUE(SurrogateDescrs()[[i]]$tags$predict.sd))
    }))
    return(SdList)
  }

  listmodels <- reactiveValues(names_surrogatemodel = NULL,
    models = NULL, tableQ2loo = NULL, tableQ2test = NULL,
    bestQ2loo = NULL, bestQ2test = NULL,
    selected = NULL, trainedModels = NULL, finalpredfun = NULL,
    categorical = NULL, levels.models = NULL, withsdmodels = NULL,
    currentlyUsed = NULL
  )
  

  
  # start afresh when training data changed
  observeEvent(list(currentDOE$XY, currentDOE$xnames, currentDOE$ynames, currentDOE$Xinfos, currentDOE$idon, currentDOE$Yinfos), {

    req(currentDOE$nY > 0)

    nb_usermodels <- length(SurrogateDescrs())
    if(nb_usermodels>0){
      df_user <- data.frame(Type=rep(SurrogateNames(),each=3),
                          Var=rep(c("All", "Lasso", "Acosso1"), nb_usermodels),
                          Trend=rep(c(NA), 3*nb_usermodels))
      df1 <- rbind(df1, df_user)
    }

    nb_models <- nrow(df1)
    if (use_simulator() | length(listmodels$selected$id)==0 | length(listmodels$selected$id)==currentDOE$nY){
      df2 <- data.frame(matrix(NA,nb_models,currentDOE$nY))
      colnames(df2) <- currentDOE$ynamesvisu
      tableQ2init <- cbind(df1,df2)
      listmodels$names_surrogatemodel <- BUILTIN_SURROGATE_NAMES
      
      listmodels$withsdmodels <- rep(TRUE, length(BUILTIN_SURROGATE_NAMES))
                        
      if(nb_usermodels>0){listmodels$names_surrogatemodel <- c(listmodels$names_surrogatemodel, paste(df_user[,1], "with", df_user[,2], "vars", sep=" "))}
      if(nb_usermodels>0){listmodels$withsdmodels <- c(listmodels$withsdmodels, rep(SurrogateWithRefine(),each=3))}


      listmodels$models <- vector('list',nb_models)
      for (i in 1:nb_models){
        listmodels$models[[i]] <- vector('list',currentDOE$nY)
      }
      listmodels$tableQ2loo <- tableQ2init
      listmodels$tableQ2test <- tableQ2init
      listmodels$bestQ2loo <- list(id=rep(NA,currentDOE$nY),Q2=rep(NA,currentDOE$nY))
      listmodels$bestQ2test <- list(id=rep(NA,currentDOE$nY),Q2=rep(NA,currentDOE$nY))
      listmodels$selected <- list(id=rep(NA,currentDOE$nY),Q2=rep(NA,currentDOE$nY))
      listmodels$trainedModels <- NULL
      listmodels$finalpredfun <- NULL
      listmodels$categorical <- which(sapply(currentDOE$Xinfos, function(var){var$type}) == 'categorical')
      listmodels$levels.models <- lapply(listmodels$categorical,
                                         function(i) lapply(currentDOE$Xinfos, function(Xinfos){Xinfos$levels})[[i]])
    }else{
      # New composite (or OF) or delete composite
      if(currentDOE$nY > length(listmodels$selected$id)){
        # New composite (or OF)
        
        for (i in 1:nb_models){
          listmodels$models[[i]] <- c(listmodels$models[[i]], vector('list', 1))
        }
        
        newOutput <- currentDOE$ynamesvisu[!(currentDOE$ynamesvisu %in% colnames(listmodels$tableQ2loo))]
        dfNewOutput <- data.frame(matrix(NA, nb_models, length(newOutput)))
        colnames(dfNewOutput) <- newOutput
        
        listmodels$tableQ2loo <- cbind(listmodels$tableQ2loo, dfNewOutput)
        listmodels$tableQ2test <- cbind(listmodels$tableQ2test, dfNewOutput)
        listmodels$bestQ2loo <- list(id = c(listmodels$bestQ2loo$id, rep(NA, length(newOutput))), Q2 = c(listmodels$bestQ2loo$Q2, rep(NA, length(newOutput))))
        listmodels$bestQ2test <- list(id = c(listmodels$bestQ2test$id, rep(NA, length(newOutput))), Q2 = c(listmodels$bestQ2test$Q2, rep(NA, length(newOutput))))
        listmodels$selected <- list(id = c(listmodels$selected$id, rep(NA, length(newOutput))), Q2 = c(listmodels$selected$Q2, rep(NA, length(newOutput))))
      }else{
        # Deleted composite
        listmodelsNames <- colnames(listmodels$tableQ2loo[-c(1:3)])
        deletedOutputName <- listmodelsNames[!(listmodelsNames %in% currentDOE$ynamesvisu)]
        deletedOutputId <- match(deletedOutputName, listmodelsNames)
        nModels <- length(listmodelsNames)
        
        listmodels$tableQ2loo <- listmodels$tableQ2loo[-match(deletedOutputName, names(listmodels$tableQ2loo))]
        listmodels$tableQ2test <- listmodels$tableQ2test[-match(deletedOutputName, names(listmodels$tableQ2test))]
        listmodels$bestQ2loo <- list(id = listmodels$bestQ2loo$id[-deletedOutputId], Q2 = listmodels$bestQ2loo$Q2[-deletedOutputId])
        listmodels$bestQ2test <- list(id = listmodels$bestQ2test$id[-deletedOutputId], Q2 = listmodels$bestQ2test$Q2[-deletedOutputId])
        listmodels$selected <- list(id = listmodels$selected$id[-deletedOutputId], Q2 = listmodels$selected$Q2[-deletedOutputId])
        listmodels$models <- lapply(listmodels$models, function(m){
          m[-deletedOutputId]
        })
      }
    }
    
  }, priority = 1, ignoreNULL = FALSE)
  
  observeEvent(input$surrogate.viewDOE, {
    req(currentDOE$XY)
    toggleModal(session, "modalViewDOEsurrogate", toggle = "open")
  })
  
  observeEvent(input$open.modalconfirm.refresh, {
    toggleModal(session, "modalconfirm.refresh", toggle = "open")
  })
  
  observeEvent(input$surrogate.refresh, {
    toggleModal(session, "modalconfirm.refresh", toggle = "close")
  })
  
  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "surrogate-currentDOE") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      
      if (!is.null(persistence$loadedStudy$listmodels)) {
        currentlyUsed <- persistence$loadedStudy$listmodels$currentlyUsed

        # If 'currentlyUsed' is not found (loaded study is too old), use values that are probably correct
        if (is.null(currentlyUsed)) {
          trainedModels <- persistence$loadedStudy$listmodels$trainedModels
          if (length(trainedModels) > 0) {
            model <- persistence$loadedStudy$listmodels$models[[trainedModels[1]]][[1]]
            currentlyUsed <- seq_along(model$yloo)
          }
          else {
            idnotna <- !apply(is.na(DOE$Y), 1, all)
            currentlyUsed <- which(idnotna)
          }
        }
        listmodels$currentlyUsed <- currentlyUsed

        currentDOE$Xopt <- DOE$X[currentlyUsed,,drop=F]
        currentDOE$XY <- DOE$XY[currentlyUsed,,drop=F]
        currentDOE$X <- currentDOE$Xopt
        currentDOE$Y <- DOE$Y[currentlyUsed,,drop=F]
        currentDOE$nobs <- length(currentlyUsed)
        currentDOE$nX <- DOE$nX
        currentDOE$nY <- DOE$nY
        currentDOE$xnames <- DOE$xnames
        currentDOE$ynames <- DOE$ynames
        currentDOE$xnamesvisu <- DOE$xnamesvisu
        currentDOE$ynamesvisu <- DOE$ynamesvisu
        currentDOE$xnamesmenu <- DOE$xnamesmenu
        currentDOE$ynamesmenu <- DOE$ynamesmenu
        currentDOE$adapt.visu <- DOE$adapt.visu
        currentDOE$Xinfos <- DOE$Xinfos
        currentDOE$Yinfos <- DOE$Yinfos
        currentDOE$nYsurrogate <- DOE$nYsurrogate
        currentDOE$idon <- DOE$idon
        currentDOE$compositeInfos <- DOE$compositeInfos
        currentDOE$OFtot <- DOE$OFtot
        updateCurrentDOE$launch <- FALSE
        updateCurrentDOE$initial <- FALSE
      }

      progressToNextStep(persistence)
    }
    else if (persistence$updatingStep == "surrogate-listmodel") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))
      
      if (!is.null(persistence$loadedStudy$listmodels)) {
        # Check if all selected surrogate model names are known (some user defined models can be unknown)
        unkwownNames <- c()
        for (index in persistence$loadedStudy$listmodels$selected$id) {
          if (!is.na(index)) {
            fixedIndex <- which(persistence$loadedStudy$listmodels$names_surrogatemodel[index] == listmodels$names_surrogatemodel)
            if (length(fixedIndex) == 0) {
              unkwownNames <- append(unkwownNames, persistence$loadedStudy$listmodels$names_surrogatemodel[index])
            }
          }
        }

        # If all selected surrogate model names are known, update 'listmodels' attributes
        if (length(unkwownNames) == 0) {
          # Update 'listmodels$models', 'listmodels$tableQ2loo' and 'listmodels$tableQ2test'
          for(i in seq_along(persistence$loadedStudy$listmodels$names_surrogatemodel)) {
            name <- persistence$loadedStudy$listmodels$names_surrogatemodel[[i]]
            index <- which(name == listmodels$names_surrogatemodel)
            if (length(index) != 0) {
              listmodels$models[[index]] <- persistence$loadedStudy$listmodels$models[[i]]
              listmodels$tableQ2loo[index,] <- persistence$loadedStudy$listmodels$tableQ2loo[i,]
              listmodels$tableQ2test[index,] <- persistence$loadedStudy$listmodels$tableQ2test[i,]
            }
          }

          # Update 'listmodels$bestQ2loo'
          listmodels$bestQ2loo$id <- unlist(lapply(persistence$loadedStudy$listmodels$bestQ2loo$id, function(id)
            ifelse(is.na(id), id, which(persistence$loadedStudy$listmodels$names_surrogatemodel[id] == listmodels$names_surrogatemodel))
          ))
          listmodels$bestQ2loo$Q2 <- persistence$loadedStudy$listmodels$bestQ2loo$Q2

          # Update 'listmodels$bestQ2test'
          listmodels$bestQ2test$id <- unlist(lapply(persistence$loadedStudy$listmodels$bestQ2test$id, function(id)
            ifelse(is.na(id), id, which(persistence$loadedStudy$listmodels$names_surrogatemodel[id] == listmodels$names_surrogatemodel))
          ))
          listmodels$bestQ2test$Q2 <- persistence$loadedStudy$listmodels$bestQ2test$Q2

          # Update 'listmodels$selected'
          listmodels$selected$id <- unlist(lapply(persistence$loadedStudy$listmodels$selected$id, function(id) 
            ifelse(is.na(id), id, which(persistence$loadedStudy$listmodels$names_surrogatemodel[id] == listmodels$names_surrogatemodel))
          ))
          listmodels$selected$Q2 <- persistence$loadedStudy$listmodels$selected$Q2

          # Update 'listmodels$trainedModels'
          listmodels$trainedModels <- unlist(lapply(persistence$loadedStudy$listmodels$trainedModels, function(id)
            which(persistence$loadedStudy$listmodels$names_surrogatemodel[id] == listmodels$names_surrogatemodel)
          ))

          listmodels$categorical <- persistence$loadedStudy$listmodels$categorical
          listmodels$levels.models <- persistence$loadedStudy$listmodels$levels.models

          listmodels <- updateFinalpredfun(listmodels)
        }
        else {
          persistence$report <- c(persistence$report, paste("Failed to find some surrogate model names:", unique(unkwownNames)))
        }
      }

      progressToNextStep(persistence)
    }
    
  }, priority = -1) # Reduce priority to execute later than OF updating
  
  output$DTcurrentDOEsurrogate <- DT::renderDataTable({
    req(currentDOE$XY)
    dimd <- ncol(currentDOE$XY)
    DT::datatable(
      currentDOE$XY,
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip',
        buttons = list(list(extend = 'colvis', columns = 1:dimd)),
        scrollX = TRUE,scrollY = 400,scroller = TRUE,fixedColumns = TRUE
      ))
  })
  
  output$info.text0 <- renderText({
    req(advance.importDOE$total & currentDOE$nobs>0)
    paste0("Total Simulations ",advance.importDOE$total)
  })
  
  output$info.text1 <- renderText({
    req(advance.importDOE$total & currentDOE$nobs>0)
    paste0("Currently Used ",currentDOE$nobs)
  })
  
  
  output$info.text2 <- renderText({
    req(advance.importDOE$total & currentDOE$nobs>0)
    if (sum(advance.importDOE$status == "ended") > currentDOE$nobs){
      font <- 'red'
    }else{
      font <- 'black'
    }
    formatedFont <- sprintf('<font color="%s">%s</font>',font,paste0("Completed Simulations ",sum(advance.importDOE$status == "ended")))
  })
  
  output$infoDOEsurrogate.dynui <- renderUI({
    req(advance.importDOE$total & currentDOE$nobs>0)
    
    uilist <- list()
    uilist[[1]] <- 
      column(3,
             actionButton(ns("surrogate.viewDOE"), label = "View DOE", class = "btn-primary",icon = icon("table"))
      )
    uilist[[2]] <- 
      column(3,
             actionButton(ns("open.modalconfirm.refresh"), label = "Refresh DOE", class = "btn-primary",icon = icon("sync"))
      )
    return(uilist)
  })
  
  buildsurrogate.server("buildsurrogate", currentDOE, listmodels, settings, surrogate.clicked, SurrogateEnv, persistence)
  callModule(choosesurrogate.server, "choosesurrogate", currentDOE, listmodels, settings)
  exportSurrogateServer("export_surrogate", currentDOE, listmodels)
  simulations <- callModule(refinesurrogate.server, "refinesurrogate", currentDOE, listmodels, doeProblemDef, settings)
  
  return(list(listmodels = listmodels, simulations = simulations))
}