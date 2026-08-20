#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

SAVE_MODULE_VER <- 0.2
studiesDir <- if (nchar(Sys.getenv("APPDATA")) == 0) "saved_studies" else file.path(Sys.getenv("APPDATA"), "LAGUN", "saved_studies")
MAX_SIZE_MB <- 50
choicesToSave <- c("DOE", "Models", "Uncertainty Quantification", 
                   "Sensitivity Analysis", "Unconstrained Optimization" , 
                   "Constrained Optimization", "Direct Optimization")
filesInfo <- function(showAutoSaved){
  files <- list.files(path=studiesDir, 
                      pattern=".*.RData.bz2$|.*.RDS.bz2$", 
                      full.names=TRUE)

  files <- Filter(function(file) {
    return(showAutoSaved || !grepl("saved_studies/autosaving", file))
  }, files)

  infos <- file.info(files)[, c("size", "mtime")]
  infos <- tibble::rownames_to_column(infos, "path")
  infos["names"] <- tools::file_path_sans_ext(basename(infos$path), 
                                              compression = TRUE)
  infos["size_raw"] <- infos$size
  infos["size"] <- bytesToHumanSize(infos$size)
  DateTimeOrder <- order(timeDate::as.timeDate(infos$mtime), decreasing = TRUE)
  return(infos[DateTimeOrder, ])
  
}

autoSavingFileName <- function(autoSavingId, studyFileName = NULL) {
  if (length(studyFileName) == 0) {
    paste("autosaving", autoSavingId, sep = "-")
  }
  else {
    paste("autosaving", autoSavingId, studyFileName, sep = "-")
  }
}

checkAutoSavedLimit <- function() {
  files <- list.files(path=studiesDir, 
                      pattern=".*.RData.bz2$|.*.RDS.bz2$", 
                      full.names=TRUE)

  files <- Filter(function(file) {
    return(grepl("saved_studies/autosaving", file))
  }, files)

  infos <- file.info(files)[, c("size", "mtime")]
  dateTimeOrder <- order(timeDate::as.timeDate(infos$mtime), decreasing = TRUE)
  orderedInfos <- infos[dateTimeOrder, ]
  sizeCumsum <- cumsum(orderedInfos[["size"]])
  toRemove <- rownames(orderedInfos)[sizeCumsum > (MAX_SIZE_MB * 1024 * 1024)]

  if (length(toRemove) != 0) {
    logger$print(paste(length(toRemove), "log file(s) to remove:", toString(toRemove)))
    unlink(toRemove)
  }
}

refhook <- function(e){
  if (reticulate::is_py_object(e)) {
    serializedPyFile <- tempfile("serializedPy")
    reticulate::py_save_object(e, serializedPyFile)
    serializedPy <- readBin(serializedPyFile, "raw", 10e6)
    return(paste0("serializedPy", base64encode(serializedPy)))
  }
  else if (typeof(e) == "environment") {
    return("environment")
  }
  return(NULL)
}  

saveData <- function(fileName, study) {
  fileName <- paste0(fileName, ".RDS.bz2")
  
  if (!file.exists(studiesDir)) {
    dir.create(studiesDir, recursive = T)
  }
  
  saveRDS(study, 
       file = file.path(studiesDir, fileName, fsep = .Platform$file.sep),
       refhook = refhook,
       compress = "bzip2")
  
}

bytesToHumanSize <- function(sizes){
  units <- c("B", "kB", "MB", "GB")
  exponents <- floor(log(sizes, base=1000))
  exponents[exponents > 3] <- 3
  sizes / 1000^exponents
  humanSizes <- paste(signif(sizes / 1000^exponents, 3), units[exponents+1])
  return(humanSizes)
}

buildSaveList <- function(eltsToSave, DOEX, DOE.manual, DOE, doeProblemDef, calibration, listmodels, sensitivityAnalysis,
                          UQparams, UQres, UQproba, unconstrOptim, constrOptim, directOptim) {
  calib <- NULL
  lm <- NULL
  uq <- list(UQparams = NULL,
             UQres = NULL,
             UQproba = NULL)
  sa <- NULL
  optim <- list(unconstrained = list(resoptim = NULL,
                                     Xinfos = NULL),
                constrained = list(COformulation = NULL,
                                   resCoptim = NULL,
                                   Xinfos = NULL)
                )
  directOptimConf <- list(
    COformulation = NULL,
    Xinfos = NULL,
    initialXVal = NULL,
    calibration = NULL
  )
    
  if (!is.null(calibration)) {
    calib <- reactiveValuesToList(calibration)
  }
  
  if ("Models" %in% eltsToSave){
    if (!all(is.na(listmodels$selected$id))){
      lm <- reactiveValuesToList(listmodels)
      lm <- lm[names(lm) != "finalpredfun"]
    }
  }
  
  
  if ("Uncertainty Quantification" %in% eltsToSave){
    uq$UQparams <- reactiveValuesToList(UQparams)
    
    if(!is.null(UQres$nsample)){
      uq$UQres <- reactiveValuesToList(UQres)
    }
    
    if(!is.null(UQproba$nsample)){
      uq$UQproba <- reactiveValuesToList(UQproba)
    }
    
  }
  
  
  if ("Sensitivity Analysis" %in% eltsToSave){
    uq$UQparams <- reactiveValuesToList(UQparams)
    sa <- lapply(sensitivityAnalysis, reactiveValuesToList)
  }
  
  
  if ("Unconstrained Optimization" %in% eltsToSave){
    optim$unconstrained$resoptim <- reactiveValuesToList(unconstrOptim$resoptim)
    optim$unconstrained$Xinfos <- unconstrOptim$Xinfos$Xinfos
  }
  
  if ("Constrained Optimization" %in% eltsToSave){
    optim$constrained$COformulation <- reactiveValuesToList(constrOptim$COformulation)
    optim$constrained$resCoptim <- reactiveValuesToList(constrOptim$resCoptim)
    optim$constrained$Xinfos <- constrOptim$Xinfos$Xinfos
  }
  
  directOptimConf <- reactiveValuesToList(directOptim)
  
  return(list(version = SAVE_MODULE_VER,
              DOEX = reactiveValuesToList(DOEX),
              DOE.manual = reactiveValuesToList(DOE.manual),
              DOE = reactiveValuesToList(DOE),
              doeProblemDef = reactiveValuesToList(doeProblemDef),
              calibration = calib,
              listmodels = lm,
              UQparams = uq$UQparams,
              UQres = uq$UQres,
              UQproba = uq$UQproba,
              sensitivityAnalysis = sa,
              unconstrOptim = optim$unconstrained,
              constrOptim = optim$constrained,
              directOptim = directOptimConf
              ))
}

saveStudyUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    useShinyjs(),
    useShinyFeedback(),
    
    tags$style(
      HTML(paste0("
      
            #savedStudy {
              width: 70%;
              margin: 0 auto;
              min-height: 50px;
              line-height: 40px;
              border-top: 1px lightgray solid;
              padding-top: 5px;
              padding-bottom: 5px;
              padding-left: 15px;
              transition: min-height .2s ease-in-out;
            }
            
            #toast-container.toast-top-center-custom {
              top: 5%;
              margin-left: 45%;
            }
            
           "))
    ),
    
    tagList(
      h2(paste0("Save current study (v", SAVE_MODULE_VER, ")")),
      hr(),
      br(),
      tags$div(
        id=ns("saveAvailable"),
        
        checkboxGroupButtons(
          inputId = ns("elementsToSave"),
          label = "Choose elements to save", 
          choices = choicesToSave,
          individual = TRUE,
          status = "primary",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")),
          disabled = TRUE
        ),
        prettyRadioButtons(
          inputId = ns("saveOrDownload"),
          label = "Save or download?", 
          choices = c("Save in app", "Download"),
          inline = TRUE, 
          status = "primary",
          fill = TRUE
        ),
        fluidRow(
          column(3, disabled(textInput(ns("filename"), "Enter Filename",
                                       width = "100%"))),
          column(1, style = "padding-top: 25px;", 
                 disabled(loadingButton(ns("save"), 
                                        "Save", 
                                        loadingSpinner = "circle-notch",
                                        loadingLabel = "Please wait...")),
                 hidden(disabled(downloadButton(ns("download"))))),
          
        )
      ),
      hidden(tags$div(id=ns("saveNotAvailable"), h4("Save is not available for functional outputs.")))
      
    ),
    br(),
    uiOutput(ns("savedStudies")) %>% withSpinner(type = 3, 
                                                 color.background = "#FFFFFF")
  )
  
}


saveStudyServer <- function(id, persistence, DOEX, DOE.manual, DOE, doeProblemDef, calibration, listmodels, sensitivityAnalysis, UQparams,
                            UQres, UQproba, unconstrOptim, constrOptim, directOptim) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      showAutoSaved <- reactiveVal(FALSE)
      savedStudies <- reactiveVal(filesInfo(FALSE))
      choicesToDisable <- reactiveVal(choicesToSave)

      observeEvent(persistence$autoSavingCount, {
        req(persistence$autoSavingCount != 0, persistence$autoSavingEnabled)
        logger$print(paste("autoSavingCount in", persistence$autoSavingFileName, "caller:", persistence$autoSavingCaller))
        
        tryCatch({
          saveList <- buildSaveList(choicesToSave, DOEX, DOE.manual, DOE, doeProblemDef, calibration, listmodels, 
                                    sensitivityAnalysis, UQparams, 
                                    UQres, UQproba, unconstrOptim, constrOptim, directOptim)
          saveData(persistence$autoSavingFileName, saveList)
          savedStudies(filesInfo(showAutoSaved()))
          checkAutoSavedLimit()
        },
        error = function(err) {
            logger$print("autoSaving failed")
            logger$print(err)
        })
      }, priority = -1)

      observeEvent(DOE$XY, {
        shinyjs::enable("filename")
        shinyjs::enable("save")
        shinyjs::enable("download")
        
        choicesToDisable(choicesToDisable()[choicesToDisable() != "DOE"])
        selection <- unique(c("DOE", input$elementsToSave))
        
        updateCheckboxGroupButtons(session, 
                                   "elementsToSave", 
                                   selected = selection,
                                   disabledChoices = choicesToDisable(),
                                   disabled = FALSE)
        updateTextInput(session, "filename", 
                        value = paste0("study_", format(Sys.Date(), "%Y%m%d")))
      })
      
      observeEvent(doeProblemDef$choice, {
        req(doeProblemDef$choice == 4)

        shinyjs::enable("filename")
        shinyjs::enable("save")
        shinyjs::enable("download")
        
        choicesToDisable(choicesToDisable()[choicesToDisable() != "Direct Optimization"])
        selection <- unique(c("Direct Optimization", input$elementsToSave))
        
        updateCheckboxGroupButtons(session, 
                                   "elementsToSave", 
                                   selected = selection,
                                   disabledChoices = choicesToDisable(),
                                   disabled = FALSE)
        updateTextInput(session, "filename", 
                        value = paste0("study_", format(Sys.Date(), "%Y%m%d")))
      })
      
      observeEvent(listmodels$selected$id, {
        
        req(listmodels$selected$id)
        
        choicesToDisable(choicesToDisable()[!choicesToDisable() %in% c("Models", "Uncertainty Quantification")])
        selection <- unique(c("DOE", "Models", "Uncertainty Quantification", input$elementsToSave))
        
        updateCheckboxGroupButtons(session, 
                                   "elementsToSave", 
                                   selected = selection,
                                   disabledChoices = choicesToDisable())
      })
      
      
      observeEvent(list(sensitivityAnalysis$GSAres$S1, 
                        sensitivityAnalysis$Shapleyres$S,
                        sensitivityAnalysis$GSAOptimres$S1), {
                          
        choicesToDisable(choicesToDisable()[choicesToDisable() != "Sensitivity Analysis"])
        selection <- unique(c("DOE", "Models", "Uncertainty Quantification", 
                              "Sensitivity Analysis", input$elementsToSave)) 
        
        updateCheckboxGroupButtons(session, 
                                   "elementsToSave", 
                                   selected = selection, 
                                   disabledChoices = choicesToDisable())
      }, ignoreInit = TRUE)
      
      
      observeEvent(req(unconstrOptim$resoptim$yname1), {
        choicesToDisable(choicesToDisable()[choicesToDisable() != "Unconstrained Optimization"])
        selection <- unique(c("DOE", "Models", "Unconstrained Optimization",
                              input$elementsToSave)) 
        updateCheckboxGroupButtons(session, 
                                   "elementsToSave", 
                                   selected = selection,
                                   disabledChoices = choicesToDisable())
      })
      
      
      observeEvent(req(constrOptim$COformulation$idC), {
        choicesToDisable(choicesToDisable()[choicesToDisable() != "Constrained Optimization"])
        selection <- unique(c("DOE", "Models", "Constrained Optimization",
                              input$elementsToSave)) 
        updateCheckboxGroupButtons(session, 
                                   "elementsToSave", 
                                   selected = selection,
                                   disabledChoices = choicesToDisable())
      })
      
      
      observeEvent(input$saveOrDownload, {
        if(input$saveOrDownload == "Download"){
          hideElement("save", time = 0.2, anim = FALSE, animType = "fade")
          showElement("download", time = 0.25, anim = TRUE, animType = "fade")
        }else{
          hideElement("download", time = 0.2, anim = FALSE, animType = "fade")
          showElement("save", time = 0.25, anim = TRUE, animType = "fade")
        }
      })
      
      observeEvent(input$elementsToSave, {
        eltsToSave <- input$elementsToSave
        if (is.null(eltsToSave)){
          shinyjs::disable("save")
          shinyjs::disable("download")
        }else{
          
          if (!("DOE" %in% eltsToSave)){
            updateCheckboxGroupButtons(session, 
                                       "elementsToSave", 
                                       selected = c("DOE", eltsToSave),
                                       disabledChoices = choicesToDisable())
          }
          
          
          needModel <- c("Uncertainty Quantification", "Sensitivity Analysis", 
                         "Unconstrained Optimization", "Constrained Optimization")
          
          if (any(needModel %in% eltsToSave) &
              !("Models" %in% eltsToSave)){
            updateCheckboxGroupButtons(session, 
                                       "elementsToSave", 
                                       selected = c("Models", eltsToSave),
                                       disabledChoices = choicesToDisable())
          }
          
          
          if (input$filename != ""){
            shinyjs::enable("save")
            shinyjs::enable("download")
          }
        }
        
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      observeEvent(list(input$filename, savedStudies(), input$saveOrDownload), {
        req(DOE$XY)
        shinyjs::enable("save")
        shinyjs::enable("download")
        hideFeedback("filename")
        
        
        if (input$filename==""){
          shinyjs::disable("save")
          shinyjs::disable("download")
          showFeedbackDanger(
            inputId = "filename",
            text = "A name must be provided!"
          )
        }else if (input$filename %in% filesInfo(TRUE)$names 
                  & input$saveOrDownload == "Save in app"){
          showFeedbackWarning(
            inputId = "filename",
            text = "Filename already used, the existing file will be overwritten!"
          )
        }
      })
      
      
      observeEvent(input$save, {
        eltsToSave <- input$elementsToSave
        
        studiesDirSize <- signif(sum(filesInfo(TRUE)$size_raw)/10**6, digits = 3)
        
        showToast(
          type="info",
          message="Saving the file",
          title = "Just a moment...",
          .options = list(positionClass = "toast-top-center-custom"),
          keepVisible = TRUE,
          session = session
        )
        
        if (MAX_SIZE_MB > studiesDirSize){
          
          tryCatch({
            saveList <- buildSaveList(eltsToSave, DOEX, DOE.manual, DOE, doeProblemDef, calibration, listmodels, 
                                      sensitivityAnalysis, UQparams, 
                                      UQres, UQproba, unconstrOptim, constrOptim, directOptim)
            saveData(input$filename, saveList)

            curAutoSavingFile <- file.path(studiesDir, paste0(persistence$autoSavingFileName, ".RDS.bz2"), fsep = .Platform$file.sep)
            if (file.exists(curAutoSavingFile)) {
              unlink(curAutoSavingFile)
            }
            persistence$autoSavingFileName <- autoSavingFileName(persistence$autoSavingId, input$filename)
            
            resetLoadingButton("save")
            savedStudies(filesInfo(showAutoSaved()))
            
            hideToast(animate = TRUE, session = session)
            showToast(
              type="success",
              message="The project has been successfully saved",
              title = "Success",
              .options = list(positionClass = "toast-top-center-custom"),
              session = session
            )
          },
          error = function(err) {
            errMessage <- paste("An unexpected error occured.", err)
            hideToast(animate = TRUE, session = session)
            showToast(
              type="error",
              message=errMessage,
              title = "Unable to save...",
              .options = list(positionClass = "toast-top-center-custom"),
              keepVisible = TRUE,
              session = session
            )
          })
        }else{
          resetLoadingButton("save")
          errMessage <- paste(MAX_SIZE_MB, 
                              "MB limit exceeded, remove existing files.",
                              "The current space used is", studiesDirSize, "MB")
          hideToast(animate = TRUE, session = session)
          showToast(
            type="error",
            message=errMessage,
            title = "Unable to save...",
            .options = list(positionClass = "toast-top-center-custom"),
            keepVisible = TRUE,
            session = session
          )
        }
      })
      
      output$download <- downloadHandler(
        filename <- function() {
          paste(input$filename, 
                "RDS", 
                "bz2", 
                sep=".")
        },
        content <- function(file) {
          showToast(
            type="info",
            message="Preparing the file for download",
            title = "Just a moment...",
            .options = list(positionClass = "toast-top-center-custom"),
            keepVisible = TRUE,
            session = session
          )
          path <- paste0(tempfile(), ".RDS")
          
          study <- buildSaveList(input$elementsToSave, DOEX, DOE.manual, DOE, doeProblemDef, calibration, listmodels, 
                                 sensitivityAnalysis, UQparams, UQres, UQproba, 
                                 unconstrOptim, constrOptim, directOptim)
          saveRDS(study, 
               file = path,
               refhook = refhook,
               compress = "bzip2")
          file.copy(path, file)
          
          hideToast(animate = TRUE, session = session)
          showToast(
            type="success",
            message="The file is ready for download",
            title = "Success",
            .options = list(positionClass = "toast-top-center-custom"),
            session = session
          )
        }
      )
      
      output$savedStudies <- renderUI({
        
        if(nrow(filesInfo(TRUE))>0){
          tagList(
            br(),
            br(),
            h2("Saved studies"),
            hr(),
            if (persistence$autoSavingEnabled) checkboxInput(ns("showAutoSaved"), "Show automatically saved studies", value = showAutoSaved()) else "",
            br(),
            tagList(
              fluidRow(style = "width: 70%; margin: 0 auto;",
                       column(1, ""),
                       column(3, h4("Filename")),
                       column(2, h4("Size")),
                       column(3, h4("Last modified")),
              )
            ),
            lapply(seq_len(nrow(savedStudies())), function(i){
              div(id = "savedStudy",
                  tagList(
                    fluidRow(
                      column(1,""),
                      column(3, style = "padding-left: 15px;", paste(savedStudies()$names[i])),
                      column(2, style = "padding-left: 10px;", paste(savedStudies()$size[i])),
                      column(3, style = "padding-left: 10px;", paste(savedStudies()$mtime[i])),
                      column(1,
                             downloadButton(ns(paste0("downloadStudy_", i)),
                                            label = NULL, class = "btn-primary", 
                                            style = "width:100%;")),
                      column(1,
                             tagList(
                               actionButton(ns(paste0("deleteStudy_", i)),
                                            label = NULL, icon = icon("trash"),
                                            class = "btn-danger", width = "100%"),
                               hidden(list(
                                 p(id = ns(paste0("confirm", i)), 
                                   HTML("Are you sure?"),
                                   actionButton(ns(paste0("yes", i)), label = "Yes", width = "50%"),
                                   actionButton(ns(paste0("no", i)), label = "No", width = "45%"))
                               ))
                             )
                      )
                    )
                  )
              )
            })
          )
        }
      })
      
      observeEvent(input$showAutoSaved, {
        showAutoSaved(input$showAutoSaved)
        savedStudies(filesInfo(input$showAutoSaved))
      })
      
      delButtons <- reactiveVal(NULL)
      
      observe({
        lapply(seq_len(nrow(savedStudies())), function(i){
          
          downloadId <- paste0("downloadStudy_", i)
          delId <- paste0("deleteStudy_", i)
          delYes <- paste0("yes", i)
          delNo <- paste0("no", i)
          
          
          if (!(delId %in% delButtons())){
            delButtons(c(delButtons(), delId))
            
            observeEvent(input[[delId]], {
              toggleElement(paste0("confirm", i), time = 0.25, anim = TRUE, animType = "slide")
            })
            
            observeEvent(input[[delNo]], {
              hideElement(paste0("confirm", i), time = 0.25, anim = TRUE, animType = "slide")
            })
            
            observeEvent(input[[delYes]], {
              f <- savedStudies()[i,"path"]
              if (file.exists(f)) {
                file.remove(f)
                savedStudies(filesInfo(showAutoSaved()))
                showToast(
                  type="success",
                  message=paste("The file", f,  "has been successfully removed"),
                  title = "Success",
                  .options = list(positionClass = "toast-top-center-custom"),
                  session = session
                )
              }
            })
            
            output[[downloadId]] <- downloadHandler(
              filename <- function() {
                paste(savedStudies()[i, "names"], 
                      "RDS", 
                      "bz2", 
                      sep=".")
              },
              content <- function(file) {
                showToast(
                  type="info",
                  message="Preparing the file for download",
                  title = "Just a moment...",
                  .options = list(positionClass = "toast-top-center-custom"),
                  keepVisible = TRUE,
                  session = session
                )
                f <- savedStudies()[i, "path"]
                if (file.exists(f)) {
                  file.copy(f, file)
                  hideToast(animate = TRUE, session = session)
                  showToast(
                    type="success",
                    message="The file is ready for download",
                    title = "Success",
                    .options = list(positionClass = "toast-top-center-custom"),
                    session = session
                  )
                }
              }
            )
            
          }
        })
      })
      
    }
  )
}