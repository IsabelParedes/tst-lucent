#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

source("modules/saveLoadStudy/saveStudy.R", local = TRUE)

SAVE_MODULE_MIN_VER <- 0.1

PROGRESS_STEPS <- c(
  "defineObjective-clean",
  "generateDOE-nX",
  "generateDOE-xInfos",
  "generateDOE-results",
  "importDOE",
  "uploadDOE",
  "confSimulator-simulatorName",
  "confSimulator-runs",
  "importExperimentalData-menuImport",
  "defineObjective-menuImport",
  "surrogate-currentDOE",
  "surrogate-listmodel",
  "buildsurrogate-surrogateMode",
  "uqParamsChange-uncertaintyDefinition",
  "uqParamsDependence-uncertaintyDefinition",
  "uncertaintyPropagation",
  "sensitivityAnalysis",
  "unconstrained-xinfos",
  "unconstrained-results",
  "constrainedDefine-constrained-initialXinfos",
  "constrainedDefine-constrained-COformulation",
  "constrainedSolve",
  "directOptim-simulator",
  "importExperimentalData-directoptim",
  "defineObjective-directOptim",
  "constrainedDefine-directOptim-initialXinfos",
  "constrainedDefine-directOptim-COformulation",
  "constrainedDefine-directOptim-x0",
  "directOptim-optimArgs",
  "directOptim-results"
)

progressToNextStep <- function(persistence) {
  progressIndex <- which(persistence$updatingStep == persistence$updatingSteps)
  if (length(progressIndex) == 1) {
    if (progressIndex == length(persistence$updatingSteps)) {
      persistence$updatingStep <- "OFF"
    }
    else {
      persistence$updatingStep <- persistence$updatingSteps[progressIndex + 1]
    }
  }
  else if (length(progressIndex) == 0) {
    logger$print(paste0(
      "Unknown updating step: '", persistence$updatingStep, "'. Known: ",
      paste(persistence$updatingSteps, collapse = ",")
    ))
  }
  else if (length(progressIndex) > 1) {
    logger$print(paste0(
      "Several updating steps have the same name: '", persistence$updatingStep, "' among: ",
      paste(persistence$updatingSteps, collapse = ",")
    ))
  }
}

loadStudyUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    useShinyFeedback(),
    radioGroupButtons(
      inputId = ns("loadMethod"),
      choiceNames = c("Load from app", "Upload file"),
      choiceValues = c("app", "file"),
      justified = TRUE,
      size = "sm",
      status = "primary"
    ),
    
    hidden(tags$div(id=ns("fromApp"),
                    fluidRow(
                      column(8, pickerInput(inputId = ns("selectedStudy"),
                                            label = "Select study",
                                            choices = filesInfo(TRUE)$names,
                                            options = pickerOptions(
                                              style = "btn-primary"
                                            ),
                                            width = "100%")),
                      column(4, style = "padding-top: 25px;", 
                             loadingButton(ns("loadFromApp"), 
                                           "Load", 
                                           loadingSpinner = "circle-notch",
                                           loadingLabel = "Please wait...",
                                           style = "width: 100%;"))
                    ),
                    hidden(
                      tags$div(
                        id=ns("showAutoSavedDiv"),
                        fluidRow(
                          column(12, checkboxInput(ns("showAutoSaved"), "Show automatically saved studies", value = FALSE))
                        )
                      )
                    ))
    ),
    
    hidden(
      tags$div(id=ns("fromFile"),
               
               fluidRow(
                 column(8, fileInput(ns("uploadedFile"), 
                                     "Upload study", 
                                     accept = ".bz2")),
                 column(4, style = "padding-top: 25px;", 
                        disabled(
                          loadingButton(ns("loadFromFile"), 
                                        "Load", 
                                        loadingSpinner = "circle-notch",
                                        loadingLabel = "Please wait...",
                                        style = "width: 100%;")))
               )
               
               
      )
    
    
    )
  )
  
}


loadStudyServer <- function(id, persistence) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$loadMethod, {
        
        if (persistence$autoSavingEnabled) {
          shinyjs::show("showAutoSavedDiv")
        }
        if (input$loadMethod=="file"){
          showElement("fromFile", time = 0.5, anim = TRUE, animType = "slide")
          hideElement("fromApp", time = 0.5, anim = TRUE, animType = "slide")
          
        }else{
          hideElement("fromFile", time = 0.5, anim = TRUE, animType = "slide")
          showElement("fromApp", time = 0.5, anim = TRUE, animType = "slide")
          
        }
      })
      
      refhook <- function(e){
        if (startsWith(e, "serializedPy")) {
          serializedPyFile <- tempfile("serializedPy")
          serializedPy <- substr(e, nchar("serializedPy") + 1, nchar(e))
          writeBin(base64decode(serializedPy), serializedPyFile)
          return(reticulate::py_load_object(serializedPyFile))
        }
        else if (e == "environment") {
          return(globalenv())
        }
        return(NULL)
      }  

      observeEvent(input$showAutoSaved, {
        updatePickerInput(
          session = session,
          inputId = "selectedStudy",
          choices = filesInfo(input$showAutoSaved)$names
        )
      })

      observeEvent(input$loadFromApp, {
        
        hideFeedback("selectedStudy")
        savedStudies <- filesInfo(input$showAutoSaved)
        path <- savedStudies[savedStudies$names==input$selectedStudy, "path"]
        path <- ifelse(length(path)>0, path, "")
        
        if (file.exists(path)){
          showFeedbackSuccess(
            inputId = "selectedStudy",
            text = "Study found!"
          )

          if (grepl('RDS.bz2$', path)) {
            study <- readRDS(bzfile(path), refhook = refhook)
          }
          else {
            load(bzfile(path)) #variable name is "study"
          }
          
          if (study$version < SAVE_MODULE_MIN_VER){
            hideFeedback("selectedStudy")
            showFeedbackDanger(
              inputId = "selectedStudy",
              text = paste0("File version must be >= ", SAVE_MODULE_MIN_VER, ", yours is ", study$version)
            )
          }else{
            persistence$loadedStudy <- study
            persistence$updatingSteps <- PROGRESS_STEPS
            persistence$updatingStep <- PROGRESS_STEPS[1]
            persistence$report <- c()

            curAutoSavingFile <- file.path(studiesDir, paste0(persistence$autoSavingFileName, ".RDS.bz2"), fsep = .Platform$file.sep)
            if (file.exists(curAutoSavingFile)) {
              unlink(curAutoSavingFile)
            }
            fileName <- gsub("\\.RDS\\.bz2|\\.RData\\.bz2", "", savedStudies[savedStudies$names==input$selectedStudy, "names"])
            if (!startsWith(fileName, "autosaving")) {
              persistence$autoSavingFileName <- autoSavingFileName(persistence$autoSavingId, fileName)
            }
          }
          
          resetLoadingButton("loadFromApp")
          
        }else{
          
          updatePickerInput(
            session = session,
            inputId = "selectedStudy",
            choices = filesInfo(input$showAutoSaved)$names
          )
          
          showFeedback(
            inputId = "selectedStudy",
            text = "Study not found, the list has been updated. Please try again"
          )
          
          resetLoadingButton("loadFromApp")
        }
      })
      

      observeEvent(input$loadFromFile, {
        path <- input$uploadedFile$datapath
        
        study <- tryCatch({
          readRDS(bzfile(path), refhook = refhook)
        },
        error = function(err_msg){
          message(paste("Failed:", err_msg))
        })
        
        # If 'RDS' loading failed, try 'RData' loading
        out <- "study"
        if (is.null(study)) {
          out <- tryCatch({
            load(bzfile(path)) #variable name is "study"
          },
          error = function(err_msg){
            message(paste("Failed:", err_msg))
          })
        }

        hideFeedback("uploadedFile")
        if (is.null(study) && is.null(out)){
          showFeedbackDanger(
            inputId = "uploadedFile",
            text = "Unable to load file!"
          )
        }else if (out != "study"){
          showFeedbackDanger(
            inputId = "uploadedFile",
            text = "File not supported!"
          )
        }else if (study$version < SAVE_MODULE_MIN_VER){
          showFeedbackDanger(
            inputId = "uploadedFile",
            text = paste0("File version must be >= ", SAVE_MODULE_MIN_VER, ", yours is ", study$version)
          )
        }else{
          persistence$loadedStudy <- study
          persistence$updatingSteps <- PROGRESS_STEPS
          persistence$updatingStep <- PROGRESS_STEPS[1]
          persistence$report <- c()

          curAutoSavingFile <- file.path(studiesDir, paste0(persistence$autoSavingFileName, ".RDS.bz2"), fsep = .Platform$file.sep)
          if (file.exists(curAutoSavingFile)) {
            unlink(curAutoSavingFile)
          }
          if (!startsWith(input$uploadedFile$name, "autosaving")) {
            fileName <- gsub("\\.RDS\\.bz2|\\.RData\\.bz2", "", input$uploadedFile$name)
            persistence$autoSavingFileName <- autoSavingFileName(persistence$autoSavingId, fileName)
          }
        }
        
        resetLoadingButton("loadFromFile")
      })
      
      observeEvent(persistence$updatingStep, {
        if (!is.null(persistence$loadedStudy)) {
          if (persistence$updatingStep == "OFF") {
            html("selectedStudy-text", "")
            html("uploadedFile_progress", "")
          }
          else {
            html("selectedStudy-text", paste("Updating", persistence$updatingStep, "..."))
            html("uploadedFile_progress", paste("Updating", persistence$updatingStep, "..."))
          }
        }
      })
      
      observeEvent(persistence$showReport, {
        if (persistence$showReport) {
          showModal(modalDialog(HTML(paste(persistence$report, collapse = '<br/>')), title = "Loading Report", size = 'l'))
          persistence$showReport <- FALSE
        }
      })
      
      observeEvent(input$uploadedFile, {
        
        ext <- tools::file_ext(input$uploadedFile$datapath)
        hideFeedback("uploadedFile")
        
        if (ext == "bz2"){
          
          showFeedbackSuccess(
            inputId = "uploadedFile",
            text = "Upload complete, click on load"
          )
          shinyjs::enable("loadFromFile")
          
        }else{
          
          showFeedbackDanger(
            inputId = "uploadedFile",
            text = "Please upload a bz2 file"
          )
          
          shinyjs::disable("loadFromFile")
        }
      })
      
      return(persistence)
    }
  )
}