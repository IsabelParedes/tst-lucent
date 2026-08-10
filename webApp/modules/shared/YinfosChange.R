#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module YinfosChange
source("modules/shared/dynamicSelectpicker.R", local = TRUE)

get.Yinfos.from.file <- function(datapath, ynames, Yinfos, separator) {
  newYinfos <- Yinfos$Yinfos
  lines <- readLines(datapath)
  namelines <- unlist(strsplit(lines[[1]],separator))
  idlines <- unlist(strsplit(lines[[2]],separator))
  allidstemp <- as.data.frame(matrix(newYinfos$all.ids,nrow=1))
  colnames(allidstemp) <- ynames
  allidstemp[namelines] <- idlines
  all.ids <- matrix(unlist(c(allidstemp)),nrow=1)
  int.ids <- which(all.ids=="Interest")
  control.ids <- which(all.ids=="Control")
  status.ids <- which(all.ids=="Status")
  const.ids <- which(all.ids=="Constant")
  func.ids <- which(all.ids=="Functional")
  surrogate.ids <- c(int.ids, control.ids)
  newYinfos$all.ids <- all.ids
  newYinfos$int.ids <- int.ids
  newYinfos$control.ids <- control.ids
  newYinfos$status.ids <- status.ids
  newYinfos$const.ids <- const.ids
  newYinfos$func.ids <- func.ids
  newYinfos$surrogate.ids <- surrogate.ids
  return(list(Yinfos=newYinfos,nY=length(surrogate.ids)))
}

YinfosChange.ui <- function(id, label = "Change Output Group", width = NULL) {
  ns <- NS(id)
  
  modalContent <- tagList(
    fluidRow(
      column(4, h4("Import Output Group"))
    ),
    fluidRow(
      column(4, radioButtons(ns("separator"), "Separator",
                             choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"))
      ),
      column(7,
             fileInput(ns('file'), 'Select file', accept = c('.txt', '.dat','.csv')),
             tags$script(paste0('$( "#', ns('file'), '" ).on( "click", function() { this.value = null; });'))
      )
    ),
    hr(),
    fluidRow(
      column(4, h4("Manually Change Outputs", div("(please save)", class = "small"))),
      column(4, actionButton(ns("reset"), label = "Reset", class = "btn-primary", icon = icon("sync")))
    ),
    hr(),
    fluidRow(
      column(6,
             h5("Outputs of interest will be available for surrogate modelling and will always be displayed by default in visual explorations.")),
      column(6,
             h5("Control outputs will be available for surrogate modelling but will be hidden by default in visual explorations."))
    ),
    fluidRow(
      column(6,
             uiOutput(ns("choose.out.interest.ui"))),
      column(6,
             uiOutput(ns("choose.out.control.ui")))
    ),
    hr(),
    fluidRow(
      column(6,
             h5("Status outputs only give information about the loaded observations and cannot be approximated with a surrogate.")),
      column(6,
             h5("Constant outputs are outputs with very small variations and will be handled with simple surrogate models automatically without 
                user intervention. By default they will not be displayed in visual explorations."))
    ),
    fluidRow(
      column(6,
             uiOutput(ns("choose.out.status.ui"))),
      column(6,
             uiOutput(ns("choose.out.constant.ui")))
    ),
    hr(),
    fluidRow(
      column(6,
             uiOutput(ns("choose.out.functional.desc.ui"))
             )),
    fluidRow(
      column(6,
             uiOutput(ns("choose.out.functional.ui")))
    ),
    uiOutput(ns("errorSave")),
    hr(),
    uiOutput(ns("footer"))
  )
  
  tagList(
    actionButton(ns("change"), label = label, class = "btn-primary", width = width),
    bsModal(ns("modal"), "Change Output Groups", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}"))))
  )
}

YinfosChange.server <- function(input, output, session, initialYinfos, hideFunctional) {
  
  ns <- session$ns
 
  Yinfos <- reactiveValues(nY = NULL, Yinfos = NULL, ynames = NULL, ynamesvisu = NULL, ynamesmenu = NULL)
  Yinfostemp <- reactiveValues(nY = NULL, Yinfos = NULL)

  # we reinitialize Yinfos and Yinfostemp when initialYinfos has changed
  observe({
    Yinfos$Yinfos <- initialYinfos$Yinfos
    Yinfos$nY <- initialYinfos$nY
    Yinfos$ynames <- initialYinfos$ynames
    Yinfos$ynamesvisu <- initialYinfos$ynamesvisu
    Yinfos$ynamesmenu <- initialYinfos$ynamesmenu
    Yinfostemp$Yinfos <- initialYinfos$Yinfos
    Yinfostemp$nY <- initialYinfos$nY
  })
  
  # We populate the pickers with Yinfos
  choicesY <- reactive({
    req(initialYinfos$ynamesmenu)
    choices <- initialYinfos$ynamesmenu
    
    idComposites <- sapply(initialYinfos$compositeInfos, function(x) x$id)
    
    if(length(idComposites) > 0){
      choices <- initialYinfos$ynamesmenu[-idComposites]
    }
    
    return(choices)
  })
  
  choicesYint <- reactive({
    req(initialYinfos$ynamesmenu)
    
    initialYinfos$ynamesmenu
  })
  
  output$choose.out.interest.ui <- renderUI({
    req(choicesYint(),isolate(Yinfos$Yinfos))
    isolate({
    pickerInput(ns("choose.out.int"), label = "Choose Outputs of Interest", 
                choices = choicesYint(), selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$int.ids], multiple = TRUE,
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3",
                               style = "btn-primary",`live-search` = TRUE))
    })
  })
  output$choose.out.control.ui <- renderUI({
    req(choicesY(),isolate(Yinfos$Yinfos))
    isolate({
    pickerInput(ns("choose.out.control"), label = "Choose Control Outputs", 
                choices = choicesY(), selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$control.ids], multiple = TRUE,
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3",
                               style = "btn-primary",`live-search`= TRUE))
    })
  })
  output$choose.out.status.ui <- renderUI({
    req(choicesY(),isolate(Yinfos$Yinfos))
    isolate({
    pickerInput(ns("choose.out.status"), label = "Choose Status Outputs", 
                choices = choicesY(), selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$status.ids], multiple = TRUE,
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3",
                               style = "btn-primary",`live-search` = TRUE))
    })
  })
  output$choose.out.constant.ui <- renderUI({
    req(choicesY(),isolate(Yinfos$Yinfos))
    isolate({
    pickerInput(ns("choose.out.constant"), label = "Choose Constant Outputs", 
                choices = choicesY(), selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$const.ids], multiple = TRUE,
                options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3",
                               style = "btn-primary",`live-search` = TRUE))
    })
  })
  output$choose.out.functional.ui <- renderUI({
    req(choicesY(),isolate(Yinfos$Yinfos), hideFunctional==FALSE)
    isolate({
      pickerInput(ns("choose.out.functional"), label = "Choose Functional Outputs", 
                  choices = choicesY(), selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$func.ids], multiple = TRUE,
                  options = list(`actions-box` = TRUE,`selected-text-format` = "count > 3",
                                 style = "btn-primary",`live-search` = TRUE))
    })
  })
  
  output$choose.out.functional.desc.ui <- renderUI({
    req(choicesY(),isolate(Yinfos$Yinfos), hideFunctional==FALSE)
    return(h5("Functional outputs are only available for calibration."))
  })
  
  observe({
    # Detect change in outputs of interest
    req(input$choose.out.int)
    new.out <- input$choose.out.int
    isolate({
      # If there is a new output of interest coming from another group, update the selection
      common.control <- intersect(new.out,input$choose.out.control)
      if (length(common.control)>0){
        new.control <- setdiff(input$choose.out.control,common.control)
        updatePickerInput(session, inputId = "choose.out.control", selected = new.control)
      }
      common.status <- intersect(new.out,input$choose.out.status)
      if (length(common.status)>0){
        new.status <- setdiff(input$choose.out.status,common.status)
        updatePickerInput(session, inputId = "choose.out.status", selected = new.status)
      }
      common.constant <- intersect(new.out,input$choose.out.constant)
      if (length(common.constant)>0){
        new.constant <- setdiff(input$choose.out.constant,common.constant)
        updatePickerInput(session, inputId = "choose.out.constant", selected = new.constant)
      }
      common.functional <- intersect(new.out, input$choose.out.functional)
      if (length(common.functional)>0){
        new.functional <- setdiff(input$choose.out.functional, common.functional)
        updatePickerInput(session, inputId = "choose.out.functional", selected = new.functional)
      }
    })
  })
  observe({
    # Detect change in control outputs
    req(input$choose.out.control)
    new.out <- input$choose.out.control
    isolate({
      # If there is a new control output coming from another group, update the selection
      common.int <- intersect(new.out,input$choose.out.int)
      if (length(common.int)>0){
        new.int <- setdiff(input$choose.out.int,common.int)
        updatePickerInput(session, inputId = "choose.out.int", selected = new.int)
      }
      common.status <- intersect(new.out,input$choose.out.status)
      if (length(common.status)>0){
        new.status <- setdiff(input$choose.out.status,common.status)
        updatePickerInput(session, inputId = "choose.out.status", selected = new.status)
      }
      common.constant <- intersect(new.out,input$choose.out.constant)
      if (length(common.constant)>0){
        new.constant <- setdiff(input$choose.out.constant,common.constant)
        updatePickerInput(session, inputId = "choose.out.constant", selected = new.constant)
      }
      common.functional <- intersect(new.out, input$choose.out.functional)
      if (length(common.functional)>0){
        new.functional <- setdiff(input$choose.out.functional, common.functional)
        updatePickerInput(session, inputId = "choose.out.functional", selected = new.functional)
      }
    })
  })
  observe({
    # Detect change in status outputs
    req(input$choose.out.status)
    new.out <- input$choose.out.status
    isolate({
      # If there is a new status output coming from another group, update the selection
      common.int <- intersect(new.out,input$choose.out.int)
      if (length(common.int)>0){
        new.int <- setdiff(input$choose.out.int,common.int)
        updatePickerInput(session, inputId = "choose.out.int", selected = new.int)
      }
      common.control <- intersect(new.out,input$choose.out.control)
      if (length(common.control)>0){
        new.control <- setdiff(input$choose.out.control,common.control)
        updatePickerInput(session, inputId = "choose.out.control", selected = new.control)
      }
      common.constant <- intersect(new.out,input$choose.out.constant)
      if (length(common.constant)>0){
        new.constant <- setdiff(input$choose.out.constant,common.constant)
        updatePickerInput(session, inputId = "choose.out.constant", selected = new.constant)
      }
      common.functional <- intersect(new.out, input$choose.out.functional)
      if (length(common.functional)>0){
        new.functional <- setdiff(input$choose.out.functional, common.functional)
        updatePickerInput(session, inputId = "choose.out.functional", selected = new.functional)
      }
    })
  })
  observe({
    # Detect change in constant outputs
    req(input$choose.out.constant)
    new.out <- input$choose.out.constant
    isolate({
      # If there is a new constant output coming from another group, update the selection
      common.int <- intersect(new.out,input$choose.out.int)
      if (length(common.int)>0){
        new.int <- setdiff(input$choose.out.int,common.int)
        updatePickerInput(session, inputId = "choose.out.int", selected = new.int)
      }
      common.control <- intersect(new.out,input$choose.out.control)
      if (length(common.control)>0){
        new.control <- setdiff(input$choose.out.control,common.control)
        updatePickerInput(session, inputId = "choose.out.control", selected = new.control)
      }
      common.status <- intersect(new.out,input$choose.out.status)
      if (length(common.status)>0){
        new.status <- setdiff(input$choose.out.status,common.status)
        updatePickerInput(session, inputId = "choose.out.status", selected = new.status)
      }
      common.functional <- intersect(new.out, input$choose.out.functional)
      if (length(common.functional)>0){
        new.functional <- setdiff(input$choose.out.functional, common.functional)
        updatePickerInput(session, inputId = "choose.out.functional", selected = new.functional)
      }
    })
  })
  observe({
    # Detect change in functional outputs
    req(input$choose.out.functional)
    new.out <- input$choose.out.functional
    isolate({
      # If there is a new constant output coming from another group, update the selection
      common.int <- intersect(new.out,input$choose.out.int)
      if (length(common.int)>0){
        new.int <- setdiff(input$choose.out.int,common.int)
        updatePickerInput(session, inputId = "choose.out.int", selected = new.int)
      }
      common.control <- intersect(new.out,input$choose.out.control)
      if (length(common.control)>0){
        new.control <- setdiff(input$choose.out.control,common.control)
        updatePickerInput(session, inputId = "choose.out.control", selected = new.control)
      }
      common.status <- intersect(new.out,input$choose.out.status)
      if (length(common.status)>0){
        new.status <- setdiff(input$choose.out.status,common.status)
        updatePickerInput(session, inputId = "choose.out.status", selected = new.status)
      }
      common.constant <- intersect(new.out,input$choose.out.constant)
      if (length(common.constant)>0){
        new.constant <- setdiff(input$choose.out.constant,common.constant)
        updatePickerInput(session, inputId = "choose.out.constant", selected = new.constant)
      }
    })
  })

  # Update Yinfostemp with selected pickers
  observe({
    req(initialYinfos$ynamesmenu, c(input$choose.out.int, input$choose.out.control, input$choose.out.status,
                                    input$choose.out.constant, input$choose.out.functional))
    nYtot <- length(initialYinfos$ynames)
    int.ids <- which(initialYinfos$ynamesmenu %in% input$choose.out.int)
    control.ids <- which(initialYinfos$ynamesmenu %in% input$choose.out.control)
    status.ids <- which(initialYinfos$ynamesmenu %in% input$choose.out.status)
    const.ids <- which(initialYinfos$ynamesmenu %in% input$choose.out.constant)
    func.ids <- which(initialYinfos$ynamesmenu %in% input$choose.out.functional)
    surrogate.ids <- c(int.ids, control.ids)
    type <- initialYinfos$Yinfos$type
    all.ids <- matrix(NA,1,nYtot)
    all.ids[int.ids] <- "Interest"
    all.ids[control.ids] <- "Control"
    all.ids[status.ids] <- "Status"
    all.ids[const.ids] <- "Constant"
    all.ids[func.ids] <- "Functional"
    Yinfos.temp <- list()
    Yinfos.temp$all.ids <- all.ids
    Yinfos.temp$int.ids <- int.ids
    Yinfos.temp$control.ids <- control.ids
    Yinfos.temp$status.ids <- status.ids
    Yinfos.temp$const.ids <- const.ids
    Yinfos.temp$func.ids <- func.ids
    Yinfos.temp$surrogate.ids <- surrogate.ids
    Yinfos.temp$type <- type
    Yinfostemp$Yinfos <- Yinfos.temp
    Yinfostemp$nY <- length(surrogate.ids)
  })
  
  # When user uses a file
  observeEvent(input$file$datapath, {
    newYinfos <- get.Yinfos.from.file(input$file$datapath, initialYinfos$ynames,Yinfostemp,
                                      input$separator)
    if (!is.null(newYinfos$Yinfos)){
      updatePickerInput(session, inputId = "choose.out.int", selected = initialYinfos$ynames[newYinfos$Yinfos$int.ids])
      updatePickerInput(session, inputId = "choose.out.control", selected = initialYinfos$ynames[newYinfos$Yinfos$control.ids])
      updatePickerInput(session, inputId = "choose.out.status", selected = initialYinfos$ynames[newYinfos$Yinfos$status.ids])
      updatePickerInput(session, inputId = "choose.out.constant", selected = initialYinfos$ynames[newYinfos$Yinfos$const.ids])
      updatePickerInput(session, inputId = "choose.out.functional", selected = initialYinfos$ynames[newYinfos$Yinfos$func.ids])
    }
  })
  
  # reinitialize selectors when the user actively reset the Outputs
  observeEvent(input$reset, {
    updatePickerInput(session, inputId = "choose.out.int", selected = initialYinfos$ynamesmenu[initialYinfos$Yinfos$int.ids])
    updatePickerInput(session, inputId = "choose.out.control", selected = initialYinfos$ynamesmenu[initialYinfos$Yinfos$control.ids])
    updatePickerInput(session, inputId = "choose.out.status", selected = initialYinfos$ynamesmenu[initialYinfos$Yinfos$status.ids])
    updatePickerInput(session, inputId = "choose.out.constant", selected = initialYinfos$ynamesmenu[initialYinfos$Yinfos$const.ids])
    updatePickerInput(session, inputId = "choose.out.functional", selected = initialYinfos$ynamesmenu[initialYinfos$Yinfos$func.ids])
  })
  observeEvent(input$change, {
    toggleModal(session, "modal", toggle = "open")
  })
  observeEvent(input$save, {
    
    err <- NULL
    
    if(!is.null(initialYinfos$compositeInfos)){
      usedOutputs <- unlist(sapply(initialYinfos$compositeInfos, 
                            function(x) if (x$modelMode=="Combine") x$usedY))
      
        if (!all(usedOutputs %in% input$choose.out.int)){
          err <- paste("The following outputs are used for composites in combine mode, they must be in Interest Group:", 
                       paste0(usedOutputs, collapse = ", "))
        }
          
    }
    
    if (anyNA(Yinfostemp$Yinfos$all.ids)){
      err <- paste(err, "Make sure to assign a group to each output", sep = "<br>")
    }
    
    if(is.null(err)){
      Yinfos$Yinfos <- Yinfostemp$Yinfos
      Yinfos$nY <- Yinfostemp$nY
      Yinfos$ynames <- initialYinfos$ynames
      Yinfos$ynamesvisu <- initialYinfos$ynamesvisu
      Yinfos$ynamesmenu <- initialYinfos$ynamesmenu
      toggleModal(session, "modal", toggle = "close")
    }
    
    output$errorSave <- renderUI({
      return(h4(HTML(err)))
    })
    
  })
  observeEvent(input$close, {
    updatePickerInput(session, inputId = "choose.out.int", selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$int.ids])
    updatePickerInput(session, inputId = "choose.out.control", selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$control.ids])
    updatePickerInput(session, inputId = "choose.out.status", selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$status.ids])
    updatePickerInput(session, inputId = "choose.out.constant", selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$const.ids])
    updatePickerInput(session, inputId = "choose.out.functional", selected = initialYinfos$ynamesmenu[Yinfos$Yinfos$func.ids])
    toggleModal(session, "modal", toggle = "close")
    output$errorSave <- NULL
  })
  
  output$footer <- renderUI({
    list(
      fluidRow(
        column(3, actionButton(ns("save"), label = "Save and Close", class = "btn-warning",
                               width = '100%'), offset = 2),
        column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                               width = '100%'), offset = 2)
      )
    )
  })
  
  return(Yinfos)
}
