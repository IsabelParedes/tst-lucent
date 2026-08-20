#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module UQparamsDependence




UQparamsDependence.ui <- function(id, label = HTML(paste('Change Uncertainty Definition', '(Dependence)', sep = '<br>'))) {
  ns <- NS(id)
  
  modalContent <- tagList(
    uiOutput(ns("copulaInputs.dynui")) %>% withSpinner(),
    hr(),
    uiOutput(ns("footer"))
  )
  
  tagList(
    fluidRow(
      column(6,
             actionButton(ns("change"), label = label, class = "btn-primary")
      ),
      column(6,
             uiOutput(ns("update.dynui"))
      )
    ),
    bsModal(ns("modal"), "Change Inputs Dependence Definition", NULL, size = "large", modalContent,
            tags$head(tags$style(paste0("#", ns("modal")," .modal-footer{display:none}",
                                        " .modal-lg{width: 70%}"))))
  )
}

UQparamsDependence.server <- function(input, output, session, initiallistCopulas, fitdistUQparams, DOE, persistence) {
  
  ns <- session$ns
  
  listCopulasTemp <- reactiveValues(listCopulas=NULL)
  listCopulas <- reactiveValues(listCopulas=NULL)
  
  boundsParamCopula <- reactiveValues(min = NULL, max = NULL)

  # we reinitialize listCopulasTemp and listCopulas when initiallistCopulas has changed
  observeEvent(initiallistCopulas$listCopulas, {
    listCopulasTemp$listCopulas <- initiallistCopulas$listCopulas
    listCopulas$listCopulas <- initiallistCopulas$listCopulas
  })
  
  output$update.dynui <- renderUI({
    req(fitdistUQparams$listCopulas)
    if (fitdistUQparams$selection.copulas=="confirmed"){
      actionButton(ns("update"), label = HTML(paste('Update', 'with fitted copulas', sep = '<br>')), class = "btn-warning")
    }else{
      NULL
    }
  })
  
  # we reinitialize listCopulasTemp and listCopulas when initiallistCopulas when fitdistUQparams is updated
  observeEvent(input$update, {
    listCopulasTemp$listCopulas <- fitdistUQparams$listCopulas
    listCopulas$listCopulas <- fitdistUQparams$listCopulas
  })
  
  observeEvent(input$change, {
    if (length(Xnum())>1){
      toggleModal(session, "modal", toggle = "open")
    }else{
      showModal(modalDialog(HTML(
        "Less than 2 numeric inputs, cannot define a dependence structure.")
        , title = "Warning !")
      )
    }
  })
  
  Xnum <- reactive({
    req(DOE$Xinfos)
    num <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'numeric')
    return(DOE$xnamesmenu[num])
  })
  
  counterpanel <- reactiveValues(count=0,finished=FALSE,inputs=NULL)
  
  output$copulaInputs.dynui <- renderUI({
    req(counterpanel$count,length(Xnum())>1)
    if (counterpanel$finished){
      # Last panel to save the groups definition
      text <- paste0("Type of dependence: ",input$choiceDependence)
      if (input$choiceDependence=="Groups Known"){
        text <- paste0(text," (",input$ngroups," groups)")
      }
      t <- tagList(
        h4("You have finished the groups and copulas definition. Please click on Save 
         to confirm your choice or Dismiss to delete it."),
        br(),
        h5(text),
        br()
      )
    }else{
      if (counterpanel$count == 0){
        # Introduction panel to set type of dependence
        t <- tagList(
          h4("Please choose the type of dependence for the inputs"),
          h5("A group is a subset of numeric inputs which are dependent. 
                  For each group you will have to define the inputs composing it in the following panels."),
          br(),
          fluidRow(
            column(12,
                   radioGroupButtons(
                     inputId = ns("choiceDependence"),
                     label = "Choose Dependence Type",
                     choices = c("Independent", 
                                 "Groups Known", "Groups Unknown"),
                     status = "primary",
                     individual = TRUE,
                     checkIcon = list(
                       yes = icon("ok", 
                                  lib = "glyphicon"),
                       no = icon("remove",
                                 lib = "glyphicon"))
                   ))
          ),
          br(),
          br()
        )
      }
      if (counterpanel$count == 1){
        # First panel to define the number of groups
        t <- tagList(
          h4("Please select the number of dependent inputs groups"),
          h5("A group is a subset of numeric inputs which are dependent. 
                  For each group you will have to define the inputs composing it in the following panels."),
          br(),
          fluidRow(
            column(4,
                   numericInput(ns("ngroups"), "Number of groups", 1, min = 1, max = floor(length(Xnum())/2))
            ),
            column(8,"")
          ),
          br(),
          br()
        )
      }else{
        if (counterpanel$count > 1){
          # Panel to define a group
          t <- tagList(
            uiOutput(ns("group.dynui"))
          )
        }
      }
    }
    return(t)
  })
  
  output$group.dynui <- renderUI({
    t <- tagList(
      h4(paste0("Please select the dependent inputs belonging to Group "),counterpanel$count-1),
      br(),
      fluidRow(
        column(4,
               pickerInput(
                 inputId = ns("groupinputs"),
                 label = "Select Inputs",
                 multiple = TRUE,
                 choices = counterpanel$inputs,
                 options = list(style = "btn-primary"),
               )),
        column(8,"")
      ),
      br(),
      hr(),
      h4(paste0("Please select the copula for Group "),counterpanel$count-1),
      h5("If you want to estimate it with a dataset select Estimated"),
      fluidRow(
        column(4,
               pickerInput(
                 inputId = ns("groupcopula"),
                 label = "Select Copula",
                 multiple = FALSE,
                 choices = list("Elliptical"=list("Gaussian"),"Archimedean"=list("Clayton","Frank","Gumbel","Joe"),"Other"=list("Estimated")),
                 options = list(style = "btn-primary")),
               column(8,"")
        )
      ),
      uiOutput(ns("paramcopula.dynui"))
    )
    return(t)
  })
  
  output$paramcopula.dynui <- renderUI({
    req(input$groupcopula,input$groupinputs,length(input$groupinputs)>=2)
    if (input$groupcopula != "Estimated"){
      if (input$groupcopula=="Clayton"){
        selp <- 0
        boundsParamCopula$max <- NA
        if (length(input$groupinputs)==2){
          boundsParamCopula$min <- -1
        }else{
          boundsParamCopula$min <- 0
        }
      }
      if (input$groupcopula=="Frank"){
        selp <- 1
        boundsParamCopula$min <- 0
        boundsParamCopula$max <- NA
      }
      if (input$groupcopula=="Gaussian"){
        selp <- 0
        boundsParamCopula$min <- -1
        boundsParamCopula$max <- 1
      }
      if (input$groupcopula%in%c("Gumbel","Joe")){
        selp <- 1
        boundsParamCopula$min <- 1
        boundsParamCopula$max <- NA
      }
      t <- tagList(
        fluidRow(
          column(4,
                 numericInput(ns("paramcopula"), "Copula Parameter", value= selp, min = boundsParamCopula$min, max = boundsParamCopula$max)
                 ,
                 column(8,"")
          )
        )
      )
    }else{
      t <- NULL
    }
    return(t)
  })

  output$footer <- renderUI({
    req(!is.null(counterpanel$finished))
    if (!counterpanel$finished){
      t <- tagList(
        fluidRow(
          column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                                 width = '100%')),
          column(6,""),
          column(3, actionButton(ns("nextpanel"), label = "Next", class = "btn-primary",
                                 width = '100%'))
        )
      )
    }else{
      t <- tagList(
        fluidRow(
          column(3, actionButton(ns("close"), label = "Dismiss", class = "btn-secondary",
                                 width = '100%')),
          column(6,""),
          column(3, actionButton(ns("save"), label = "Save", class = "btn-warning",
                                 width = '100%'))
        )
      )
    }
    return(t)
  })
  
  observeEvent(input$nextpanel, {
    
    if ((counterpanel$count > 1) & (length(input$groupinputs) < 2)){
      showModal(modalDialog(HTML(
        "You must select at least 2 inputs in a group.")
        , title = "Warning !")
      )
    }else{
      validParameter <- TRUE
      if (counterpanel$count == 0){
        # Depending on the type of dependence we skip some panels
        # and set listCopulasTemp
        switch(input$choiceDependence,
               "Independent"={
                 # We reinitialize listCopulasTemp with NULLs
                 listCopulasTemp$listCopulas <- list(inputs=NULL,groups=NULL,unique.groups=NULL,typeCopulas=NULL,Copulas=NULL)
                 counterpanel$finished <- TRUE
               },
               "Groups Unknown"={
                 # All inputs are kept but copulas objects are NULL
                 # (will be estimated later in distribution fitting)
                 listCopulasTemp$listCopulas <- list(inputs=Xnum(),groups=rep("Estimated",length(Xnum())),unique.groups=NULL,
                                                     typeCopulas=rep("Estimated",length(Xnum())),Copulas=NULL)
                 counterpanel$finished <- TRUE
               },
               "Groups Known"={
                 # We do not do anything
               })
      }else{
        if (counterpanel$count == 1){
          
          maxGroups <- floor(length(Xnum())/2)
          
          if(input$ngroups < 1 || input$ngroups > maxGroups){
            validParameter <- FALSE
            showModal(modalDialog(HTML(
              paste("The number of groups is not consistent: <br />", 
                    "A group should have at least two inputs, 
                    so the number of groups should be between 1 and", maxGroups))
              , title = "Warning !")
              )
          }else{
            # Initialize list of selectable inputs
            counterpanel$inputs <- Xnum()
            listCopulasTemp$listCopulas$inputs <- NULL
            # Define unique groups
            listCopulasTemp$listCopulas$unique.groups <-  NULL
            listCopulasTemp$listCopulas$groups <- rep("0",length(Xnum()))
            listCopulasTemp$listCopulas$typeCopulas <- character(input$ngroups)
            listCopulasTemp$listCopulas$Copulas <- vector('list',input$ngroups)
            names(listCopulasTemp$listCopulas$Copulas) <- as.character(1:input$ngroups)
          }
          
        }else{
          if (input$groupcopula!="Estimated"){
            
            if(is.na(input$paramcopula)){
              validParameter <- FALSE
              showModal(modalDialog(HTML(
                paste("Copula parameter must not be empty"))
                , title = "Warning !")
              )
            }else if(is.na(boundsParamCopula$max)){
              if(input$paramcopula < boundsParamCopula$min){
                validParameter <- FALSE
                showModal(modalDialog(HTML(
                  paste(input$groupcopula, "copula parameter must be: <br />", 
                        " - Greater than or equal to", boundsParamCopula$min))
                  , title = "Warning !")
                )
              }
            }else if(input$paramcopula < boundsParamCopula$min || input$paramcopula > boundsParamCopula$max){
              validParameter <- FALSE
              showModal(modalDialog(HTML(
                paste(input$groupcopula, "copula parameter must be: <br />", 
                      " - Greater than or equal to ", boundsParamCopula$min, "<br />",
                      " - Lower than or equal to ", boundsParamCopula$max))
                , title = "Warning !")
              )
              
            }
          }
          
          if(validParameter){
            # Update selectable inputs with the previous choice
            counterpanel$inputs <- setdiff(counterpanel$inputs,input$groupinputs)
            listCopulasTemp$listCopulas$inputs <- c(listCopulasTemp$listCopulas$inputs,input$groupinputs)
            # Assign selected inputs to the group
            id <- which(Xnum()%in%input$groupinputs)
            listCopulasTemp$listCopulas$groups[id] <- as.character(counterpanel$count-1)
            # Assign copula
            listCopulasTemp$listCopulas$typeCopulas[counterpanel$count-1] <- input$groupcopula
            
            if (input$groupcopula!="Estimated"){
              
              if (input$groupcopula!="Gaussian"){
                copstruct <- archmCopula(input$groupcopula, param = input$paramcopula, dim = length(id))
              }else{
                copstruct <- ellipCopula("normal", param = input$paramcopula, dim = length(id), dispstr = "ex")
              }
              listCopulasTemp$listCopulas$Copulas[[as.character(counterpanel$count-1)]] <- copstruct
            }
          }
          
        }
        # Check if there are more than 2 selectable inputs left
        if (validParameter & length(counterpanel$inputs)<2){
          counterpanel$finished <- TRUE
        }
      }
      if(validParameter){
        counterpanel$count <- counterpanel$count + 1
      }
    }
  })
  
  observe({
    req(input$ngroups,counterpanel$count)
    if (counterpanel$count == (input$ngroups+2)){
      counterpanel$finished <- TRUE
    }
  })
  
  observeEvent(input$close, {
    toggleModal(session, "modal", toggle = "close")
    listCopulasTemp$listCopulas <- initiallistCopulas$listCopulas
    # Re-initialize counterpanel
    counterpanel$count <- 0
    counterpanel$finished <- FALSE
    counterpanel$inputs <- NULL
  })
  
  observeEvent(input$save, {
    toggleModal(session, "modal", toggle = "close")
    listCopulasTemp$listCopulas$inputs <- (DOE$xnamesmenu%in%listCopulasTemp$listCopulas$inputs)
    if (any(listCopulasTemp$listCopulas$inputs)){
      groups <- rep("0",DOE$nX)
      groups[DOE$xnamesmenu%in%Xnum()] <- listCopulasTemp$listCopulas$groups
      listCopulasTemp$listCopulas$groups <- groups
      listCopulasTemp$listCopulas$unique.groups <- as.character(unique(as.numeric(setdiff(groups,c("0","Estimated")))))
    }
    listCopulas$listCopulas <- listCopulasTemp$listCopulas
    # Re-initialize counterpanel
    counterpanel$count <- 0
    counterpanel$finished <- FALSE
    counterpanel$inputs <- NULL
    
    persistence$autoSavingCount <- persistence$autoSavingCount + 1
    persistence$autoSavingCaller <- "UQparamsDependence-save"
  })
  
  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "uqParamsDependence-uncertaintyDefinition") {
      logger$print(paste("Loaded study, updating", persistence$updatingStep))
      
      if (!is.null(persistence$loadedStudy$UQparams)) {
        listCopulas$listCopulas <- persistence$loadedStudy$UQparams$listCopulas
        listCopulasTemp$listCopulas <- listCopulas$listCopulas
      }
      progressToNextStep(persistence)
    }        
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  return(listCopulas)
}
