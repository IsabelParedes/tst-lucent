#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module choosesurrogate

plotQ2final <- function(DOE,surrogatemodel, yname, ynamemenu, namesurrogatemodel, typeQ2) {
  if (typeQ2 == "Q2 LOO"){
    Ypred <- surrogatemodel$yloo
    Ytrue <- DOE$Y[surrogatemodel$idYok,yname]
    nameplot <- "LOO"
  }else{
    Ypred <- surrogatemodel$ypredtest
    Ytrue <- surrogatemodel$ytest
    nameplot <- "Test"
  }
  
  Ytype <- DOE$Yinfos$type[which(names(DOE$Y)==yname)]
  
  if (Ytype == 'numeric'){
    mm <- min(c(Ypred, Ytrue))
    MM <- max(c(Ypred, Ytrue))
    dm <- data.frame(x = c(mm,MM), y = c(mm,MM))
    colnames(dm) <- c('x','y')
    df <- data.frame(yTrue = Ytrue, yPred = Ypred, numexp = 1:length(Ypred))
    colnames(df) <- c('yTrue','yPred','numexp')
    p <- plot_ly(df, x = ~yTrue, y = ~yPred, text=~paste('Exp nb:',numexp), hoverinfo='x+y+text', mode = "markers", name = nameplot, showlegend = FALSE, type="scatter")
    p <- add_trace(p,x = dm$x, y = dm$y, mode = "lines", name = "Perfect Match", showlegend = FALSE, type="scatter", inherit=FALSE) 
    layout(
      p, 
      title = paste("Q2",nameplot,"for", ynamemenu,"and metamodel",namesurrogatemodel), 
      xaxis = list(title = "True Value"), yaxis = list(title = "Predicted Value")
    )
  }
  
  if (Ytype == 'categorical'){
    df <- data.frame(yTrue = as.factor(Ytrue), yPred = as.factor(Ypred), numexp = 1:length(Ypred))
    colnames(df) <- c('yTrue','yPred','numexp')
    nx <- length(levels(df$yTrue))
    ny <- length(levels(df$yPred))
    p <- plot_ly(df,x = ~jitter(as.numeric(yTrue)), y = ~jitter(as.numeric(yPred)), 
                         type = 'scatter', mode = 'markers',showlegend = TRUE, alpha=0.5)%>%
      layout(title = paste("Q2",nameplot,"for", ynamemenu,"and metamodel",namesurrogatemodel),
             xaxis=list(title = "True Class",tickvals=1:nx,ticktext=levels(df$yTrue)),
             yaxis=list(title = "Predicted Class",tickvals=1:ny,ticktext=levels(df$yPred)),
             legend = list(orientation = 'h')
      )
  }
  return(p)
}
choosesurrogate.ui <- function(id) {
  ns <- NS(id)
  
  Q2Modalfinal <- bsModal(
    ns("modalQ2final"), "Q2", NULL, 
    plotlyOutput(ns("plotQ2final"),height=500),
    size="large"
  )
  
  
  fluidRow(
    column(
      4,
      wellPanel(
        dynamicSelect.ui(ns("chooseSelCrit")),
        h5("Here you can choose the criterion used to automatically assign the best surrogate model to each output."),
        h5("If you make a manual change in the list of selected metamodels, it will not be saved unless you click on the save button below."),
        actionButton(ns("saveFinalSurrogate"), label = "Save Final Surrogate", class = "btn-warning",
                     width = '100%')
      )
    ),
    column(
      8, 
      wellPanel(
      uiOutput(ns("chooseSurrogate")),
      style = "background-color: #ffffff")
    ),
    Q2Modalfinal
  )
}

choosesurrogate.server <- function(input, output, session, DOE, listmodels, settings) {
  
  Q2output <- reactiveValues(id = NULL)
  
  choicesSelCrit <- reactive({
    req(!is.null(listmodels$trainedModels))
    if (is.na(listmodels$bestQ2test$id[1])){
      return("Q2 LOO")
    }else{
      return(c("Q2 Test","Q2 LOO"))
    }
    
  })
  selcrit <- callModule(
    dynamicSelect.server, "chooseSelCrit", label = "Choose Criterion", choicesSelCrit
  )
  
  output$chooseSurrogate <- renderUI({
    req(!is.null(listmodels$trainedModels),selcrit(),DOE$Yinfos$surrogate.ids)
    ns <- session$ns
    ynames <- DOE$ynames
    ynamesvisu <- DOE$ynamesvisu
    lmodels <- listmodels
    if (selcrit()=="Q2 Test"){
      idsel <- lmodels$bestQ2test$id
    }else{
      idsel <- lmodels$bestQ2loo$id
    }
  
    lapply(DOE$Yinfos$surrogate.ids, function(i){
      idTrainedModels <- which(!is.na(lmodels$tableQ2loo[, 3 + i]))
      fluidRow(
        column(2, h4(HTML(ynamesvisu[i]))),
        column(4,
               selectInput(
                 inputId = ns(paste0('SelSurrogate', i)),
                 label = "Choose Final Surrogate",
                 choices = lmodels$names_surrogatemodel[idTrainedModels],
                 selected = lmodels$names_surrogatemodel[idsel[[i]]]
               )
               ),
        column(4,
               selectInput(
                 inputId = ns(paste0('SavedSurrogate', i)),
                 label = "Currently Saved Surrogate",
                 choices = lmodels$names_surrogatemodel[lmodels$selected$id[[i]]],
                 selected = lmodels$names_surrogatemodel[lmodels$selected$id[[i]]]
               )
        ),
        column(2,
               actionButton(ns(paste0('Q2Surrogate', i)), label = "Check Q2")
               )
        )
    })
  })
  
  observeEvent(input$saveFinalSurrogate, {
    req(!is.null(listmodels$trainedModels),selcrit(),DOE$Yinfos$surrogate.ids)
    finalsurrogates <- lapply(DOE$Yinfos$surrogate.ids, function(i){
      modelIndex <- which(listmodels$names_surrogatemodel==input[[paste0('SelSurrogate',i)]])
      if (length(modelIndex) == 0){modelIndex <- NA}
      return(modelIndex)
    })
    listmodels$selected$id[DOE$Yinfos$surrogate.ids] <- unlist(finalsurrogates)
    listmodels <- updateFinalpredfun(listmodels)
  })
  
  observe({
    req(DOE$nY,DOE$Yinfos$surrogate.ids)
    lapply(DOE$Yinfos$surrogate.ids, function(i){
      observeEvent(input[[paste0('Q2Surrogate', i)]], {
        Q2output$id <- NULL
        Q2output$id <- i
      })
    })
  })
  
  observeEvent(Q2output$id, {
    toggleModal(session, "modalQ2final", toggle = "open")
  })
  
  output$plotQ2final <- renderPlotly({
      req(listmodels$selected$id[[Q2output$id]], cancelOutput = TRUE)
      names_surrogatemodel <- listmodels$names_surrogatemodel
      plotQ2final(DOE,listmodels$models[[listmodels$selected$id[[Q2output$id]]]][[Q2output$id]], DOE$ynames[Q2output$id], DOE$ynamesmenu[Q2output$id],
             names_surrogatemodel[listmodels$selected$id[[Q2output$id]]],selcrit())
  })
  
  return(listmodels)
}