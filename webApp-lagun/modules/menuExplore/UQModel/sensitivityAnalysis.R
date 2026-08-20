#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module sensitivityAnalysis
source("modules/menuImport/exploreDOE/quantitativeExploration.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)

# FIX separate in modules GSAOptim and GSASobol ?

subsets <- function(set, size = length(set), type = "leq") {
  if (size <= 0) {
    NULL
  } else if (size == 1) {
    as.list(set)
  } else if (type == "leq") {
    c(Recall(set, size - 1, "leq"), Recall(set, size, "eq"))
  } else if (size == length(set)) {
    list(set)
  } else {
    c(lapply(Recall(set[-1], size - 1, "eq"), function(x) c(set[1], x)),
      Recall(set[-1], size, "eq"))
  }
}

getDataframeGSAres <- function(DOE, GSAres, Yinfos) {
  df <- as.data.frame(rbind(GSAres$S1med, GSAres$STmed))
  colnames(df) <- DOE$ynamesmenu[Yinfos$visu.ids]
  rownames(df) <- c(paste('S1',DOE$xnamesmenu),paste('ST',DOE$xnamesmenu))
  df
}

getDataframeShapleyres <- function(DOE, Shapleyres, Yinfos) {
  df <- as.data.frame(Shapleyres$Smed)
  colnames(df) <- DOE$ynamesmenu[Yinfos$visu.ids]
  rownames(df) <- paste('Shapley',DOE$xnamesmenu)
  df
}

computeGSAres <- function(DOE, UQparams, predfun, nrep, nsample, Yinfos, Ylevel_target, callback) {
  dimx <- DOE$nX
  dimy <- DOE$nY
  S1 <- ST <- array(0,dim = c(nrep,dimx,dimy))
  Ytype <- DOE$Yinfos$type
  for (r in 1:nrep) {
    X1 <- data.frame(generateUQ(UQparams,nsample,DOE))
    X2 <- data.frame(generateUQ(UQparams,nsample,DOE))
    for (j in Yinfos$visu.ids) {
      if (Ytype[j] == 'numeric'){
        mcurrent <- function(x){predfun(x,j)}
      }
      if (Ytype[j] == 'categorical'){
        mcurrent <- function(x){as.numeric(predfun(x,j) == Ylevel_target[j])}
      }
      res <- sobolmartinez(model = mcurrent, X1, X2, nboot = 0)
      S1[r,,j] <- res$S[,1]
      ST[r,,j] <- res$T[,1]
    }
    callback(r)
  }
  S1med <- apply(S1,c(2,3),median)
  STmed <- apply(ST,c(2,3),median)
  list(S1 = S1, ST = ST, S1med = S1med, STmed = STmed)
}

computeShapleyres <- function(DOE, UQparams, predfun, nrep, nsample, Yinfos, Ylevel_target, permutations, callback) {
  dimx <- DOE$nX
  dimy <- DOE$nY
  S <- array(0,dim = c(nrep,dimx,dimy))
  Ytype <- DOE$Yinfos$type
  for (r in 1:nrep) {
    X <- data.frame(generateUQ(UQparams,nsample,DOE))
    Y <- matrix(NA,nsample,dimy)
    id.cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
    for (j in Yinfos$visu.ids){
      if (Ytype[j] == 'numeric'){
        Y[,j] <- predfun(X,j)
      }
      if (Ytype[j] == 'categorical'){
        Y[,j] <- as.numeric(predfun(X,j) == Ylevel_target[j])
      }
    }
    res <- sensitivity::sobolshap_knn(model = NULL, X = X, id.cat = id.cat, U = NULL, method = "knn", n.knn = 3,
                         return.shap = TRUE, n.limit = 500, randperm = permutations)
    tell(res,Y)
    S[r,,Yinfos$visu.ids] <- t(unname(res$Shap))[, Yinfos$visu.ids, drop=F]
    callback(r)
  }
  Smed <- apply(S,c(2,3),median)
  list(S = S, Smed = Smed)
}

detectGSAgaps <- function(S1,S1med,ST,STmed,th,Yinfos){
  nY <- dim(S1)[3]
  nrep <- dim(S1)[1]
  # Loop on all outputs
  list.indices <- vector('list',nY)
  for (j in Yinfos$visu.ids) {
    delta <- ST[,,j] - S1[,,j]
     inds <- which(colMeans(delta>th)>0.5)
     if (length(inds)==1){
       # Detection has failed, we select the two highest ones
       inds <- sort.int(colMeans(delta>th),decreasing=TRUE,index.return = TRUE)$ix
     }
     list.indices[[j]] <- inds
  }
  return(list.indices)
}

computeGSAres.highorder <- function(DOE, UQparams, predfun, nrep, nsample, Yinfos, list.indices, Ylevel_target, callback) {
  nall <- rep(0, length(list.indices))
  nall[Yinfos$visu.ids] <- unlist(lapply(Yinfos$visu.ids,function(i){length(list.indices[[i]])}))
  nmax <- max(nall)
  dimy <- DOE$nY
  S2 <- array(NA,dim = c(nrep,nmax*(nmax-1)/2,dimy))
  # First get input indices involved for each output
  idS2 <- list()
  idS2.origin <- list()
  Ytype <- DOE$Yinfos$type
  for (j in Yinfos$visu.ids){
    if (nall[j]){
      lind <- c(1:nall[j],subsets(1:nall[j],2,"eq"))
      mind <- matrix(unlist(lind[(nall[j]+1):length(lind)]),ncol=2,byrow=TRUE)
      idS2[[j]] <- mind
      lind.origin <- c(list.indices[[j]],subsets(list.indices[[j]],2,"eq"))
      mind.origin <- matrix(unlist(lind.origin[(nall[j]+1):length(lind.origin)]),ncol=2,byrow=TRUE)
      idS2.origin[[j]] <- mind.origin
    }
  }
  for (r in 1:nrep) {
    X1 <- data.frame(generateUQ(UQparams,nsample,DOE))
    X2 <- data.frame(generateUQ(UQparams,nsample,DOE))
    for (j in Yinfos$visu.ids) {
      if (nall[j]){
        l <- c(list.indices[[j]],subsets(list.indices[[j]],2,"eq"))
        mind <- idS2[[j]]
        if (Ytype[j] == 'numeric'){
          mcurrent <- function(x){predfun(x,j)}
        }
        if (Ytype[j] == 'categorical'){
          mcurrent <- function(x){as.numeric(predfun(x,j) == Ylevel_target[j])}
        }
        res <- sobol(model = mcurrent, X1, X2, order = l, nboot = 0)
        V <- res$V
        ntot <- nrow(V)
        V2 <- (V[(nall[j]+2):ntot,1] - V[1+mind[,1],1] - V[1+mind[,2],1])/V[1,1]
        S2[r,1:length(V2),j] <- V2
      }
    }
    callback(r)
  }
  S2med <- apply(S2,c(2,3),median,na.rm=TRUE)
  list(S2 = S2, S2med = S2med, idS2 = idS2.origin)
}

computeGSAOptimres <- function(DOE, UQparams, predfun, ynamemenu, GSAquantile, GSAsign, nrep, nsample, Yinfos, callback) {
  dimx <- DOE$nX
  S1 <- matrix(0,nrep,dimx)
  numy <- which(DOE$ynamesmenu == ynamemenu)
  mcurrent <- function(x){predfun(x,numy)}
  isNum <- which(sapply(DOE$Xinfos, function(Xinfo){Xinfo$type == 'numeric'}))
  for (r in 1:nrep) {
    X1 <- generateUQ(UQparams, nsample, DOE)
    Ypred <- mcurrent(X1)
    qGSA <- quantile(Ypred,GSAquantile/100)
    sGSA <- GSAsign
    if (sGSA == "Minimize") {
      idYOK <- which(Ypred < qGSA)
    } else {
      idYOK <- which(Ypred > qGSA)
    }
    kX <- "rbf_hsic"
    for (i in isNum) {
      pX <- median(dist(X1[,i]))
      S1[r,i] <- MMD(matrix(X1[,i],ncol = 1),matrix(X1[idYOK,i],ncol = 1),kX,pX)$estimate
    }
    callback(r)
  }
  S1med <- apply(S1,2,median)
  list(S1 = S1, S1med = S1med, yname = ynamemenu)
}

plotGSAOptim <- function(DOE, S1, ynamemenu, nsample) {
  dimx <- DOE$nX
  S1melt <- as.data.frame(melt(S1))
  S1melt <- cbind(S1melt, data.frame(SI = rep("MMD", nsample*dimx)))
  layout(
    plot_ly(S1melt, x = ~Var2, y = ~value, type = "box"),
    title = paste("GSA Optimization for ", ynamemenu), 
    xaxis = list(title = "Inputs",tickvals = 1:dimx,ticktext = DOE$xnamesvisu), 
    yaxis = list(title = "Sensitivity Index")
  )
}

plotGSA <- function(DOE, ynamemenu, S1, ST, nsample, Yinfos) {
  numy <- which(DOE$ynamesmenu == ynamemenu)
  nrep <- dim(S1)[1]
  dimx <- DOE$nX
  S1temp <- S1[,,numy]
  STtemp <- ST[,,numy]
  if (nrep==1){
    S1temp <- matrix(S1temp,nrow=1)
    STtemp <- matrix(STtemp,nrow=1)
  }
  S1melt <- as.data.frame(melt(S1temp))[,2:3]
  S1melt <- cbind(S1melt,data.frame(SI = rep("S1", nsample*dimx)))
  STmelt <- as.data.frame(melt(STtemp))[,2:3]
  STmelt <- cbind(STmelt,data.frame(SI = rep("ST", nsample*dimx)))
  SIall <- rbind(S1melt,STmelt)
  layout(
    plot_ly(SIall, x = ~Var2, y = ~value, color = ~as.factor(SI), type = "box"),
    title = paste("GSA for ", ynamemenu), 
    boxmode = "group", 
    xaxis = list(title = "Inputs", tickvals = 1:dimx, ticktext = DOE$xnamesvisu), 
    yaxis = list(title = "Sensitivity Index", range = c(-0.2,1.2))
  )
}

plotGSAhighorder <- function(DOE, ynamemenu, S2, idS2, nsample, Yinfos, adapt.visu){
  if (adapt.visu){
    margin=list(b = -2, l = -1)
  }else{
    margin=list(b = -1)
  }
  numy <- which(DOE$ynamesmenu == ynamemenu)
  idS2visu <- idS2[[numy]]
  nS2 <- nrow(idS2visu)
  if (nS2){
    nrep <- dim(S2)[1]
    dimx <- DOE$nX
    if (nS2==1){
      S2temp <- matrix(S2[,1:nS2,numy],ncol=1)
    }else{
      S2temp <- S2[,1:nS2,numy]
    }
    
    if (nrep==1){
      S2temp <- matrix(S2temp,nrow=1)
    }
    S2melt <- as.data.frame(melt(S2temp))[,2:3]
    xnames <- paste0(DOE$xnamesvisu[idS2visu[,1]],rep("<br>&<br>",nS2),DOE$xnamesvisu[idS2visu[,2]],sep="")
    layout(
      plot_ly(S2melt, x = ~Var2, y = ~value, type = "box"),
      title = paste("GSA for ", ynamemenu), 
      boxmode = "group", 
      xaxis = list(title = "Inputs", tickvals = 1:nS2, ticktext = xnames), 
      yaxis = list(title = "Sensitivity Index", range = c(-0.2,1.2)), margin=margin
    )
  }else{
    return(NULL)
  }
}

plotShapley <- function(DOE, ynamemenu, S, nsample, Yinfos) {
  numy <- which(DOE$ynamesmenu == ynamemenu)
  nrep <- dim(S)[1]
  dimx <- DOE$nX
  Stemp <- S[,,numy]
  if (nrep==1){
    Stemp <- matrix(Stemp,nrow=1)
  }
  Smelt <- as.data.frame(melt(Stemp))[,2:3]
  Smelt <- cbind(Smelt,data.frame(SI = rep("Shapley Effect", nsample*dimx)))
  layout(
    plot_ly(Smelt, x = ~Var2, y = ~value, type = "box"),
    title = paste("Shapley Effects for ", ynamemenu),
    xaxis = list(title = "Inputs",tickvals = 1:dimx,ticktext = DOE$xnamesvisu),
    yaxis = list(title = "Shapley Effect", range = c(-0.2,1.2))
  )
}

plotGSAcomplete <- function(DOE, S1med, STmed, Yinfos) {
  Slevels <- seq(0,1,0.05)
  cols <- scales::col_numeric("Blues", domain = NULL)(Slevels)
  colz <- setNames(data.frame(Slevels, cols), NULL)
  S1Heatmap <-  plot_ly(
    z = t(S1med[,Yinfos$visu.ids]),x = DOE$xnames,y = DOE$ynames[Yinfos$visu.ids], type = "heatmap", 
    colorscale = colz,showlegend = F,zmin = 0,zmax = 1,showscale=FALSE
  )
  STHeatmap <-  plot_ly(
    z = t(STmed[,Yinfos$visu.ids]),x = DOE$xnames,y = DOE$ynames[Yinfos$visu.ids],type = "heatmap",
    colorscale = colz,showlegend = F,zmin = 0,zmax = 1,showscale=FALSE
  )
  layout(
    subplot(S1Heatmap, STHeatmap, margin = 0.05),
    xaxis = list(title = 'First-order Index'), 
    yaxis = list(title = ""), 
    xaxis2 = list(title = 'Total Index'), 
    yaxis2 = list(title = ""), 
    margin = list(b = 160)
  )
}

plotShapleycomplete <- function(DOE, Smed, Yinfos) {
  Slevels <- seq(0,1,0.05)
  cols <- scales::col_numeric("Blues", domain = NULL)(Slevels)
  colz <- setNames(data.frame(Slevels, cols), NULL)
  SHeatmap <-  plot_ly(
    z = t(Smed[,Yinfos$visu.ids]),x = DOE$xnames,y = DOE$ynames[Yinfos$visu.ids], type = "heatmap",
    colorscale = colz,showlegend = F,zmin = 0,zmax = 1,showscale=FALSE
  )
  layout(
    SHeatmap,
    xaxis = list(title = 'Shapley Effects'),
    yaxis = list(title = "")
  )
}

sensitivityAnalysis.ui <- function(id) {
  ns <- NS(id)
  
  dependenceModal <-   bsModal(
    ns("modalDependence"), "Warning", NULL,
    h4("The inputs are dependent in your study. In this case the interpretation
       of Sobol indices is not straightforward, since they mix up informations
       about input interactions and correlations. We recommend that you compute Shapley effects instead."),
    br(),
    br(),
    actionButton(ns("goGSAanyway"),label="Proceed anyway ?", class = "btn-primary"),
    size = "small"
  )

  summarySobolModal <-   bsModal(
    ns("modalGSA"), "GSA Summary", NULL,
    downloadButton(ns("downloadSobol"), "Export GSA Results", class = "btn-info"),
    plotlyOutput(ns("plotGSAcomplete"), height = 500),
    size = "small"
  )
  
  summaryShapleyModal <-   bsModal(
    ns("modalShapley"), "GSA Summary", NULL,
    downloadButton(ns("downloadShapley"), "Export GSA Results", class = "btn-info"),
    plotlyOutput(ns("plotShapleycomplete"), height = 500),
    size = "small"
  )

  highorderModal <-   bsModal(
    ns("modalHighOrder"), "Choose Interactions to Compute", NULL,
    uiOutput(ns("selectOutputsHighOrder")),
    uiOutput(ns("selectInteractionsHighOrder")),
    br(),
    br(),
    uiOutput(ns("errorSaveHighOrder")),
    br(),
    fluidRow(
      column(3, actionButton(ns("saveModalHighOrder"), label = "Save and Close", class = "btn-warning",
                             width = '100%'), offset = 2),
      column(3, actionButton(ns("closeModalHighOrder"), label = "Dismiss", class = "btn-secondary",
                             width = '100%'), offset = 2)
    ),
    size = "small",
    tags$head(
      tags$style(
        paste0("#", 
               ns("modalHighOrder"),
               " .modal-footer{display:none}")))
  )
  
  selectLevelsModalSobol <-   bsModal(
    ns("modalSelectLevelsSobol"), "Select Category for each Output", NULL,
    uiOutput(ns("selectLevelUISobol"))
  )
  
  selectLevelsModalShapley <-   bsModal(
    ns("modalSelectLevelsShapley"), "Select Category for each Output", NULL,
    uiOutput(ns("selectLevelUIShapley"))
  )
  
  tabsetPanel(id = ns('tabs'), type = "tabs",
              tabPanel(h4("First & Total Sobol Indices"), value = ns('first-sobol'),
                       tagList(
                         br(),
                         uiOutput(ns('ui.sobol')), selectLevelsModalSobol, summarySobolModal, dependenceModal
                       )
              ),
              tabPanel(h4("Higher-Order Sobol Indices"), value = ns('high-sobol'),
                       tagList(
                         br(),
                         uiOutput(ns('ui.sobolhigh')), highorderModal
                       )
              ),
              tabPanel(h4("Shapley Effects"), value = ns('shapley'),
                       tagList(
                         br(),
                         uiOutput(ns('ui.shapley')), selectLevelsModalShapley, summaryShapleyModal
                       )
              ),
              tabPanel(h4("Optimization Indices"), value = ns('optim'),
                       tagList(
                         br(),
                         uiOutput(ns('ui.optim'))
                       )
              )
  )
}

sensitivityAnalysis.server <- function(id, DOE, ML, listmodels, UQparams, window.dimension, persistence, settings) {
  moduleServer(
    id,
    function(input, output, session) {  
      
      ns <- session$ns
      
      # Update output types for the visualization only if the surrogate models are updated
      Yinfos <- reactiveValues(int.ids=NULL, control.ids=NULL, const.ids=NULL, visu.ids=NULL, nY=NULL)
      observeEvent(list(listmodels$selected$id, DOE$nY), {
        
        YwithSelectedModel <- seq(DOE$nY)
        
        if (!is.null(listmodels$selected))
          YwithSelectedModel <- YwithSelectedModel[sapply(listmodels$selected$id, function(x) !is.na(x[1]))]
        
        Yinfos$int.ids <- intersect(DOE$Yinfos$int.ids, YwithSelectedModel)
        Yinfos$control.ids <- intersect(DOE$Yinfos$control.ids, YwithSelectedModel)
        Yinfos$const.ids <- intersect(DOE$Yinfos$const.ids, YwithSelectedModel)
        Yinfos$visu.ids <- c(Yinfos$int.ids, Yinfos$control.ids, Yinfos$const.ids)
        Yinfos$nY <- length(Yinfos$visu.ids)
      })
      
      Xcat <- reactive({
        req(DOE$Xinfos)
        cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
        return(DOE$xnames[cat])
      })
      
      GSAres <- reactiveValues(S1 = NULL, ST = NULL, S1med = NULL, STmed = NULL, Ylevel_target = NULL, highorder.indices = NULL,
                               auto.highorder.indices = NULL, S2 = NULL, S2med = NULL, idS2 = NULL)
      Shapleyres <- reactiveValues(S = NULL, Smed = NULL)
      GSAOptimres <- reactiveValues(S1 = NULL, S1med = NULL, yname = NULL)
      resetTrigger <- reactiveValues(sobol = FALSE, shapley = FALSE)
      
      # reset GSA if uncertainty definition is changed
      observeEvent(list(UQparams$UQparams, UQparams$listCopulas, listmodels$selected$id),{
        req(GSAres$S1 | Shapleyres$S)
        if(!is.null(UQparams$listCopulas$inputs)){
          resetTrigger$sobol <- TRUE
          resetTrigger$shapley <- TRUE
        }
        
        
      }, ignoreInit = TRUE)
      
      observeEvent(input$resetGSA, {
        req(input$resetGSA)
        resetTrigger$sobol <- TRUE
      })
      
      observeEvent(resetTrigger$sobol, {
        GSAres$S1 <- NULL
        GSAres$ST <- NULL
        GSAres$S1med <- NULL
        GSAres$STmed <- NULL
        GSAres$Ylevel_target <- NULL
        GSAres$highorder.indices <- NULL
        GSAres$auto.highorder.indices <- NULL
        GSAres$S2 <- NULL
        GSAres$S2med <- NULL
        GSAres$idS2 <- NULL
        GSAOptimres$S1 <- NULL
        GSAOptimres$S1med <- NULL
        GSAOptimres$yname <- NULL
        goGSA$compute <- FALSE
        resetTrigger$sobol <- FALSE
        resetTrigger$shapley <- FALSE
        ynameModalHighOrderSelected(NULL)
        interactionSelected(list())
        interactionSelectedOK(FALSE)
      }, ignoreInit = TRUE)
      
      observeEvent(persistence$updatingStep, {
        if (persistence$updatingStep == "sensitivityAnalysis") {
          logger$print(paste("Loaded study, updating", persistence$updatingStep))
          if (!is.null(persistence$loadedStudy$sensitivityAnalysis)) {
            # updating 1st & total Sobol indices
            
            loadedSobol <- persistence$loadedStudy$sensitivityAnalysis$GSAres
            
            GSAres$S1 <- loadedSobol$S1
            GSAres$ST <- loadedSobol$ST
            GSAres$S1med <- loadedSobol$S1med
            GSAres$STmed <- loadedSobol$STmed
            GSAres$Ylevel_target <- loadedSobol$Ylevel_target
            GSAres$auto.highorder.indices <- loadedSobol$auto.highorder.indices
        
            # updating High-Order Sobol indices

            loadedSobol <- persistence$loadedStudy$sensitivityAnalysis$GSAres

            GSAres$highorder.indices <- loadedSobol$highorder.indices
            GSAres$S2 <- loadedSobol$S2
            GSAres$S2med <- loadedSobol$S2med
            GSAres$idS2 <- loadedSobol$idS2

            # updating Shapley effects

            loadedShapley <- persistence$loadedStudy$sensitivityAnalysis$Shapleyres

            Shapleyres$S <- loadedShapley$S
            Shapleyres$Smed <- loadedShapley$Smed

            # updating optimization indices

            loadedGSAOptimres <- persistence$loadedStudy$sensitivityAnalysis$GSAOptimres

            GSAOptimres$S1 <- loadedGSAOptimres$S1
            GSAOptimres$S1med <-loadedGSAOptimres$S1med
            GSAOptimres$yname <- loadedGSAOptimres$yname
          }
          progressToNextStep(persistence)
        }
        
      }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
      
      
      
      # reset Shapley if uncertainty definition is changed
      observeEvent(input$resetShapley, {
        req(input$resetShapley)
        resetTrigger$shapley <- TRUE
      })
      
      observeEvent(resetTrigger$shapley, {
        Shapleyres$S <- NULL
        Shapleyres$Smed <- NULL
      })
      
      choicesY <- reactive({
        req(DOE$ynamesmenu,Yinfos)
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[Yinfos$int.ids])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[Yinfos$control.ids])
        if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[Yinfos$const.ids])
        return(l)
      })
      
      choicesYHighOrder <- reactive({
        req(DOE$ynamesmenu, Yinfos, GSAres$highorder.indices)
        
        selectedOutputs <- unlist(
          sapply(seq_len(length(GSAres$highorder.indices)), 
                 function(x) if(length(GSAres$highorder.indices[[x]])!=0) x))
        
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[Yinfos$int.ids[Yinfos$int.ids %in% selectedOutputs]])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[Yinfos$control.ids[Yinfos$control.ids %in% selectedOutputs]])
        if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[Yinfos$const.ids[Yinfos$const.ids %in% selectedOutputs]])
        
        return(l)
      })
      
      choicesYnum <- reactive({
        req(DOE$ynamesmenu,Yinfos)
        idCat <- which(DOE$Yinfos$type == 'categorical')
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$int.ids, idCat)])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$control.ids, idCat)])
        if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$const.ids, idCat)])
        if (length(unlist(l)) == 0 || all(sapply(DOE$Xinfos, function(Xinfo){Xinfo$type == 'categorical'}))){
          disableActionButton(ns("goGSAoptim"), session)
        }
        return(l)
      })
      yname <- callModule(
        dynamicSelectpicker.server, "chooseY", label.title = "Choose Output to Visualize", choices = choicesY,
        multiple = FALSE, livesearch = TRUE, selected = DOE$ynamesmenu[Yinfos$int.ids[1]]
      )
      ynamehighorder <- callModule(
        dynamicSelectpicker.server, "chooseYhighorder", label.title = "Choose Output to Visualize", choices = choicesYHighOrder,
        multiple = FALSE, livesearch = TRUE, selected = choicesYHighOrder()[1]
      )
      ynameshapley <- callModule(
        dynamicSelectpicker.server, "chooseYshapley", label.title = "Choose Output to Visualize", choices = choicesY,
        multiple = FALSE, livesearch = TRUE, selected = DOE$ynamesmenu[Yinfos$int.ids[1]]
      )
      
      ynameOptim <- callModule(
        dynamicSelectpicker.server, "chooseYOptim", label.title = "Output", choices = choicesYnum,
        multiple = FALSE, livesearch = TRUE, selected = choicesYnum()[1]
      )
      
      output$selectLevelUISobol <- renderUI({ 
        lapply(1:length(Yinfos$int.ids), function(j){
          k <- Yinfos$int.ids[j]
          if (DOE$Yinfos$type[k] == 'categorical'){
            fluidRow(column(12, selectInput(ns(paste0("SobolYlevel", j)), label = HTML(paste0('Category for Output ', DOE$ynamesmenu[k])),
                                            choices = levels(DOE$Y[, k])), align="center"))
          }
        })
      })
      
      output$selectLevelUIShapley <- renderUI({ 
        lapply(1:length(Yinfos$int.ids), function(j){
          k <- Yinfos$int.ids[j]
          if (DOE$Yinfos$type[k] == 'categorical'){
            fluidRow(column(12, selectInput(ns(paste0("ShapleyYlevel", j)), label = HTML(paste0('Category for Output ', DOE$ynamesmenu[k])),
                                            choices = levels(DOE$Y[, k])), align="center"))
          }
        })
      })
      
      output$ui.sobol <- renderUI({
        req(DOE$XY)
        if (is.null(GSAres$S1)){
          k <- which(DOE$Yinfos$type[Yinfos$int.ids] == "categorical")[1]
          if (!is.null(input[[paste0("SobolYlevel", k)]]) || all(DOE$Yinfos$type[Yinfos$int.ids] == 'numeric')){
            gobutton <- column(4, actionButton(ns("goGSA"), label=HTML(paste("Compute","Sobol 1st & Total",sep="<br>")), class = "btn-warning"), align="center")
          }else{
            gobutton <- column(4, NULL)
          }
          if (all(DOE$Yinfos$type[Yinfos$int.ids] == 'numeric')){
            firstrow <- fluidRow(
              column(4, NULL),
              gobutton,
              column(4, NULL)
            )
          }else{
            firstrow <- fluidRow(
              column(2, NULL),
              column(4, actionButton(ns("selectLevelsSobol"),label=HTML(paste("Select","Output Categories",sep="<br>")), class = "btn-warning"), align="center"),
              gobutton,
              column(2, NULL)
            )
          }
          if(is.null(UQparams$listCopulas$Copulas)){
            tagList(
              firstrow,
              br(),
              fluidRow(
                column(12,checkboxInput(ns("resamplesobol"), "Resample ?", value = T), align="center")
              ),
              fluidRow(
                column(12,h5("If you use resampling, we will automatically detect the candidate variables for higher-order interaction effects by comparing first and total indices."), align="center")
              )
            )
          }
        }else{
          tagList(
            fluidRow(
              column(4,dynamicSelect.ui(ns("chooseY"))),
              column(4,"",br(),actionButton(ns("goGSAcomplete"),label="Summary", class = "btn-primary")),
              column(4, "", br(), actionButton(ns("resetGSA"),label = HTML(paste("Reset Sensitivity", "Analysis",sep="<br>")), class = "btn-warning"))
            ),
            br(),
            fluidRow(
              column(12,
                     plotlyOutput(ns("plotGSA"), width = paste0(0.95*window.dimension$width,"px"), height = paste0(0.7*window.dimension$height,"px"))%>% withSpinner()
              )
            )
          )
        }
      })
      
      output$ui.sobolhigh <- renderUI({
        req(!is.null(GSAres$auto.highorder.indices))
        
        if(!is.null(GSAres$highorder.indices))
          goGSAbutton <- column(3, 
                                actionButton(
                                  ns("goGSAhighorder"),
                                  label=HTML(paste("Compute Sobol","Second-Order Interactions",sep="<br>")),
                                  class = "btn-warning"), 
                                align="center")
        else
          goGSAbutton <- column(3, "")
        
        if (is.null(GSAres$S2)){
          tagList(
            fluidRow(
              column(3,""),
              column(3,actionButton(ns("selecthighorder"),label=HTML(paste("Select","Second-Order Interactions",sep="<br>")), class = "btn-warning"), align="center"),
              goGSAbutton,
              column(3,"")
            ),
            br(),
            fluidRow(
              column(12,checkboxInput(ns("resamplesobolhighorder"), "Resample ?", value = T), align="center")
            )
          )
        }else{
          tagList(
            dynamicSelect.ui(ns("chooseYhighorder")),
            br(),
            fluidRow(
              column(12,
                     plotlyOutput(ns("plotGSAhighorder"), width = paste0(0.95*window.dimension$width,"px"), height = paste0(0.7*window.dimension$height,"px"))%>% withSpinner()
              )
            )
          )
        }
      })
      
      output$ui.shapley <- renderUI({
        req(DOE$XY)
        if (is.null(Shapleyres$S)){
          k <- which(DOE$Yinfos$type[Yinfos$int.ids] == "categorical")[1]
          if (!is.null(input[[paste0("ShapleyYlevel", k)]]) || all(DOE$Yinfos$type[Yinfos$int.ids] == 'numeric')){
            gobutton <- column(4, actionButton(ns("goShapley"), label=HTML(paste("Compute","Shapley Effects",sep="<br>")), class = "btn-warning"), align="center")
          }else{
            gobutton <- column(4, NULL)
          }
          if (all(DOE$Yinfos$type[Yinfos$int.ids] == 'numeric')){
            firstrow <- fluidRow(
              column(4, NULL),
              gobutton,
              column(4, NULL)
            )
          }else{
            firstrow <- fluidRow(
              column(2, NULL),
              column(4, actionButton(ns("selectLevelsShapley"),label=HTML(paste("Select","Output Categories",sep="<br>")), class = "btn-warning"), align="center"),
              gobutton,
              column(2, NULL)
            )
          }
          tagList(
            firstrow,
            br(),
            fluidRow(
              column(6,checkboxInput(ns("resampleshapley"), "Resample ?", value = T), align="center"),
              column(6,checkboxInput(ns("permshapley"), "Use permutations ?", value = F), align="center")
            ),
            fluidRow(
              column(12,h5("When the number of inputs is large or moderate (>=10-15), it is recommended to use an
                       approximate but faster algorithm based on permutations, especially if you use resampling."), align="center")
            )
          )
        }else{
          tagList(
            fluidRow(
              column(4,dynamicSelect.ui(ns("chooseYshapley"))),
              column(4,"",br(),actionButton(ns("goShapleycomplete"),label="Summary", class = "btn-primary")),
              column(4, "", br(), actionButton(ns("resetShapley"),label = HTML(paste("Reset Shapley", "Analysis",sep="<br>")), class = "btn-warning"))
            ),
            br(),
            fluidRow(
              column(12,
                     plotlyOutput(ns("plotShapley"), width = paste0(0.95*window.dimension$width,"px"), height = paste0(0.7*window.dimension$height,"px"))%>% withSpinner()
              )
            )
          )
        }
      })
      
      output$ui.optim <- renderUI({
        req(DOE$XY)
        t <- tagList(
          fluidRow(
            column(4,dynamicSelect.ui(ns("chooseYOptim"))),
            column(8,"")
          ),
          fluidRow(
            column(4,selectInput(ns("GSAsign"),label = "Type", choices = list("Minimize","Maximize"),selected = "Minimize")),
            column(4,actionButton(ns("goGSAoptim"),label=HTML(paste("Compute","Optimization Indices",sep="<br>")), class = "btn-warning"), align="center"),
            column(4,"")
          ),
          fluidRow(
            column(4,sliderInput(ns("GSAquantile"),label = "Percentage",min = 25,max = 75,value = 50,step = 25)),
            column(8,"")
          )
        )
        if (!is.null(GSAOptimres$S1)){
          t <- c(t,tagList(
            fluidRow(
              column(12,
                     plotlyOutput(ns("plotGSAOptim"), width = paste0(0.95*window.dimension$width,"px"), height = paste0(0.7*window.dimension$height,"px"))%>% withSpinner()
              )
            )
          ))
        }
        return(t)
      })
      
      ynameModalHighOrderSelected <- reactiveVal(NULL)
      
      ynameModalHighOrder <- callModule(
        dynamicSelectpicker.server, "selectOutputsHighOrderInteractions", label.title = "Select outputs to study", choices = choicesY,
        multiple = TRUE, livesearch = TRUE, selected = ynameModalHighOrderSelected() #DOE$ynamesmenu[Yinfos$int.ids[1]]
      )
      

      output$selectOutputsHighOrder <- renderUI({
        
        tagList(
          div(align="center", 
              dynamicSelectpicker.ui(ns("selectOutputsHighOrderInteractions")))
        )
      })
      
      interactionSelected <- reactiveVal(list())
      interactionSelectedOK <- reactiveVal(FALSE)
      
      output$selectInteractionsHighOrder <- renderUI({
        req(interactionSelectedOK())
        
        ynames <- unlist(choicesY())
        
        lapply(seq_len(length(ynames)), function(i){
            hidden(
              tagList(
                pickerInput(
                  inputId = ns(paste0("selectInter_", match(ynames[[i]], DOE$ynamesmenu))),
                  label = paste0("Interactions with ", ynames[[i]]),
                  choices = DOE$xnamesvisu,
                  selected = interactionSelected()[match(ynames[[i]], DOE$ynamesmenu)][[1]],
                  options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE),
                  multiple = TRUE
                )
              )
            )
        })
      })
      
      observeEvent(ynameModalHighOrder(), {
        
        ynames <- unlist(choicesY())
        
        for(y in ynames){
          pickerID <- paste0("selectInter_", match(y, DOE$ynamesmenu))
          if(y %in% ynameModalHighOrder()){
            shinyjs::show(pickerID)
          }else{
            shinyjs::hide(pickerID)
          }
        }
      })
        
      goGSA <- reactiveValues(compute = FALSE)
      
      observeEvent(input$selectLevelsSobol, {
        toggleModal(session, "modalSelectLevelsSobol", toggle = "open")
      })
      
      observeEvent(input$selectLevelsShapley, {
        toggleModal(session, "modalSelectLevelsShapley", toggle = "open")
      })
      
      observeEvent(input$goGSA, {
        req(listmodels$finalpredfun)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          goGSA$compute <- FALSE
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          # Check if there are dependences
          dependence <- any(UQparams$listCopulas$inputs)
          if (dependence){
            toggleModal(session, "modalDependence", toggle = "open")
          }else{
            goGSA$compute <- TRUE
          }
        }
      })
      
      observeEvent(input$goGSAanyway,{
        toggleModal(session, "modalDependence", toggle = "close")
        goGSA$compute <- TRUE
      })
      
      observeEvent(goGSA$compute, {
        req(listmodels$finalpredfun, goGSA$compute)
        predfun <- listmodels$finalpredfun
        nrep <- if (input$resamplesobol)  {settings$nrepGSA} else {1}
        nsample <- settings$nsample
        Ylevel_target <- lapply(1:length(Yinfos$int.ids), function(j){
          if (DOE$Yinfos$type[Yinfos$int.ids[j]] == 'categorical'){input[[paste0("SobolYlevel", j)]]}else{NULL}
        })
        callback <- function(r)  {
          incProgress(1/nrep, detail = paste("Resample", r,"/",nrep))
        }
        withProgress(message = 'Computing...', value = 0, {
          newGSAres <-  computeGSAres(
            DOE, UQparams, predfun, nrep, nsample, Yinfos, Ylevel_target, callback
          )
        })
        GSAres$S1 <- newGSAres$S1
        GSAres$S1med <- newGSAres$S1med
        GSAres$ST <- newGSAres$ST
        GSAres$STmed <- newGSAres$STmed
        GSAres$Ylevel_target <- Ylevel_target
        if (nrep >1){
          GSAres$auto.highorder.indices <- detectGSAgaps(GSAres$S1,GSAres$S1med,GSAres$ST,GSAres$STmed,0.05,Yinfos)
        }else{
          GSAres$auto.highorder.indices <- lapply(1:DOE$nY,function(i){1:DOE$nX})
        }
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "sensitivityAnalysis-goGSA"
      })
      
      observeEvent(input$goShapley, {
        req(listmodels$finalpredfun)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          predfun <- listmodels$finalpredfun
          nrep <- if (input$resampleshapley)  {settings$nrepGSA} else {1}
          nsample <- settings$nsampleShapley
          Ylevel_target <- lapply(1:length(Yinfos$int.ids), function(j){
            if (DOE$Yinfos$type[Yinfos$int.ids[j]] == 'categorical'){input[[paste0("ShapleyYlevel", j)]]}else{NULL}
          })
          callback <- function(r)  {
            incProgress(1/nrep, detail = paste("Resample", r,"/",nrep))
          }
          withProgress(message = 'Computing...', value = 0, {
            newShapleyres <-  computeShapleyres(
              DOE, UQparams, predfun, nrep, nsample, Yinfos, Ylevel_target, input$permshapley, callback
            )
          })
          Shapleyres$S <- newShapleyres$S
          Shapleyres$Smed <- newShapleyres$Smed
        }
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "sensitivityAnalysis-goShapley"
      })
 
      observeEvent(input$selecthighorder, {
        if(is.null(GSAres$highorder.indices)){
          
          yToSelect <- sapply(seq_len(length(GSAres$auto.highorder.indices)), 
                              function(i){
                                length(GSAres$auto.highorder.indices[[i]])!=0
                              }
          )
          ynameModalHighOrderSelected(DOE$ynamesmenu[yToSelect])
          
          if(!interactionSelectedOK()){
            for(i in seq_len(length(ynameModalHighOrderSelected()))){
              ynameID <- match(ynameModalHighOrderSelected()[i], DOE$ynamesmenu)
              inter <- interactionSelected()
              inter[[ynameID]] <- DOE$xnamesvisu[GSAres$auto.highorder.indices[[ynameID]]]
              interactionSelected(inter)
              
            }
            interactionSelectedOK(TRUE)
          }
        }
        toggleModal(session, "modalHighOrder", toggle = "open")
      })
      
      observeEvent(input$closeModalHighOrder, {
        
        toggleModal(session, "modalHighOrder", toggle = "close")
        
        if (is.null(GSAres$highorder.indices))
          highOrderIndices <- GSAres$auto.highorder.indices
        else
          highOrderIndices <- GSAres$highorder.indices
        
        for(i in seq_len(length(DOE$ynamesmenu))){
          updatePickerInput(session = session, 
                            inputId = paste0("selectInter_", i),
                            selected = DOE$xnamesvisu[highOrderIndices[[i]]])
        }
        
        updatePickerInput(session = session,
                          inputId = "selectOutputsHighOrderInteractions-choice",
                          selected = ynameModalHighOrderSelected())
        
      })
      
      observeEvent(input$saveModalHighOrder, {
        nY <- length(DOE$ynamesmenu)
        l <- vector(mode = "list", length = nY)
        errorMsg <- NULL
        for (i in seq_len(length(ynameModalHighOrder()))){
          ynameID <- match(ynameModalHighOrder()[i], DOE$ynamesmenu)
          l[[ynameID]] <- match(input[[paste0("selectInter_", ynameID)]], DOE$xnamesvisu)
        }
        
        if (all(sapply(l, function(i) length(l[i])!=1))){
          GSAres$highorder.indices <- l
          toggleModal(session, "modalHighOrder", toggle = "close")
        }else{
          errorMsg <- h4("Error: you cannot select only one input per output (0 or >= 2)")
        }
        
        output$errorSaveHighOrder <-  renderUI({
          return(errorMsg)
        })
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "sensitivityAnalysis-saveModalHighOrder"

        
      })
      
      observeEvent(input$goGSAhighorder, {
        req(listmodels$finalpredfun)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          predfun <- listmodels$finalpredfun
          nrep <- if (input$resamplesobolhighorder)  {settings$nrepGSA} else {1}
          nsample <- settings$nsample
          Ylevel_target <- GSAres$Ylevel_target
          callback <- function(r)  {
            incProgress(1/nrep, detail = paste("Resample", r,"/",nrep))
          }
          withProgress(message = 'Computing...', value = 0, {
            isolate({
              newGSAres <- computeGSAres.highorder(DOE, UQparams, predfun, nrep, nsample,
                                                   Yinfos, GSAres$highorder.indices, Ylevel_target, callback)
            })
          })
          GSAres$S2 <- newGSAres$S2
          GSAres$S2med <- newGSAres$S2med
          GSAres$idS2 <- newGSAres$idS2
        }
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "sensitivityAnalysis-goGSAhighorder"
      })
      
      observeEvent(input$goGSAoptim, {
        req(listmodels$finalpredfun)
        marginals.estimated <- any(sapply(UQparams$UQparams,function(l){l$typeDistr=="estimated"}))
        copulas.estimated <- any(UQparams$listCopulas$typeCopulas=="estimated")
        if (marginals.estimated | copulas.estimated){
          showModal(modalDialog(
            title = "Warning",
            "Some inputs have not been assigned a distribution or some copulas have not been estimated."
          ))
        }else{
          predfun <- listmodels$finalpredfun
          nrep <- if (!is.null(input$resamplesobol))  {settings$nrepGSA} else {1}
          nsample <- settings$nsample
          callback <- function(r)  {
            incProgress(1/nrep, detail = paste("Resample", r,"/",nrep))
          }
          withProgress(message = 'Computing...', value = 0, {
            newGSAOptimres <- computeGSAOptimres(
              DOE, UQparams, predfun, 
              ynameOptim(), input$GSAquantile, input$GSAsign, 
              nrep, nsample, Yinfos, callback
            )
          })
          GSAOptimres$S1 <- newGSAOptimres$S1
          GSAOptimres$S1med <- newGSAOptimres$S1med
          GSAOptimres$yname <- newGSAOptimres$yname
        }
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "sensitivityAnalysis-goGSAoptim"
      })
      
      output$plotGSAOptim <- renderPlotly({
        req(GSAOptimres$S1, ynameOptim() == GSAOptimres$yname, cancelOutput = TRUE) 
        plotGSAOptim(DOE, GSAOptimres$S1, GSAOptimres$yname, settings$nsample)
      })
      
      output$plotGSA <- renderPlotly({
        req(yname(), GSAres$S1, GSAres$ST, cancelOutput = TRUE)
        plotGSA(DOE, yname(), GSAres$S1, GSAres$ST, settings$nsample, Yinfos)    
      })
      
      output$plotShapley <- renderPlotly({
        req(ynameshapley(), Shapleyres$S, cancelOutput = TRUE)
        plotShapley(DOE, ynameshapley(), Shapleyres$S, settings$nsampleShapley, Yinfos)
      })
      
      output$plotGSAhighorder <- renderPlotly({
        req(ynamehighorder(), GSAres$S2, GSAres$idS2, cancelOutput = TRUE)
        plotGSAhighorder(DOE, ynamehighorder(), GSAres$S2, GSAres$idS2, settings$nsample, Yinfos, DOE$adapt.visu)    
      })
      
      output$plotML <- renderPlotly({
        req(input$goML, yname(), cancelOutput = TRUE)
        message <- "Please Compute Dependence Measure in Problem Definition > Preliminary Exploration > Quantitative Exploration"
        validate(need(ML$DCOR, message))
        yname <- DOE$ynames[yname()]
        df <- ML$DCOR[,setdiff(DOE$xnames,Xcat()),yname]
        df <- melt(df)
        p <- plot_ly(df, x = ~Var2, y = ~value, type = "box")%>%
          layout(title = "Distance Correlation", 
                 xaxis = list(title = "Inputs"), 
                 yaxis = list(title = yname(), range=c(0,1)))
      })
      
      observeEvent(input$goGSAcomplete, {
        toggleModal(session, "modalGSA", toggle = "open")
      })
      
      observeEvent(input$goShapleycomplete, {
        toggleModal(session, "modalShapley", toggle = "open")
      })
      
      output$plotGSAcomplete <- renderPlotly({
        req(GSAres$S1med, GSAres$STmed, Yinfos, cancelOutput = TRUE)
        plotGSAcomplete(DOE, GSAres$S1med, GSAres$STmed, Yinfos)
      })
      
      output$plotShapleycomplete <- renderPlotly({
        req(Shapleyres$S, Yinfos, cancelOutput = TRUE)
        plotShapleycomplete(DOE, Shapleyres$Smed, Yinfos)
      })
      
      output$downloadSobol <- downloadHandler(
        filename = 'SobolSensitivityAnalysis.csv',
        content = function(con) {
          df <- getDataframeGSAres(DOE, GSAres, Yinfos)
          write.table(x = df, file = con, row.names = T, col.names = T, sep = ",")
        }
      )
      
      output$downloadShapley <- downloadHandler(
        filename = 'ShapleySensitivityAnalysis.csv',
        content = function(con) {
          df <- getDataframeShapleyres(DOE, Shapleyres, Yinfos)
          write.table(x = df, file = con, row.names = T, col.names = T, sep = ",")
        }
      )
      
      return(list(GSAres = GSAres, Shapleyres = Shapleyres, GSAOptimres = GSAOptimres))
      
    }
  )
}