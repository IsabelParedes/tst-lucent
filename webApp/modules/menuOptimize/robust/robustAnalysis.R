#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module robustAnalysis
source("modules/shared/dynamicSelect.R", local = TRUE)

getResPrelimC.proba <- function(DOE, ROformulation, resPrelimC) {
  dimy <- length(ROformulation$idC)
  Ptest <- resPrelimC$Ptest
  alpha <- c(0.9,0.95,0.99)
  nalpha <- length(alpha)
  resP <- matrix(0,nrow = dimy + 1, ncol = nalpha)
  for (i in 1:nalpha) {
    resP[,i] <- matrix(apply(Ptest > alpha[i],2,mean),ncol = 1)
  }
  colnames(resP) <- alpha
  rownames(resP) <- c("Joint",DOE$ynames[ROformulation$idC])
  return(resP)
}

evaluateConstraints <- function(DOE, lb, ub, predfun, ROformulation, nlhs, callback) {
  dimx <- DOE$nX
  idC <- ROformulation$idC
  dimy <- length(idC)
  idD <- ROformulation$idD
  dimD <- length(idD)
  idUD <- ROformulation$idUD
  dimUD <- length(idUD)
  idU <- ROformulation$idU
  dimU <- length(idU)
  muUD <- matrix(0,ncol = dimUD)
  sigUD <- matrix(as.matrix(ROformulation$ROsigUD),ncol = dimUD)
  muU <- matrix(as.matrix(ROformulation$ROmuU),ncol = dimU)
  sigU <- matrix(as.matrix(ROformulation$ROsigU),ncol = dimU)
  signconstraints <- matrix(as.matrix(ROformulation$ROsign),ncol = dimy)
  threshconstraints <- matrix(as.matrix(ROformulation$ROt),ncol = dimy)
  nMC <- 1000
  Xtest <- matrix(NA,nlhs,dimx)
  Xtest[,c(idD,idUD)] <- runif.sobol(nlhs,dimD + dimUD)
  Xtestopt <- repmat(lb,nlhs,1) + repmat(ub - lb,nlhs,1)*Xtest
  Ytest <- matrix(NA,nrow = nlhs,ncol = dimy*nMC)
  Ptest <- matrix(NA,nrow = nlhs,ncol = 1 + dimy)
  for (i in 1:nlhs) {
    # Generate MC sampling around the proposed nominal
    XMC <- matrix(NA,nrow = nMC,ncol = dimx)
    if (dimUD > 0) {
      for (j in 1:dimUD){
        XMC[,idUD[j]] <- rtruncnorm(nMC,a = lb[idUD[j]],b = ub[idUD[j]],mean = Xtestopt[i,idUD[j]],sd = sigUD[j])
      }
    }
    if (dimU > 0) {
      for (j in 1:dimU){
        XMC[,idU[j]] <- rtruncnorm(nMC,a = lb[idU[j]],b = ub[idU[j]],mean = muU[j],sd = sigU[j])
      }
    }
    if (dimD > 0) {
      XMC[,idD] <- repmat(matrix(Xtestopt[i,idD],nrow = 1),nMC,1)
    }
    
    # Compute constraints violation
    Xkrig <- XMC
    Yc <- matrix(NA,nrow = nMC,ncol = dimy)
    for (j in 1:dimy) {
      Yc[,j] <- predfun(Xkrig,idC[j])
    }
    s <- rep(1,nMC) %*% signconstraints
    t <- rep(1,nMC) %*% threshconstraints
    Pc_indiv <- apply(s*Yc > s*t,2,sum)/nMC
    Pc <- sum(apply(s*Yc > s*t,1,all))/nMC
    
    # Store result
    Ytest[i,] <- c(Yc)
    Ptest[i,1] <- Pc
    Ptest[i,-1] <- Pc_indiv
    
    callback(i)
  }
  
  resPrelimC <- list()
  resPrelimC$Xtest <- Xtestopt
  resPrelimC$Ytest <- Ytest
  resPrelimC$Ptest <- Ptest
  resPrelimC$proba <- getResPrelimC.proba(DOE, ROformulation, resPrelimC)
  resPrelimC
}

plotPrelimC <- function(DOE, ROformulation, resPrelimC, yname) {
  nlhs <- 100
  nMC <- 1000
  idC <- ROformulation$idC
  Ptest <- resPrelimC$Ptest
  Ytest <- resPrelimC$Ytest
  bb <- c(0,seq(0.5,0.9,0.1),0.95,0.99,1)
  numy <- which(DOE$ynames[idC] == yname)
  
  Ytemp <- data.frame(id = 1:nlhs,C = Ytest[,(1 + nMC*(numy - 1)):(numy*nMC)])
  Yvisu <- melt(Ytemp,id = "id")
  dens <- with(Yvisu, tapply(value, INDEX = id, density))
  df <- data.frame(
    x = unlist(lapply(dens, "[[", "x")),
    y = unlist(lapply(dens, "[[", "y")),
    cut = rep(
      cut(Ptest[,1 + numy], breaks = bb,include.lowest = T), 
      each = length(dens[[1]]$x)
    )
  )
  
  layout(
    plot_ly(df, x = ~x, y = ~y, color = ~cut, colors = "Blues", opacity = 0.7,type="scatter",mode="lines"),
    title = "Constraint Satisfaction Probability", 
    xaxis = list(title = yname), 
    yaxis = list(title = "Probability")
  )
}

plotpairPrelimIndiv <- function(DOE, ROformulation, resPrelimC, yname) {
  idC <- ROformulation$idC
  idD <- ROformulation$idD
  idUD <- ROformulation$idUD
  datavisu <- resPrelimC$Xtest
  colnames(datavisu) <- DOE$xnames
  datavisu <- datavisu[,c(idD,idUD)]
  Ptest <- resPrelimC$Ptest
  numy <- which(DOE$ynames[idC] == yname)
  bb <- c(0,seq(0.5,0.9,0.1),0.95,0.99,1)
  cc <- cut(Ptest[,1 + numy],breaks = bb,include.lowest = T)
  ncc <- length(bb) - 1
  cc <- unclass(cc)
  pal <- brewer.pal(ncc,"Blues")
  pairs(datavisu,col = pal[cc],pch = 21,main = yname)
}
plotpairPrelimJoint <- function(DOE, ROformulation, resPrelimC, yname) {
  idC <- ROformulation$idC
  idD <- ROformulation$idD
  idUD <- ROformulation$idUD
  datavisu <- resPrelimC$Xtest
  colnames(datavisu) <- DOE$xnames
  datavisu <- datavisu[,c(idD,idUD)]
  Ptest <- resPrelimC$Ptest
  bb <- c(0,seq(0.5,0.9,0.1),0.95,0.99,1)
  cc <- cut(Ptest[,1],breaks = bb,include.lowest = T)
  ncc <- length(bb) - 1
  cc <- unclass(cc)
  pal <- brewer.pal(ncc,"Blues")
  numy <- which(DOE$ynames[idC] == yname)
  pairs(datavisu,col = pal[cc],pch = 21,main = "Joint")
}

# NB : not implemented 
pD3prelimindiv <- function(DOE, ROformulation, resPrelimC, yname) {
  idC <- ROformulation$idC
  datavisu <- resPrelimC$Xtest
  colnames(datavisu) <- DOE$xnames
  Ptest <- resPrelimC$Ptest
  numy <- which(DOE$ynames[idC] == yname)
  bb <- c(0,seq(0.5,0.9,0.1),0.95,0.99,1)
  cc <- cut(Ptest[,1 + numy],breaks = bb,include.lowest = T)
  ncc <- length(bb) - 1
  cc <- unclass(cc)
  pal <- brewer.pal(ncc,"Blues")
  pairsD3(datavisu,group = cc,col = pal, tooltip = NULL, big = T)
}
# NB : not implemented 
pD3prelimjoint <- function(DOE, resPrelimC) {
  datavisu <- resPrelimC$Xtest
  colnames(datavisu) <- DOE$xnames
  Ptest <- resPrelimC$Ptest
  bb <- c(0,seq(0.5,0.9,0.1),0.95,0.99,1)
  cc <- cut(Ptest[,1],breaks = bb,include.lowest = T)
  ncc <- length(bb) - 1
  cc <- unclass(cc)
  pal <- brewer.pal(ncc,"Blues")
  pairsD3(datavisu,group = cc,col = pal, tooltip = NULL, big = T)
}

robustAnalysis.ui <- function(id) {
  ns <- NS(id)
  
  firstPanel <- wellPanel(
    actionButton(
      ns("go"),"Preliminary Analysis of Constraints",icon = icon("chart-bar"), class = "btn-info"
  ))
  firstRow <- fluidRow(
    column(4, firstPanel),
    column(8, dynamicSelect.ui(ns("chooseY")))
  )
  
  secondPanel <- wellPanel(
    p("Estimation of Problem Complexity"),
    p("(% of points in the domain which satisfy the constraints with a given probability)"),
    br(),
    DT::dataTableOutput(ns('contentC'))
  )
  secondRow <- fluidRow(
    column(4,secondPanel),
    column(4,
           #uiOutput(ns("plotD3pairPrelimIndiv"))
           plotOutput(ns("plotpairPrelimIndiv"))
    ),
    column(4,
           #uiOutput(ns("plotD3pairPrelimJoint"))
           plotOutput(ns("plotpairPrelimJoint"))
    )
  )
  
  tagList(
    firstRow,
    secondRow,
    plotlyOutput(ns("plotPrelimC"), height = "600px")
  )
}

robustAnalysis.server <- function(input, output, session, DOE, listmodels, ROformulation) {
  choicesY <- reactive({
    req(DOE$ynames, ROformulation$idC)
    DOE$ynames[ROformulation$idC]
  })
  yname <- callModule(dynamicSelect.server, "chooseY", label = "Constraint", choicesY)
  
  resPrelimC <- reactiveValues(Xtest = NULL, Ytest = NULL, Ptest = NULL, proba = NULL)
  
  observeEvent(input$go, {
    req(listmodels$finalpredfun, DOE$Xinfos)
    nlhs <- 100
    predfun <- listmodels$finalpredfun
    Xbounds <- get.bounds(DOE$Xinfos)
    lb <- Xbounds[1,,drop=F]
    ub <- Xbounds[2,,drop=F]
    callback <- function(i) {
      incProgress(1/nlhs, detail = paste("Test Point", i,"/",nlhs))
    }
    withProgress(message = 'Evaluating constraints...', value = 0, {
      newResPrelimC <- evaluateConstraints(
        DOE, lb, ub, predfun, ROformulation, nlhs, callback
      )
      resPrelimC$Xtest <- newResPrelimC$Xtest
      resPrelimC$Ytest <- newResPrelimC$Ytest
      resPrelimC$Ptest <- newResPrelimC$Ptest
      resPrelimC$proba <- newResPrelimC$proba
    })
  })
  
  
  output$contentC  <- DT::renderDataTable({
    req(resPrelimC$proba)
    df <- resPrelimC$proba
    DT::datatable(df, options = list(dom = 't'))
  })
  
  output$plotPrelimC <- renderPlotly({
    req(ROformulation$idC, resPrelimC$Ptest, yname(), cancelOutput = TRUE)
    plotPrelimC(DOE, ROformulation, resPrelimC, yname()) 
  })
  
  output$plotpairPrelimIndiv <- renderPlot({
    req(ROformulation$idC, resPrelimC$Ptest, yname())
    plotpairPrelimIndiv(DOE, ROformulation, resPrelimC, yname()) 
  })
  
  output$plotpairPrelimJoint <- renderPlot({
    req(ROformulation$idC, resPrelimC$Ptest, yname())
    plotpairPrelimJoint(DOE, ROformulation, resPrelimC, yname())
  })
  
  output$plotD3pairPrelimIndiv <- renderUI({
    ns <- session$ns
    pairsD3Output(ns("pD3prelimindiv"),width = 300,height = 300)
  })
  output$pD3prelimindiv <- renderPairsD3({
    req(ROformulation$idC, resPrelimC$Ptest, yname())
    pD3prelimindiv(DOE, ROformulation, resPrelimC, yname())
  })
  
  output$plotD3pairPrelimJoint <- renderUI({
    ns <- session$ns
    pairsD3Output(ns("pD3prelimjoint"),width = 300,height = 300)
  })
  output$pD3prelimjoint <- renderPairsD3({
    req(ROformulation$idC, resPrelimC$Ptest, yname())
    pD3prelimjoint(DOE, resPrelimC) 
  })
  
}
