#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module unconstrained
source("modules/shared/XinfosChange.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)

addConfPointsFromUnconstrOptim <- function(DOE, resoptim, nadd) {
  nsimu <- min(nadd, nrow(resoptim$allx))
  Xadd <- as.data.frame(resoptim$allx)[1:nsimu,]
  colnames(Xadd) <- colnames(DOE$X)
  rownames(Xadd) <- paste0("Simu", 1:nsimu)
  return(Xadd)
}

biObjectiveOptim <- function(dimx, DOE, Xinfos, lb, ub, predfun, yname1, sign1, yname2, sign2) {
  id1 <- which(DOE$ynamesmenu == yname1)
  id2 <- which(DOE$ynamesmenu == yname2)
  fnscale1 <- 1
  if (sign1 == "Maximize") {
    fnscale1 <- -1
  }
  fnscale2 <- 1
  if (sign2 == "Maximize") {
    fnscale2 <- -1
  }
  nvar <- get.nb.num(Xinfos)
  
  if (nvar < dimx){
    nlevels.all <- sapply(Xinfos, function(Xinfos){Xinfos$nlevels})
    nlevels.all <- nlevels.all[!is.na(nlevels.all)]
    nslices <- prod(nlevels.all)
    levels <- expand.grid(lapply(nlevels.all, function(nlevel){1:nlevel}))
    num.index <- unlist(sapply(1:dimx, function(ind, Xinfos){
      if (Xinfos[[ind]]$type=='numeric') {ind}
    }, Xinfos = Xinfos))
    index <- 1:dimx
    cat.index <- index[!index %in% num.index]
    fn <- function(x,currentxcat,num.index,cat.index){
      nx <- nrow(x)
      xcomplete <- as.data.frame(matrix(NA,nx,dimx))
      colnames(xcomplete) <- DOE$xnames
      xcomplete[,num.index] <- x
      xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nx,1))
      y <- matrix(NA,nrow = 2,ncol = nrow(x))
      p1 <- predfun(xcomplete,id1)
      p2 <- predfun(xcomplete,id2)
      y[1,] <- fnscale1*p1
      y[2,] <- fnscale2*p2
      return(y)
    }
    allxtemp <- NULL
    allftemp <- NULL
    for (s in 1:nslices){
      currentlevels = as.matrix(levels[s,])
      currentxcat <- matrix(as.matrix(sapply(1:(dimx-nvar), function(ind){
        as.factor(unlist(Xinfos[[cat.index[ind]]]$levels)[currentlevels[ind]])
      })),nrow=1)
      r <- nsga2(fn, nvar, 2,lower.bounds = as.numeric(lb), upper.bounds = as.numeric(ub), vectorized = T, 
                 currentxcat = currentxcat, num.index = num.index, cat.index = cat.index)
      xtemp <- r$par
      nxtemp <- nrow(xtemp)
      xcomplete <- matrix(NA,nxtemp,dimx)
      xcomplete[,num.index] <- xtemp
      xcomplete[,cat.index] <- rep(currentxcat,each=nxtemp)
      allxtemp <- rbind(allxtemp,xcomplete)
      allftemp <- rbind(allftemp,r$value)
      incProgress(1/nslices, detail = paste("Categorical Combination", s,"/",nslices))
    }
    incProgress(0, detail = "Final Pareto Front Identification")
    row.names(allftemp) <- 1:nrow(allftemp)
    idpareto <- row.names(paretoFilterFast2obj(allftemp))
    idpareto <- as.numeric(idpareto)
    idpareto <- idpareto[!is.na(idpareto)]
    resoptim = list()
    resoptim$allx <- allxtemp[idpareto,,drop=FALSE]
    resoptim$allf <- allftemp[idpareto,,drop=FALSE]
  }else{
    fn <- function(x){
      y <- matrix(NA,nrow = 2,ncol = nrow(x))
      p1 <- predfun(matrix(x,ncol = dimx),id1)
      p2 <- predfun(matrix(x,ncol = dimx),id2)
      y[1,] <- fnscale1*p1
      y[2,] <- fnscale2*p2
      return(y)
    }
    r <- nsga2(fn, dimx, 2,lower.bounds = as.numeric(lb), upper.bounds = as.numeric(ub), vectorized = T)
    idp <- r$pareto.optimal
    np <- length(idp)
    resoptim = list()
    resoptim$allx <- matrix(r$par[idp,],np,dimx)
    allf1 <- matrix(fnscale1*r$value[idp,1],np,1)
    allf2 <- matrix(fnscale2*r$value[idp,2],np,1)
    resoptim$allf <- cbind(allf1,allf2)
  }
  resoptim$allxiter <- NULL
  resoptim$allfiter <- NULL
  resoptim$fmin <- NULL
  resoptim$xmin <- NULL
  resoptim$nslices <- NULL
  resoptim$xcat <- NULL
  resoptim$type <- "biObjective"
  
  df <- as.data.frame(cbind(resoptim$allx,resoptim$allf))
  colnames(df) <- c(DOE$xnamesmenu,yname1,yname2)
  resoptim$df <- df
  
  return(resoptim)
}

monoObjectiveOptimWithoutCat <- function(dimx, id, lb, ub, predfun, yname1, sign1, callback, inc, nmultistart) {
  xmin <- matrix(NA,nrow = 1,ncol = dimx)
  allx <- matrix(NA,nrow = nmultistart,ncol = dimx)
  allf <- matrix(NA,nrow = nmultistart,ncol = 1)
  Xmultistart <- runif.sobol(nmultistart,dimx)
  Xmultistart <- repmat(lb,nmultistart,1) + repmat(ub - lb,nmultistart,1)*Xmultistart 
  fmin <- Inf
  maxiter <- 100
  fiter <- matrix(NA,nrow = nmultistart,ncol = maxiter*dimx*10)
  xiter <- array(NA,dim = c(nmultistart, maxiter*dimx*10,dimx))
  fnscale <- 1
  if (sign1 == "Maximize") {
    fnscale <- -1
  }
  for (nm in 1:nmultistart) {
    iter <- 0
    fn <- function(x){
      iter <<- iter + 1
      p <- predfun(matrix(x,ncol = dimx),id)
      fiter[nm,iter] <<- p
      xiter[nm,iter,] <<- matrix(x,ncol = dimx)
      return(p)
    }
    # Minimize
    o <- optim(
      par = Xmultistart[nm,], fn = fn, method = "L-BFGS-B",
      lower = lb, upper = ub, control = list(trace = 0, maxit = maxiter, fnscale = fnscale)
    )
    allx[nm,] <- o$par
    allf[nm,] <- o$value
    if (fnscale*o$value < fmin) {
      fmin <- fnscale*o$value
      xmin <- o$par
    }
    callback(inc,nm)
  }
  return(list(fiter=fiter,xiter=xiter,allx=allx,allf=allf,fmin=fnscale*fmin,xmin=xmin))
}

monoObjectiveOptim <- function(dimx, DOE, Xinfos, lb, ub, predfun, yname1, sign1, callback, nmultistart) {
  id <- which(DOE$ynamesmenu == yname1)
  fnscale <- 1
  if (sign1 == "Maximize") {
    fnscale <- -1
  }
  nvar <- get.nb.num(Xinfos)
  
  if (nvar < dimx){
    nlevels.all <- sapply(Xinfos, function(Xinfos){Xinfos$nlevels})
    nlevels.all <- nlevels.all[!is.na(nlevels.all)]
    nslices <- prod(nlevels.all)
    levels <- expand.grid(lapply(nlevels.all, function(nlevel){1:nlevel}))
    num.index <- unlist(sapply(1:dimx, function(ind, Xinfos){
      if (Xinfos[[ind]]$type=='numeric') {ind}
    }, Xinfos = Xinfos))
    index <- 1:dimx
    cat.index <- index[!index %in% num.index]
    fn <- function(x,currentxcat,num.index,cat.index,id){
      nx <- nrow(x)
      xcomplete <- as.data.frame(matrix(NA,nx,dimx))
      colnames(xcomplete) <- DOE$xnames
      xcomplete[,num.index] <- x
      xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nx,1))
      y <- matrix(NA,nrow = 2,ncol = nrow(x))
      p <- predfun(xcomplete,id)
      return(p)
    }
    
    fiter <- xiter <- allx <- allf <- fmin <- xmin <- xcat <- NULL
    for (s in 1:nslices){
      incProgress(0, detail = paste("Categorical Combination", s,"/",nslices))
      currentlevels = as.matrix(levels[s,])
      currentxcat <- matrix(as.matrix(sapply(1:(dimx-nvar), function(ind){
        as.factor(unlist(Xinfos[[cat.index[ind]]]$levels)[currentlevels[ind]])
      })),nrow=1)
      
      ResOptimTemp <- monoObjectiveOptimWithoutCat(
        nvar, id, lb = lb, ub, predfun = function(x,id){fn(x,currentxcat,num.index,cat.index,id)}, yname1, sign1, callback, 1/nmultistart/nslices, nmultistart
      )
      
      nxtemp <- nrow(ResOptimTemp$allx)
      xcomplete <- matrix(NA,nxtemp,dimx)
      xcomplete[,num.index] <- ResOptimTemp$allx
      xcomplete[,cat.index] <- rep(currentxcat,each=nxtemp)
      allx <- rbind(allx,xcomplete)
      
      xcomplete <- matrix(NA,1,dimx)
      xcomplete[,num.index] <- ResOptimTemp$xmin
      xcomplete[,cat.index] <- currentxcat
      xmin <- rbind(xmin,xcomplete)
      
      fiter <- rbind(fiter,ResOptimTemp$fiter)
      allf <- rbind(allf,ResOptimTemp$allf)
      fmin <- rbind(fmin,ResOptimTemp$fmin)
      
      xcat <- rbind(xcat,matrix(rep(paste(DOE$xnames[cat.index],"=",currentxcat,collapse=", "),nmultistart),nmultistart))
    }
    resoptim = list()
    imax <- max(rowSums(!is.na(fiter)))
    resoptim$allfiter <- fiter[,1:imax]
    resoptim$allxiter <- NULL
    
    allx <- as.data.frame(allx)
    allx[,num.index] <- as.data.frame(apply(allx[,num.index], 2, function(col){
      as.numeric(as.character(col))}))
    allx[,num.index] <- signif(allx[,num.index],digits = 4)
    allxu <- unique(allx,margin=2)
    idu <- as.numeric(row.names(allxu))
    nu <- length(idu)
    resoptim$allx <- allxu
    resoptim$allf <- matrix(allf[idu,1],nu,1)
    
    resoptim$fmin <- ResOptimTemp$fmin
    resoptim$xmin <- ResOptimTemp$xmin
    resoptim$type <- "monoObjectiveCat"
    
    df <- as.data.frame(cbind(resoptim$allx,resoptim$allf))
    colnames(df) <- c(DOE$xnamesmenu,yname1)
    resoptim$df <- df
    
    resoptim$nslices <- nslices
    resoptim$xcat <- xcat

  }else{
    ResOptimTemp <- monoObjectiveOptimWithoutCat(
      dimx, id, lb, ub, predfun, yname1, sign1, callback, 1/nmultistart, nmultistart
    )
    resoptim = list()
    imax <- max(rowSums(!is.na(ResOptimTemp$fiter)))
    resoptim$allfiter <- ResOptimTemp$fiter[,1:imax]
    resoptim$allxiter <- ResOptimTemp$xiter
    
    allx <- as.data.frame(ResOptimTemp$allx)
    allxu <- unique(signif(allx,digits = 4),margin=2)
    idu <- as.numeric(row.names(allxu))
    nu <- length(idu)
    resoptim$allx <- allxu
    resoptim$allf <- matrix(ResOptimTemp$allf[idu,1],nu,1)
    
    resoptim$fmin <- ResOptimTemp$fmin
    resoptim$xmin <- ResOptimTemp$xmin
    resoptim$type <- "monoObjective"
    
    df <- as.data.frame(cbind(resoptim$allx,resoptim$allf))
    colnames(df) <- c(DOE$xnamesmenu,yname1)
    resoptim$df <- df
    
    resoptim$nslices <- NULL
    resoptim$xcat <- NULL
  }

  # Order results by 'allf' (affect order of 'Proposed confirmation runs')
  fOrder <- order(resoptim$allf, decreasing = (sign1 == "Maximize"))
  resoptim$allf <- resoptim$allf[fOrder]
  resoptim$allx <- resoptim$allx[fOrder,]
  resoptim$df <- resoptim$df[fOrder,]
  
  return(resoptim)
}

plotMonoObjectiveOptim <- function(resoptim) {
  allfiter <- cbind(resoptim$allfiter,NA)
  df <- melt(allfiter)
  if (is.null(resoptim$nslices)){
  p <- layout(
    plot_ly(df,y = ~value, mode = "lines",split = ~Var1, showlegend = F, type="scatter"),
    title = "Optimization Results", 
    xaxis = list(title = "Iterations"), 
    yaxis = list(title = resoptim$yname1)
  )
  }else{
    niter <- ncol(allfiter)
    nmultistart <- nrow(resoptim$allfiter)/resoptim$nslices
    df$idslice <- rep(rep(1:resoptim$nslices,each=nmultistart),niter)
    df$names <- resoptim$xcat[df$Var1]
    dforder <- df[order(df$Var1),]
    p <- layout(
      plot_ly(dforder,x=~Var2,y = ~value, mode = "lines",color = ~as.factor(idslice), type="scatter",name=~names),
      title = "Optimization Results", 
      xaxis = list(title = "Iterations"), 
      yaxis = list(title = resoptim$yname1)
    )
  }
  return(p)
}
plotBiObjectiveOptim <- function(resoptim) {
  df <- as.data.frame(resoptim$allf)
  colnames(df) <- c("o1","o2")
  layout(
    plot_ly(df,x = ~o1,y = ~o2, showlegend = F, mode = "markers", type="scatter"),
    title = "Optimization Results", 
    xaxis = list(title = resoptim$yname1), 
    yaxis = list(title = resoptim$yname2)
  )
}

unconstrained.ui <- function(id) {
  ns <- NS(id)
  
  panel <- wellPanel(
    fluidRow(
      column(6, dynamicSelect.ui(ns("chooseY1"))),
      column(
        6,
        selectInput(
          ns("sign1"),  
          label = "Type", 
          choices = list("Minimize","Maximize"), 
          selected = "Minimize"
      ))
    ),
    fluidRow(
      column(6, dynamicSelect.ui(ns("chooseY2"))),
      column(
        6,
        selectInput(
          ns("sign2"),  
          label = "Type", 
          choices = list("Minimize","Maximize"), 
          selected = "Minimize"
        ))
    ),
    XinfosChange.ui(ns("bounds")),
    hr(),
    fluidRow(
      column(4,
             actionButton(ns("goOptim"), "Optimize", icon = icon("chart-bar"), class = "btn-primary")
      ),
      column(8,
             h5('For mono-objective optimization, the local BFGS algorithm is used.'),
             h5('For multi-objective optimization, the problem is solved with the genetic algorithm NSGAII.')
             )
    ),
    br(),
    br(),
    h4("Check Optimization Results"),
    fluidRow(
      column(4, numericInput(ns("nadd"), "Number of Confirmation Points", 1, min = 1)),
      uiOutput(ns("tagDOEUI"))
    ),
    fluidRow(
      column(4, disabled(actionButton(ns("generate"), HTML("Show<br>Confirmation<br>Points"),
                                      icon = icon("table"), class = "btn-info", width = '100%'))),
      conditionalPanel(condition = paste0("output['", ns("use_simulator"), "']"),
                       column(4, disabled(actionButton(ns("launch.simu"), HTML("Launch<br>Simulations"), class = 'btn-primary',
                                                       icon = icon('cog'), width = '100%'))),
                       column(4, disabled(actionButton(ns("postOptimPlot"), 
                                                       HTML("Post-optim<br>Plot"), 
                                                       class = "btn-primary",
                                                       icon = icon("chart-bar"),
                                                       width = "100%")))
      )
    ),
    
    bsModal(ns("modalPostOptimPlot"), 
            "Post Optim Plot", 
            NULL, 
            uiOutput(ns("uiPlotPostOptim")),
            size = "large"
    )
  )
  
  tagList(
    fluidRow(
      column(4, panel),
      column(
        8, 
        actionButton(ns("resetUnconstrOptim"), 
                     "Reset", 
                     class="btn-warning", 
                     style="float:right",
                     width = "10%"),
        XinfosChange.ui.preview(ns("bounds")),
        br(),
        uiOutput(ns("preview.dynui"))
      )
    ),
    hr(),
    fluidRow(
      plotlyOutput(ns("plotOptim"))
    ),
    br(),
    downloadButton(ns("download"), "Export Optimization Results", class = "btn-info"),
    br(),
    br(),
    fluidRow(
      DT::dataTableOutput(ns('tableOptim'))
    ),
    bsModal(ns("modalLaunchSimu"), "Choose Mode to Launch Additional Simulations", NULL,
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
}

unconstrained.server <- function(input, output, session, DOE, listmodels, persistence, settings, doeProblemDef) {
  
  ns <- session$ns

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
  
  initialXinfos <- reactiveValues(nX = NULL, Xinfos = NULL)
  
  observeEvent(DOE$Xinfos, {
    initialXinfos$nX <- DOE$nX
    initialXinfos$Xinfos <- DOE$Xinfos
  })
  
  
  resoptim <- reactiveValues(
    allx = NULL, allf = NULL, allxiter = NULL, allfiter = NULL, fmin = NULL, xmin = NULL, 
    type = NULL, df = NULL,  yname1 = NULL,  yname2 = NULL,  sign1 = NULL,  sign2 = NULL, 
    nslices = NULL, xcat = NULL, OF = NULL
  )
  
  observeEvent(Xinfos$Xinfos, {
    
    req(resoptim$yname1)
    
    resoptim$allx <- NULL 
    resoptim$allf <- NULL
    resoptim$allxiter <- NULL
    resoptim$allfiter <- NULL
    resoptim$fmin <- NULL 
    resoptim$xmin <- NULL
    resoptim$type <- NULL
    resoptim$df <- NULL
    resoptim$yname1 <- NULL
    resoptim$yname2 <- NULL
    resoptim$sign1 <- NULL
    resoptim$sign2 <- NULL
    resoptim$nslices <- NULL
    resoptim$xcat <- NULL
    resoptim$OF <- NULL
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$resetUnconstrOptim, {

      resoptim$allx <- NULL 
      resoptim$allf <- NULL
      resoptim$allxiter <- NULL
      resoptim$allfiter <- NULL
      resoptim$fmin <- NULL 
      resoptim$xmin <- NULL
      resoptim$type <- NULL
      resoptim$df <- NULL
      resoptim$yname1 <- NULL
      resoptim$yname2 <- NULL
      resoptim$sign1 <- NULL
      resoptim$sign2 <- NULL
      resoptim$nslices <- NULL
      resoptim$xcat <- NULL
      resoptim$OF <- NULL
      
      initialXinfos$nX <- DOE$nX
      initialXinfos$Xinfos <- DOE$Xinfos

    
  })
  
  
  
  choicesY1 <- reactive({
    req(DOE$ynamesmenu,Yinfos)
    idYCat <- which(DOE$Yinfos$type == 'categorical')
    l <- list()
    if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$int.ids, idYCat)])
    if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$control.ids, idYCat)])
    if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$const.ids, idYCat)])
    return(l)
  })
  
  selectedYname1 <- reactive({
    req(DOE$ynamesmenu, Yinfos, choicesY1())
    
    name <- isolate(resoptim$yname1)
    if(is.null(name)){
      choicesY1()[1]
    }else{
      name
    }
    
  })
  
  yname1 <- callModule(
    dynamicSelectpicker.server, "chooseY1", label.title = "Objective 1", choices = choicesY1,
    multiple = FALSE, livesearch = TRUE, selected = selectedYname1()
  )
  
  choicesY2 <- reactive({
    req(DOE$ynames,Yinfos)
    idYCat <- which(DOE$Yinfos$type == 'categorical')
    l <- list()
    l[["None"]] <- "None"
    if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$int.ids, idYCat)])
    if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$control.ids, idYCat)])
    if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[setdiff(Yinfos$const.ids, idYCat)])
    return(l)
  })
  
  selectedYname2 <- reactive({
    req(DOE$ynamesmenu, Yinfos, choicesY1())
    
    name <- isolate(resoptim$yname2)
    if(is.null(name)){
      choicesY1()[1]
    }else{
      name
    }
    
  })
  
  
  yname2 <- callModule(
    dynamicSelectpicker.server, "chooseY2", label.title = "Objective 2", choices = choicesY2,
    multiple = FALSE, livesearch = TRUE, selected = selectedYname2()
  )
  
  # initialize with bounds coming from DOE reactiveValues
  Xinfos <- callModule(XinfosChange.server, "bounds", initialXinfos, data = DOE, edit.disable = TRUE)
  
  observeEvent(input$goOptim, {
    req(listmodels$finalpredfun, yname1(), yname2())
    
    dimx <- DOE$nX
    Xbounds <- get.bounds(Xinfos$Xinfos)
    lb <- Xbounds[1,,drop=F]
    ub <- Xbounds[2,,drop=F]
    predfun <- listmodels$finalpredfun
    nmultistart <- settings$nmultistart
    callback <- function(inc,nm) {
      incProgress(inc, detail = paste("Multistart", nm,"/",nmultistart))
    }
    withProgress(message = 'Optimizing...', value = 0, {
      if (yname2() == "None" | yname1() ==yname2()) { 
        # Mono-objective optimization
        newResOptim <- monoObjectiveOptim(
          dimx, DOE, Xinfos$Xinfos, lb, ub, predfun, yname1(), input$sign1, callback, nmultistart
        ) 
      } else {
        # Bi-objective optimization
        newResOptim <- biObjectiveOptim(
          dimx, DOE, Xinfos$Xinfos, lb, ub, predfun, yname1(), input$sign1, yname2(), input$sign2
        )
      }
      resoptim$allx <- newResOptim$allx
      resoptim$allf <- newResOptim$allf
      resoptim$allxiter <- newResOptim$allxiter
      resoptim$allfiter <- newResOptim$allfiter
      resoptim$fmin <- newResOptim$fmin
      resoptim$xmin <- newResOptim$xmin
      resoptim$type <- newResOptim$type
      resoptim$df <- newResOptim$df
      resoptim$nslices <- newResOptim$nslices
      resoptim$xcat <- newResOptim$xcat
      resoptim$yname1 <- yname1()
      resoptim$yname2 <- yname2()
      resoptim$sign1 <- input$sign1
      resoptim$sign2 <- input$sign2
      resoptim$OF <- NULL
      
      # Functional outputs
      if(length(DOE$Yinfos$func.ids) > 0){
        
        OFnames <- c(colnames(DOE$OF), colnames(DOE$OFtot))
        OFnames <- OFnames[!(OFnames %in% c(yname1(), yname2()))]
        
        resoptim$OF <- data.frame(matrix(nrow = nrow(resoptim$allx), ncol = 0))
        
        for (OF in OFnames){
          OFidx <- which(DOE$ynamesmenu == OF)
          resoptim$OF[OF] <- predfun(resoptim$allx, OFidx)
        }
      }
      
    })
    persistence$autoSavingCount <- persistence$autoSavingCount + 1
    persistence$autoSavingCaller <- "unconstrained-goOptim"
  })
  
  observeEvent(persistence$updatingStep, {
    if (persistence$updatingStep == "unconstrained-xinfos") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))

      if (!is.null(persistence$loadedStudy$unconstrOptim$resoptim$yname1)) {
        initialXinfos$nX <- persistence$loadedStudy$DOE$nX
        initialXinfos$Xinfos <- persistence$loadedStudy$unconstrOptim$Xinfos
      }
      progressToNextStep(persistence)
    }
    else if (persistence$updatingStep == "unconstrained-results") {
      logger$print(paste("Loaded study, updating",  persistence$updatingStep))

      if (!is.null(persistence$loadedStudy$unconstrOptim$resoptim$yname1)) {
        resoptim$allx <- persistence$loadedStudy$unconstrOptim$resoptim$allx 
        resoptim$allf <- persistence$loadedStudy$unconstrOptim$resoptim$allf
        resoptim$allxiter <- persistence$loadedStudy$unconstrOptim$resoptim$allxiter
        resoptim$allfiter <- persistence$loadedStudy$unconstrOptim$resoptim$allfiter
        resoptim$fmin <- persistence$loadedStudy$unconstrOptim$resoptim$fmin 
        resoptim$xmin <- persistence$loadedStudy$unconstrOptim$resoptim$xmin
        resoptim$type <- persistence$loadedStudy$unconstrOptim$resoptim$type
        resoptim$df <- persistence$loadedStudy$unconstrOptim$resoptim$df
        resoptim$yname1 <- persistence$loadedStudy$unconstrOptim$resoptim$yname1
        resoptim$yname2 <- persistence$loadedStudy$unconstrOptim$resoptim$yname2
        resoptim$sign1 <- persistence$loadedStudy$unconstrOptim$resoptim$sign1
        resoptim$sign2 <- persistence$loadedStudy$unconstrOptim$resoptim$sign2
        resoptim$nslices <- persistence$loadedStudy$unconstrOptim$resoptim$nslices
        resoptim$xcat <- persistence$loadedStudy$unconstrOptim$resoptim$xcat
        resoptim$OF <- persistence$loadedStudy$unconstrOptim$resoptim$OF

        updatePickerInput(session = session, inputId = "sign1", selected = resoptim$sign1)
        updatePickerInput(session = session, inputId = "sign2", selected = resoptim$sign2)
      }
      progressToNextStep(persistence)
    }
  }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
  
  output$tableOptim  <- DT::renderDataTable({
    req(resoptim$df, yname1(), yname2())
    
    df <- resoptim$df
    row.names(df) <- 1:nrow(df)
    isolate({
      if (yname2() == "None" | yname1() ==yname2()) {
        colnames(df) <- c(DOE$xnamesvisu,DOE$ynamesvisu[yname1()])
      }else{
        colnames(df) <- c(DOE$xnamesvisu,DOE$ynamesvisu[yname1()],DOE$ynamesvisu[yname2()])
      }
    })
    
    # Functional outputs
    if(!is.null(resoptim$OF)){
      df <- cbind(df, resoptim$OF)
    }
    
    DT::datatable(
      df, escape = FALSE,
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip', buttons = list(list(extend = 'colvis', columns = 1:ncol(df))), 
        scrollX = TRUE,scrollY = 400,scroller = TRUE, fixedColumns = TRUE
      ))
  })
  
  output$plotOptim <- renderPlotly({
    req(list(resoptim$type, Xinfos$Xinfos), cancelOutput = TRUE)
    if(is.null(resoptim$type)){
      NULL
    }else{
      switch(
        resoptim$type,
        "biObjective" = plotBiObjectiveOptim(resoptim),
        "monoObjective" = plotMonoObjectiveOptim(resoptim),
        "monoObjectiveCat" = plotMonoObjectiveOptim(resoptim)
      )
    }
    
  })
  
  output$download <- downloadHandler(
    filename = 'OptimizationResults.csv',
    content = function(con) {
      df <- resoptim$df
      if(!is.null(resoptim$OF)){
        df <- cbind(df, resoptim$OF)
      }
      write.table(x = df, file = con, row.names = F, col.names = T, sep = ",")
    }
  )

  output$tagDOEUI <- renderUI({
    req(use_simulator())
    column(6, textInput(ns("tagDOE"), label = 'Tag DOE Info', value = simulations$tagDOE, width = '100%'))
  })
  
  output$preview.dynui <- renderUI({
    req(simulations$Xadd)
    tagList(
      fluidRow(
        column(8, h4("Proposed Confirmation Points")),
        column(4, downloadButton(ns("downloadXAdd"), "Export Confirmation Points", class = "btn-info"))
      ),
      hr(),
      DT::dataTableOutput(ns("tableXAdd"))
    )
  })
  
  observeEvent(resoptim$allx, {
    if (is.null(resoptim$allx)) {
      shinyjs::disable("generate")
    }
    else {
      shinyjs::enable("generate")
    }
  })
  
  observeEvent(input$generate, {
    req(!is.null(resoptim$allx))
    simulations$Xadd <- addConfPointsFromUnconstrOptim(DOE, resoptim, input$nadd)
    if (use_simulator()){
      if (simulations$tagDOE == input$tagDOE){
        simulations$tagDOE <- paste("Confirm Unconstrained Optim", simulations$nConf)
      }else{
        simulations$tagDOE <- input$tagDOE
      }
      simulations$nConf <- simulations$nConf + 1
    }
  })

  output$use_simulator <- use_simulator
  outputOptions(output, 'use_simulator', suspendWhenHidden = FALSE)
  
  simulations = reactiveValues(Xadd = NULL, mode.manual = NULL, mode.automatic = NULL, 
                                tagDOE = "Confirm Unconstrained Optim 1", nConf = 1)
  
  observeEvent(list(listmodels$selected, resoptim$allx), {
    simulations$Xadd <- NULL
  })
  
  observeEvent(simulations$Xadd, {
    if (is.null(simulations$Xadd)) {
      shinyjs::disable("launch.simu")
    }
    else {
      shinyjs::enable("launch.simu")
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

  output$tableXAdd <- DT::renderDataTable({
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
  
  output$downloadXAdd <- downloadHandler(
    filename = 'AdditionalSimulations.csv',
    content = function(con) {
      isolate({
        df <- simulations$Xadd
        colnames(df) <- DOE$xnamesmenu
      })
      write.table(x=df, file=con, row.names=F, col.names=T, sep=",")
    }
  )
  
  #### Post Optim Plot ####
  
  
  observeEvent(input$postOptimPlot, {
    toggleModal(session, "modalPostOptimPlot", toggle = "open")
  })
  
  observeEvent(DOE$OF, {
    req(!is.null(resoptim$OF))
    
    if (all(!is.na(DOE$OF))){
      shinyjs::enable("postOptimPlot")
    }else{
      shinyjs::disable("postOptimPlot")
    }
    
    
  })
  
  choicesYFunc <- reactive({
    req(DOE$Fnames, DOE$Yinfos)
    l <- list()
    
    if (length(DOE$Yinfos$func.ids)>0){
      l[["Simulation Outputs"]] <- as.list(DOE$Fnames)
    } 
    
    return(l)
  })
  
  choicesObs <- reactive({
    req(DOE$Y, simulations$Xadd, funcName())
    
    l <- list()

    if(is.null(DOE$OFtot)){
      l <- seq(nrow(DOE$Y))
    }else{
      dfTot <- as.data.frame(DOE$OFtot)
      l <- sort(dfTot$OFtotal, index.return = TRUE)$ix
    }
    
    return(intersect(l, which(do.call(paste, DOE$X) %in% do.call(paste, simulations$Xadd))))
  })
  
  selectedObs <- reactive({
    req(choicesObs())
    
    sampleLength <- ifelse(length(choicesObs())>=50, 50, length(choicesObs()))
    
    return(choicesObs()[1:sampleLength])
    
  })
  
  choicesDim <- reactive({
    req(funcName())
    
    l <- list()
    
    if (!grepl("experimental", funcName(), fixed=TRUE)){
      funcNameId <- match(funcName(), DOE$Fnames)
      l <- colnames(DOE$discF[[funcNameId]])
      
      if(!is.null(DOE$Z)){
        l <- c(l, paste0("experimental_", funcName()))
      }
      
    }else{
      simuFuncName <- stringr::str_split(funcName(), pattern = "_", n = 2)[[1]][2]
      funcNameId <- match(simuFuncName, DOE$Fnames)
      l <- c(colnames(DOE$discZ[[funcNameId]]))
    }
    
    return(l)
  })
  
  choicesColor <- reactive({
    req(funcName())
    
    l <- "Simulation"
    
    if(!is.null(DOE$OFtot)){
      l <- c(l, colnames(DOE$OF), colnames(DOE$OFtot))
    }
    
    return(l)
    
  })
  
  
  funcName <- callModule(dynamicSelectpicker.server, "chooseFunc", label.title = "Y-axis", choices = choicesYFunc,
                         selected = choicesYFunc()[1], multiple = FALSE, livesearch = TRUE)
  
  funcObs <- callModule(dynamicSelectpicker.server, "chooseObs", label.title = "Simulation number", choices = choicesObs,
                        selected = selectedObs, multiple = TRUE, livesearch = TRUE)
  
  funcDim <- callModule(dynamicSelectpicker.server, "chooseDim", label.title = "X-axis", choices = choicesDim,
                        selected = choicesDim()[1], multiple = FALSE, livesearch = TRUE)
  
  funcColor <- callModule(dynamicSelectpicker.server, "chooseColor", label.title = "Color by", choices = choicesColor,
                          selected = choicesColor()[1], multiple = FALSE, livesearch = TRUE)
  
  
  output$uiPlotPostOptim <- renderUI({
    tagList(
      br(),
      fluidRow(
        column(2,dynamicSelectpicker.ui(ns("chooseFunc"))),
        column(2,dynamicSelectpicker.ui(ns("chooseDim"))),
        column(2,dynamicSelectpicker.ui(ns("chooseObs"))),
        column(2,dynamicSelectpicker.ui(ns("chooseColor"))),
        column(1,switchInput(ns("showExpData"), 
                             label = "Exp data", 
                             value = TRUE,
                             disabled = TRUE)),
        column(1,switchInput(ns("showErrBars"), 
                             label = "Error bars"))
      ),
      plotlyOutput(ns('plotPostOptim'))
    )
  })
  
  output$plotPostOptim <- renderPlotly({
    
    req(DOE$Y, funcDim(), funcName(), funcObs(), funcColor(),
        cancelOutput = TRUE)
    
    funcNameId <- match(funcName(), DOE$Fnames)
    
    dims <- c(colnames(DOE$discF[[funcNameId]]), 
              paste("experimental", funcName(), sep = "_"))
    
    req(funcDim() %in% dims, cancelOutput = TRUE)
    
    if(!is.null(DOE$Z)){
      updateSwitchInput(session,
                        "showExpData",
                        disabled = FALSE)
    }
    
    isExperimentalDim <- grepl("experimental", funcDim(), fixed=TRUE)
    err <- c()
    expData <- NULL
    showExp <- input$showExpData & !is.null(DOE$Z)
    showErr <- input$showErrBars
    
    y <- t(DOE$Y[funcObs(), DOE$idF[[funcNameId]]])
    rownames(y) <- NULL
    
    if (length(funcObs())==1){
      colnames(y) <- funcName()
    }else{
      colnames(y) <- paste(funcName(), funcObs(), sep = "_")
    }
    
    if(!showExp & !isExperimentalDim){
      updateSwitchInput(session, 
                        "showErrBars",
                        value = FALSE,
                        disabled = TRUE)
    }else{
      updateSwitchInput(session, 
                        "showErrBars",
                        disabled = FALSE)
    }
    
    
    if(!isExperimentalDim){
      x <- DOE$discF[[funcNameId]][funcDim()]
    }else{
      x <- t(DOE$Z[, DOE$idZY[[funcNameId]]])
      rownames(x) <- NULL
      
      updateSwitchInput(session,
                        "showExpData",
                        disabled = TRUE,
                        value = FALSE)
    }
    
    
    colnames(x) <- "dim"
    x <- as.data.frame(x)
    
    if(showExp){
      expData <- t(DOE$Z[, DOE$idZY[[funcNameId]]])
      expData <- cbind(expData, paste0("experimental_", funcName()))
      rownames(expData) <- NULL
      colnames(expData) <- c("values", "names")
      expData <- cbind(x, expData)
      
      emptyOF <- data.frame(matrix(ncol = length(DOE$OF)+1, nrow = length(x$dim)))
      colnames(emptyOF) <- c(colnames(DOE$OF), colnames(DOE$OFtot))
      expData <- cbind(expData, emptyOF)
    }
    
    d <- cbind(x, y)
    d <- tidyr::gather(d, "names", "values", -dim)
    
    errx <- list()
    erry <- list()
    if (showErr){
      err <- t(DOE$sigZ[, DOE$idZY[[funcNameId]]])
      if (!isExperimentalDim)
        erry <- list(array = err, color = "red")
      else
        errx <- list(array = err, color = "red")
    }
    
    hoverText <- sapply(
      seq_len(length(DOE$discF[[funcNameId]][,1])), 
      function(i) paste(
        paste0("<b>", colnames(DOE$discF[[funcNameId]]), "</b>"),
        " : ",
        DOE$discF[[funcNameId]][i,],
        collapse="\n"))
    
    h <- rep(hoverText, length(funcObs()))
    h <- paste(paste0("<b>", d$names, "</b>"),
               paste0("<b>", funcName(), "</b> : ", d$values), 
               h, sep = "\n")
    
    if(!is.null(DOE$Z)){
      filteredDOEOF <- cbind(DOE$OF[funcObs(),], OFtotal = DOE$OFtot[funcObs(),])
      filteredDOEOF <- filteredDOEOF[rep(seq_len(nrow(filteredDOEOF)), each=DOE$nF[funcNameId]), ]
      
      hOF <- sapply(
        seq_len(length(filteredDOEOF[,1])),
        function(i) paste(
          paste0("<b>", colnames(filteredDOEOF), "</b>"),
          " : ",
          filteredDOEOF[i,],
          collapse="\n"
        )
      )
      
      h <- paste(h, hOF, sep = "\n")
      
      d <- cbind(d, filteredDOEOF)
    }
    
    if(funcColor() == "Simulation"){
      colorBy <- "names"
      colorPalette <- "Pastel2"
      traceMode <- ifelse(isExperimentalDim, "markers", "lines+markers")
    }else{
      colorBy <- funcColor()
      colorPalette <- NULL
      traceMode <- "markers"
    }
    
    p <- plot_ly(data = d, 
                 x = ~dim, 
                 y = ~values, 
                 color = as.formula(paste0("~", colorBy)),
                 colors = colorPalette,
                 type = "scatter",
                 mode = traceMode,
                 error_x = ~errx,
                 text = ~h,
                 hoverinfo = "text",
                 showlegend = FALSE)
    
    
    if (showExp & !isExperimentalDim){
      
      markerStyle <- list(color = "red",
                          size = 20,
                          symbol = "square",
                          line = list(
                            color = "#31ABFA",
                            width = 3
                          ))
      
      p <- p %>% add_trace(data = expData,
                           x = ~dim,
                           y = ~values,
                           type = "scatter",
                           mode = "markers",
                           marker = markerStyle,
                           error_y = ~erry,
                           text = ~paste(paste0("<b>", expData$names, "</b>"), 
                                         paste0("<b>", "y = ", "</b>", expData$values),
                                         hoverText, 
                                         sep = "\n"),
                           hoverinfo = "text",
                           inherit = FALSE)
    }
    
    if (isExperimentalDim){
      p <- p %>% add_segments(x = min(d$dim), xend = max(d$dim), 
                              y = min(d$dim), yend = max(d$dim), 
                              name = "Ref", color = I("black"),
                              error_x = NULL,
                              text = NULL)
      
      xAxisList <- list(title = paste0("experimental_", DOE$Fnamesvisu[funcNameId]),
                        scaleanchor = "y",
                        scaleratio = 1)
      
    }else{
      xAxisList <- list(title = funcDim())
    }
    
    p <- p %>% layout(yaxis = list(title = DOE$Fnamesvisu[funcNameId]),
                      xaxis = xAxisList,
                      hoverlabel = list(align = "left"))
    
    
    
    return(p)
    
  })
  
  return(list(simulations = simulations, resoptim = resoptim, Xinfos = Xinfos))
}
