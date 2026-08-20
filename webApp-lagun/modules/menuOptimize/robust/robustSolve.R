#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module robustSolve

# FIX : bug happens sometimes in get.resRoptim.export 
# when RO did not find a solution which statisfies the constraints

get.resRoptim.table  <- function(DOE, ROformulation, resRoptim, ROcrit) {
  idD <- ROformulation$idD
  idUD <- ROformulation$idUD
  idcrit <- isolate(which(namesROcrit == ROcrit))
  idhrespect <- resRoptim$idhrespect
  nh <- sum(idhrespect)
  if (nh > 0) {
    d <- as.data.frame(cbind(resRoptim$allx,resRoptim$allf,resRoptim$allh))
    nr <- nrow(resRoptim$allx)
    rownames(d) <- paste("Optimum",1:nr)
  } else {
    d <- as.data.frame(cbind(resRoptim$allxiffail,resRoptim$allfiffail,resRoptim$allhiffail))
  }
  idO <- ROformulation$idO
  colnames(d) <- c(DOE$xnames[c(idD,idUD)],paste(namesROcrit[[idcrit]],DOE$ynames[idO]),"Joint Proba.")
  d
}

get.resRoptim.export <- function(DOE, ROformulation, resRoptim) {
  nh <- sum(resRoptim$idhrespect)
  # Save optim results in special format for export
  idO <- ROformulation$idO
  idC <- ROformulation$idC
  idD <- ROformulation$idD
  idUD <- ROformulation$idUD
  if (nh > 0) {
    # We'll display optima which respect the constraints
    dataoptim <- as.data.frame(cbind(resRoptim$allx,resRoptim$allf,resRoptim$allh))
    nr <- nrow(resRoptim$allx)
  } else {
    # We'll display all optima if none respects the constraints
    dataoptim <- as.data.frame(cbind(resRoptim$allxiffail,resRoptim$allfiffail,resRoptim$allhiffail))
    nr <- nrow(resRoptim$allxiffail)
  }
  nO <- length(idO)
  m <- matrix(as.matrix(ROformulation$ROobj),ncol = nO)
  bO <- matrix(rep(": Max",nO),ncol = nO)
  idmin <- which(m == -1)
  bO[idmin] <- ": Min"
  # FIX : this line sometimes generates error (bad number of columns)
  colnames(dataoptim) <- c(DOE$xnames[c(idD,idUD)],paste0(DOE$ynames[idO],bO),"Joint Proba.")
  dataoptim
}

computeRobustOptim <- function(DOE, lb, ub, predfun, ROformulation, threshold, ROalgo, ROcrit, nmultistart, callback) {
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
  threshproba <- threshold
  idcrit <- which(namesROcrit == ROcrit)
  
  allx <- matrix(NA,nrow = nmultistart,ncol = dimx)
  allf <- matrix(NA,nrow = nmultistart,ncol = 1)
  Xmultistart <- matrix(NA,nmultistart,dimx)
  Xmultistart[,c(idD,idUD)] <- runif.sobol(nmultistart,dimD + dimUD)
  Xmultistart <- repmat(lb,nmultistart,1) + repmat(ub - lb,nmultistart,1)*Xmultistart
  nC <- 1 # Joint constraint probability only
  maxiter <- 1000
  fiter <- matrix(NA,nrow = nmultistart,ncol = maxiter*dimx*10)
  xiter <- array(NA,dim = c(nmultistart,maxiter*dimx*10,dimx))
  hiter <- array(NA,dim = c(nmultistart,maxiter*nC*10,nC))
  if (ROalgo == "COBYLA") {
    opts <- list("algorithm" = "NLOPT_LN_COBYLA","maxeval" = maxiter,"xtol_rel" = 1.0e-8,"tol_constraints_ineq" = 1e-12)
  }
  if (ROalgo == "ISRES") {
    opts <- list("algorithm" = "NLOPT_GN_ISRES","xtol_rel" = 1.0e-8,"tol_constraints_ineq" = 1e-12)
  }
  if (ROalgo == "AUGLAG + COBYLA") {
    local_opts <- list("algorithm" = "NLOPT_LN_COBYLA","xtol_rel" = 1.0e-8)
    opts <- list("algorithm" = "NLOPT_LN_AUGLAG","maxeval" = maxiter,"xtol_rel" = 1.0e-8,"tol_constraints_ineq" = 1e-12,"local_opts" = local_opts)
  }
  nMC <- 1000
  for (nm in 1:nmultistart) {
 
    # Define objective and constraint functions
    iter <- 0
    fn <- function(x){
      iter <<- iter + 1
      Xtemp <- matrix(NA,nrow = nMC,ncol = dimx)
      Xtemp[,idD] <- repmat(matrix(x[1:dimD],nrow = 1),nMC,1)
      if (dimUD > 0) {
        for (j in 1:dimUD){
          Xtemp[,idUD[j]] <- rtruncnorm(nMC,a = lb[idUD[j]],b = ub[idUD[j]],mean = x[(dimD + 1):(dimD + dimUD)][j],sd = sigUD[j])
        }
      }
      if (dimU > 0) {
        for (j in 1:dimU){
          Xtemp[,idU[j]] <- rtruncnorm(nMC,a = lb[idU[j]],b = ub[idU[j]],mean = muU[j],sd = sigU[j])
        }
      }
      
      p <- - listROcrit[[idcrit]](ROformulation$ROobj*predfun(Xtemp,ROformulation$idO))
      fiter[nm,iter] <<- p
      xiter[nm,iter,c(idD,idUD)] <<- matrix(x,ncol = dimD + dimUD)
      return(p)
    }
    hn <- function(x){
      Xtemp <- matrix(NA,nrow = nMC,ncol = dimx)
      Xtemp[,idD] <- repmat(matrix(x[1:dimD],nrow = 1),nMC,1)
      if (dimUD > 0) {
        for (j in 1:dimUD){
          Xtemp[,idUD[j]] <- rtruncnorm(nMC,a = lb[idUD[j]],b = ub[idUD[j]],mean = x[(dimD + 1):(dimD + dimUD)][j],sd = sigUD[j])
        }
      }
      if (dimU > 0) {
        for (j in 1:dimU){
          Xtemp[,idU[j]] <- rtruncnorm(nMC,a = lb[idU[j]],b = ub[idU[j]],mean = muU[j],sd = sigU[j])
        }
      }
      Yc <- matrix(NA,nrow = nMC,ncol = dimy)
      for (j in 1:dimy) {
        Yc[,j] <- predfun(Xtemp,idC[j])
      }
      s <- rep(1,nMC) %*% signconstraints
      t <- rep(1,nMC) %*% threshconstraints
      Pc <- sum(apply(s*Yc > s*t,1,all))/nMC
      hiter[nm,iter,] <<- threshproba - Pc
      h <- threshproba - Pc
      return(h)
    }
    # Minimize
    if (ROalgo != "SQP") {
      nlo <- try(nloptr( 
        x0 = Xmultistart[nm,c(idD,idUD)], 
        eval_f = fn, 
        eval_grad_f = NULL,
        lb = lb[c(idD,idUD)], 
        ub = ub[c(idD,idUD)], 
        eval_g_ineq = hn, 
        eval_jac_g_ineq = NULL,
        eval_g_eq = NULL, 
        eval_jac_g_eq = NULL,
        opts = opts
        ),TRUE)
      if (class(nlo) != "try-error") {
        allx[nm,c(idD,idUD)] <- nlo$solution
        allf[nm,] <- nlo$objective
      }
    } else {
      x0 <- matrix(Xmultistart[nm,c(idD,idUD)],ncol = 1)
      confun <- function(x){return(list(ceq = NULL,c = matrix(hn(x),ncol = 1)))}
      sqpo <- try(NlcOptim(X = x0, objfun = fn, confun = confun,
                           lb = matrix(lb[c(idD,idUD)],ncol = 1), ub = matrix(ub[c(idD,idUD)],ncol = 1)),TRUE)
      if (class(sqpo) != "try-error") {
        allx[nm,c(idD,idUD)] <- sqpo$p
        allf[nm,] <- sqpo$fval
      }
    }
    callback(nm)
  }
  
  # Store all iterations
  imax <- max(rowSums(!is.na(fiter)))
  resRoptim <- list()
  resRoptim$allfiter <- fiter[,1:imax]
  resRoptim$allxiter <- xiter
  resRoptim$allhiter <- hiter[,1:imax,,drop = F]
  
  # Sort optima among multistart
  allx <- as.data.frame(allx)
  allx <- allx[!is.na(allx[,c(idD,idUD)[1]]),]
  allx <- allx[is.finite(allf),]
  allxu <- unique(round(allx,digits = 4),margin = 2)
  idu <- as.numeric(row.names(allxu))
  nu <- length(idu)
  allhu <- matrix(NA,nrow = nu,ncol = 1)
  for (i in 1:nu){
    allhu[i,] <- hn(as.matrix(allxu[i,c(idD,idUD)]))
  }
  idhrespect <- allhu <= 0
  
  # Stock results in case no optimum respects the constraints
  resRoptim$allxiffail <- as.matrix(allx[idu,c(idD,idUD)])
  resRoptim$allfiffail <- matrix(-ROformulation$ROobj*allf[idu,1],ncol = 1)
  hreal <- function(x){
    Yc <- matrix(NA,nrow = nMC,ncol = dimy)
    Xtemp <- matrix(NA,nrow = nMC,ncol = dimx)
    Xtemp[,idD] <- repmat(matrix(x[1:dimD],nrow = 1),nMC,1)
    if (dimUD > 0) {
      for (j in 1:dimUD){
        Xtemp[,idUD[j]] <- rtruncnorm(nMC,a = lb[idUD[j]],b = ub[idUD[j]],mean = x[(dimD + 1):(dimD + dimUD)][j],sd = sigUD[j])
      }
    }
    if (dimU > 0) {
      for (j in 1:dimU){
        Xtemp[,idU[j]] <- rtruncnorm(nMC,a = lb[idU[j]],b = ub[idU[j]],mean = muU[j],sd = sigU[j])
      }
    }
    for (j in 1:dimy) {
      Yc[,j] <- predfun(Xtemp,idC[j])
    }
    s <- rep(1,nMC) %*% signconstraints
    t <- rep(1,nMC) %*% threshconstraints
    Pc <- sum(apply(s*Yc > s*t,1,all))/nMC
    return(Pc)
  }
  allhu <- matrix(NA,nrow = nu,ncol = nC)
  for (i in 1:nu){
    allhu[i,] <- hreal(as.matrix(allxu[i,c(idD,idUD)]))
  }
  resRoptim$allhiffail <- allhu
  
  
  # Identify optima which respect all the constraints
  resRoptim$idhrespect <- idhrespect
  nh <- sum(idhrespect)
  if (nh > 0) {
    allxh <- as.matrix(allxu[idhrespect,])
    rownames(allxh) <- 1:nh
    allfh <- matrix(-ROformulation$ROobj*allf[idu[idhrespect],1],ncol = 1)
    rownames(allfh) <- 1:nh
    allhh <- matrix(allhu[idhrespect,],nrow = nh)
    rownames(allhh) <- 1:nh
    
    resRoptim$allx <- allxh[,c(idD,idUD)]
    resRoptim$allf <- allfh
    resRoptim$allh <- allhh
  }
  # Save optim results in special format for export
  resRoptim$export <- get.resRoptim.export(DOE, ROformulation, resRoptim)
  # View optim results as dataframe
  resRoptim$table  <- get.resRoptim.table(DOE, ROformulation, resRoptim, ROcrit) 
  
  return(resRoptim)
}

plotRobustOptim <- function(DOE, ROformulation, resRoptim){

  ff <- resRoptim$allfiter
  hh <- apply(resRoptim$allhiter<=0,c(1,2),all)
  idhh <- which(!hh)
  ff[!hh] <- NA
  nr <- nrow(ff)
  nc <- ncol(ff)
  cumfiter <- matrix(NA,nr,nc)
  for (i in 1:nr){
    idnotna <- !is.na(ff[i,])
    if (sum(idnotna)>0){
      nbidnotna <- which(idnotna)
      if (ROformulation$ROobj == -1) {
        cumtemp <- cummin(ff[i,idnotna])
      } else {
        cumtemp <- cummax(ff[i,idnotna])
      }
      cumfiter[i,idnotna] <- cumtemp
      cumfiter[i,] <- sapply(1:nc,function(col){
        if ((col > which(idnotna)[1]) & is.na(cumfiter[i,col])){
          idtemp <- floor(col-nbidnotna)
          idtemp <- which.min(idtemp[idtemp>0])
          cumfiter[i,nbidnotna[idtemp]]}else{cumfiter[i,col]}
      })
    }
  }
  
  idO <- ROformulation$idO
  df <- melt(cumfiter)
  
  idC <- ROformulation$idC
  Probability <- t(apply(resRoptim$allhiter <= 0,c(2,3),mean,na.rm=T))
  vals <- unique(scales::rescale(c(c(Probability),0,1)))
  o <- order(vals, decreasing = FALSE)
  cols <- scales::col_numeric("Blues", domain = NULL)(vals)
  colz <- setNames(data.frame(vals[o], cols[o]), NULL)
  
  lines <- plot_ly(df,y = ~value,mode = "lines",split = ~Var1, showlegend = F, type="scatter")
  heatmap <- plot_ly(
    z = Probability, y = DOE$ynames[idC], 
    colorscale = colz, type = "heatmap", zmin = 0,zmax = 1
  )
  
  layout(
    subplot(lines, heatmap, margin = 0.05),
    xaxis = list(title = "Iterations"),
    yaxis = list(title = DOE$ynames[idO]),
    xaxis2 = list(title = "Iterations"),
    yaxis2 = list(title = "Constraint Satisfaction"),
    showlegend = FALSE,
    title = "Robust Optimization Results"
  )
}

robustSolve.ui <- function(id) {
  ns <- NS(id)
  
  panel <- wellPanel(
    actionButton(ns("go"), "Robust Optimization",icon = icon("chart-bar"), class = "btn-primary"),
    hr(),
    selectInput(
      ns("ROcrit"), 
      label = "Select Robust Criterion",
      choices = list("Mean","Quantile 10%","Quantile 90%"),
      selected = "Mean"
    ),
    br(),
    numericInput(ns("threshproba"), "Constraint Probability", 0.9, min = 0, max = 1),
    br(),
    downloadButton(ns("download"), "Export Optimization Results", class = "btn-info")
  )
  
  rowSetting <- fluidRow(
    column(4, panel), 
    column(8, "")
  )
  
  visu <- bsCollapse(
    id = ns("collapse"), multiple = TRUE,
    bsCollapsePanel(
      "Graphical Representation", plotlyOutput(ns("plot"))
    ),
    bsCollapsePanel(
      "Table Representation", DT::dataTableOutput(ns('table'))
    )
  )
  
  tagList(
    rowSetting, visu 
  )
}

robustSolve.server <- function(input, output, session, DOE, listmodels, ROformulation, settings) {
  resRoptim <- reactiveValues(
    allfiter = NULL, allxiter = NULL, allhiter = NULL, 
    allf = NULL, allx = NULL, allh = NULL, idhrespect = NULL, 
    allfiffail = NULL, allxiffail = NULL, allhiffail = NULL, 
    fmin = NULL, xmin = NULL, export = NULL, table = NULL 
  )
  observeEvent(input$go, {
    req(listmodels$finalpredfun, DOE$Xinfos)
    nmultistart <- settings$nRmultistart
    predfun <- listmodels$finalpredfun
    Xbounds <- get.bounds(DOE$Xinfos)
    lb <- Xbounds[1,,drop=F]
    ub <- Xbounds[2,,drop=F]
    threshold <- input$threshproba
    callback <- function(i) {
      incProgress(1/nmultistart, detail = paste("Multistart", i,"/",nmultistart))
    }
    withProgress(message = 'Optimizing...', value = 0, {
      newResCoptim <- computeRobustOptim(
        DOE, matrix(as.numeric(lb),1), matrix(as.numeric(ub),1), predfun, ROformulation, threshold, settings$ROalgo, input$ROcrit, nmultistart, callback)
      resRoptim$allfiter <- newResCoptim$allfiter
      resRoptim$allxiter <- newResCoptim$allxiter
      resRoptim$allhiter <- newResCoptim$allhiter
      resRoptim$allf <- newResCoptim$allf
      resRoptim$allx <- newResCoptim$allx
      resRoptim$allh <- newResCoptim$allh
      resRoptim$idhrespect <- newResCoptim$idhrespect
      resRoptim$allfiffail <- newResCoptim$allfiffail
      resRoptim$allxiffail <- newResCoptim$allxiffail
      resRoptim$allhiffai <- newResCoptim$allhiffai
      resRoptim$fmin <- newResCoptim$fmin
      resRoptim$xmin <- newResCoptim$xmin
      resRoptim$export <- newResCoptim$export
      resRoptim$table  <- newResCoptim$table
    })
    # Open visualization panel
    updateCollapse(session, "collapse", open = "Graphical Representation")
  }) 
  
  output$plot <- renderPlotly({
    req(resRoptim$allfiter, resRoptim$allhiter, cancelOutput = TRUE)
    plotRobustOptim(DOE, ROformulation, resRoptim)
  })
  
  output$table  <- DT::renderDataTable({
    req(resRoptim$table)
    df <- resRoptim$table
    DT::datatable(
      df, 
      extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
      options = list(
        dom = 'Brtip', buttons = list(list(extend = 'colvis', columns = 1:ncol(df))), 
        scrollX = TRUE,scrollY = 400,scroller = TRUE, fixedColumns = TRUE
      ))
  })
  
  output$download <- downloadHandler(
    filename = 'RobustOptimizationResults.csv',
    content = function(con) {
      write.table(x = resRoptim$export, file = con, row.names = F, col.names = T, sep = ",")
    }
  )
  
  return(resRoptim)
}
