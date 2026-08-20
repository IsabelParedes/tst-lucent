#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module constrainedSolve

source("modules/shared/dynamicSelectpicker.R", local = TRUE)

addConfPointsFromConstrOptim <- function(DOE, resCoptim, nadd) {
  if (resCoptim$nh>0) {
    nsimu <- min(nadd, nrow(resCoptim$allx))
    Xadd <- as.data.frame(resCoptim$allx)[1:nsimu,]
    colnames(Xadd) <- colnames(DOE$X)
    rownames(Xadd) <- paste0("Simu", 1:nsimu)
    return(Xadd)
  }
  else {
    nsimu <- min(nadd, nrow(resCoptim$table))
    Xadd <- as.data.frame(resCoptim$table)[1:nsimu, 1:ncol(DOE$X)]
    colnames(Xadd) <- colnames(DOE$X)
    rownames(Xadd) <- paste0("Simu", 1:nsimu)
    return(Xadd)
  }
}

get.resCoptim.table  <- function(DOE, COformulation, resCoptim, choiceOptimFailed) {
  nh <- resCoptim$nh
  if (is.null(resCoptim$xcat)){
    if (nh > 0){
      tableCOptim <- as.data.frame(cbind(resCoptim$allx,resCoptim$allf,resCoptim$allh))
      nr <- nrow(tableCOptim)
      rownames(tableCOptim) <- paste("Optimum",1:nr)
    }else{
      tableCOptim <- as.data.frame(cbind(resCoptim$allxiffail,resCoptim$allfiffail,resCoptim$allhiffail))
      rownames(tableCOptim) <- c(paste("Best Obj.",1:resCoptim$nbest),paste("Min Const. Viol.",1:resCoptim$nmaxnum),paste("Min Const. Dep.",1:resCoptim$ndepart))
      idrows <- switch(choiceOptimFailed,
                       "Best Objective"=1:resCoptim$nbest,
                       "Maximum Number of Satisfied Constraints"=(resCoptim$nbest+1):(resCoptim$nbest+resCoptim$nmaxnum),
                       "Minimum Departure from Constraint Satisfaction"=(resCoptim$nbest+resCoptim$nmaxnum+1):(resCoptim$nbest+resCoptim$nmaxnum+resCoptim$ndepart),
                       "All Criteria" = 1:(resCoptim$nbest+resCoptim$nmaxnum+resCoptim$ndepart))
      tableCOptim <- tableCOptim[idrows,]
    }
    idO <- COformulation$idO
    idC <- COformulation$idC
    colnames(tableCOptim) <- c(DOE$xnamesvisu,DOE$ynamesvisu[idO],DOE$ynamesvisu[idC])
  }else{
    if (nh > 0){
      tableCOptim <- as.data.frame(cbind(resCoptim$allx,resCoptim$allf,resCoptim$allh))
      nr <- nrow(tableCOptim)
      rownames(tableCOptim) <- paste("Optimum",1:nr)
    }else{
      tableCOptim <- as.data.frame(cbind(resCoptim$allxiffail,resCoptim$allfiffail,resCoptim$allhiffail))
      nr <- nrow(tableCOptim)
      ncat <- nrow(resCoptim$xcat)
      g <- NULL
      lg <- NULL
      for (i in seq_along(resCoptim$nbest)){
        gtemp1 <-  c(rep("Best Obj.",resCoptim$nbest[i]),rep("Min Const. Viol.",resCoptim$nmaxnum[i]),rep("Min Const. Dep.",resCoptim$ndepart[i]))
        lgtemp <- length(gtemp1)
        gtemp2 <- rep(paste("Cat",i),lgtemp)
        g <- rbind(g,matrix(c(gtemp1,gtemp2),ncol=2))
        lg <- c(lg,lgtemp)
      }
      rownames(tableCOptim) <- paste(g[,1],g[,2])
      idrows <- switch(choiceOptimFailed,
                       "Best Objective"=which(g[,1]=="Best Obj."),
                       "Maximum Number of Satisfied Constraints"=which(g[,1]=="Min Const. Viol."),
                       "Minimum Departure from Constraint Satisfaction"=which(g[,1]=="Min Const. Dep."),
                       "All Criteria" = 1:sum(lg))
      tableCOptim <- tableCOptim[idrows,]
    }
    idO <- COformulation$idO
    idC <- COformulation$idC
    colnames(tableCOptim) <- c(DOE$xnamesvisu,DOE$ynamesvisu[idO],DOE$ynamesvisu[idC])
    
  }
  return(tableCOptim)
}

get.resCoptim.export <- function(DOE, COformulation, resCoptim, choiceOptimFailed) {
  idO <- COformulation$idO
  idC <- COformulation$idC
  nO <- length(idO)
  m <- COformulation$COobj
  bO <- matrix(rep(": Max",nO),ncol = nO)
  idmin <- which(m == -1)
  bO[idmin] <- ": Min"
  nC <- length(idC)
  threshconstraints <- matrix(as.matrix(COformulation$COt),ncol = nC)
  signconstraints <- matrix(as.matrix(COformulation$COsign),ncol = nC)
  bC <- matrix(NA,ncol = nC)
  for (j in 1:nC) {
    idsign <- (as.numeric(COformulation$COsign[j]) + 3)/2
    bC[j] <- paste0(tablesign[idsign],threshconstraints[j])
  }
  
  nh <- resCoptim$nh
  if (is.null(resCoptim$xcat)){
    if (nh==0){
      dataoptim <- as.data.frame(cbind(resCoptim$allxiffail,resCoptim$allfiffail,resCoptim$allhiffail))
      nr <- nrow(resCoptim$allxiffail)
      colnames(dataoptim) <- c(DOE$xnamesmenu,paste0(DOE$ynamesmenu[idO],bO),paste0(DOE$ynamesmenu[idC],bC))
      dataoptim <- cbind(dataoptim,Select=c(rep("Best Obj.",resCoptim$nbest),rep("Min Const. Viol.",resCoptim$nmaxnum),rep("Min Const. Dep.",resCoptim$ndepart)))
      export <- dataoptim
      
    }else{
      dataoptim <- as.data.frame(cbind(resCoptim$allx,resCoptim$allf,resCoptim$allh))
      nr <- nrow(resCoptim$allx)
      colnames(dataoptim) <- c(DOE$xnamesmenu,paste0(DOE$ynamesmenu[idO],bO),paste0(DOE$ynamesmenu[idC],bC))
      dataoptim <- cbind(dataoptim,Select=rep("Optim",nr))
      ptemp <- dataoptim[,c(paste0(DOE$ynamesmenu[idO],bO),paste0(DOE$ynamesmenu[idC],bC))]
      ptemp <- as.matrix(ptemp)
      ptemp <- cbind(-repmat(matrix(m,nrow=1),nr,1)*ptemp[,1:nO,drop=FALSE],repmat(matrix(signconstraints,nrow=1),nrow(ptemp),1)*(repmat(matrix(threshconstraints,nrow=1),nrow(ptemp),1) - ptemp[,-seq_len(nO),drop=FALSE]))
      idpareto <- row.names(paretoFilter(ptemp))
      datapareto <- dataoptim[idpareto,]
      datapareto$Select <- rep("Pareto",length(idpareto))
      dataoptim <- dataoptim[-na.exclude(as.numeric(idpareto)),]
      export <- rbind(dataoptim,datapareto)
    }
  }else{
    if (nh==0){
      dataoptim <- as.data.frame(cbind(resCoptim$allxiffail,resCoptim$allfiffail,resCoptim$allhiffail))
      nr <- nrow(resCoptim$allxiffail)
      colnames(dataoptim) <- c(DOE$xnamesmenu,paste0(DOE$ynamesmenu[idO],bO),paste0(DOE$ynamesmenu[idC],bC))
      dataoptim <- cbind(dataoptim,Select=c("Best Obj.","Min Const. Viol.","Min Const. Dep."))
      dataoptim <- cbind(dataoptim,Select=c(rep("Best Obj.",sum(resCoptim$nbest)),rep("Min Const. Viol.",sum(resCoptim$nmaxnum)),rep("Min Const. Dep.",sum(resCoptim$ndepart))))
      export <- dataoptim
      
    }else{
      dataoptim <- as.data.frame(cbind(resCoptim$allx,resCoptim$allf,resCoptim$allh))
      nr <- nrow(dataoptim)
      rownames(dataoptim) <- 1:nr
      colnames(dataoptim) <- c(DOE$xnames,paste0(DOE$ynamesmenu[idO],bO),paste0(DOE$ynamesmenu[idC],bC))
      dataoptim <- cbind(dataoptim,Select=rep("Optim",nr))
      ptemp <- dataoptim[,c(paste0(DOE$ynamesmenu[idO],bO),paste0(DOE$ynamesmenu[idC],bC))]
      ptemp <- as.data.frame(apply(ptemp, 2, function(col){
        as.numeric(as.character(col))}))
      ptemp <- as.matrix(ptemp)
      ptemp <- cbind(-repmat(matrix(m,nrow=1),nr,1)*ptemp[,1:nO,drop=FALSE],repmat(matrix(signconstraints,nrow=1),nrow(ptemp),1)*(repmat(matrix(threshconstraints,nrow=1),nrow(ptemp),1) - ptemp[,-seq_len(nO),drop=FALSE]))
      row.names(ptemp) <- 1:nr
      idpareto <- row.names(paretoFilter(ptemp))
      idpareto <- as.numeric(idpareto)
      idpareto <- idpareto[!is.na(idpareto)]
      datapareto <- dataoptim[idpareto,]
      datapareto$Select <- rep("Pareto",length(idpareto))
      dataoptim <- dataoptim[-na.exclude(as.numeric(idpareto)),]
      export <- rbind(dataoptim,datapareto)
    }
    
  }
  return(export)
}

computeConstrainedOptimWithoutCat <- function(dimx, lb, ub, predfun, COformulation, COalgo, inc, nmultistart, ranseed=0, callback) {
  allx <- matrix(NA,nrow = nmultistart,ncol = dimx)
  allf <- matrix(NA,nrow = nmultistart,ncol = 1)
  Xmultistart <- fOptions::runif.sobol(nmultistart,dimx, scrambling = 1)
  Xmultistart <- repmat(lb,nmultistart,1) + repmat(ub - lb,nmultistart,1)*Xmultistart 
  nC <- length(COformulation$idC)
  maxiter <- 1000
  fiter <- matrix(NA,nrow = nmultistart,ncol = maxiter*dimx*10)
  xiter <- array(NA,dim = c(nmultistart,maxiter*dimx*10,dimx))
  hiter <- array(NA,dim = c(nmultistart,maxiter*nC*10,nC))
  if (COalgo == "COBYLA") {
    opts <- list("algorithm" = "NLOPT_LN_COBYLA","maxeval" = maxiter,"xtol_rel" = 1.0e-8,"tol_constraints_ineq" = rep(1e-12,nC))
  }
  if (COalgo == "ISRES") {
    opts <- list("algorithm" = "NLOPT_GN_ISRES","xtol_rel" = 1.0e-8,"tol_constraints_ineq" = rep(1e-12,nC), "ranseed" = ranseed)
  }
  if (COalgo == "AUGLAG + COBYLA") {
    local_opts <- list("algorithm" = "NLOPT_LN_COBYLA","xtol_rel" = 1.0e-8)
    opts <- list("algorithm" = "NLOPT_LN_AUGLAG","maxeval" = maxiter,"xtol_rel" = 1.0e-8,"tol_constraints_ineq"=rep(1e-12,nC),"local_opts" = local_opts)
  }
  for (nm in 1:nmultistart) {
    iter <- 0
    fn <- function(x) {
      iter <<- iter + 1
      p <- -COformulation$COobj*predfun(matrix(x,ncol = dimx),COformulation$idO)
      fiter[nm,iter] <<- p
      xiter[nm,iter,] <<- matrix(x,ncol = dimx)
      return(p)
    }
    hn <- function(x){
      h <- numeric(nC)
      for (j in 1:nC) {
        predc <- predfun(matrix(x,ncol = dimx),COformulation$idC[j])
        h[j] <- as.numeric(-COformulation$COsign[j]*(predc - as.numeric(COformulation$COt[j])))
      }
      hiter[nm,iter,] <<- matrix(h,ncol = nC)
      return(h)
    }
    # Minimize
    if (COalgo != "SQP"){
      nlo <- try(
        nloptr(
          x0 = Xmultistart[nm,], 
          eval_f = fn, 
          eval_grad_f = NULL,
          lb = lb, 
          ub = ub, 
          eval_g_ineq = hn, 
          eval_jac_g_ineq = NULL,
          eval_g_eq = NULL, 
          eval_jac_g_eq = NULL,
          opts = opts
        ), TRUE)
      if (class(nlo) != "try-error") {
        allx[nm,] <- nlo$solution
        allf[nm,] <- nlo$objective
      }
    } else {
      x0 <- matrix(Xmultistart[nm,],ncol = 1)
      confun <- function(x){return(list(ceq = NULL,c = matrix(hn(x),ncol = 1)))}
      sqpo <- try(NlcOptim(X = x0, objfun = fn, confun = confun,
                           lb = matrix(lb,ncol = 1), ub = matrix(ub,ncol = 1)),TRUE)
      if (class(sqpo) != "try-error") {
        allx[nm,] <- sqpo$p
        allf[nm,] <- sqpo$fval
      }
    }
    callback(inc,nm)
  }
  
  # Store all iterations
  imax <- max(rowSums(!is.na(fiter)))
  resCoptim <- list()
  resCoptim$allfiter <- fiter[,1:imax]
  resCoptim$allxiter <- xiter
  resCoptim$allhiter <- hiter[,1:imax,,drop = F]
  
  # Sort optima among multistart
  allx <- as.data.frame(allx)
  allx <- allx[!is.na(allx[,1]),]
  allx <- allx[is.finite(allf),]
  allxu <- unique(round(allx,digits = 4),margin = 2)
  idu <- as.numeric(row.names(allxu))
  nu <- length(idu)
  allhu <- matrix(NA,nrow = nu,ncol = nC)
  for (i in 1:nu) {
    allhu[i,] <- hn(as.matrix(allxu[i,]))
  }
  idhrespect <- apply(allhu <= 0,1,all)
  resCoptim$idhrespect <- idhrespect
  nh <- sum(idhrespect)
  resCoptim$nh <- nh
  
  hreal <- function(x){
    h <- numeric(nC)
    for (j in 1:nC){
      predc <- predfun(matrix(x,ncol=dimx),COformulation$idC[j])
      h[j] <- predc
    }
    return(h)
  }
  
  # Stock results in case no optimum respects the constraints
  if (nh==0){
    # Identify among each iterations the best point with all criteria
    # Best objective
    idbest <- arrayInd(which.min(fiter),c(nmultistart,maxiter*dimx*10))
    fbest <- fiter[idbest]
    xbest <- xiter[idbest[1],idbest[2],]
    hbest <- hreal(xbest)
    # Maximum Number of Satisfied Constraints
    numconstr <- apply(hiter<=0,c(1,2),sum)
    maxnumconstr <- max(numconstr,na.rm=TRUE)
    idmax <- which(numconstr==maxnumconstr)
    fmax <- fiter
    fmax[-idmax] <- Inf
    idmaxnum <- arrayInd(which.min(fmax),c(nmultistart,maxiter*dimx*10))
    fmaxnum <- fiter[idmaxnum]
    xmaxnum <- xiter[idmaxnum[1],idmaxnum[2],]
    hmaxnum <- hreal(xmaxnum)
    # Minimum Departure from Constraint Satisfaction
    idneg <- which(hiter<=0)
    depart <- hiter^2
    depart[idneg] <- 0
    departcum <- apply(depart,c(1,2),sum)
    depart <- array(NA,c(nmultistart,imax,nC))
    idmindepart <- arrayInd(which.min(departcum),c(nmultistart,maxiter*dimx*10))
    fmindepart <- fiter[idmindepart]
    xmindepart <- xiter[idmindepart[1],idmindepart[2],]
    hmindepart <- hreal(xmindepart)
    
    allxiffail <- rbind(xbest,xmaxnum,xmindepart)
    rownames(allxiffail) <- 1:3
    allhiffail <- rbind(hbest,hmaxnum,hmindepart)
    rownames(allhiffail) <- 1:3
    allfiffail <- matrix(-COformulation$COobj*c(fbest,fmaxnum,fmindepart),ncol=1)
    rownames(allfiffail) <- 1:3
    
    resCoptim$allxiffail <- allxiffail
    resCoptim$allhiffail <- allhiffail
    resCoptim$allfiffail <- allfiffail
    resCoptim$nbest <- 1
    resCoptim$nmaxnum <- 1
    resCoptim$ndepart <- 1
    
  }else{
    allhu <- matrix(NA,nrow=nu,ncol=nC)
    for (i in 1:nu){
      allhu[i,] <- hreal(as.matrix(allxu[i,]))
    }
    allxh <- as.matrix(allxu[idhrespect,])
    rownames(allxh) <- 1:nh
    allfh <- matrix(-COformulation$COobj*allf[idu[idhrespect],1],ncol=1)
    rownames(allfh) <- 1:nh
    allhh <- matrix(allhu[idhrespect,],nrow=nh)
    rownames(allhh) <- 1:nh
    
    resCoptim$allx <- allxh
    resCoptim$allf <- allfh
    resCoptim$allh <- allhh
  }
  
  return(resCoptim)
}

computeConstrainedBiOptimWithoutCat <- function(dimx, lb, ub, predfun, COformulation) {
  popsize <- 100*dimx
  generations <- 100
  nO <- length(COformulation$idO)
  nC <- length(COformulation$idC)
  allx <- matrix(NA,nrow = popsize,ncol = dimx)
  allf <- matrix(NA,nrow = popsize,ncol = 2)
  fiter <- array(NA,dim = c(popsize,generations*10,2))
  xiter <- array(NA,dim = c(popsize,generations*10,dimx))
  hiter <- array(NA,dim = c(popsize,generations*10,nC))
  
  iter <- 0
  fn <- function(x) {
    iter <<- iter + 1
    f <- matrix(NA,nrow(x),2)
    for (j in 1:nO) {
      f[,j] <- -COformulation$COobj[j]*predfun(matrix(x,ncol = dimx),COformulation$idO[j])
    }
    fiter[,iter,] <<- f
    xiter[,iter,] <<- matrix(x,ncol = dimx)
    return(t(f))
  }
  hn <- function(x){
    h <- matrix(NA,nrow(x),nC)
    for (j in 1:nC) {
      predc <- predfun(matrix(x,ncol = dimx),COformulation$idC[j])
      h[,j] <- as.numeric(COformulation$COsign[j]*(predc - as.numeric(COformulation$COt[j]))) # Warning, NSGA2 expects h(x) >=0
    }
    hiter[,iter,] <<- h
    return(t(h))
  }
  r <- mco::nsga2(fn=fn, idim=dimx, odim=2, constraints = hn, cdim=nC,
             lower.bounds = as.numeric(lb), upper.bounds = as.numeric(ub), 
             popsize=popsize,generations = generations,vectorized = T)
  
  
  allx <- r$par
  allf <- r$value
  # Store all iterations
  imax <- max(rowSums(!is.na(apply(fiter,c(1,2),prod))))
  resCoptim <- list()
  resCoptim$allfiter <- fiter[,1:imax,]
  resCoptim$allxiter <- xiter[,1:imax,]
  resCoptim$allhiter <- -hiter[,1:imax,,drop = F]
  
  # Identify optima which respect the constraints
  allx <- as.data.frame(allx)
  allx <- allx[!is.na(allx[,1]),]
  allx <- allx[apply(is.finite(allf),1,all),]
  nx <- nrow(allx)
  allh <- -t(hn(as.matrix(allx)))
  idhrespect.temp <- which(apply(allh <= 0,1,all))
  
  idunique <- which(!duplicated(signif(allx[idhrespect.temp,],digits = 8)))
  idhrespect <- idhrespect.temp[idunique]
  allxu <- allx[idhrespect,]
  allhu <- allh[idhrespect,,drop=FALSE]
  allfu <- allf[idhrespect,]
  resCoptim$idhrespect <- (seq_len(nx) %in% idhrespect)
  nh <- length(idhrespect)
  resCoptim$nh <- nh
  
  hreal <- function(x){
    h <- matrix(NA,nrow(x),nC)
    for (j in 1:nC){
      predc <- predfun(matrix(x,ncol=dimx),COformulation$idC[j])
      h[,j] <- predc
    }
    return(h)
  }
  
  # Stock results in case no optimum respects the constraints
  if (nh==0){
    # Identify among the best points with all criteria
    # Best objectives
    allftemp <- fiter[,1:imax,]
    ntemp <- dim(allftemp)[1]
    allf <- matrix(NA,ntemp*imax,2)
    for (i in 1:2){
      allf[,i] <- c(allftemp[,,i])
    }
    row.names(allf) <- 1:nrow(allf)
    idpareto <- row.names(paretoFilterFast2obj(allf))
    idpareto <- as.numeric(idpareto)
    idpareto <- idpareto[!is.na(idpareto)]
    fbest <- allf[idpareto,]
    indpareto <- arrayInd(idpareto,c(ntemp,imax))
    npareto <- length(idpareto)
    xbest <- matrix(unlist(lapply(1:npareto,function(i) xiter[indpareto[i,1],indpareto[i,2],])),ncol=dimx,byrow = TRUE)
    hbest <- hreal(xbest)
    nbest <- nrow(xbest)
    # Maximum Number of Satisfied Constraints
    numconstr <- apply(hiter[,1:imax,]>=0,c(1,2),sum)
    maxnumconstr <- max(numconstr,na.rm=TRUE)
    if (maxnumconstr==0){
      fmaxnum <- NULL
      xmaxnum <- NULL
      hmaxnum <- NULL
      nmaxnum <- 0
    }else{
      idmax <- which(numconstr==maxnumconstr)
      indmax <- arrayInd(idmax,c(ntemp,imax))
      nmax <- length(idmax)
      fmax <- matrix(unlist(lapply(1:nmax,function(i) fiter[indmax[i,1],indmax[i,2],])),ncol=2,byrow = TRUE)
      row.names(fmax) <- 1:nrow(fmax)
      idpareto <- row.names(paretoFilterFast2obj(fmax))
      idpareto <- as.numeric(idpareto)
      idpareto <- idpareto[!is.na(idpareto)]
      npareto <- length(idpareto)
      fmaxnum <- matrix(unlist(lapply(1:npareto,function(i) fiter[indmax[idpareto[i],1],indmax[idpareto[i],2],])),ncol=2,byrow = TRUE)
      xmaxnum <- matrix(unlist(lapply(1:npareto,function(i) xiter[indmax[idpareto[i],1],indmax[idpareto[i],2],])),ncol=dimx,byrow = TRUE)
      hmaxnum <- hreal(xmaxnum)
      nmaxnum <- nrow(xmaxnum)
    }
    # Minimum Departure from Constraint Satisfaction
    idneg <- which(hiter[,1:imax,]>=0)
    depart <- hiter[,1:imax,]^2
    depart[idneg] <- 0
    departcum <- apply(depart,c(1,2),sum)
    idmindepart <- arrayInd(which.min(departcum),c(ntemp,imax))
    ndepart <- nrow(idmindepart)
    fdepart <- matrix(unlist(lapply(1:ndepart,function(i) fiter[idmindepart[i,1],idmindepart[i,2],])),ncol=2,byrow = TRUE)
    row.names(fdepart) <- 1:nrow(fdepart)
    idpareto <- row.names(paretoFilter(fdepart))
    idpareto <- as.numeric(idpareto)
    idpareto <- idpareto[!is.na(idpareto)]
    npareto <- length(idpareto)
    fmindepart <- matrix(unlist(lapply(1:npareto,function(i) fiter[idmindepart[idpareto[i],1],idmindepart[idpareto[i],2],])),ncol=2,byrow = TRUE)
    xmindepart <- matrix(unlist(lapply(1:npareto,function(i) xiter[idmindepart[idpareto[i],1],idmindepart[idpareto[i],2],])),ncol=dimx,byrow = TRUE)
    hmindepart <- hreal(xmindepart)
    nmindepart <- nrow(xmindepart)
    
    allxiffail <- as.data.frame(rbind(xbest,xmaxnum,xmindepart))
    rownames(allxiffail) <- c(paste(rep("Best",nbest),seq_len(nbest)),paste(rep("MaxNum",nmaxnum),seq_len(nmaxnum)),paste(rep("MinDepart",nmindepart),seq_len(nmindepart)))
    allhiffail <- rbind(hbest,hmaxnum,hmindepart)
    rownames(allhiffail) <- c(paste(rep("Best",nbest),seq_len(nbest)),paste(rep("MaxNum",nmaxnum),seq_len(nmaxnum)),paste(rep("MinDepart",nmindepart),seq_len(nmindepart)))
    allfiffail <- repmat(matrix(-COformulation$COobj,nrow=1),nbest+nmaxnum+1,1) * rbind(fbest,fmaxnum,fmindepart)
    rownames(allfiffail) <- c(paste(rep("Best",nbest),seq_len(nbest)),paste(rep("MaxNum",nmaxnum),seq_len(nmaxnum)),paste(rep("MinDepart",nmindepart),seq_len(nmindepart)))
    resCoptim$allxiffail <- allxiffail
    resCoptim$allhiffail <- allhiffail
    resCoptim$allfiffail <- allfiffail
    resCoptim$nbest <- nbest
    resCoptim$nmaxnum <- nmaxnum
    resCoptim$ndepart <- nmindepart
    
  }else{
    rownames(allfu) <- 1:nh
    idpareto <- row.names(paretoFilterFast2obj(allfu))
    idpareto <- as.numeric(idpareto)
    idpareto <- idpareto[!is.na(idpareto)]
    np <- length(idpareto)
    
    allxh <- allxu[idpareto,,drop=FALSE]
    rownames(allxh) <- 1:np
    allhh <- hreal(as.matrix(allxh))
    rownames(allhh) <- 1:np
    allfh <- repmat(matrix(-COformulation$COobj,nrow=1),np,1)*allfu[idpareto,]
    rownames(allfh) <- 1:np
    
    resCoptim$allx <- allxh
    resCoptim$allf <- allfh
    resCoptim$allh <- allhh
  }
  
  return(resCoptim)
}

computeConstrainedOptim <- function(dimx, DOE, Xinfos, lb, ub, predfun, COformulation, COalgo, nmultistart, ranseed=0, callback) {
  nC <- length(COformulation$idC)
  
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
      xcomplete[,num.index] <- x
      xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nx,1))
      y <- matrix(NA,nrow = 2,ncol = nrow(x))
      p <- predfun(xcomplete,id)
      return(p)
    }
    
    fiter <- hiter <- xiter <- allx <- allf <- allh <- fmin <- xmin <- xcat <- allxiffail <- allfiffail <- allhiffail <- nbest <- nmaxnum <- ndepart <- NULL
    idhrespect <- list()
    
    for (s in 1:nslices){
      
      incProgress(0, detail = paste("Categorical Combination", s,"/",nslices))
      currentlevels = as.matrix(levels[s,])
      currentxcat <- matrix(as.matrix(sapply(1:(dimx-nvar), function(ind){
        as.factor(unlist(Xinfos[[cat.index[ind]]]$levels)[currentlevels[ind]])
      })),nrow=1)
      
      ResOptimTemp <- computeConstrainedOptimWithoutCat(nvar, lb, ub, predfun = function(x,id){fn(x,currentxcat,num.index,cat.index,id)}, COformulation, COalgo, 1/nmultistart/nslices, nmultistart, ranseed=ranseed, callback)
      
      nxtemp <- nrow(ResOptimTemp$allx)
      
      if (!is.null(nxtemp)){
        
        xcomplete <- matrix(NA,nxtemp,dimx)
        xcomplete[,num.index] <- ResOptimTemp$allx
        xcomplete[,cat.index] <- rep(currentxcat,each=nxtemp)
        allx <- rbind(allx,xcomplete)
        
        fiter <- rbind(fiter,ResOptimTemp$allfiter)
        allf <- rbind(allf,ResOptimTemp$allf)

        hiter <- abind(hiter,ResOptimTemp$allhiter,along=1)
        allh <- rbind(allh,ResOptimTemp$allh)

        idhrespect <- c(idhrespect,list(ResOptimTemp$idhrespect))
        
      }else{
        nxtemp <- nrow(ResOptimTemp$allxiffail)
        xcomplete <- matrix(NA,nxtemp,dimx)
        xcomplete[,num.index] <- ResOptimTemp$allxiffail
        xcomplete[,cat.index] <- rep(currentxcat,each=nxtemp)
        allxiffail <- rbind(allxiffail,xcomplete)
        allx <- rbind(allx,matrix(NA,nmultistart,dimx))

        fiter <- rbind(fiter,ResOptimTemp$allfiter)
        allf <- rbind(allf,matrix(NA,nmultistart,1))
        allfiffail <- rbind(allfiffail,ResOptimTemp$allfiffail)
        
        hiter <- abind(hiter,ResOptimTemp$allhiter,along=1)
        allh <- rbind(allh,matrix(NA,nmultistart,nC))
        allhiffail <- rbind(allhiffail,ResOptimTemp$allhiffail)
        
        idhrespect <- c(idhrespect,list(rep(F,nmultistart)))
        nbest <- c(nbest,ResOptimTemp$nbest)
        nmaxnum <- c(nmaxnum,ResOptimTemp$nmaxnum)
        ndepart <- c(ndepart,ResOptimTemp$ndepart)
      }
      
      xcat <- rbind(xcat,matrix(rep(paste(DOE$xnames[cat.index],"=",currentxcat,collapse=", "),nmultistart),nmultistart))
    }
    
    # Store all iterations
    imax <- max(rowSums(!is.na(fiter)))
    resCoptim <- list()
    resCoptim$allfiter <- fiter[,1:imax]
    resCoptim$allhiter <- hiter[,1:imax,,drop = F]
    
    # Store all optima (whether they respect the constraints or not) 
    resCoptim$allxiffail <- allxiffail
    resCoptim$allfiffail <- allfiffail
    resCoptim$allhiffail <- allhiffail
    resCoptim$idhrespect <- idhrespect
    resCoptim$nh <- sum(unlist(resCoptim$idhrespect))
    resCoptim$nbest <- nbest
    resCoptim$nmaxnum <- nmaxnum
    resCoptim$ndepart <- ndepart
    
    
    # Store all optima which respect the constraints
    resCoptim$allx <- allx
    resCoptim$allf <- allf
    resCoptim$allh <- allh
    
    resCoptim$xcat <- xcat
    
  }else{
    resCoptim <- computeConstrainedOptimWithoutCat(dimx, lb, ub, predfun, COformulation, COalgo, 1/nmultistart, nmultistart, ranseed=ranseed, callback)
    resCoptim$xcat <- NULL
  }
  
  return(resCoptim)
}

computeConstrainedBiOptim <- function(dimx, DOE, Xinfos, lb, ub, predfun, COformulation, callback) {
  nC <- length(COformulation$idC)
  
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
      xcomplete[,num.index] <- x
      xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nx,1))
      y <- matrix(NA,nrow = 2,ncol = nrow(x))
      p <- predfun(xcomplete,id)
      return(p)
    }
    
    ids <- fiter <- hiter <- xiter <- allx <- allf <- allh <- fmin <- xmin <- xcat <- allxiffail <- allfiffail <- allhiffail <- nbest <- nmaxnum <- ndepart <- NULL
    idhrespect <- list()
    
    for (s in 1:nslices){
      
      incProgress(0, detail = paste("Categorical Combination", s,"/",nslices))
      currentlevels = as.matrix(levels[s,])
      currentxcat <- matrix(as.matrix(sapply(1:(dimx-nvar), function(ind){
        as.factor(unlist(Xinfos[[cat.index[ind]]]$levels)[currentlevels[ind]])
      })),nrow=1)
      
      ResOptimTemp <- computeConstrainedBiOptimWithoutCat(nvar, lb, ub, predfun = function(x,id){fn(x,currentxcat,num.index,cat.index,id)}, COformulation)
      nxtemp <- nrow(ResOptimTemp$allx)
      
      if (!is.null(nxtemp)){
        
        xcomplete <- as.data.frame(matrix(NA,nxtemp,dimx))
        xcomplete[,num.index] <- ResOptimTemp$allx
        xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nxtemp,1))
        allx <- rbind(allx,xcomplete)
        
        fiter <- abind(fiter,ResOptimTemp$allfiter,along=1)
        allf <- rbind(allf,ResOptimTemp$allf)
        
        hiter <- abind(hiter,ResOptimTemp$allhiter,along=1)
        allh <- rbind(allh,ResOptimTemp$allh)
        
        idhrespect <- c(idhrespect,list(ResOptimTemp$idhrespect))
        
      }else{
        nxtemp <- nrow(ResOptimTemp$allxiffail)
        xcomplete <- as.data.frame(matrix(NA,nxtemp,dimx))
        xcomplete[,num.index] <- ResOptimTemp$allxiffail
        xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nxtemp,1))
        allxiffail <- rbind(allxiffail,xcomplete)

        fiter <- abind(fiter,ResOptimTemp$allfiter,along=1)
        allfiffail <- rbind(allfiffail,ResOptimTemp$allfiffail)
        
        hiter <- abind(hiter,ResOptimTemp$allhiter,along=1)
        allhiffail <- rbind(allhiffail,ResOptimTemp$allhiffail)
        
        idhrespect <- c(idhrespect,rep(FALSE,nxtemp))
        nbest <- c(nbest,ResOptimTemp$nbest)
        nmaxnum <- c(nmaxnum,ResOptimTemp$nmaxnum)
        ndepart <- c(ndepart,ResOptimTemp$ndepart)
      }
      ids <- c(ids,rep(s,nxtemp))
      
      xcat <- rbind(xcat,matrix(paste(DOE$xnames[cat.index],"=",currentxcat,collapse=", "),1))
    }
    
    # Store all iterations
    imax <- max(rowSums(!is.na(apply(fiter,c(1,2),prod))))
    resCoptim <- list()
    resCoptim$allfiter <- fiter[,1:imax,]
    resCoptim$allhiter <- hiter[,1:imax,,drop = F]
    
    # Store all optima (whether they respect the constraints or not) 
    resCoptim$allxiffail <- allxiffail
    resCoptim$allfiffail <- allfiffail
    resCoptim$allhiffail <- allhiffail
    resCoptim$idhrespect <- idhrespect
    resCoptim$nh <- sum(unlist(resCoptim$idhrespect))
    resCoptim$nbest <- nbest
    resCoptim$nmaxnum <- nmaxnum
    resCoptim$ndepart <- ndepart
    
    # Store all optima which respect the constraints
    # after a final pareto filter 
    if (is.null(allf)){
      resCoptim$allx <- allx
      resCoptim$allf <- allf
      resCoptim$allh <- allh
      resCoptim$ids <- NULL
    }else{
      rownames(allf) <- 1:nrow(allf)
      idpareto <- row.names(paretoFilterFast2obj(allf))
      idpareto <- as.numeric(idpareto)
      idpareto <- idpareto[!is.na(idpareto)]
      resCoptim$allx <- allx[idpareto,,drop=FALSE]
      resCoptim$allf <- allf[idpareto,,drop=FALSE]
      resCoptim$allh <- allh[idpareto,,drop=FALSE]
      resCoptim$ids <- ids[idpareto]
    }
    
    resCoptim$xcat <- xcat
    
  }else{
    resCoptim <- computeConstrainedBiOptimWithoutCat(dimx, lb, ub, predfun, COformulation)
    resCoptim$xcat <- NULL
  }
  
  return(resCoptim)
}

plotConstrainedOptimBiObj <- function(DOE, COformulation, resCoptim) {
  
  ff <- as.data.frame(resCoptim$allf)
  colnames(ff) <- c("f1","f2")
  idO <- COformulation$idO
  if (is.null(resCoptim$xcat)){
    p <- layout(
      plot_ly(ff,x = ~f1, y = ~f2,mode = "markers",type = "scatter"),
      xaxis = list(title = DOE$ynamesmenu[idO[1]]),
      yaxis = list(title = DOE$ynamesmenu[idO[2]]),
      showlegend = FALSE,
      title = "Optimization Results - Objectives"
    )
  }else{
    ids <- resCoptim$ids
    ff$xcat <- resCoptim$xcat[ids,]
    p <- layout(
      plot_ly(ff,x = ~f1, y = ~f2,mode = "markers",color = ~as.factor(xcat), type = "scatter"),
      xaxis = list(title = DOE$ynamesmenu[idO[1]]),
      yaxis = list(title = DOE$ynamesmenu[idO[2]]),
      showlegend = TRUE,
      title = "Optimization Results - Objectives"
    )
  }
}

plotConstrainedOptimObj <- function(DOE, COformulation, resCoptim) {
  
  ff <- resCoptim$allfiter
  hh <- apply(resCoptim$allhiter<=0,c(1,2),all)
  idhh <- which(!hh)
  ff[!hh] <- NA
  ff <- - COformulation$COobj*ff
  nr <- nrow(ff)
  nc <- ncol(ff)
  cumfiter <- matrix(NA,nr,nc)
  for (i in 1:nr){
    idnotna <- !is.na(ff[i,])
    if (sum(idnotna)>0){
      nbidnotna <- which(idnotna)
      if (COformulation$COobj == -1) {
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

  idO <- COformulation$idO
  cumfiter <- cbind(cumfiter,NA)
  df <- melt(cumfiter)
  
  if (is.null(resCoptim$xcat)){
    dforder <- df[order(df$Var1),]
    p <- layout(
      plot_ly(df,y = ~value,mode = "lines",split = ~Var1, showlegend = F, type = "scatter"),
      xaxis = list(title = "Iterations"),
      yaxis = list(title = DOE$ynamesmenu[idO]),
      showlegend = FALSE,
      title = "Optimization Results - Objective"
    )
  }else{
    niter <- ncol(cumfiter)
    xcat <- rep(resCoptim$xcat,niter)
    df$xcat <- xcat
    dforder <- df[order(df$Var1),]
    p <- layout(
      plot_ly(dforder,x=~Var2, y = ~value,mode = "lines",color = ~as.factor(xcat), showlegend = T, type = "scatter"),
      xaxis = list(title = "Iterations"),
      yaxis = list(title = DOE$ynamesmenu[idO]),
      showlegend = TRUE,
      title = "Optimization Results - Objective"
    )
  }
}

plotConstrainedOptimConstr <- function(DOE, COformulation, resCoptim) {
  
  idC <- COformulation$idC
  Probability <- t(apply(resCoptim$allhiter <= 0,c(2,3),mean,na.rm = T))
  vals <- unique(scales::rescale(c(c(Probability),0,1)))
  o <- order(vals, decreasing = FALSE)
  cols <- scales::col_numeric("Blues", domain = NULL)(vals)
  colz <- setNames(data.frame(vals[o], cols[o]), NULL)

  p <- layout(
    plot_ly(z = Probability, y = DOE$ynamesvisu[idC], colorscale = colz, type = "heatmap", zmin = 0,zmax = 1),
    xaxis = list(title = "Iterations"),
    yaxis = list(title = "Constraint Satisfaction"),
    showlegend = TRUE,
    title = "Optimization Results - Constraints"
  )
}

constrainedSolve.ui <- function(id) {
  ns <- NS(id)
  
  panel <- wellPanel(
    actionButton(ns("go"), "Optimize", icon = icon("chart-bar"), class = "btn-primary"),
    hr(),
    h6("Please select what to display & export if no optimum respecting all constraints was found."),
    selectInput(
      ns("OptimFailed"), 
      label = "Select Filter",
      choices = list("Best Objective","Maximum Number of Satisfied Constraints","Minimum Departure from Constraint Satisfaction","All Criteria"),
      selected = "All Criteria"
    ),
    br(),
    downloadButton(ns("download"), "Export Optimization Results", class = "btn-info"),
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
  
  rowSetting <- fluidRow(
    column(4, panel), 
    column(8, uiOutput(ns("preview.dynui")))
  )
  
  visu <- bsCollapse(
    id = ns("collapse"), multiple = TRUE,
    bsCollapsePanel(
      "Graphical Representation",
      fluidRow(
        column(6, plotlyOutput(ns("plotObj"))), 
        column(6, plotlyOutput(ns("plotConstr")))
      )
    ),
    bsCollapsePanel(
      "Table Representation", DT::dataTableOutput(ns('table'))
    )
  )

  modalLaunchSimu <- bsModal(ns("modalLaunchSimu"), "Choose Mode to Launch Additional Simulations", NULL,
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
  
  tagList(
   rowSetting, visu, modalLaunchSimu,
   # impose seed for tests
   conditionalPanel(
     condition = "false",
     numericInput(ns('ranseed'), label = "Seed for constrained optim", value =  0)
   ),
   # To open and close visu panels in tests
   conditionalPanel(
     condition = "false",
     selectInput(
       ns("openCollapse"),
       label = "Open Panel:",
       choices = c("",
                   "Graphical Representation",
                   "Table Representation"),
       selected = ""
     )
   ),
   conditionalPanel(
     condition = "false",
     selectInput(
       ns("closeCollapse"),
       label = "Close Panel:",
       choices = c("",
                   "Graphical Representation",
                   "Table Representation"),
       selected = ""
     )
   )
  )
}

constrainedSolve.server <- function(id, DOE, listmodels, persistence, Xinfos, COformulation, settings, doeProblemDef) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      use_simulator <- reactive({
        bool <- FALSE
        if (!is.null(doeProblemDef$choice)){
          bool <- (doeProblemDef$choice != 1)
        }
        return(bool)
      })

      resCoptim <- reactiveValues(
        allfiter = NULL, allxiter = NULL, allhiter = NULL, 
        allf = NULL, allx = NULL, allh = NULL, idhrespect = NULL, 
        allfiffail = NULL, allxiffail = NULL, allhiffail = NULL, 
        fmin = NULL, xmin = NULL, export = NULL, table = NULL, xcat = NULL
      )
      
      # To open and close visu panels in tests
      observeEvent(input$openCollapse,
                   {
                     updateCollapse(session,
                                    "collapse",
                                    open = input$openCollapse)
                   })
      observeEvent(input$closeCollapse,
                   {
                     updateCollapse(session,
                                    "collapse",
                                    close = input$closeCollapse)
                   })
      
      observeEvent(persistence$updatingStep, {
        if (persistence$updatingStep == "constrainedSolve") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))
          
          if (!is.null(persistence$loadedStudy$constrOptim$resCoptim$allfiter)) {
            resCoptim$allfiter <- persistence$loadedStudy$constrOptim$resCoptim$allfiter
            resCoptim$allxiter <- persistence$loadedStudy$constrOptim$resCoptim$allxiter
            resCoptim$allhiter <- persistence$loadedStudy$constrOptim$resCoptim$allhiter
            resCoptim$ids <- persistence$loadedStudy$constrOptim$resCoptim$ids
            resCoptim$allf <- persistence$loadedStudy$constrOptim$resCoptim$allf
            resCoptim$allx <- persistence$loadedStudy$constrOptim$resCoptim$allx
            resCoptim$allh <- persistence$loadedStudy$constrOptim$resCoptim$allh
            resCoptim$idhrespect <- persistence$loadedStudy$constrOptim$resCoptim$idhrespect
            resCoptim$nh <- persistence$loadedStudy$constrOptim$resCoptim$nh
            resCoptim$allfiffail <- persistence$loadedStudy$constrOptim$resCoptim$allfiffail
            resCoptim$allxiffail <- persistence$loadedStudy$constrOptim$resCoptim$allxiffail
            resCoptim$allhiffail <- persistence$loadedStudy$constrOptim$resCoptim$allhiffail
            resCoptim$nbest <-persistence$loadedStudy$constrOptim$resCoptim$nbest
            resCoptim$nmaxnum <- persistence$loadedStudy$constrOptim$resCoptim$nmaxnum
            resCoptim$ndepart <- persistence$loadedStudy$constrOptim$resCoptim$ndepart
            resCoptim$fmin <- persistence$loadedStudy$constrOptim$resCoptim$fmin
            resCoptim$xmin <- persistence$loadedStudy$constrOptim$resCoptim$xmin
            resCoptim$export <- persistence$loadedStudy$constrOptim$resCoptim$export
            resCoptim$table <- persistence$loadedStudy$constrOptim$resCoptim$table
            resCoptim$xcat <- persistence$loadedStudy$constrOptim$resCoptim$xcat
            resCoptim$OptimFailed <- persistence$loadedStudy$constrOptim$resCoptim$OptimFailed
            
            updatePickerInput(session = session, inputId = "OptimFailed", selected = resCoptim$OptimFailed)
          }
          progressToNextStep(persistence)
        }
      }, priority = -1) # Reduce priority (each updating step must be done after any consequences of its previous updating step)
      
      observe({
        req(COformulation$haschanged)
        if (COformulation$haschanged>0){
          # An optimization has already been performed
          # We check if the formulation has changed, if this is the case
          # we remove the previous optimization results
          if (!is.null(isolate(resCoptim$allfiter))){
            resCoptim$allfiter <- NULL
            resCoptim$allxiter <- NULL
            resCoptim$allhiter <- NULL
            resCoptim$ids <- NULL
            resCoptim$allf <- NULL
            resCoptim$allx <- NULL
            resCoptim$allh <- NULL
            resCoptim$idhrespect <- NULL
            resCoptim$nh <- NULL
            resCoptim$allfiffail <- NULL
            resCoptim$allxiffail <- NULL
            resCoptim$allhiffail <- NULL
            resCoptim$nbest <-NULL
            resCoptim$nmaxnum <- NULL
            resCoptim$ndepart <- NULL
            resCoptim$fmin <- NULL
            resCoptim$xmin <- NULL
            resCoptim$xcat  <- NULL
            resCoptim$export <- NULL
            resCoptim$table <- NULL
            updateCollapse(session, "collapse", close = c("Graphical Representation","Table Representation"))
          }
        }
      }, priority = 10)
      
      observeEvent(input$go, {
        req(COformulation$idO,listmodels$finalpredfun, Xinfos)
        nobj <- length(COformulation$idO)
        nmultistart <- settings$nmultistart
        predfun <- listmodels$finalpredfun
        dimx <- DOE$nX
        Xbounds <- get.bounds(Xinfos$Xinfos)
        lb <- Xbounds[1,,drop=F]
        ub <- Xbounds[2,,drop=F]
        callback <- function(inc,i) {
          incProgress(inc, detail = paste("Multistart", i,"/",nmultistart))
        }
        withProgress(message = 'Optimizing...', value = 0, {
          if (nobj==1){
            newResCoptim <- computeConstrainedOptim(
              dimx, DOE, Xinfos$Xinfos, matrix(as.numeric(lb),1), matrix(as.numeric(ub),1), predfun, COformulation, settings$COalgo, nmultistart, ranseed=input$ranseed, callback
            )
          }else{
            newResCoptim <- computeConstrainedBiOptim(
              dimx, DOE, Xinfos$Xinfos, matrix(as.numeric(lb),1), matrix(as.numeric(ub),1), predfun, COformulation
            )
          }
          resCoptim$allfiter <- newResCoptim$allfiter
          resCoptim$allxiter <- newResCoptim$allxiter
          resCoptim$allhiter <- newResCoptim$allhiter
          resCoptim$ids <- newResCoptim$ids
          resCoptim$allf <- newResCoptim$allf
          resCoptim$allx <- newResCoptim$allx
          resCoptim$allh <- newResCoptim$allh
          resCoptim$idhrespect <- newResCoptim$idhrespect
          resCoptim$nh <- newResCoptim$nh
          resCoptim$allfiffail <- newResCoptim$allfiffail
          resCoptim$allxiffail <- newResCoptim$allxiffail
          resCoptim$allhiffail <- newResCoptim$allhiffail
          resCoptim$nbest <- newResCoptim$nbest
          resCoptim$nmaxnum <- newResCoptim$nmaxnum
          resCoptim$ndepart <- newResCoptim$ndepart
          resCoptim$fmin <- newResCoptim$fmin
          resCoptim$xmin <- newResCoptim$xmin
          resCoptim$xcat  <- newResCoptim$xcat
          resCoptim$export <- get.resCoptim.export(DOE, COformulation, resCoptim, isolate(input$OptimFailed))
          resCoptim$OptimFailed <- isolate(input$OptimFailed)
        })

        if (resCoptim$nh > 0 & nobj == 1) {
          # Order results by 'allf' (affect order of 'Proposed confirmation runs')
          fOrder <- order(resCoptim$allf, decreasing = (COformulation$COobj == 1))
          resCoptim$allf <- resCoptim$allf[fOrder,]
          resCoptim$allx <- resCoptim$allx[fOrder,]
          resCoptim$allh <- resCoptim$allh[fOrder,]
        }

        tabletemp  <- get.resCoptim.table(DOE, COformulation, resCoptim, input$OptimFailed)
        resCoptim$table <- tabletemp
        resCoptim$OF <- NULL
        
        # Functional outputs
        if(length(DOE$Yinfos$func.ids) > 0){
          
          OFnames <- c(colnames(DOE$OF), colnames(DOE$OFtot))
          OFnames <- OFnames[!(OFnames %in% DOE$ynamesmenu[unique(c(COformulation$idC, COformulation$idO))])]
          
          allx <- if (resCoptim$nh > 0) resCoptim$allx else resCoptim$allxiffail
          resCoptim$OF <- data.frame(matrix(nrow = nrow(allx), ncol = 0))
          
          for (OF in OFnames){
            OFidx <- which(DOE$ynamesmenu == OF)
            resCoptim$OF[OF] <- predfun(allx, OFidx)
          }
          
          # Update export field
          resCoptim$export <- cbind(resCoptim$export, resCoptim$OF)
          # Change order to keep "Select" column at the end
          colIDs <- seq_len(length(colnames(resCoptim$export)))
          selID <- which(colnames(resCoptim$export)=="Select")
          resCoptim$export <- resCoptim$export[, c(colIDs[-selID], selID)] 
          
        }
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "constrainedSolve-go"
        
        # Open visualization panel
        updateCollapse(session, "collapse", open = "Graphical Representation")
      }) 
      
      output$plotObj <- renderPlotly({
        req(COformulation$haschanged, cancelOutput = TRUE)
        
        if (any(is.null(resCoptim$allfiter),
                is.null(resCoptim$allhiter),
                is.null(resCoptim$nh))){
          NULL
        }else{
          if (length(COformulation$idO)==1){
            validate(need(resCoptim$nh>0,
                          paste("For all multistart optimizations, ", 
                                "no iteration respects all constraints. ",
                                "Therefore, objective function evolution cannot be displayed.",sep="")))
            plotConstrainedOptimObj(DOE, COformulation, resCoptim)
          }else{
            validate(need(resCoptim$nh>0,
                          paste("No solution satisfies all constraints, ", 
                                "Therefore, the Pareto Front cannot be displayed.",sep="")))
            plotConstrainedOptimBiObj(DOE, COformulation, resCoptim)
          }
        }
      })
      
      output$plotConstr <- renderPlotly({
        req(COformulation$haschanged, cancelOutput = TRUE)
        if (any(is.null(resCoptim$allfiter),
                is.null(resCoptim$allhiter))){
          NULL
        }else{
          plotConstrainedOptimConstr(DOE, COformulation, resCoptim)
        }
      })
      
      output$table  <- DT::renderDataTable({
        req(COformulation$haschanged)
        # View optim results as dataframe
        df <- resCoptim$table
        if(!is.null(df)){
          # Functional outputs
          if(!is.null(resCoptim$OF)){
            df <- cbind(df, resCoptim$OF)
          }
          
          DT::datatable(
            df, escape = FALSE,
            extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
            options = list(
              dom = 'Brtip', buttons = list(list(extend = 'colvis', columns = 1:ncol(df))), 
              scrollX = TRUE,scrollY = 400,scroller = TRUE, fixedColumns = TRUE
            ))
        }else{
          NULL
        }
        
      })
      
      output$download <- downloadHandler(
        filename = 'ConstrainedOptimizationResults.csv',
        content = function(con) {
          # Save optim results in special format for export
          # exporttemp <- get.resCoptim.export(DOE, COformulation, resCoptim, input$OptimFailed)
          write.table(x = resCoptim$export, file = con, row.names = F, col.names = T, sep = ",")
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
      
      observeEvent(list(resCoptim$allx, resCoptim$table), {
        if (is.null(resCoptim$allx) && is.null(resCoptim$table)) {
          shinyjs::disable("generate")
        }
        else {
          shinyjs::enable("generate")
        }
      })
      
      observeEvent(input$generate, {
        req(!is.null(resCoptim$allx) || !is.null(resCoptim$table))
        simulations$Xadd <- addConfPointsFromConstrOptim(DOE, resCoptim, input$nadd)
        if (use_simulator()){
          if (simulations$tagDOE == input$tagDOE){
            simulations$tagDOE <- paste("Confirm Constrained Optim", simulations$nConf)
          }else{
            simulations$tagDOE <- input$tagDOE
          }
          simulations$nConf <- simulations$nConf + 1
        }
      })

      output$use_simulator <- use_simulator
      outputOptions(output, 'use_simulator', suspendWhenHidden = FALSE)
      
      simulations = reactiveValues(Xadd = NULL, mode.manual = NULL, mode.automatic = NULL, 
                                    tagDOE = "Confirm Constrained Optim 1", nConf = 1)
      
      observeEvent(list(listmodels$selected, resCoptim$allx, resCoptim$table), {
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
        req(!is.null(resCoptim$OF))
        
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
      
      return(list(resCoptim = resCoptim, simulations = simulations))
    }
  )
}
