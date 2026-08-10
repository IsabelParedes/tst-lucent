#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

source("modules/shared/XinfosChange.R", local = TRUE)


disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

enableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}

# New input file (Q2 test file / PCP/ prediction)
check.header <- function(DOE, datapath, separator, decimal, nY = 0){
  
  error.msg = list()
  xynames <- unlist(strsplit(readLines(datapath, n = 1), separator))
  xynames <- gsub('\"', '', xynames)
  doe.names <- if (nY > 0){c(DOE$xnamesmenu, DOE$ynamesmenu)}else{DOE$xnamesmenu}
  header <- all(doe.names %in% xynames) & length(unique(xynames)) == length(xynames)
  if (!header){
    error.msg$header <- paste('Input file must have (unique) column names. The following names are expected: ',
                               paste0(doe.names, collapse = ' '))
  }
  return(list(valid = header, error.msg = error.msg))
  
}
check.new.data <- function(nX, Xinfos, newData, nY = 0){

  error.msg = list()
  valid.ncol <- (ncol(newData) == nX + nY)
  if (!valid.ncol){
    error.msg$ncol <- 'Wrong number of inputs'
    if (nY > 0){
      error.msg$ncol <- 'Wrong number of inputs/outputs'
    }
  }else{
    valid.values <- all(sapply(1:ncol(newData), function(i){
      if (i <= nX){
        if (Xinfos[[i]]$type == 'categorical'){
          all(newData[,i] %in% Xinfos[[i]]$levels, na.rm=TRUE)
        }else{
          # At least one value is not NA
          suppressWarnings(any(!is.na(lapply(as.character(newData[,i]), as.numeric))))
        }
      }else{
        # At least one value is not NA
        suppressWarnings(any(!is.na(lapply(as.character(newData[,i]), as.numeric))))
      }
    }))
    if (!valid.values){
      error.msg$values <- 'Imported values are not consistent with DOE definition : non-existing levels or non-numeric values.'
    }
  }
  
  return(list(valid = (length(error.msg) == 0), error.msg = error.msg))
}
get.new.data.from.file <- function(DOE, datapath, separator, decimal, nY = 0) {

  d <- read.csv(datapath, header = TRUE, check.names = F, sep = separator, dec = decimal)
  doe.names <- if (nY > 0){c(DOE$xnames, DOE$ynames)}else{DOE$xnames}
  d <- d[doe.names]
  return(d)
  
}

# Useful functions
repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}
dchi2LHS <- function(x,n,min,max){
  nc <- ncol(x)
  if (length(min) == 1) min <- rep(min,nc)
  if (length(max) == 1) max <- rep(max,nc)
  
  d <- matrix(NA,ncol=nc)
  for (i in 1:nc){
    bb <- seq(min[i],max[i],length.out=n+1)
    hh <- hist(x[,i],breaks=bb,plot=F)
    d[i] <- sum((hh$counts-1)^2)
  }
  return(d)
}
discrepancyL2centered <- function(X){
  X <- as.matrix(X)
  dimension <- dim(X)[2]
  n <- dim(X)[1]
  s1 <- sum(apply((1 + 0.5 * abs(X - 0.5) - 0.5 * ((abs(X - 0.5))^2)),1,prod))
  s2 <- 0
  for (i in 1:n) {
    for (k in 1:n) {
      q <- prod((1 + 0.5 * abs(X[i, ] - 0.5) + 0.5 * abs(X[k, ] - 0.5) - 0.5 * abs(X[i, ] - X[k,])))
      s2 <- s2 + q
    }
  }
  DisC2 <- sqrt(((13/12)^dimension) - ((2/n) *s1) + ((1/n^2) * s2))
  return(DisC2)
}
maxprocrit <- function(x){
  nc <- ncol(x)
  ns <- nrow(x)
  xtemp <- array(NA,c(ns,ns,nc))
  for (l in 1:nc){
    xtemp[,,l] <- repmat(matrix(x[,l],ncol=1),1,ns) - repmat(matrix(x[,l],nrow=1),ns,1)
  }
  p <- apply(1/xtemp^2,c(1,2),prod)
  s <- sum(p[upper.tri(p)])*choose(ns,2)
  return(log(s)/nc)
}
generateUQ.univariate <- function(UQparams,n,Xinfos,lb,ub){
  if (Xinfos$type == 'numeric'){
    if (UQparams$typeDistr=="kde"){
      sampleUQ <- rkde(n, UQparams$struct)
    }else{
      l <- list(n)
      if (UQparams$typeDistr=="truncnorm"){
        l <- c(l, list(as.numeric(UQparams$P3Distr), as.numeric(UQparams$P4Distr)))
      }
      l <- c(l, list(as.numeric(UQparams$P1Distr)))
      if (!is.na(UQparams$P2Distr)){
        l[[3]] <- as.numeric(UQparams$P2Distr)
      }
      # Special Treatment for Beta distribution
      if (UQparams$typeDistr=="beta"){
        a <- lb
        b <- ub-lb
      }else{
        a <- 0
        b <- 1
      }
      sampleUQ <- a+b*do.call(paste("r",UQparams$typeDistr,sep=""),l)
    }
  }else{
    sampleUQ <- sample(unlist(UQparams$levels), size = n, replace = TRUE, prob = unlist(UQparams$weights))
  }
  return(sampleUQ)
}
generateUQ.univariate.inverseCDF <- function(UQparams,u,Xinfos,lb,ub){
  # Only for numerical variables 
  if (UQparams$typeDistr=="kde"){
    sampleUQ <- qkde(u, UQparams$struct)
  }else{
    l <- list(u)
    if (UQparams$typeDistr=="truncnorm"){
      l <- c(l, list(as.numeric(UQparams$P3Distr), as.numeric(UQparams$P4Distr)))
    }
    l <- c(l, list(as.numeric(UQparams$P1Distr)))
    if (!is.na(UQparams$P2Distr)){
      l[[3]] <- as.numeric(UQparams$P2Distr)
    }
    # Special Treatment for Beta distribution
    if (UQparams$typeDistr=="beta"){
      a <- lb
      b <- ub-lb
    }else{
      a <- 0
      b <- 1
    }
    sampleUQ <- a+b*do.call(paste("q",UQparams$typeDistr,sep=""),l)
  }
  return(sampleUQ)
}
generateUQ <- function(UQparams,n,DOE){
  listCopulas <- UQparams$listCopulas
  UQparams <- UQparams$UQparams
  dimx <- DOE$nX
  Xinfos <- DOE$Xinfos
  Xbounds <- get.bounds(Xinfos)
  categorical <- which(sapply(DOE$Xinfos, function(var){var$type}) %in% c('constant', 'categorical'))
  numerical <- setdiff(1:dimx,categorical)
  lb <- ub <- numeric(dimx)
  lb[numerical] <- Xbounds[1,,drop=F]
  ub[numerical] <- Xbounds[2,,drop=F]
  sampleUQ <- as.data.frame(matrix(0,nrow=n,ncol=dimx))
  if (all(!listCopulas$inputs)){
    # No dependence
    for (i in 1:dimx){
      sampleUQ[,i] <- generateUQ.univariate(UQparams[[i]],n,Xinfos[[i]],lb[i],ub[i])
    }
  }else{
    # Dependence
    # Step 1: generate pseudo-observations with the copulas
    groups <- listCopulas$groups
    id.copulas <- which(groups!="0")
    unique.groups <- listCopulas$unique.groups
    for (k in unique.groups){
      id.group <- which(groups==k)
      if (listCopulas$typeCopulas[as.numeric(k)]=="Vine"){
        sampleUQ[,id.group] <- rvinecop(n, listCopulas$Copulas[[k]])
      }else{
        sampleUQ[,id.group] <- rCopula(n, listCopulas$Copulas[[k]])
      }
    }
    # Step 2: Inverse CDF on the pseudo-observations
    for (i in id.copulas){
      sampleUQ[,i] <- generateUQ.univariate.inverseCDF(UQparams[[i]],
                                                             sampleUQ[,i],Xinfos[[i]],
                                                             lb[i],ub[i])
    }
    # Step 3: generate rest of variables which are not dependent
    id.notcopulas <- setdiff(1:dimx,id.copulas)
    for (i in id.notcopulas){
      sampleUQ[,i] <- generateUQ.univariate(UQparams[[i]],n,Xinfos[[i]],lb[i],ub[i])
    }
  }
  colnames(sampleUQ) <- DOE$xnames
  return(sampleUQ)
}
generateXtest <- function(Xbounds,n,DOE){
  dimx <- DOE$nX
  Xinfos <- DOE$Xinfos
  sampleTest <- as.data.frame(matrix(0,nrow=n,ncol=dimx))
  colnames(sampleTest) <- DOE$xnames
  categorical <- which(sapply(DOE$Xinfos, function(var){var$type}) %in% c('constant', 'categorical'))
  levels.models <- lapply(DOE$Xinfos, function(Xinfos){Xinfos$levels})
  numerical <- setdiff(1:dimx,categorical)
  lb <- Xbounds[1,,drop=F]
  ub <- Xbounds[2,,drop=F]
  Xtestnum <- runif.sobol(n, length(numerical))
  Xtestnum <- repmat(lb, n, 1) + repmat(ub - lb, n, 1)*Xtestnum
  sampleTest[,numerical] <- Xtestnum
  for (i in 1:dimx){
    if (Xinfos[[i]]$type == 'categorical'){
      sampleTest[,i] <- sample(unlist(levels.models[[i]]), size = n, replace = TRUE)
    }
  }
  return(sampleTest)
}

rbf_hsic <- function(x,param){
  d <- as.matrix(dist(x))/param
  return(exp(-0.5*d^2))
}

rbf_hsic_2 <- function(x,y,param){
  d <- outer(x,y,"-")/param
  return(exp(-0.5*d^2))
}

MMD <- function(X,Y,kernel,param,...){
  nxobs <- nrow(X)
  nyobs <- nrow(Y)
  p <- ncol(X)
  KXX <- 1
  for (i in 1:p){
    KXX <- KXX*do.call(get(kernel[i]), list(x=as.matrix(X[,i]),param=param[i],...))
  }
  KYY <- 1
  for (i in 1:p){
    KYY <- KYY*do.call(get(kernel[i]), list(x=as.matrix(Y[,i]),param=param[i],...))
  }
  KXY <- 1
  for (i in 1:p){
    KXY <- KXY*do.call(get(paste(kernel[i],"_2",sep="")), list(x=as.matrix(X[,i]),y=as.matrix(Y[,i]),param=param[i],...))
  }
  mxx <- sum(2*c(KXX[lower.tri(KXX, diag = FALSE)]))/nxobs/(nxobs-1)
  myy <- sum(2*c(KYY[lower.tri(KYY, diag = FALSE)]))/nyobs/(nyobs-1)
  mxy <- 2*sum(c(KXY))/nxobs/nyobs
  return(list(estimate=mxx+myy-mxy,estimateX=mxx,estimateY=myy,estimateXY=mxy))
}

# For fast pareto filtering with 2 objectives only
paretoFilterFast2obj <- function(d){
  if (!is.data.frame(d)) d <- as.data.frame(d)
  colnames(d) <- c('x','y')
  D = d[order(d$x,d$y,decreasing=FALSE),]
  front = D[which(!duplicated(cummin(D$y))),]
  return(front)
}
