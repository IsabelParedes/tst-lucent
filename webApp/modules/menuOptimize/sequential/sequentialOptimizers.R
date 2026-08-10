#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

available.seq.optimizers <- function(){
  l <- c("EGO","RS")
  return(l)
}

computeEI <- function(Xtest, dimx, objs, boundsO, boundsC, sdreweightedloo = FALSE){
  
  p <- predict.metamodel(objs[[1]], Xtest, sdreweightedloo)
  mu <- p$mean
  sig <- p$sd
  fmin <- switch(objs[[1]]$type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@output))
  amin <- boundsO[1]
  bmin <- min(fmin,boundsO[2])
  ei <- (fmin-mu)*(dnorm(bmin,mu,sig)-dnorm(amin,mu,sig)) + sig*(pnorm(bmin,mu,sig)-pnorm(amin,mu,sig))
  ei[is.nan(ei)] <- 0
  return(ei)
  
}


acquisition.function <- function(dimx, objs, minimize, idconstr, boundsO, boundsC, sdreweightedloo = FALSE, UCB = FALSE){
  
  type.metamodel <- objs[[1]]$type.metamodel
  
  if (UCB){
    if (minimize){
      "ucb.min"
      fun.crit <- function(x){
        p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
        mu <- p$mean
        sig <- p$sd
        ucb <- mu - 3*sig
        ucb[is.nan(ucb)] <- 0
        return(ucb)
      }
    }else{
      "ucb.max"
      fun.crit <- function(x){
        p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
        mu <- p$mean
        sig <- p$sd
        ucb <- mu + 3*sig
        ucb[is.nan(ucb)] <- 0
        return(-ucb)
      }
    }
  }else{
    if (length(idconstr)>0){
      if (minimize){
        "efi.min"
        fmin <- switch(type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@output))
        amin <- boundsO[1]
        bmin <- min(fmin,boundsO[2])
        fun.crit <- function(x){
          p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
          mu <- p$mean
          sig <- p$sd
          ei <- (fmin-mu)*(dnorm(bmin,mu,sig)-dnorm(amin,mu,sig)) + sig*(pnorm(bmin,mu,sig)-pnorm(amin,mu,sig))
          ei[is.nan(ei)] <- 0
          
          nc <- length(idconstr)
          proba <- 1
          for (j in 1:nc){
            p <- predict.metamodel(objs[[idconstr[j]]],x,sdreweightedloo)
            mu <- p$mean
            sig <- p$sd
            probatemp <- pnorm(boundsC[2,j],mu,sig) - pnorm(boundsC[1,j],mu,sig)
            proba <- proba*probatemp
          }
          efi <- ei*proba
          return(-efi)
        }
      }else{
        "efi.max"
        fmax <- switch(type.metamodel,Lasso=max(objs[[1]]$Y),Acosso1=max(objs[[1]]$model$acosso$y),Acosso2=max(objs[[1]]$model$acosso$y),Kriging=max(objs[[1]]$model@output))
        amax <- max(fmax,boundsO[1])
        bmax <- boundsO[2]
        fun.crit <- function(x){
          p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
          mu <- p$mean
          sig <- p$sd
          ei <- (mu-fmax)*(dnorm(bmax,mu,sig)-dnorm(amax,mu,sig)) - (pnorm(bmax,mu,sig)-pnorm(amax,mu,sig))
          ei[is.nan(ei)] <- 0
          
          nc <- length(idconstr)
          proba <- 1
          for (j in 1:nc){
            p <- predict.metamodel(objs[[idconstr[j]]],x,sdreweightedloo)
            mu <- p$mean
            sig <- p$sd
            probatemp <- pnorm(boundsC[2,j],mu,sig) - pnorm(boundsC[1,j],mu,sig)
            proba <- proba*probatemp
          }
          efi <- ei*proba
          return(-efi)
        }
      }
    }
    else{
      if (minimize){
        "ego.min"
        fmin <- switch(type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@output))
        amin <- boundsO[1]
        bmin <- min(fmin,boundsO[2])
        fun.crit <- function(x){
          p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
          mu <- p$mean
          sig <- p$sd
          ei <- (fmin-mu)*(dnorm(bmin,mu,sig)-dnorm(amin,mu,sig)) + sig*(pnorm(bmin,mu,sig)-pnorm(amin,mu,sig))
          ei[is.nan(ei)] <- 0
          return(-ei)
        }
      }else{
        "ego.max"
        fmax <- switch(type.metamodel,Lasso=max(objs[[1]]$Y),Acosso1=max(objs[[1]]$model$acosso$y),Acosso2=max(objs[[1]]$model$acosso$y),Kriging=max(objs[[1]]$model@output))
        amax <- max(fmax,boundsO[1])
        bmax <- boundsO[2]
        fun.crit <- function(x){
          p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
          mu <- p$mean
          sig <- p$sd
          ei <- (mu-fmax)*(dnorm(bmax,mu,sig)-dnorm(amax,mu,sig)) - (pnorm(bmax,mu,sig)-pnorm(amax,mu,sig))
          ei[is.nan(ei)] <- 0
          return(-ei)
        }
      }
    }
  }
  
  return(fun.crit)
}

acquisition.function.MO <- function(dimx, objs, minimize, idconstr, boundsO, boundsC, sdreweightedloo = FALSE){
  
  type.metamodel <- objs[[1]]$type.metamodel
  
  if (length(idconstr)>0){
    if (minimize){
      "efi.min"
      fmin <- switch(type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@output))
      amin <- boundsO[1]
      bmin <- min(fmin,boundsO[2])
      fun.crit <- function(x){
        nc <- length(idconstr)
        y <- matrix(NA,nrow = 1+nc,ncol = nrow(x))
        p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
        mu <- p$mean
        sig <- p$sd
        ei <- (fmin-mu)*(dnorm(bmin,mu,sig)-dnorm(amin,mu,sig)) + sig*(pnorm(bmin,mu,sig)-pnorm(amin,mu,sig))
        ei[is.nan(ei)] <- 0
        y[1,] <- -ei
        
        for (j in 1:nc){
          p <- predict.metamodel(objs[[idconstr[j]]],x,sdreweightedloo)
          mu <- p$mean
          sig <- p$sd
          probatemp <- pnorm(boundsC[2,j],mu,sig) - pnorm(boundsC[1,j],mu,sig)
          y[1+j,] <- -probatemp
        }
        return(y)
      }
    }else{
      "efi.max"
      fmax <- switch(type.metamodel,Lasso=max(objs[[1]]$Y),Acosso1=max(objs[[1]]$model$acosso$y),Acosso2=max(objs[[1]]$model$acosso$y),Kriging=max(objs[[1]]$model@output))
      amax <- max(fmax,boundsO[1])
      bmax <- boundsO[2]
      fun.crit <- function(x){
        nc <- length(idconstr)
        y <- matrix(NA,nrow = 1+nc,ncol = nrow(x))
        p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
        mu <- p$mean
        sig <- p$sd
        ei <- (mu-fmax)*(dnorm(bmax,mu,sig)-dnorm(amax,mu,sig)) - (pnorm(bmax,mu,sig)-pnorm(amax,mu,sig))
        ei[is.nan(ei)] <- 0
        y[1,] <- -ei
        
        nc <- length(idconstr)
        for (j in 1:nc){
          p <- predict.metamodel(objs[[idconstr[j]]],x,sdreweightedloo)
          mu <- p$mean
          sig <- p$sd
          probatemp <- pnorm(boundsC[2,j],mu,sig) - pnorm(boundsC[1,j],mu,sig)
          y[1+j,] <- -probatemp
        }
        return(y)
      }
    }
  }
  else{
    if (minimize){
      "ego.min"
      fmin <- switch(type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@output))
      amin <- boundsO[1]
      bmin <- min(fmin,boundsO[2])
      fun.crit <- function(x){
        p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
        mu <- p$mean
        sig <- p$sd
        ei <- (fmin-mu)*(dnorm(bmin,mu,sig)-dnorm(amin,mu,sig)) + sig*(pnorm(bmin,mu,sig)-pnorm(amin,mu,sig))
        ei[is.nan(ei)] <- 0
        return(-ei)
      }
    }else{
      "ego.max"
      fmax <- switch(type.metamodel,Lasso=max(objs[[1]]$Y),Acosso1=max(objs[[1]]$model$acosso$y),Acosso2=max(objs[[1]]$model$acosso$y),Kriging=max(objs[[1]]$model@output))
      amax <- max(fmax,boundsO[1])
      bmax <- boundsO[2]
      fun.crit <- function(x){
        p <- predict.metamodel(objs[[1]],x,sdreweightedloo)
        mu <- p$mean
        sig <- p$sd
        ei <- (mu-fmax)*(dnorm(bmax,mu,sig)-dnorm(amax,mu,sig)) - (pnorm(bmax,mu,sig)-pnorm(amax,mu,sig))
        ei[is.nan(ei)] <- 0
        return(-ei)
      }
    }
  }
  return(fun.crit)
}

EGO.seq.optimize <- function(models, Xinfos, DOE, npts = 1, idobj = 1, minimize=TRUE, idconstr=NULL, boundsO, boundsC, settings, UCB = FALSE){

  inneropt <- settings[[1]]
  dimx <- DOE$nX
  Xinfos <- Xinfos$Xinfos
  Xbounds <- get.bounds(Xinfos)
  ntest <- as.numeric(settings[[2]])
  sdreweightedloo <- as.logical(settings[[3]])
  
  Xaddfinal <- as.data.frame(matrix(nrow = npts, ncol = dimx))
  idmodels <- c(idobj,idconstr)
  nmodels <- length(models)
  constrainedOptim <- (length(idconstr) > 0) && UCB
  nC <- length(idconstr)

  for (a in 1:npts) {
    
    if (inneropt=="BOBYQA"){
      fun.crit <- acquisition.function(dimx, models ,minimize, idconstr, boundsO, boundsC, sdreweightedloo = sdreweightedloo, UCB)
      # Generate multistart candidates from Xinfos (possibily updated by users in the seq optim tab)
      nm <- min(5*ncol(Xbounds),ntest)
      lb <- Xbounds[1,,drop=FALSE]
      ub <- Xbounds[2,,drop=FALSE]
      Xtest <- runif.sobol(nm,ncol(Xbounds), scrambling = 1)
      Xtest <- repmat(lb,nm,1) + repmat(ub - lb,nm,1)*Xtest
      
      # Count nb of numerical variables
      nvar <- get.nb.num(Xinfos)
      if (nvar < dimx){
        # There are categorical inputs, continuous optim on each slice
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
          xcomplete <- as.data.frame(matrix(NA,1,dimx))
          xcomplete[,num.index] <- x
          xcomplete[,cat.index] <- as.data.frame(currentxcat)
          p <- fun.crit(xcomplete)
          return(p)
        }
        if (constrainedOptim){
          hn <- function(x, currentxcat, num.index, cat.index){
            h <- numeric(nC)
            xcomplete <- as.data.frame(matrix(NA,1,dimx))
            xcomplete[,num.index] <- x
            xcomplete[,cat.index] <- as.data.frame(currentxcat)
            for (j in 1:nC) {
              p <- predict.metamodel(models[[idconstr[j]]], xcomplete, sdreweightedloo)
              muC <- p$mean
              sigC <- p$sd
              if (boundsC[j,1] == -Inf){
                h[j] <- as.numeric(muC + 3*sigC - boundsC[j,2])
              }
              if (boundsC[j,2] == Inf){
                h[j] <- as.numeric(-(muC - 3*sigC - boundsC[j,1]))
              }
            }
            return(h)
          }
        }
        
        # Loop on categorical slices
        Xallslices <- as.data.frame(matrix(NA,nslices,dimx))
        fallslices <- matrix(NA,nslices,1)
        for (s in 1:nslices){
          currentlevels = as.matrix(levels[s,])
          currentxcat <- matrix(as.matrix(sapply(1:(dimx-nvar), function(ind){
            as.factor(unlist(Xinfos[[cat.index[ind]]]$levels)[currentlevels[ind]])
          })),nrow=1)
          
          Xoptim <- matrix(NA,nm,ncol(Xtest))
          foptim <- matrix(NA,nm,1)
          for (i in 1:nm){
            if (!constrainedOptim){
              otemp <- try(bobyqa(c(Xtest[i,]), fn = function(x){fn(x,currentxcat,num.index,cat.index)}, lower = as.numeric(c(Xbounds[1,,drop=F])), upper = as.numeric(c(Xbounds[2,,drop=F]))), TRUE)
              if (class(otemp) != "try-error") {
                Xoptim[i,] <- otemp$par
                foptim[i] <- otemp$value
              }
            }else{
              nlo <- try(
                nloptr(
                  x0 = Xtest[i,], 
                  eval_f = function(x){fn(x, currentxcat, num.index, cat.index)}, 
                  eval_grad_f = NULL,
                  lb = as.numeric(c(Xbounds[1,,drop=F])), 
                  ub = as.numeric(c(Xbounds[2,,drop=F])), 
                  eval_g_ineq = function(x){hn(x, currentxcat, num.index, cat.index)}, 
                  eval_jac_g_ineq = NULL,
                  eval_g_eq = NULL, 
                  eval_jac_g_eq = NULL,
                  opts = list("algorithm" = "NLOPT_GN_ISRES","xtol_rel" = 1.0e-8,
                              "tol_constraints_ineq" = rep(1e-12, nC), "ranseed" = 0)
                ), TRUE)
              if (class(nlo) != "try-error") {
                Xoptim[i,] <- nlo$solution
                foptim[i] <- nlo$objective
              }
            }
          }
          if (all(is.na(foptim))){
            idbest <- sample.int(nm,1)
            Xtemp <- Xtest[sample.int(nm,1),,drop=F]
            ftemp <- fn(Xtemp,currentxcat,num.index,cat.index)
          }else{
            if (all(abs(foptim)<1e-12)){
              idbest <- sample.int(nm,1)
            }else{
              idbest <- which.min(foptim)
            }
            Xtemp <- Xoptim[idbest,,drop=F]
            ftemp <- foptim[idbest]
          }
          Xallslices[s,num.index] <- Xtemp
          Xallslices[,cat.index] <- as.data.frame(currentxcat)
          fallslices[s] <- ftemp
        }
        bestslice <- which.min(fallslices)
        Xadd <- Xallslices[bestslice,]
      }else{
        # Only numerical inputs, direct continuous optim
        Xoptim <- matrix(NA,nm,ncol(Xtest))
        foptim <- matrix(NA,nm,1)
        if (constrainedOptim){
          hn <- function(x){
            h <- numeric(nC)
            for (j in 1:nC) {
              p <- predict.metamodel(models[[idconstr[j]]],matrix(x,ncol=dimx),sdreweightedloo)
              muC <- p$mean
              sigC <- p$sd
              if (boundsC[j,1] == -Inf){
                h[j] <- as.numeric(muC + 3*sigC - boundsC[j,2])
              }
              if (boundsC[j,2] == Inf){
                h[j] <- as.numeric(-(muC - 3*sigC - boundsC[j,1]))
              }
            }
            return(h)
          }
        }
        for (i in 1:nm){
          print(paste("Multistart",i,"/",nm))
          if (!constrainedOptim){
            otemp <- try(bobyqa(c(Xtest[i,]), fn = function(x){fun.crit(matrix(x,ncol=dimx))}, lower = as.numeric(c(Xbounds[1,,drop=F])), upper = as.numeric(c(Xbounds[2,,drop=F]))), TRUE)
            if (class(otemp) != "try-error") {
              Xoptim[i,] <- otemp$par
              foptim[i] <- otemp$value
            }
          }else{
            nlo <- try(
              nloptr(
                x0 = Xtest[i,], 
                eval_f = function(x){fun.crit(matrix(x,ncol=dimx))}, 
                eval_grad_f = NULL,
                lb = as.numeric(c(Xbounds[1,,drop=F])), 
                ub = as.numeric(c(Xbounds[2,,drop=F])), 
                eval_g_ineq = hn, 
                eval_jac_g_ineq = NULL,
                eval_g_eq = NULL, 
                eval_jac_g_eq = NULL,
                opts = list("algorithm" = "NLOPT_GN_ISRES","xtol_rel" = 1.0e-8,
                            "tol_constraints_ineq" = rep(1e-12, nC), "ranseed" = 0)
              ), TRUE)
            if (class(nlo) != "try-error") {
              Xoptim[i,] <- nlo$solution
              foptim[i] <- nlo$objective
            }
          }
        }
        
        if (all(is.na(foptim))){
          Xadd <- Xtest[sample.int(nm,1),,drop=F]
        }else{
          if (all(abs(foptim)<1e-12)){
            idbest <- sample.int(nm,1)
          }else{
            idbest <- which.min(foptim)
          }
          Xadd <- Xoptim[idbest,,drop=F]
        }
      }
    }
    
    if (inneropt=="GRID"){
      # Generate candidates from Xinfos (possibily updated by users in the seq optim tab)
      ngrid <- 100*ncol(Xbounds)
      Xgrid <- generateXtest(Xbounds,ngrid,DOE)
      fun.crit <- acquisition.function(dimx,models ,minimize, idconstr, boundsO, boundsC,sdreweightedloo = sdreweightedloo)
      fgrid <- fun.crit(Xgrid)
      idbest <- which.min(fgrid)
      Xadd <- Xgrid[idbest,,drop=F]
    }
    
    if (inneropt=="MO"){
      fun.crit <- acquisition.function.MO(dimx,models ,minimize, idconstr, boundsO, boundsC,sdreweightedloo = sdreweightedloo)
      
      # Count nb of numerical variables
      nvar <- get.nb.num(Xinfos)
      if (nvar < dimx){
        # There are categorical inputs, continuous optim on each slice
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
          xcomplete[,num.index] <- x
          xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nx,1))
          p <- fun.crit(xcomplete)
          return(p)
        }
        
        # Loop on categorical slices
        Xallslices <- NULL
        fallslices <- NULL
        for (s in 1:nslices){
          currentlevels = as.matrix(levels[s,])
          currentxcat <- matrix(as.matrix(sapply(1:(dimx-nvar), function(ind){
            as.factor(unlist(Xinfos[[cat.index[ind]]]$levels)[currentlevels[ind]])
          })),nrow=1)
          
          r <- nsga2(function(x){fn(x,currentxcat,num.index,cat.index)}, nvar, 1+length(idconstr),
                     lower.bounds = as.numeric(Xbounds[1,]), upper.bounds = as.numeric(Xbounds[2,]), 
                     popsize=100*nvar,generations = 100,vectorized = T)
          xtemp <- r$par
          nxtemp <- nrow(xtemp)
          xcomplete <- as.data.frame(matrix(NA,nxtemp,dimx))
          xcomplete[,num.index] <- xtemp
          xcomplete[,cat.index] <- as.data.frame(repmat(currentxcat,nxtemp,1))
          
          fall <- matrix(r$value,nrow=nxtemp)
          row.names(fall) <- 1:nrow(fall)
          idpareto <- row.names(paretoFilter(fall))
          idpareto <- as.numeric(idpareto)
          idpareto <- idpareto[!is.na(idpareto)]
          
          Xallslices <- rbind(Xallslices,xcomplete[idpareto,,drop=FALSE])
          fallslices <- rbind(fallslices,fall[idpareto,,drop=FALSE])
        }
        row.names(fallslices) <- 1:nrow(fallslices)
        idpareto <- row.names(paretoFilter(fallslices))
        idpareto <- as.numeric(idpareto)
        idpareto <- idpareto[!is.na(idpareto)]
        rprod <- -apply(-fallslices[idpareto,,drop=FALSE],1,prod)
        
        if (any(rprod!=0)){
          # Select the best one according to prod if all prods are different from 0
          idbest <- idpareto[which.min(rprod)]
        }else{
          # Choose at random on the PF
          idbest <- sample(idpareto,1)
        }
        Xadd <- Xallslices[idbest,,drop=F]
        
      }else{
        # Only numerical inputs, direct continuous optim
        r <- nsga2(fun.crit, dimx, 1+length(idconstr),
                   lower.bounds = as.numeric(Xbounds[1,]), upper.bounds = as.numeric(Xbounds[2,]), 
                   popsize=100*dimx,generations = 100,vectorized = T)
        idp <- which(r$pareto.optimal)
        rprod <- -apply(-r$value[idp,,drop=FALSE],1,prod)
        if (any(rprod!=0)){
          # Select the best one on the PF according to prod if all prods are different from 0
          idbest <- idp[which.min(rprod)]
        }else{
          # Choose at random
          idbest <- sample(idp,1)
        }
        Xadd <- r$par[idbest,,drop=F]
      }
    }
     
    Xaddfinal[a,] <- Xadd
    
    if (a<npts){
      # Constant liar prediction
      constantliar <- matrix(NA, nrow = 1, ncol = nmodels)
      for (i in 1:nmodels){
        constantliar[i] <- predict.metamodel(models[[idmodels[i]]],Xadd,computesd=FALSE)$mean
      }
      
      # Update models
      for (i in 1:nmodels){
        models[[idmodels[i]]] <- update.metamodel(models[[idmodels[i]]],Xadd = Xadd,Yadd = constantliar[i])
      }
    }
  }
  
  return(Xaddfinal)
  
}

EGO.seq.settings <- function(){
  s1 <- list(name="Inner Optimizer",type="choice",default.value="BOBYQA",choices=c("BOBYQA","MO","GRID"))
  s2 <- list(name="Multistart (if BOBYQA)",type="numeric",default.value=50)
  s3 <- list(name="Use reweighted prediction variance",type="switch",default.value=TRUE)
  s4 <- list(name="Minimal Euclidian distance between new points and DOE points (normalized variables)",type="numeric",default.value=0.05)
  return(list(s1,s2,s3,s4))
}

RS.seq.optimize <- function(models, Xinfos, DOE, npts = 1, idobj = 1, minimize=TRUE, idconstr=NULL, boundsO=NULL, boundsC=NULL, settings = NULL, UCB = NULL){

  n <- npts
  Xbounds <- get.bounds(Xinfos$Xinfos)
  dimx <- DOE$nX
  Xinfos <- DOE$Xinfos
  sampleTest <- as.data.frame(matrix(0,nrow=n,ncol=dimx))
  colnames(sampleTest) <- DOE$xnames
  categorical <- which(sapply(DOE$Xinfos, function(var){var$type}) %in% c('constant', 'categorical'))
  levels.models <- lapply(DOE$Xinfos, function(Xinfos){Xinfos$levels})
  numerical <- setdiff(1:dimx,categorical)
  lb <- Xbounds[1,,drop=F]
  ub <- Xbounds[2,,drop=F]
  Xtestnum <- matrix(runif(n*length(numerical)),nrow=n)
  Xtestnum <- repmat(lb, n, 1) + repmat(ub - lb, n, 1)*Xtestnum
  sampleTest[,numerical] <- Xtestnum
  for (i in 1:dimx){
    if (Xinfos[[i]]$type == 'categorical'){
      sampleTest[,i] <- sample(unlist(levels.models[[i]]), size = n, replace = TRUE)
    }
  }
  
  return(sampleTest)
  
}

RS.seq.settings <- function(){
  return(NULL)
}

