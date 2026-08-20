#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# load acosso functions
source('modules/surrogate/acosso.R', local = TRUE)

build.metamodel <- function(X,Y,Ytype='numeric',type.metamodel="Lasso",categorical=NULL,levels=NULL,acosso2.selvar=NULL,
                            kriging.trend="Constant",kriging.cov=c("Matern32","Matern52","Gauss"),kriging.selvar=NULL,
                            kriging.nugget=FALSE,kriging.estim="MLE",kriging.multi=FALSE,udefined.selvar=NULL,trendobj=NULL,tag.failY="NA", args=NULL){
  obj <- list()
  obj$type.metamodel <- type.metamodel
  # Remove missing values
  # First screen outputs
  if (tag.failY=="NA"){
    idYok <- !is.na(Y)
  }else{
    idYok <- Y!=tag.failY
  }
  nobs <- sum(idYok)
  #Y <- Y[idYok]
  # To keep the column name :
  Y <- matrix(Y[idYok], dimnames=list(NULL, colnames(Y)) )

  # Then screen inputs (useful for metamodel fusion)
  Xtemp <- X[idYok,,drop=FALSE]
  idXok <- which(apply(!is.na(Xtemp),2,all))
  nX <- length(idXok)
  if (is.null(acosso2.selvar) | length(acosso2.selvar)==0){
    acosso2.selvar <- 1:nX
  }
  if (is.null(kriging.selvar) | length(kriging.selvar)==0){
    kriging.selvar <- 1:nX
  }
  if (is.null(udefined.selvar) | length(udefined.selvar)==0){
    udefined.selvar <- 1:nX
  }
  obj$idXok <- idXok
  X <- Xtemp[,idXok,drop=FALSE]
  ncat <- length(categorical)
  if (ncat>0){
    idcommon <- which(idXok %in% categorical)
    if (length(idcommon)>0){
      levels <- levels[which(categorical%in%idXok)]
      categorical <- idcommon
      ncat <- length(categorical)
    }
  }else{
    levels <- NULL
  }


  obj$categorical <- categorical
  obj$levels <- levels
  if (!kriging.multi){
    multistart <- 1
  }else{
    multistart <- min(5*nX,10)
  }

  # Lasso
  if (type.metamodel=="Lasso"){
    Xmodel <- X
    # If categorical, one-hot encoding
    if (ncat>0){
      for (j in 1:ncat){
        Xmodel[, categorical[j]] <- factor(Xmodel[, categorical[j]], levels = levels[[j]])
      }
      Xmodel <- model.matrix( ~ ., Xmodel, xlev = levels)[, -1]
    }else{
      Xmodel <- model.matrix( ~ ., Xmodel)[, -1]
    }
    y <- Y

    if (Ytype == 'numeric'){
      if (nX > 1){
        # Compute optimal lambda with glmnet and CV
        cv <- cv.glmnet(Xmodel,Y)
        idopt <- which(cv$lambda==cv$lambda.min)
        cc <- coef(cv,s='lambda.min')
        idcc <- cc@i[cc@i>0]
        if (length(idcc)>0){
          # Train linear model with selected variables from optimal glmnet 
          x <- Xmodel[,idcc]
          ll <- lm(y~x,x=TRUE,y=TRUE)
        }else{
          ll <- lm(y~1,x=TRUE,y=TRUE)
        }
      }else{
        idcc <- 1
        x <- Xmodel
        ll <- lm(y~x,x=TRUE,y=TRUE)
      }
      
      sig2 <- summary(ll)$sigma^2
      ee <- residuals(ll)
      pp <- predict(ll, se.fit = TRUE)
      yy <- pp$fit
      ss <- pp$se.fit
      hh <- hatvalues(ll)
      yloo <- yy - hh/(1-hh)*ee
      maxprobaloo <- NULL
      errorsloo <- - hh/(1-hh)*ee
      Q2loo <- 1 - sum((y-yloo)^2)/sum((y-mean(y))^2)
      sig2loo <- (ss^2 - sig2*hh^2)/(1-hh)^2
      Ylevels <- NA
      
    }
    
    if (Ytype == 'categorical'){

      if (nX > 1){
        # multinomial logistic regression
        # Compute optimal lambda with glmnet and CV
        cv <- glmnet::cv.glmnet(Xmodel,Y, family = 'multinomial', type.measure = 'class')
        idopt <- which(cv$lambda==cv$lambda.min)
        cc <- coef(cv,s='lambda.min')
        idcc <- which(apply(sapply(cc, function(z){as.numeric(z) != 0}), 1, any)[-1])
        if (length(idcc)>0){
          # Train linear model with selected variables from optimal glmnet 
          x <- Xmodel[, idcc, drop = FALSE]
          ll <- VGAM::vglm(y ~ x, family = 'multinomial')
          xdata <- cbind(1, x)
        }else{
          ll <- VGAM::vglm(y ~ 1, family = 'multinomial')
          xdata <- matrix(rep(1, nrow(Xmodel)), ncol = 1)
        }
      }else{
        idcc <- 1
        x <- Xmodel
        ll <- VGAM::vglm(y ~ x, family = 'multinomial')
        xdata <- cbind(1, x)
      }

      ee <- residuals(ll)
      sig2 <- apply(ee, 2, var)
      yy <- VGAM::predictvglm(ll, as.data.frame(Xmodel))
      proba <- VGAM::predictvglm(ll, as.data.frame(Xmodel), type = 'response')
      Ylevels <- colnames(proba)
      pred_class <- Ylevels[apply(proba, 1, which.max)]
      hh <- hatvalues(ll)
      yloo <- yy - hh/(1-hh)*ee
      probaloo <- lapply(1:nrow(yloo), function(i){
        exp_Xbeta <- as.numeric(sapply(yloo[i,], exp))
        mu0 <- 1/(1 + sum(exp_Xbeta))
        proba <- c(mu0*exp_Xbeta, mu0)
        return(proba)
      })
      probaloo <- do.call('rbind', probaloo)
      yloo <- Ylevels[apply(probaloo, 1, which.max)]
      maxprobaloo <- apply(probaloo, 1, max)
      errorsloo <- as.numeric(yloo != y)
      Q2loo <- sum(1 - errorsloo)/length(errorsloo)
      beta_variance <- VGAM::vcov(ll)
      nlevel <- length(Ylevels)
      pred_var <- sapply(1:nlevel, function(k){
        proba_k <- - proba[, -nlevel, drop=F]
        if (k < nlevel){proba_k[, k] <- 1 + proba_k[, k, drop=F]}
        grad_proba <- lapply(1:ncol(xdata), function(j){
          apply(proba_k, 2, function(z){z * xdata[, j, drop=F]})
        })
        grad_proba <- do.call('cbind', grad_proba)
        var_proba <- grad_proba %*% beta_variance
        var_proba <- proba[, k, drop=F]^2*rowSums(var_proba * grad_proba)
        return(var_proba)
      })
      sig2loo <- sapply(1:nrow(pred_var), function(i){pred_var[i, which.max(proba[i,])]})
      
      obj$beta_variance <- beta_variance
      
    }
    # Store results in metamodel object
    obj$selvar <- idcc
    obj$model <- ll
    obj$yloo <- yloo 
    obj$maxprobaloo <- maxprobaloo
    obj$errorsloo <- errorsloo
    obj$sig2loo <- sig2loo 
    obj$Q2loo <- Q2loo
    obj$X <- X
    obj$Y <- Y
    obj$Ytype <- Ytype
    obj$Ylevels <- Ylevels
  
  }
  
  # Acosso 1st order
  if (type.metamodel=="Acosso1"){

    Xmodel <- X
    
    # If categorical, integer coding
    if (ncat>0){
      Xmodel[, categorical] <- sapply(1:ncat, function(i){
        sapply(Xmodel[,categorical[i]], function(x, levels){
          which(x == levels)
        }, levels = unlist(levels[[i]]) )
      })
    }else{
      categorical <- NULL
    }
    # Compute acosso fit
    aa <- acosso(Xmodel, Y, order=1, categorical=categorical)
    C <- get.gram.predict(aa$X,aa$X, order=aa$order, theta=aa$theta, w=aa$w, cat.pos=aa$cat.pos) + aa$lambda.0*diag(nobs)
    T <- chol(C)
    x <- backsolve(t(T), Y, upper.tri = FALSE)
    M <- backsolve(t(T), rep(1,nobs), upper.tri = FALSE)
    Q <- qr.Q(qr(M))
    H <- Q %*% t(Q)
    z <- x - H %*% x
    sig2 <- drop(crossprod(z)/length(z))
    l <- lm(x ~ M-1)
    beta.hat <- as.numeric(l$coef)
    modelacosso <- list()
    modelacosso$acosso <- aa
    modelacosso$T <- T
    modelacosso$z <- z
    modelacosso$beta.hat <- beta.hat
    modelacosso$sig2 <- sig2
    # Store results in metamodel object
    obj$selvar <- which(aa$theta>0)
    obj$model <- modelacosso
    # LOO
    Cinv <- chol2inv(T)
    Cinv.F <- Cinv %*% rep(1,nobs)
    T.M <- chol(crossprod(M))
    aux <- backsolve(t(T.M), t(Cinv.F), upper.tri=FALSE)
    Q <- Cinv - crossprod(aux)
    Q.y <- Q%*%Y
    sigma2 <- 1/diag(Q)
    epsilon <- sigma2 * (Q.y)
    yloo <- as.vector(Y - epsilon)
    Q2loo <- 1 - sum((Y-yloo)^2)/sum((Y-mean(Y))^2)
    obj$yloo <- yloo
    obj$errorsloo <- - epsilon
    obj$sig2loo <- sigma2
    obj$Q2loo <- Q2loo
    obj$Ytype <- 'numeric'
  }
  
  # Acosso 2nd order
  if (type.metamodel=="Acosso2"){
    Xmodel <- X
    
    # If categorical, integer coding
    if (ncat>0){
      Xmodel[, categorical] <- sapply(1:ncat, function(i){
        sapply(Xmodel[,categorical[i]], function(x, levels){
          which(x == levels)
        }, levels = unlist(levels[[i]]) )
      })
    }else{
      categorical <- NULL
    }
    # Compute acosso fit
    idX <- acosso2.selvar
    aa <- acosso(Xmodel[,idX,drop=FALSE], Y, order=2)
    C <- get.gram.predict(aa$X,aa$X, order=aa$order, theta=aa$theta, w=aa$w, cat.pos=aa$cat.pos) + aa$lambda.0*diag(nobs)
    T <- chol(C)
    x <- backsolve(t(T), Y, upper.tri = FALSE)
    M <- backsolve(t(T), rep(1,nobs), upper.tri = FALSE)
    Q <- qr.Q(qr(M))
    H <- Q %*% t(Q)
    z <- x - H %*% x
    sig2 <- drop(crossprod(z)/length(z))
    l <- lm(x ~ M-1)
    beta.hat <- as.numeric(l$coef)
    modelacosso <- list()
    modelacosso$acosso <- aa
    modelacosso$T <- T
    modelacosso$z <- z
    modelacosso$beta.hat <- beta.hat
    modelacosso$sig2 <- sig2
    # Store results in metamodel object
    obj$selvar <- idX
    obj$model <- modelacosso
    # LOO
    Cinv <- chol2inv(T)
    Cinv.F <- Cinv %*% rep(1,nobs)
    T.M <- chol(crossprod(M))
    aux <- backsolve(t(T.M), t(Cinv.F), upper.tri=FALSE)
    Q <- Cinv - crossprod(aux)
    Q.y <- Q%*%Y
    sigma2 <- 1/diag(Q)
    epsilon <- sigma2 * (Q.y)
    yloo <- as.vector(Y - epsilon)
    Q2loo <- 1 - sum((Y-yloo)^2)/sum((Y-mean(Y))^2)
    obj$errorsloo <- - epsilon
    obj$yloo <- yloo 
    obj$sig2loo <- sigma2
    obj$Q2loo <- Q2loo
    obj$Ytype <- 'numeric'
  }
  
  # Kriging
  if (type.metamodel == "Kriging"){
    
    obj$trend <- kriging.trend
    nc <- length(kriging.cov)

    # Compute traditional kriging
    if (kriging.trend == "Constant" || kriging.trend == "Linear"){
      
      # Kriging on continuous variables only
      numerical <- setdiff(1:nX,categorical)
      idX <- intersect(numerical, kriging.selvar) 
      
      # Numerical outputs
      if (Ytype == 'numeric'){
        
        if (ncat > 0){
          if (length(idX)==0) idX <- numerical
          # Constant trend only with known value equal to the mean
          coef.trend <- matrix(1,nobs,1)
        }else{
          if (length(idX)==0) idX <- 1:nX
          coef.trend <- switch(kriging.trend, Constant=matrix(1,nobs,1), Linear=cbind(matrix(1,nobs,1),as.matrix(X[,idX,drop=FALSE])))
        }
        
        if (length(idX)<nX) kriging.nugget = TRUE

        kmodeltemp <- list()
        ylootemp <- matrix(NA,nobs,nc)
        sig2lootemp <- matrix(NA,nobs,nc)
        Q2lootemp <- matrix(NA,1,nc)
        for (i in 1:nc){
          # Loop on all covariance functions to find the best one
          covtype <- switch(kriging.cov[i],Matern32="matern_3_2",Matern52="matern_5_2",Gauss="pow_exp")
          ktemp <- try(rgasp(design=X[,idX,drop=FALSE],response=Y,kernel_type=covtype, trend=coef.trend, num_initial_values=multistart,
                             nugget.est=kriging.nugget), silent=TRUE)
          lootemp <- try(leave_one_out_rgasp(ktemp))
          if (class(ktemp)=="try-error" || class(lootemp)=="try-error"){
            # Cholesky failed due to bad conditioning, try adding a nugget (and force MLE)
            ktemp <- try(rgasp(design=X[,idX,drop=FALSE],response=Y,kernel_type=covtype, trend=coef.trend, num_initial_values=multistart,
                               nugget=1e-10), silent=TRUE)
            lootemp <- try(leave_one_out_rgasp(ktemp))
          }
          if (class(ktemp)!="try-error" && class(lootemp)!="try-error"){
            kmodeltemp[[i]] <- ktemp
            ylootemp[,i] <- lootemp$mean
            sig2lootemp[,i] <- lootemp$sd^2
            Q2lootemp[i] <- 1 - sum((Y-ylootemp[,i])^2)/sum((Y-mean(Y))^2)
          }
        }
        idbest <- which.max(Q2lootemp)
        # Store results in metamodel object
        obj$selvar <- idX
        obj$model <- kmodeltemp[[idbest]]
        obj$model@R0 <- list()
        obj$yloo <- ylootemp[,idbest]
        obj$sig2loo <- sig2lootemp[,idbest]
        obj$Q2loo <- Q2lootemp[idbest]
        obj$Ytype <- Ytype
        
      }
      
      # Categorical outputs
      if (Ytype == 'categorical'){

        if (length(idX) <= 1) idX <- numerical

        # variable selection
        Xmodel <- X[, idX, drop = FALSE]

        # fit kriging for categorical outputs (vbmp)
        kriging_model <- fit.kriging.classif(Xmodel, Y)

        # Store results in metamodel object
        obj$selvar <- idX
        obj$model <- kriging_model$model
        obj$yloo <- kriging_model$yloo
        obj$maxprobaloo <- kriging_model$maxprobaloo
        obj$Q2loo <- kriging_model$Q2loo
        obj$Ylevels <- kriging_model$Ylevels
        obj$sig2loo <- kriging_model$sig2loo
        obj$Ytype <- Ytype
        obj$X <- X
        obj$Y <- Y
        
      }
      
    }
    
    # Kriging on residuals from a previous metamodel
    if (kriging.trend=="Lasso" || kriging.trend=="Acosso1"){

      newY <- Y - predict.metamodel(trendobj,X)$mean
      idX <- kriging.selvar
      if (ncat>0){
        # Kriging on continuous variables only
        numerical <- setdiff(1:nX,categorical)
        idX <- intersect(numerical,idX)
        if (length(idX)==0) idX <- numerical
      }else{
        if (length(idX)==0) idX <- 1:nX
      }
        
      if (length(idX)<nX) kriging.nugget = TRUE
      kmodeltemp <- list()
      ylootemp <- matrix(NA,nobs,nc)
      sig2lootemp <- matrix(NA,nobs,nc)
      Q2lootemp <- matrix(NA,1,nc)
      for (i in 1:nc){
        # Loop on all covariance functions to find the best one
        c <- switch(kriging.cov[i],Matern32="matern_3_2",Matern52="matern_5_2",Gauss="pow_exp")
        ktemp <- try(rgasp(design=X[,idX,drop=FALSE],response=newY,kernel_type=c, zero.mean="Yes", num_initial_values=multistart,
                           nugget.est=kriging.nugget), silent=TRUE)
        lootemp <- try(leave_one_out_rgasp(ktemp))
        if (class(ktemp)=="try-error" || class(lootemp)=="try-error"){
          # Cholesky failed due to bad conditioning, try adding a nugget (and force MLE)
          ktemp <- try(rgasp(design=X[,idX,drop=FALSE],response=newY,kernel_type=c, zero.mean="Yes", num_initial_values=multistart,
                             nugget=1e-10), silent=TRUE)
          lootemp <- try(leave_one_out_rgasp(ktemp))
        }
        if (class(ktemp)!="try-error" && class(lootemp)!="try-error"){
          kmodeltemp[[i]] <- ktemp
          ylootemp[,i] <- lootemp$mean
          sig2lootemp[,i] <- lootemp$sd^2
          Q2lootemp[i] <- 1 - sum((newY-ylootemp[,i])^2)/sum((newY-mean(newY))^2)
        }
      }
      idbest <- which.max(Q2lootemp)
      kmodel <- kmodeltemp[[idbest]]
      ylookrig <- ylootemp[,idbest]
      obj$sig2loo <- sig2lootemp[,idbest]
      Q2lookrig <- Q2lootemp[idbest]
      newytrain <- Y - trendobj$yloo
      Cinv <- chol2inv(kmodel@L)
      Q <- Cinv
      Q.y <- Q%*%newytrain
      sigma2 <- 1/diag(Q)
      epsilon <- sigma2 * (Q.y)  # cost : n, neglected
      yloo <-  trendobj$yloo + as.vector(newytrain - epsilon)
      sig2loo <- trendobj$sig2loo + sigma2
      Q2loo <- 1 - sum((Y-yloo)^2)/sum((Y-mean(Y))^2)
      # Store results in metamodel object
      obj$selvar <- idX
      obj$trendobj <- trendobj
      obj$model <- kmodel
      obj$model@R0 <- list()
      obj$ylookrig <- ylookrig
      obj$Q2lookrig <- Q2lookrig
      obj$yloo <- yloo
      obj$sig2loo <- sig2loo
      obj$Q2loo <- Q2loo
      obj$Ytype <- Ytype
        
    }
    
  }

  #USER-DEFINED
  if (type.metamodel == "UserDefined") {
    Xmodel <- X
    idX <- udefined.selvar 
    y <- Y

    # variable selection
    Xmodel <- Xmodel[, idX, drop = FALSE]
 
    source(args$SurrogateFileName)

    argslist <- list(Xmodel=Xmodel, y=y, Ytype=Ytype, categorical=categorical, levels=levels)

    obj <- do.call(args$SurrogateBuildName, argslist)
    
    obj$selvar <- idX
    obj$X <- X
    obj$Y <- Y
    obj$Ytype <- Ytype
    obj$args <- args
    obj$type.metamodel <- type.metamodel
    obj$categorical <- categorical
    obj$levels <- levels
    obj$idXok <- idXok
  }
  obj$idYok <- idYok
  return(obj)
}

predict.metamodel <- function(obj, Xnew, computesd = TRUE, sdreweightedloo = FALSE, computeProba = FALSE){

  type.metamodel <- obj$type.metamodel
  categorical <- obj$categorical
  ncat <- length(categorical)
  levels <- obj$levels
  ysd <- NULL
  Xnew <- Xnew[,obj$idXok,drop=FALSE]
  idrowok <- which(apply(!is.na(Xnew),1,all))
  Xnew <- Xnew[idrowok,,drop=FALSE]
  npred <- nrow(Xnew)
  
  if (type.metamodel=="Lasso"){
    Xmodel <- as.data.frame(Xnew)
    colnames(Xmodel) <- colnames(obj$X)
    
    # If categorical, one-hot encoding
    if (ncat > 0){
      for (j in 1:ncat){
        Xmodel[, categorical[j]] <- factor(Xmodel[, categorical[j]], levels = levels[[j]])
      }
      Xmodel <- rbind(Xmodel,obj$X)
      Xmodel <- model.matrix( ~ ., Xmodel,xlev = levels)[, -1,drop=FALSE]
    }else{
      Xmodel <- rbind(Xmodel,obj$X)
      Xmodel <- model.matrix( ~ ., Xmodel)[, -1,drop=FALSE]
    }

    if (obj$Ytype == 'numeric'){
      
      p <- predict(object=obj$model, newdata = data.frame(x = I(Xmodel[, obj$selvar])), se.fit = TRUE)
      ymean <- p$fit[1:npred]
      if (computesd){
        ysd <- p$se.fit[1:npred]
      }
      
    }
    
    if (obj$Ytype == 'categorical'){

      pred_proba <- VGAM::predictvglm(obj$model, newdata = as.data.frame(x = I(Xmodel[, obj$selvar, drop = FALSE])), type = 'response')
      ind_pred_class <- apply(pred_proba, 1, which.max)
      ymean <- obj$Ylevels[ind_pred_class][1:npred]

      if (computesd || computeProba){
        
        xdata <- cbind(1, Xmodel[, obj$selvar])
        nlevel <- length(obj$Ylevel)
        pred_var <- sapply(1:nlevel, function(k){
          proba_k <- - pred_proba[, -nlevel, drop = FALSE]
          if (k < nlevel){proba_k[, k] <- 1 + proba_k[, k, drop=F]}
          grad_proba <- lapply(1:ncol(xdata), function(j){
            apply(proba_k, 2, function(z){z * xdata[, j, drop=F]})
          })
          grad_proba <- do.call('cbind', grad_proba)
          var_proba <- grad_proba %*% obj$beta_variance
          var_proba <- pred_proba[, k, drop = FALSE]^2*rowSums(var_proba * grad_proba)
          return(var_proba)
        })
        ysd <- sapply(1:npred, function(i){pred_var[i,which.max(pred_proba[i,])]})
        ymean <- apply(pred_proba, 1, max)[1:npred]
        
        if (computeProba){
          ymean <- pred_proba[1:npred, , drop = FALSE]
          ysd <- pred_var[1:npred, , drop = FALSE]
        }
        
      }
      
    }
  }
  
  if (type.metamodel=="Acosso1" || type.metamodel=="Acosso2"){
    p <- ncol(Xnew)
    Xmodel <- Xnew
    
    # If categorical, integer coding
    if (ncat>0){
      Xmodel[, categorical] <- sapply(1:ncat, function(i){
        sapply(Xmodel[,categorical[i]], function(x, levels){
          which(x == levels)
        }, levels = levels[[i]])
      })
      Xmodel <- matrix(apply(Xmodel, 2, function(col){
        as.numeric(as.character(col))
      }), ncol = p)
    }
    aobj <- obj$model$acosso
    T <- obj$model$T
    x <- obj$model$z
    muhat <- obj$model$beta.hat
    X.orig <- aobj$X.orig
    
    rescale <- aobj$rescale
    rescale.var <- 1:p
    if (type.metamodel=="Acosso2"){
      Xmodel <- Xmodel[,obj$selvar,drop=FALSE]
      rescale.var <-(1:length(obj$selvar))
    }
    for(i in rescale.var[rescale])
      Xmodel[,i] <- (Xmodel[,i]-min(X.orig[,i]))/(max(X.orig[,i])-min(X.orig[,i]))*.9 + .05
    c.newdata <- get.gram.predict(aobj$X,Xmodel, order=aobj$order, theta=aobj$theta, w=aobj$w, cat.pos=aobj$cat.pos)
    Tinv.c.newdata <- backsolve(t(T), c.newdata, upper.tri=FALSE)
    ymean <- muhat + t(Tinv.c.newdata)%*%x
    if (computesd){
      sig <- matrix(NA,npred,1)
      for (i in 1:npred){
        sig[i] <- get.gram.predict(Xmodel[i,,drop=FALSE],Xmodel[i,,drop=FALSE], order=aobj$order, theta=aobj$theta, w=aobj$w, cat.pos=aobj$cat.pos)
      }
      sig2 <- obj$model$sig2
      ysd <- sqrt(sig2*pmax(sig - apply(Tinv.c.newdata, 2, crossprod),0))
    }
  }
  
  if (type.metamodel=="Kriging"){

    kriging.selvar <- obj$selvar
    kriging.trend <- obj$trend
    
    # Traditional kriging
    if (kriging.trend=="Constant" || kriging.trend=="Linear"){
      
      X_test <- Xnew[, kriging.selvar, drop=FALSE]
      
      if (obj$Ytype == 'numeric'){
        kriging.trend <- obj$trend
        coef.trend <- switch(kriging.trend, Constant=matrix(1,npred,1), Linear=cbind(matrix(1,npred,1),as.matrix(X_test)))
        p <- predict(obj$model, testing_input=X_test, testing_trend=coef.trend)
        ymean <- p$mean
        if (computesd){
          ysd <- p$sd
        }
        
      }
      
      if (obj$Ytype == 'categorical'){

        pred <- suppressWarnings(predictCPP(obj$model, X.TEST = X_test))
        yproba <- pred$Ptest
        ymean <- obj$Ylevels[apply(yproba, 1, which.max)]
        
        if (computesd){
          ymean <- apply(yproba, 1, max)
          ysd <- sapply(1:length(ymean), function(i){pred$Pvar[i, which.max(yproba[i, ])]})
        }
        
        if (computeProba){
          ymean <- yproba
          ysd <- pred$Pvar
        }
        
      }
      
    }
    
    # Kriging on residuals from a previous metamodel
    if (kriging.trend=="Lasso" || kriging.trend=="Acosso1"){
      
      p1 <- predict.metamodel(obj$trendobj, Xnew)
      X_test <- Xnew[, kriging.selvar, drop=FALSE]
      
      if (obj$Ytype == 'numeric'){
        p2 <- predict(obj$model, testing_input=X_test, testing_trend=matrix(0,npred,1))
        ymean <- p1$mean + p2$mean
        if (computesd){
          ysd <- sqrt(p1$sd^2+p2$sd^2)
        }
      }
      
      if (obj$Ytype == 'categorical'){
        yproba <- suppressWarnings(predictCPP(obj$model, X.TEST = X_test))
        ymean <- obj$Ylevels[apply(yproba, 1, which.max)]
        if (computesd){
          ymean <- apply(yproba, 1, max)
          ysd <- ymean * (1 - ymean)
        }
      }
      
    }
  }
  
  if (type.metamodel=="UserDefined") {

    udefined.selvar <- obj$selvar

    colnames(Xnew) <- colnames(obj$X)    
    Xmodel <- Xnew[, udefined.selvar, drop=FALSE]
    Xmodel <- as.data.frame(Xmodel)
     
    source(obj$args$SurrogateFileName)
    argslist <- list(obj=obj, Xmodel=Xmodel, computesd=computesd)
    pred <- do.call(obj$args$SurrogatePredictName, argslist)
    ysd <- pred$ysd[1:npred]
    ymean <- pred$ymean[1:npred]
  }

  if (sdreweightedloo) {
    X <- switch(type.metamodel,Lasso=min(objs[[1]]$X),Acosso1=min(objs[[1]]$model$acosso$x),Acosso2=min(objs[[1]]$model$acosso$x),Kriging=min(objs[[1]]$model@X))
    alldists <- dist2(Xnew,X)
    ids <- apply(dist2,1,which.min)
    ysd <- ysd * sqrt(1+obj$errorsloo[ids]^2/obj$sig2loo[ids])
  }
  
  return(list(mean=ymean,sd=ysd,idrowok=idrowok))
}

update.metamodel <- function(obj,Xadd,Yadd){
  
  # No hyperparameter re-estimation
  type.metamodel <- obj$type.metamodel
  categorical <- obj$categorical
  ncat <- length(categorical)
  levels <- obj$levels
  Xadd <- Xadd[,obj$idXok,drop=FALSE]
  newobj <- list()
  newobj$idXok <- obj$idXok
  
  if (type.metamodel=="Lasso"){
    newobj$type.metamodel <- type.metamodel
    newobj$categorical <- obj$categorical
    newobj$levels <- obj$levels
    ll <- obj$model
    idcc <- obj$selvar
    # Retrieve training set
    X <- obj$X
    Y <- obj$Y
    # Train new linear model 
    colnames(Xadd) <- colnames(X)
    Xmodel <- rbind(X,Xadd)
    # If categorical, one-hot encoding
    if (ncat>0){
      Xmodel[, categorical] <- sapply(categorical, function(i){
        as.factor(Xmodel[,i])
      })
      Xmodel <- model.matrix( ~ ., Xmodel,xlev = levels)[, -1]
    }else{
      Xmodel <- model.matrix( ~ ., Xmodel,xlev = levels)[, -1]
    }
    x <- Xmodel[,idcc]
    if (obj$Ytype == 'numeric'){
      y <- c(Y,Yadd)
      if (length(idcc)>0){
        newll <- lm(y~x,x=TRUE,y=TRUE)
      }else{
        newll <- lm(y~1,x=TRUE,y=TRUE)
      }
      ee <- residuals(newll)
      yy <- predict(newll)
      hh <- hatvalues(newll)
      yloo <- yy - hh/(1-hh)*ee
      Q2loo <- 1 - sum((y-yloo)^2)/sum((y-mean(y))^2)
    }
    if (obj$Ytype == 'categorical'){
      y <- unlist(list(Y,as.factor(Yadd)))
      if (length(idcc)>0){
        newll <- VGAM::vglm(y ~ x, family = 'multinomial')
      }else{
        newll <- VGAM::vglm(y ~ 1, family = 'multinomial')
      }

      ee <- residuals(newll)
      sig2 <- apply(ee, 2, var)
      yy <- VGAM::predictvglm(newll, as.data.frame(Xmodel))
      proba <- VGAM::predictvglm(newll, as.data.frame(Xmodel), type = 'response')
      Ylevels <- colnames(proba)
      pred_class <- Ylevels[apply(proba, 1, which.max)]
      hh <- hatvalues(newll)
      yloo <- yy - hh/(1-hh)*ee
      probaloo <- lapply(1:nrow(yloo), function(i){
        exp_Xbeta <- as.numeric(sapply(yloo[i,], exp))
        mu0 <- 1/(1 + sum(exp_Xbeta))
        proba <- c(mu0*exp_Xbeta, mu0)
        return(proba)
      })
      probaloo <- do.call('rbind', probaloo)
      pred_class_loo <- Ylevels[apply(probaloo, 1, which.max)]
      yloo <- apply(probaloo, 1, max)
      errorsloo <- as.numeric(pred_class_loo != y)
      Q2loo <- sum(1 - errorsloo)/length(errorsloo)
      beta_variance <- VGAM::vcov(newll)

      newobj$beta_variance <- beta_variance
      newobj$Ylevels <- Ylevels
      
    }
    # Store results in metamodel object
    newobj$selvar <- idcc
    newobj$model <- newll
    newobj$X <- rbind(X,Xadd)
    newobj$Y <- y
    newobj$yloo <- yloo
    newobj$Q2loo <- Q2loo
    newobj$Ytype <- obj$Ytype
  }
  
  if (type.metamodel=="Acosso1" || type.metamodel=="Acosso2"){
    newobj$type.metamodel <- type.metamodel
    newobj$categorical <- categorical
    newobj$levels <- levels
    if (type.metamodel=="Acosso1"){
      order <- 1
    }else{
      order <- 2
    }
    aobj <- obj$model$acosso
    X <- aobj$X.orig
    Y <- aobj$y
    p <- ncol(X)
    # If categorical, integer coding
    if (ncat>0){
      Xadd[, categorical] <- sapply(1:ncat, function(i){
        sapply(Xadd[,categorical[i]], function(x, levels){
          which(x == levels)
        }, levels = levels[[i]])
      })
      Xadd <- matrix(apply(Xadd, 2, function(col){
        as.numeric(as.character(col))
      }), ncol = ncol(Xadd))
    }
    Y <- c(Y,Yadd)
    if (type.metamodel=="Acosso2"){
      Xadd <- Xadd[,obj$selvar,drop=FALSE]
      categorical <- which(obj$selvar %in% categorical)
    }
    colnames(Xadd) <- colnames(X)
    Xnew <- rbind(X,Xadd)
    # Compute acosso fit
    aa <- acosso(Xnew, Y, order=order, categorical=categorical,cv=aobj$K, w=aobj$w, lambda.0=aobj$lambda.0)
    nobs <- nrow(Xnew)
    C <- get.gram.predict(aa$X,aa$X, order=aa$order, theta=aa$theta, w=aa$w, cat.pos=aa$cat.pos) + aa$lambda.0*diag(nobs)
    T <- chol(C)
    x <- backsolve(t(T), Y, upper.tri = FALSE)
    M <- backsolve(t(T), rep(1,nobs), upper.tri = FALSE)
    Q <- qr.Q(qr(M))
    H <- Q %*% t(Q)
    z <- x - H %*% x
    sig2 <- drop(crossprod(z)/length(z))
    l <- lm(x ~ M-1)
    beta.hat <- as.numeric(l$coef)
    modelacosso <- list()
    modelacosso$acosso <- aa
    modelacosso$T <- T
    modelacosso$z <- z
    modelacosso$beta.hat <- beta.hat
    modelacosso$sig2 <- sig2
    # Store results in metamodel object
    newobj$selvar <- obj$selvar
    newobj$model <- modelacosso
    newobj$yloo <- aa$y.hat 
    newobj$Q2loo <- aa$Rsq
    newobj$Ytype <- obj$Ytype
  }
  
  if (type.metamodel=="Kriging"){
    
    newobj$type.metamodel <- type.metamodel
    newobj$categorical <- obj$categorical
    newobj$levels <- obj$levels
    kriging.trend <- obj$trend
    kriging.selvar <- obj$selvar
    newobj$trend <- kriging.trend
    newobj$selvar <- kriging.selvar

    if (obj$Ytype == 'numeric'){
      
      kmodel <- obj$model
      X <- kmodel@input
      Y <- kmodel@output
      X <- rbind(X, as.matrix(Xadd[, kriging.selvar, drop = FALSE]))
      Y <- c(Y,Yadd)
      
      # Traditional kriging
      if (kriging.trend=="Constant" || kriging.trend=="Linear"){
        coef.trend <- switch(kriging.trend, Constant=matrix(1,nrow(X),1), Linear=cbind(matrix(1,nrow(X),1),X[,kriging.selvar,drop=FALSE]))
        newkmodel <- rgasp(design=X,response=Y,kernel_type=kmodel@kernel_type,
                           trend=coef.trend, nugget=kmodel@nugget, range.par=1/kmodel@beta_hat)
        yloo <- leave_one_out_rgasp(newkmodel)$mean
        Q2loo <- 1 - sum((Y-yloo)^2)/sum((Y-mean(Y))^2)
        # Store results in metamodel object
        newobj$model <- newkmodel
        newobj$yloo <- yloo
        newobj$Q2loo <- Q2loo
        newobj$Ytype <- obj$Ytype
      }
      
      # Kriging on residuals from a previous metamodel
      if (kriging.trend=="Lasso" || kriging.trend=="Acosso1"){
        # Update trend metamodel
        newtrendobj <- update.metamodel(obj$trendobj,Xadd,Yadd)
        # Update kriging model
        newY <-  Y - predict.metamodel(newtrendobj,X)$mean
        newkmodel <- rgasp(design=X,response=newY,kernel_type=kmodel@kernel_type,
                           zero.mean="Yes", nugget=kmodel@nugget, range.par=1/kmodel@beta_hat)
        # Compute new Q2
        newytrain <- Y - newtrendobj$yloo
        Cinv <- chol2inv(newkmodel@L)
        Q <- Cinv
        Q.y <- Q%*%newytrain
        sigma2 <- 1/diag(Q)
        epsilon <- sigma2 * (Q.y)  # cost : n, neglected
        yloo <-  newtrendobj$yloo + as.vector(newytrain - epsilon)
        Q2loo <- 1 - sum((Y-yloo)^2)/sum((Y-mean(Y))^2)
        
        # Store results in metamodel object
        newobj$trendobj <- newtrendobj
        newobj$model <- newkmodel
        newobj$yloo <- yloo
        newobj$Q2loo <- Q2loo
        newobj$Ytype <- obj$Ytype
      }

    }
    
    if (obj$Ytype == 'categorical'){

      X <- obj$X
      Y <- obj$Y
      X <- rbind(X, Xadd)
      Xmodel <- X[, kriging.selvar, drop=FALSE]
      t_class <- factor(c(as.character(Y), as.character(Yadd)), levels = obj$Ylevels)
      X_test <- Xmodel
      t_class_test <- t_class
      
      theta_opt <- obj$model$THETA

      newkmodel <- suppressWarnings(vbmp::vbmp(X = Xmodel, t.class = t_class, X.TEST = X_test, t.class.TEST = t_class_test,
                                     theta = theta_opt, control=list(maxIts = 1, sKernelType = 'gauss', method = 'classic')))
      
      # Evaluate model error via 10-fold cross-validation
      nfold <- 10
      nobs <- nrow(Xmodel)
      ind <- cut(seq(1, nobs), breaks = nfold, labels = F)
      ind <- sample(ind, size = nobs, replace = F)
      folds.ind <- lapply(1:nfold, function(fold, ind){
        test <- which(ind == fold)
        train <- setdiff(1:nobs, test)
        return(list(train = train, test = test))
      }, ind  = ind)
      ktemp_cv <- lapply(1:nfold, function(fold){
        X_train <- Xmodel[folds.ind[[fold]]$train, , drop = F]
        t_class_train <- t_class[folds.ind[[fold]]$train]
        X_test <- Xmodel[folds.ind[[fold]]$test, , drop = F]
        t_class_test <- t_class[folds.ind[[fold]]$test]
        ktemp <- suppressWarnings(vbmp::vbmp(X = X_train, t.class = t_class_train, X.TEST = X_test, t.class.TEST = t_class_test,
                                       theta = theta_opt, control=list(maxIts = 1, sKernelType = 'gauss', method = 'classic')))
        return(ktemp)
      })
      pred_proba_cv <- do.call('rbind', lapply(ktemp_cv, function(ktemp){ktemp$Ptest}))
      t_class_test <- unlist(lapply(1:nfold, function(fold){t_class[folds.ind[[fold]]$test]}))
      pred_class <- obj$Ylevels[apply(pred_proba_cv, 1, which.max)]
      pred_proba <- apply(pred_proba_cv, 1, max)
      Q2loo <- sum(pred_class == t_class_test)/length(t_class_test)
      
      # Store results in metamodel object
      newobj$selvar <- obj$selvar
      newobj$model <- newkmodel
      newobj$yloo <- pred_proba
      newobj$Q2loo <- Q2loo
      newobj$Ylevels <- obj$Ylevels
      newobj$Ytype <- obj$Ytype
      newobj$X <- X
      newobj$Y <- t_class
      
      
    }
      
  }

  #USER-DEFINED
  if (type.metamodel == "UserDefined") {

    X <- obj$X
    Y <- obj$Y
    Xmodel <- rbind(X, Xadd)
    idX <-obj$selvar

    # variable selection
    Xmodel <- Xmodel[, idX, drop = FALSE]

    if (obj$Ytype == "numeric"){
      y <- c(Y,Yadd)
    }

    if(obj$Ytype == "categorical"){
      y <- factor(c(as.character(Y), as.character(Yadd)), levels = obj$Ylevels)
    }

    source(obj$args$SurrogateFileName)
    argslist <- list(obj=obj, Xmodel=Xmodel, y=y)

    newobj <- do.call(obj$args$SurrogateUpdateName, argslist)
    newobj$selvar <- obj$selvar
    newobj$X <- Xmodel
    newobj$Y <- y
    newobj$Ytype <- obj$Ytype
    newobj$Ylevels <- obj$Ylevels
    newobj$type.metamodel <- type.metamodel
    # newobj$idXok <- obj$idXok
    newobj$args <- obj$args
  }
  
  return(newobj)
}

onestep.improve.metamodel <- function(objs,Xtotest,criterion="mse",Xint=NULL,target=0,idconstr=NULL,signconstr=NULL,thconstr=NULL){
  
  type.metamodel <- objs[[1]]$type.metamodel

  if (criterion == "mse"){
    nobjs <- length(objs)
    fun.crit <- function(x){
      res <- 0
      for (i in 1:nobjs){
        res <- res + log(predict.metamodel(objs[[i]],x)$sd)
      }
      return(res)
    }
    sign.crit <- 'max'
  }
  
  if (criterion == "imse"){
    nobjs <- length(objs)
    fun.crit.base <- function(x){
      res <- 0
      for (i in 1:nobjs){
        falsemean <- predict.metamodel(objs[[i]],matrix(x,1))$mean
        newobj <- update.metamodel(objs[[i]],matrix(x,1),falsemean)
        res <- res + log(mean(predict.metamodel(newobj,Xint)$sd^2))
      }
      return(res)
    }
    fun.crit <- function(x) apply(x,1,fun.crit.base)
    sign.crit <- 'min'
  }
  
  if (criterion == "ego.min"){
    fmin <- switch(type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@y))
    fun.crit <- function(x){
      p <- predict.metamodel(objs[[1]],x)
      mu <- p$mean
      sig <- p$sd
      ei <- (fmin-mu)*dnorm((fmin-mu)/sig) + sig*pnorm((fmin-mu)/sig)
      ei[is.nan(ei)] <- 0
      return(ei)
    }
    sign.crit <- 'max'
  }
  
  if (criterion == "ego.max"){
    fmin <- switch(type.metamodel,Lasso=-max(objs[[1]]$Y),Acosso1=-max(objs[[1]]$model$acosso$y),Acosso2=-max(objs[[1]]$model$acosso$y),Kriging=-max(objs[[1]]$model@y))
    fun.crit <- function(x){
      p <- predict.metamodel(objs[[1]],x)
      mu <- -p$mean
      sig <- p$sd
      ei <- (fmin-mu)*dnorm((fmin-mu)/sig) + sig*pnorm((fmin-mu)/sig)
      ei[is.nan(ei)] <- 0
      return(ei)
    }
    sign.crit <- 'max'
  }
  
  if (criterion == "efi.min"){
    fmin <- switch(type.metamodel,Lasso=min(objs[[1]]$Y),Acosso1=min(objs[[1]]$model$acosso$y),Acosso2=min(objs[[1]]$model$acosso$y),Kriging=min(objs[[1]]$model@y))
    fun.crit <- function(x){
      p <- predict.metamodel(objs[[1]],x)
      mu <- p$mean
      sig <- p$sd
      ei <- (fmin-mu)*dnorm((fmin-mu)/sig) + sig*pnorm((fmin-mu)/sig)
      ei[is.nan(ei)] <- 0
      
      nc <- length(idconstr)
      proba <- 1
      for (j in 1:nc){
        p <- predict.metamodel(objs[[idconstr[j]]],x)
        mu <- p$mean
        sig <- p$sd
        probatemp <- (signconstr[j]+1)/2 - signconstr[j]*pnorm((thconstr[j]-mu)/sig)
        proba <- proba*probatemp
      }
      efi <- ei*proba
      return(efi)
    }
    sign.crit <- 'max'
  }
  
  if (criterion == "efi.max"){
    fmin <- switch(type.metamodel,Lasso=-max(objs[[1]]$Y),Acosso1=-max(objs[[1]]$model$acosso$y),Acosso2=-max(objs[[1]]$model$acosso$y),Kriging=-max(objs[[1]]$model@y))
    fun.crit <- function(x){
      p <- predict.metamodel(objs[[1]],x)
      mu <- -p$mean
      sig <- p$sd
      ei <- (fmin-mu)*dnorm((fmin-mu)/sig) + sig*pnorm((fmin-mu)/sig)
      ei[is.nan(ei)] <- 0
      
      nc <- length(idconstr)
      proba <- 1
      for (j in 1:nc){
        p <- predict.metamodel(objs[[idconstr[j]]],x)
        mu <- p$mean
        sig <- p$sd
        probatemp <- (signconstr[j]+1)/2 - signconstr[j]*pnorm((thconstr[j]-mu)/sig)
        proba <- proba*probatemp
      }
      efi <- ei*proba
      return(efi)
    }
    sign.crit <- 'max'
  }
  
  if (criterion == "ranjan"){
    fun.crit <- function(x){
      p <- predict.metamodel(objs[[1]],x)
      mu <- p$mean
      sig <- p$sd
      t <- (mu - target)/sig
      alpha <- 1
      tplus <- t + alpha
      tminus <- t - alpha
      G <- (alpha^2 - 1 - t^2) * (pnorm(tplus) - pnorm(tminus)) - 
        2 * t * (dnorm(tplus) - dnorm(tminus)) + tplus * dnorm(tplus) - 
        tminus * dnorm(tminus)
      G[is.nan(G)] <- 0
      G <- G * (sig^2)
      return(G)
    }
    sign.crit <- 'max'
  }
  
  if (criterion == "local_categorical"){
    fun.crit <- function(x){
      p <- predict.metamodel(objs[[1]], x, computeProba = T)
      G <- as.numeric(p$sd[, which(objs[[1]]$Ylevels == target)])
      return(G)
    }
    sign.crit <- 'max'
  }
  
  allcrit <- fun.crit(Xtotest)
  if (sign.crit=='min'){
    idbest <- which.min(allcrit)
  }else{
    idbest <- which.max(allcrit)
  }
  Xbest <- Xtotest[idbest,,drop=FALSE]
  
  return(list(Xbest=Xbest,idbest=idbest))
}

fit.kriging.classif <- function(Xmodel, Y){
  
  # Estimate covariance function parameters with train/test samples
  ind_train <- sample(1:nrow(Xmodel), size = 0.66*nrow(Xmodel))
  ind_test <- setdiff(1:nrow(Xmodel), ind_train)
  X_train <- Xmodel[ind_train, , drop = FALSE]
  colnames(X_train) <- NULL
  row.names(X_train) <- NULL
  X_test <- Xmodel[ind_test, , drop = FALSE]
  colnames(X_test) <- NULL
  row.names(X_test) <- NULL
  t_class <- as.factor(Y)
  Ylevels <- levels(t_class)
  t_class_train <- t_class[ind_train]
  t_class_test <- t_class[ind_test]
  theta <- runif(ncol(Xmodel))
  ktheta <- suppressWarnings(vbmp::vbmp(X = X_train, t.class = t_class_train, X.TEST = X_test, t.class.TEST = t_class_test,
                                  theta = theta, control=list(bThetaEstimate = T, bPlotFitting = F, maxIts = 50, nSampsIS = 100,
                                                              sKernelType = 'gauss', method = 'classic')))
  theta_opt <- ktheta$THETA[which.min(ktheta$testErr),]
  
  # Fit final GP with estimated covariance theta
  ktemp <- suppressWarnings(vbmp::vbmp(X = Xmodel, t.class = t_class, X.TEST = X_test, t.class.TEST = t_class_test,
                                 theta = theta_opt, control=list(maxIts = 1, sKernelType = 'gauss', method = 'classic')))
  
  # Evaluate model error via 10-fold cross-validation
  nfold <- 10
  nobs <- nrow(Xmodel)
  ind <- cut(seq(1, nobs), breaks = nfold, labels = F)
  ind <- sample(ind, size = nobs, replace = F)
  folds.ind <- lapply(1:nfold, function(fold, ind){
    test <- which(ind == fold)
    train <- setdiff(1:nobs, test)
    return(list(train = train, test = test))
  }, ind  = ind)
  ktemp_cv <- lapply(1:nfold, function(fold){
    X_train <- Xmodel[folds.ind[[fold]]$train, , drop = F]
    t_class_train <- t_class[folds.ind[[fold]]$train]
    X_test <- Xmodel[folds.ind[[fold]]$test, , drop = F]
    t_class_test <- t_class[folds.ind[[fold]]$test]
    ktemp <- suppressWarnings(vbmp::vbmp(X = X_train, t.class = t_class_train, X.TEST = X_test, t.class.TEST = t_class_test,
                                   theta = theta_opt, control=list(maxIts = 1, sKernelType = 'gauss', method = 'classic')))
    return(ktemp)
  })
  pred_proba_cv <- do.call('rbind', lapply(ktemp_cv, function(ktemp){ktemp$Ptest}))
  pred_proba_cv <- pred_proba_cv[order(unlist(lapply(1:nfold, function(fold){folds.ind[[fold]]$test}))), ]
  sig2loo <- do.call('rbind', lapply(ktemp_cv, function(ktemp){ktemp$Pvar}))
  yloo <- factor(Ylevels[apply(pred_proba_cv, 1, which.max)], levels = Ylevels)
  maxprobaloo <- apply(pred_proba_cv, 1, max)
  Q2loo <- sum(yloo == t_class)/length(t_class)
  return(list(model = ktemp, yloo = yloo, maxprobaloo = maxprobaloo, Q2loo = Q2loo, Ylevels = Ylevels, sig2loo = sig2loo))

}

combineMetamodels <- function(Y, models, idUsedY, compositeInfos, tagFailY="NA"){
  
  if (tagFailY=="NA"){
    idYok <- !is.na(Y)
  }else{
    idYok <- Y!=tagFailY
  }
  
  Y <- Y[idYok]
  
  ylooForm <- predfunForm <- compositeInfos$formula
  
  for(i in seq(length(compositeInfos$usedY))){
    ylooForm <- gsub(compositeInfos$usedY[i], paste0("models[[", i, "]]$yloo"), ylooForm)
    
    predfunForm <- gsub(compositeInfos$usedY[i],
                        paste0("predict.metamodel(listmodels$models[[listmodels$selected$id[[", idUsedY[i], "]]]][[", idUsedY[i], "]], x, computesd=FALSE)$mean"),
                        predfunForm)
  }
  
  yloo <- eval(parse(text = ylooForm))
  
  if (compositeInfos$type == "numeric"){
    Q2loo <- 1 - sum((Y-yloo)^2)/sum((Y-mean(Y))^2)
  }else{
    errorsloo <- as.numeric(yloo != Y)
    Q2loo <- sum(1 - errorsloo)/length(errorsloo)
  }
  
  
  
  obj <- list()
  obj$type.metamodel <- models[[1]]$type.metamodel
  obj$idXok <- models[[1]]$idXok
  obj$idYok <- idYok
  obj$selvar <- models[[1]]$selvar
  obj$model <- NA
  obj$predfunForm <- predfunForm
  obj$yloo <- yloo 
  obj$maxprobaloo <- NA
  obj$errorsloo <- NA
  obj$sig2loo <- NA
  obj$Q2loo <- Q2loo
  obj$X <- models[[1]]$X
  obj$Y <- Y
  obj$Ytype <- compositeInfos$type
  obj$Ylevels <- compositeInfos$levels
  
  return(obj)
}
