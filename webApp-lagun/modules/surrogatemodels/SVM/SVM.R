# SVM (e1071 package) : support vector machine surrogate model

SVM.build <- function(Xmodel, y, Ytype, categorical, levels){
# Build the metamodel and return the output list
#
    obj <- list()
  	library(e1071)
    svm.kernel<-c("linear","polynomial","radial","sigmoid")
    nk <- length(svm.kernel)

    ncat <- length(categorical)

    # If categorical, one-hot encoding
    if (ncat>0){
      for (j in 1:ncat){
        Xmodel[, categorical[j]] <- factor(Xmodel[, categorical[j]], levels = levels[[j]])
      }
      Xmodel <- model.matrix( ~ ., Xmodel, xlev = levels)[, -1]
    }else{
      Xmodel <- model.matrix( ~ ., Xmodel)[, -1]
    } 

    nX <- ncol(Xmodel)

    if (Ytype == 'numeric'){
      logger$print("model tuning")
      svm.tune <- tune(svm, Xmodel, y, 
                       ranges = list( cost = 2^(0:4), degree=1:6, coef0  = 0:2, kernel=svm.kernel, epsilon =c(0.05,0.1,0.2), probability=T),
                       tunecontrol = tune.control(sampling="cross",cross=nX))
      svm.model <-  svm.tune$best.model
      best.parameters <- svm.tune$best.parameters
      loo_mse <- svm.tune$best.performance
      #loo <- svmcv(formula= y ~ . , trainxy = data.frame(cbind(Xmodel,y)),y=y, type= svm.model$type,
      #           cost = svm.model$cost, gamma =svm.model$gamma, kernel = svm.model$kernel,
      #            degree = svm.model$degree, coef0 = svm.model$coef0, validation="LOO", predacc="ALL")
      sig2 <- sqrt(sum(residuals(svm.model)^2)/summary(svm.model)$tot.nSV)
      ee <- residuals(svm.model)
      yy <- predict(svm.model)
      sig2 <- summary(svm.model)$sigma^2 
      
      yloo <- yy 
      maxprobaloo <- NULL
      errorsloo <- ee
      Q2loo <- 1 - loo_mse/sum((y-mean(y))^2)
      sig2loo <- sig2
      Ylevels <- NA
      
    }
    
    if (Ytype == 'categorical'){
      if (nX > 1){
        logger$print('model tuning')
        svm.tune <- tune(svm, Xmodel, y, 
                         ranges = list( cost = 2^(0:4), degree=1:6, coef0  = 0:2, kernel=svm.kernel, probability=T),
                         tunecontrol = tune.control(sampling="cross",cross=nX))
        svm.model <-  svm.tune$best.model
        best.parameters <- svm.tune$best.parameters
        loo_mse <- svm.tune$best.performance
        
        yy <- data.frame(predict(svm.model, Xmodel))
        probaloo <- attr(predict(svm.model, Xmodel, probability=T), "probabilities") 
        
        
        Ylevels <- colnames(probaloo)
        pred_class <- Ylevels[apply(probaloo, 1, which.max)]
        
        
        yloo <- Ylevels[apply(probaloo, 1, which.max)]
        maxprobaloo <- apply(probaloo, 1, max)
        errorsloo <- as.numeric(yloo != y)
        Q2loo <- sum(1 - errorsloo)/length(errorsloo)
        sig2loo <- NULL
        
      }
    }
    # Store results in metamodel object
    obj$yloo <- yloo 
    obj$maxprobaloo <- maxprobaloo
    obj$errorsloo <- errorsloo
    obj$sig2loo <- sig2loo 
    obj$Q2loo <- Q2loo
    obj$Ylevels <- Ylevels
    obj$model <- svm.model
    obj$categorical <- categorical
    obj$levels <- levels

	
    return(obj)
}


SVM.predict <- function(obj, Xmodel, computesd){

  #
  library(e1071)

  # If categorical, one-hot encoding
  categorical <- obj$categorical
  levels <- obj$levels
  ncat <- length(categorical)

  if (ncat > 0) {
    for (j in 1:ncat){
     Xmodel[, categorical[j]] <- factor(Xmodel[, categorical[j]], levels = levels[[j]])
    }
    Xmodel <- model.matrix( ~ ., Xmodel,xlev = levels)[, -1,drop=FALSE]
  } else {
    Xmodel <- model.matrix( ~ ., Xmodel)[, -1,drop=FALSE]
  }
  

  npred <- nrow(Xmodel)
  ysd <- NULL
  if (obj$Ytype == 'numeric'){
    
    p <- predict(object=obj$model, newdata = as.data.frame(Xmodel))
    ymean <- p[1:npred]
    
    if (computesd){
      ysd <- rep(summary(obj$model)$sigma, npred)
      
    }

    
  }
  
  if (obj$Ytype == 'categorical'){
    
    pred_proba <- attr(predict(obj$model, newdata = as.data.frame(Xmodel), probability = TRUE), "probabilities")
    ind_pred_class <- apply(pred_proba, 1, which.max)
    ymean <- obj$Ylevels[ind_pred_class][1:npred]
    
    
    if (computesd){
      ysd <- ymean * (1 - ymean)
    }
    
  }
  
  Outputs <- list(ymean=ymean, ysd=ysd)
  return(Outputs)
}


SVM.update <- function(obj, Xmodel, y){
  
  newobj <- list()
  library(e1071)

   # If categorical, one-hot encoding
  categorical <- obj$categorical
  levels <- obj$levels
  ncat <- length(categorical)

  if (ncat > 0) {
    for (j in 1:ncat){
     Xmodel[, categorical[j]] <- factor(Xmodel[, categorical[j]], levels = levels[[j]])
    }
    Xmodel <- rbind(Xmodel,obj$X)
    Xmodel <- model.matrix( ~ ., Xmodel,xlev = levels)[, -1,drop=FALSE]
  } else {
    Xmodel <- rbind(Xmodel,obj$X)
    Xmodel <- model.matrix( ~ ., Xmodel)[, -1,drop=FALSE]
  }

  nX <- ncol(Xmodel)
  Ytype <- obj$Ytype

  
  svm.kernel<-c("linear","polynomial","radial","sigmoid")
  svm.type<-c("C-classification","nu-classification","one-classification","eps-regression", "nu-regression")
  
  model <- obj$model
  kernel <- svm.kernel[1 + model$kernel]
  cost <- model$cost
  type <- svm.type[1 + model$type]
  degree <- model$degree 
  coef0  <- model$coef0
  cost <-model$cost
  x.scale <- model$x.scale
  y.scale <- model$y.scale
  
  Xmodel <- scale_data_frame(Xmodel, x.scale$`scaled:center`, x.scale$`scaled:scale`)
  if(!is.null(y.scale)){y <- as.numeric(scale_data_frame(y, y.scale$`scaled:center`, y.scale$`scaled:scale`))}
  logger$print("model update")
  svm.model <- svm(Xmodel, y, scale=F, type=type, kernel=kernel, coef0=coef0, cost=cost, degree=degree, probability = T, cross=nX)
  logger$print("done!")
  if (Ytype == 'numeric'){
    
    loo_mse <- svm.model$tot.MSE
    sig2 <- sqrt(sum(residuals(svm.model)^2)/summary(svm.model)$tot.nSV)
    ee <- residuals(svm.model)
    yy <- predict(svm.model)
    sig2 <- summary(svm.model)$sigma^2 
    
    yloo <- yy 
    maxprobaloo <- NULL
    errorsloo <- ee
    Q2loo <- 1 - loo_mse/sum((y-mean(y))^2)
    sig2loo <- sig2
    Ylevels <- NA
    
  }
  
  if (Ytype == 'categorical'){
    if (nX > 1){
      
      loo_mse <- svm.model$tot.MSE
      
      yy <- data.frame(predict(svm.model, Xmodel))
      probaloo <- attr(predict(svm.model, Xmodel, probability=T), "probabilities") 
      
      
      Ylevels <- colnames(probaloo)
      pred_class <- Ylevels[apply(probaloo, 1, which.max)]
      
      
      yloo <- Ylevels[apply(probaloo, 1, which.max)]
      maxprobaloo <- apply(probaloo, 1, max)
      errorsloo <- as.numeric(yloo != y)
      Q2loo <- sum(1 - errorsloo)/length(errorsloo)
      sig2loo <- NULL
      
    }
  }
  # Store results in metamodel object
  newobj$yloo <- yloo 
  newobj$maxprobaloo <- maxprobaloo
  newobj$errorsloo <- errorsloo
  newobj$sig2loo <- sig2loo 
  newobj$Q2loo <- Q2loo
  newobj$model <- svm.model
  newobj$categorical <- obj$categorical
  newobj$levels <- obj$levels
  
  return(newobj)
}


SVM.description <- function(){
# Returns the characteristics and parameters of the surrogate model
#
# Display Name: SURROGATE
#
# OptimTags=list( "regression", "classification", "predict.sd", "CategoricalInputs") 
# 
# Warnings=list()
# 

#

	DisplayName="SVM model"
	
	Description="Support Vector Machine model. Well suited for classification."
	
	SurrogateTags=list(classification=T, regression=T, predict.sd = F, CategoricalInputs = T)
	Warnings=list()


	return(list(dispname=DisplayName, descr=Description, tags=SurrogateTags, warn=Warnings))
}
