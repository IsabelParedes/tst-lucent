# polynomials (scikit-learn Python library) : polynomial ridge regression 

polynomials.build <- function(Xmodel, y, Ytype, categorical, levels){

  obj <- list()
	library(reticulate)
  
  # import modules
  np <- import("numpy", delay_load = TRUE)
  sk_lm <- import("sklearn.linear_model", delay_load = TRUE)
  sk_preproc <- import("sklearn.preprocessing", delay_load = TRUE)
  currentScore <- -Inf
  scalerY <- sk_preproc$StandardScaler()
  y <-  scalerY$fit_transform(as.matrix(y,ncol=1))

  for(degree in seq(1,6)){
    poly_features <- sk_preproc$PolynomialFeatures(degree=degree)
    X_poly <- poly_features$fit_transform(Xmodel)
    scalerX <- sk_preproc$StandardScaler()
    X_poly <- scalerX$fit_transform(X_poly)
    poly <- sk_lm$RidgeCV(alphas=c(1e-3,0.01,0.1,1), scoring="r2", fit_intercept=T, store_cv_values=T)$fit(X_poly,y)
    isBest <- poly$best_score_ > currentScore
    if(isBest){
      currentScore <- poly$best_score_
      currentModel <- list(poly=poly, poly_feat = poly_features, scalerX=scalerX, scalerY=scalerY) 
      currentYloo <- poly$cv_values_[,1,poly$alphas == poly$alpha_]
    }
  }

  # Store results in metamodel object
  obj$Q2loo <- currentScore
  obj$yloo <- as.numeric(scalerY$inverse_transform(as.matrix(currentYloo,ncol=1)))
  obj$Ylevels <- NULL
  obj$model <- currentModel
  obj$categorical <- categorical
  obj$levels <- levels

  return(obj)
}


polynomials.predict <- function(obj, Xmodel, computesd){
  library(reticulate)
  
  # import modules
  np <- import("numpy", delay_load = TRUE)
  sk_lm <- import("sklearn.linear_model", delay_load = TRUE)
  sk_preproc <- import("sklearn.preprocessing", delay_load = TRUE)
  poly_features <- obj$model$poly_feat
  poly <- obj$model$poly
  scalerX <- obj$model$scalerX 
  scalerY <- obj$model$scalerY
  
  X_poly <- poly_features$fit_transform(I(Xmodel[, obj$selvar]))
  X_poly <- scalerX$transform(X_poly)
  ypred <- poly$predict(X_poly)
  ymean <- as.numeric(scalerY$inverse_transform(cbind(ypred)))
  ysd <- NULL
  
  Outputs <- list(ymean=ymean, ysd=ysd)
  return(Outputs)
}


polynomials.update <- function(obj, Xmodel, y){

  newobj <- list()
  nX <- ncol(Xmodel)
  Ytype <- obj$Ytype
  library(reticulate)
  
  # import modules
  np <- import("numpy", delay_load = TRUE)
  sk_lm <- import("sklearn.linear_model", delay_load = TRUE)
  sk_preproc <- import("sklearn.preprocessing", delay_load = TRUE)
  
  poly_features <- obj$model$poly_features
  poly <- obj$model$poly
  scalerX <- obj$scalerX 
  scalerY <- obj$scalerY
  
  X_poly <- poly_features$fit_transform(Xmodel)
  X_poly <- scalerX$fit_transform(X_poly)
  y <-  scalerY$fit_transform(y)
  poly = sk_lm$RidgeCV(alphas=c(1e-3,0.01,0.1,1), scoring="r2", fit_intercept=T)$fit(X_poly,y)
  currentScore <- poly$best_score_
  currentModel <- list(poly=poly, poly_feat = poly_features, scalerX=scalerX, scalerY=scalerY) 
  currentYloo <- poly$cv_values_[,1,poly$alphas == poly$alpha_]
  
  # Store results in metamodel object
  newobj$Q2loo <- currentScore
  newobj$model <- currentModel
  newobj$yloo <- as.numeric(scalerY$inverse_transform(as.matrix(currentYloo,ncol=1)))
  
  
  return(newobj)
}


polynomials.description <- function(){
# Returns the characteristics and parameters of the surrogate model
#
# Display Name: SURROGATE MODEL
#
# Description
#
# SurrogateTags=list( "regression", "classification", "categorial", "CategoricalInputs") 
# 
# Warnings=list( )
# 
#
#
#

	DisplayName="polynomials model"
	
	Description="Polynomial regression with Ridge penalization. Only available for regression."
	
	SurrogateTags=list(regression=T, classification=F, computesd=F, CategoricalInputs = F)
	Warnings=list()


	return(list(dispname=DisplayName, descr=Description, tags=SurrogateTags, warn=Warnings))
}
