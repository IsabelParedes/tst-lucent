# polynomials (scikit-learn Python library) : polynomial ridge regression

.poly_cv_loo <- function(poly, np) {
  cv <- poly$cv_results_
  if (inherits(cv, "python.builtin.object")) {
    idx <- as.integer(py_to_r(np$flatnonzero(np$isclose(poly$alphas, poly$alpha_))[0L]))
    loo <- if (as.integer(cv$ndim) == 2L) cv[, idx] else cv[, 0L, idx]
    return(as.numeric(py_to_r(loo)))
  }
  alphas <- as.numeric(poly$alphas)
  alpha <- as.numeric(poly$alpha_)
  idx <- which.min(abs(alphas - alpha))
  d <- dim(cv)
  if (length(d) == 2L) as.numeric(cv[, idx]) else as.numeric(cv[, 1L, idx])
}

polynomials.build <- function(Xmodel, y, Ytype, categorical, levels){

  obj <- list()
	library(reticulate)

  # import modules (sklearn before numpy on wasm)
  sk_lm <- import("sklearn.linear_model", delay_load = TRUE)
  sk_preproc <- import("sklearn.preprocessing", delay_load = TRUE)
  np <- import("numpy", delay_load = TRUE)
  currentScore <- -Inf
  scalerY <- sk_preproc$StandardScaler()
  y <-  scalerY$fit_transform(as.matrix(y, ncol = 1))

  for(degree in seq(1,6)){
    poly_features <- sk_preproc$PolynomialFeatures(degree=degree)
    X_poly <- poly_features$fit_transform(as.matrix(Xmodel))
    scalerX <- sk_preproc$StandardScaler()
    X_poly <- scalerX$fit_transform(X_poly)
    poly <- sk_lm$RidgeCV(alphas=c(1e-3,0.01,0.1,1), scoring="r2", fit_intercept=T, store_cv_results=T)$fit(X_poly,y)
    isBest <- as.numeric(py_to_r(poly$best_score_)) > currentScore
    if(isBest){
      currentScore <- as.numeric(py_to_r(poly$best_score_))
      currentModel <- list(poly=poly, poly_feat = poly_features, scalerX=scalerX, scalerY=scalerY)
      currentYloo <- .poly_cv_loo(poly, np)
    }
  }

  # Store results in metamodel object
  obj$Q2loo <- currentScore
  obj$yloo <- as.numeric(scalerY$inverse_transform(matrix(currentYloo, ncol = 1)))
  obj$Ylevels <- NULL
  obj$model <- currentModel
  obj$categorical <- categorical
  obj$levels <- levels

  return(obj)
}


polynomials.predict <- function(obj, Xmodel, computesd){
  library(reticulate)
  
  # import modules (sklearn before numpy on wasm)
  sk_lm <- import("sklearn.linear_model", delay_load = TRUE)
  sk_preproc <- import("sklearn.preprocessing", delay_load = TRUE)
  np <- import("numpy", delay_load = TRUE)
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
  
  # import modules (sklearn before numpy on wasm)
  sk_lm <- import("sklearn.linear_model", delay_load = TRUE)
  sk_preproc <- import("sklearn.preprocessing", delay_load = TRUE)
  np <- import("numpy", delay_load = TRUE)
  
  poly_features <- obj$model$poly_feat
  poly <- obj$model$poly
  scalerX <- obj$model$scalerX
  scalerY <- obj$model$scalerY
  
  X_poly <- poly_features$fit_transform(as.matrix(Xmodel))
  X_poly <- scalerX$fit_transform(X_poly)
  y <-  scalerY$fit_transform(as.matrix(y, ncol = 1))
  poly <- sk_lm$RidgeCV(alphas=c(1e-3,0.01,0.1,1), scoring="r2", fit_intercept=T, store_cv_results=T)$fit(X_poly,y)
  currentScore <- as.numeric(py_to_r(poly$best_score_))
  currentModel <- list(poly=poly, poly_feat = poly_features, scalerX=scalerX, scalerY=scalerY)
  currentYloo <- .poly_cv_loo(poly, np)
  
  # Store results in metamodel object
  newobj$Q2loo <- currentScore
  newobj$model <- currentModel
  newobj$yloo <- as.numeric(scalerY$inverse_transform(matrix(currentYloo, ncol = 1)))
  
  
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
