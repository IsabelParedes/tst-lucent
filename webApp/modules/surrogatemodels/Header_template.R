#
# Template for surrogates : 1 folder for each library (ex. e1071), 1 file for each model (SVM, polynomials ...)
#
# listSurrogates.R: contains the list of model files .R associated with the models 
# if this file does not exist, all the files *.R in the folder are considered
#
# each file associated with a model should contain:
# - a function *model*.description with *model* = name of the surrogate model (same as the name of the file)
# - a function *model*.build = build the model on given DoE
# - a function *model*.predict = predict the model on given points
# - a function *model*.update = update the model with additional points
#


model.build <- function(Xmodel, y, Ytype){
# Build the surrogate model and return obj list
#
# Xmodel : a matrix of dim(m,n), DoE inputs
# y : a vector of length m, DoE output values
# Ytype : characters 'numeric' or 'categorical', output type 

	obj <- list()
	library(...)
	
	# Build surrogate model depending of output type `Ytype`: at least one model for regression or classification must be implemented. Fill `SurrogateTags` in `model.description()` accordingly.
	# Compute leave-one-out y value `yloo` and Q2 `Q2loo`
	if (Ytype == 'numeric'){  			# Only relevant if model manages 'numeric' outputs. Set `regression = TRUE` in `model.description()`
		obj$Ylevels <- NULL
	}

	if (Ytype == 'categorical'){ 		# Only relevant if model manages 'categorical' outputs. Set `classification = TRUE` in `model.description()`
		obj$Ylevels <- ... # levels (categorical values) of Y 
	}
	
	
	#Build the obj list from model outputs
		obj$Q2loo <- ...  # leave-one-out Q2 value
		obj$yloo <-  ...  # leave-one-out Y values
		obj$model <- ...  # surrogate model
	# you can add other useful args for prediction to obj (e.g. a scaler, ...) 
    return(obj)
}


model.predict <- function(obj, Xmodel, computesd){
# Predict points 
# Xmodel : a matrix of dim(m,n), DoE inputs
# obj 
# computesd : boolean, indicate computation of standard deviation of prediction 
#

	library(...)
	ysd <- NULL
	model <- obj$model

	# Compute surrogate model prediction : at least one model for regression or classification must be implemented. Fill `SurrogateTags` in `model.description()` accordingly.

	if (obj$Ytype == 'numeric'){        # Only relevant if model manages 'numeric' outputs. Set `regression = TRUE` in `model.description()`
		ymean <- ... # prediction 
		if(computesd){
			ysd <- ... 					# standard deviation error on prediction. Set `predict.sd = TRUE` in `model.description()`
		}
	}

	if (obj$Ytype  == 'categorical'){	# Only relevant if model manages 'categorical' outputs. Set `classification = TRUE` in `model.description()`
		ymean <- ...
		if(computesd){
			ysd <- ...
		}
	}
	
	
  Outputs <- list(ymean=ymean, ysd=ysd)
return(Outputs)


model.update <- function(obj, Xmodel, y){
# Update surrogate model 
# Xmodel : a matrix of dim(m,n), additional inputs
# obj 
# y : a vector of length m, additional output values
#
	newobj <- list()
	library(...)
	 Ytype <- obj$Ytype
	#Update surrogate model depending of output y type and compute leave-one-out y value and Q2
	if (Ytype == 'numeric'){
		newobj$Ylevels <- NULL
	}

	if (Ytype == 'categorical'){
		newobj$Ylevels <- # levels (categorical values) of Y 
	}
	
	
	#Build the obj list from model outputs
		newobj$Q2loo <- # leave-one-out Q2 value
		newobj$yloo <-  # leave-one-out Y values
		newobj$model <- # surrogate model
	# you can add other useful args for prediction to newobj, same as obj (e.g. a scaler, ...) 
	
    return(newobj)


model.description <- function(){
# Returns the characteristics and parameters of the surrogate model
#
# Display Name: SURROGATE MODEL
#
# Description
#
# SurrogateTags=list( "regression", "classification", "predict.sd") 
# set `regression = TRUE` if regression model available (corresponding to: `obj$Ytype == 'numeric'`), FALSE otherwise. 
# set `classification = TRUE` if classification model available (corresponding to: `obj$Ytype == 'categorical'`), FALSE otherwise. 
# set `predict.sd = TRUE` if a measure of prediction error (ysd) is available, FALSE otherwise. 
#
# Warnings=list( )
# 



	DisplayName=""
	Description=""
	SurrogateTags=list(classification=FALSE, regression=FALSE, predict.sd = FALSE)
	Warnings=list()

	return(list(dispname=DisplayName, descr=Description, tags=SurrogateTags, warn=Warnings))
}
