
kriginv.run <- function(PbDefinition, ParametersValues,AdvancedParametersValues){
# Run the optimization and returns the Outputs list
#
# ParametersValues and AdvancedParametersValues contain the values entered in GUI by the user
# e.g. ParametersValues = list(par1=10, par2=0, par3=TRUE ...)
#
	library(KrigInv)
    library(DiceKriging)
	library(DiceDesign)
	
	
	#Define inputs from PbDefinition, for instance: x0, ObjFunc, lb, ub 
	#Run optimization method

	nd <- ncol(PbDefinition$x0)
	design <- data.frame(matrix(runif(2*nd*nd),ncol=nd))
	ObjFunc <- function(x) { 
		if(is.null(dim(x))){x <- matrix(x, nrow=1)}
    	Y <- apply(x, FUN=PbDefinition$ObjFunc, MARGIN=1) 
    	return(matrix(Y,ncol=2)[,2])
  	}

	response <- ObjFunc(design)
	model <- km(formula=~., design = design,
						response = response, covtype="matern3_2")
    temp <- EGI(T=0, model=model, method=AdvancedParametersValues$criterion, fun= ObjFunc, lower=as.vector(PbDefinition$lb), upper=as.vector(PbDefinition$ub), iter=ParametersValues$maxfev)
	
	#Build the Output list from method outputs
	#Outputs=list( xsol, f@sol, constr@sol, distribution, warnings, stopcriteria, neval, niter) ... to be continued
    Outputs <- list(xsol=temp$value)
	
    return(Outputs)
}

kriginv.description <- function(){
# Returns the characteristics and parameters of the optimization method
#
#
	DisplayName="KrigInV"
	Description="Feasible set active learning with kriging surrogate"
	OptimTags=list(constraints=FALSE, categorical=FALSE, multiobj=FALSE, derivative=FALSE, monoobj=TRUE, inversion=TRUE)
	Warnings=list()
	Parameters=list(maxfev=list(default=20,lower=5,upper=Inf,type="integer",description="Maximum number of function evaluations."))
	AdvancedParameters=list(criterion=list(default="bichon",enum=c("bichon","ranjan","vorob","sur"),type="string",description="Pointwise criteria 'bichon' and 'ranjan' are fast to compute. Integral criteria 'vorob' and 'sur' require numerical integration. "))

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
