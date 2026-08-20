# NLOPT optimization library
# isres : Improved Stochastic Ranking Evolution Strategy


isres.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
    library(nloptr)
	
    opts <- append(append(list("algorithm"= "NLOPT_GN_ISRES"),ParametersValues), AdvancedParametersValues)
	
    nd <- ncol(PbDefinition$x0)
    ncons <- PbDefinition$ncons

	ObjFunc <- function(x) { nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ;
	                         Y <- PbDefinition$ObjFunc(x) ; return(matrix(t(Y[,1,drop=F]),nrow=nsim,ncol=1,byrow=F))}
    
    if (ncons > 0) ConsFunc <- function(x) { nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ; 
              Y <- PbDefinition$ObjFunc(x,FALSE) ;
							 return(matrix(t(Y[ , 2:(1+ncons), drop=F]),nrow=nsim,ncol=ncons,byrow=F)) } 
    else ConsFunc = NULL

	IndexMin = 1
	if (nrow(PbDefinition$x0) > 1) {
		# Sort the outputs to find the best point that will be the initial point of the optimizer
		Y0 <- PbDefinition$ObjFunc(PbDefinition$x0)
		IndexMin <- which.min(Y0[,1,drop=F])
	}
    
    res <- nloptr(unlist(PbDefinition$x0[IndexMin,]), ObjFunc, lb = PbDefinition$lb, ub = PbDefinition$ub, eval_g_ineq = ConsFunc, opts=opts)
    
	Outputs <- list(xsol=res$solution)
    return(Outputs)
}

isres.description <- function(){
 
	DisplayName="NLOPT isres"

	Description="The Improved Stochastic Ranking Evolution Strategy (ISRES) algorithm for nonlinearly constrained global optimization (or at least semi-global: although it has heuristics to escape local optima."

	OptimTags=list(constraints=TRUE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	xtol_rel=list(default=1.e-4,lower=0,upper=Inf,type="float",description="Stop when an optimization step (or an estimate of the optimum) changes every parameter by less than xtol_rel multiplied by the absolute value of the parameter. If there is any chance that an optimal parameter is close to zero, you might want to set an absolute tolerance with xtol_abs as well. Criterion is disabled if xtol_rel is non-positive."),
	maxeval=list(default=100,lower=0,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval. This is not a strict maximum: the number of function evaluations may exceed maxeval slightly, depending upon the algorithm. Criterion is disabled if maxeval is non-positive."),
	population=list(default=0,lower=0,upper=Inf,type="integer",description="Size of the population of random points. A population of zero implies that the heuristic default will be used (4 points).")	
	)

	AdvancedParameters=list(
	ranseed=list(default=0,lower=0,upper=Inf,type="integer",description="Set the random seed using ranseed if you want to use a 'deterministic' sequence of pseudorandom numbers, i.e. the same sequence from run to run. If ranseed is 0 (default), the seed for the random numbers is generated from the system time, so that you will get a different sequence of pseudorandom numbers each time you run your program.")
	)
  
	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
