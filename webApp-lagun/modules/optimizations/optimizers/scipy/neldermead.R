# SCIPY optimization library
# NELDER-MEAD : Local Derivative Free optimizer

neldermead.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(reticulate)
  
  np = reticulate::import("numpy")
  optimize = reticulate::import("scipy.optimize")
  
  bounds = t(rbind(PbDefinition$lb,PbDefinition$ub))
  opts <- dict(append(ParametersValues, AdvancedParametersValues), convert = TRUE)
  
  IndexMin = 1
  if (nrow(PbDefinition$x0) > 1) {
    # Sort the outputs to find the best point that will be the initial point of the optimizer
    Y0 <- PbDefinition$ObjFunc(PbDefinition$x0)
    IndexMin <- which.min(Y0[,1,drop=F])
  }

  res = optimize$minimize(PbDefinition$ObjFunc, as.vector(unlist(PbDefinition$x0[IndexMin,])), method="Nelder-Mead", bounds = bounds, options=opts)
  
  Outputs <- list(xsol=res$x)
  
  return(Outputs)
}

neldermead.description <- function(){
 
 	####options = list(maxfev=10,maxiter=10,disp=T)
	DisplayName="SCIPY NELDER-MEAD"

	Description="NELDER-MEAD is an algorithm for local derivative-free optimization with nonlinear inequality and equality constraints using iteratively constructed linear approximations for the objective function and the constraints."

	OptimTags=list(constraints=FALSE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	maxiter=list(default=200,lower=0,upper=Inf,type="integer",description="Maximum allowed number of iterations and function evaluations. Will default to N*200, where N is the number of variables, if neither maxiter or maxfev is set. If both maxiter and maxfev are set, minimization will stop at the first reached."),
	maxfev=list(default=200,lower=0,upper=Inf,type="integer",description="Maximum allowed number of iterations and function evaluations. Will default to N*200, where N is the number of variables, if neither maxiter or maxfev is set. If both maxiter and maxfev are set, minimization will stop at the first reached."),
	xatol=list(default=0,lower=0,upper=Inf,type="float",description="Absolute error in xopt between iterations that is acceptable for convergence.."),
	fatol=list(default=0,lower=0,upper=Inf,type="float",description="Absolute error in func(xopt) between iterations that is acceptable for convergence.")
	)
	
	AdvancedParameters=list(
	adaptive=list(default=FALSE,type="logical",description="Adapt algorithm parameters to dimensionality of problem. Useful for high-dimensional minimization."),
	disp=list(default=FALSE,type="logical",description="Set to TRUE to print convergence messages."),
	return_all=list(default=FALSE,type="logical",description="Set to TRUE to return a list of the best solution at each of the iterations.")
	)
	  
	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}

