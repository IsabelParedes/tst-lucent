# optim (stat package) : Nelder-Mead method

neldermead.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
    
    IndexMin = 1
	if (nrow(PbDefinition$x0) > 1) {
		# Sort the outputs to find the best point that will be the initial point of the optimizer
		Y0 <- PbDefinition$ObjFunc(PbDefinition$x0)
		IndexMin <- which.min(Y0[,1,drop=F])
	}

	solution <- optim(unlist(PbDefinition$x0[IndexMin,]), PbDefinition$ObjFunc, method = "Nelder-Mead", control=append(ParametersValues,AdvancedParametersValues))
	
	Outputs <- list(solution$par)
    return(Outputs)
	
}


neldermead.description <- function(){

	DisplayName="optim Nelder-Mead"

	Description="Nelder and Mead uses only function values and is robust but relatively slow. It will work reasonably well for non-differentiable functions."
	
	OptimTags=list(constraints=FALSE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	fnscale=list(default=1,lower=-Inf, upper=Inf,type="float", description="An overall scaling to be applied to the value of fn and gr during optimization. If negative, turns the problem into a maximization problem. Optimization is performed on fn(par)/fnscale."),
	abstol=list(default=0,lower=1.e-18,upper=1,type="float",description="The absolute convergence tolerance. Only useful for non-negative functions, as a tolerance for reaching zero."),
	reltol=list(default=1.e-8,lower=1.e-18,upper=1,type="float",description="Relative convergence tolerance. The algorithm stops if it is unable to reduce the value by a factor of reltol * (abs(val) + reltol) at a step."),
	maxit=list(default=500,lower=0,upper=Inf,type="integer",description="The maximum number of iterations."),
	trace=list(default=0,enum=c(0,1,2,3,4,5,6),type="integer",description="If positive, tracing information on the progress of the optimization is produced. Higher values may produce more tracing information.")
	)

	AdvancedParameters=list(
	alpha=list(default=1,lower=0,upper=Inf,type="float",description="Scaling parameter: the reflection factor."),
	beta=list(default=0.5,lower=0,upper=1,type="float",description="Scaling parameter: the contraction factor."),
	gamma=list(default=2,lower=1,upper=Inf,type="float",description="Scaling parameter: the expansion factor.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}