# optim (stat package) : Simulated Annealing (SANN) method

SANN.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
    
    IndexMin = 1
	if (nrow(PbDefinition$x0) > 1) {
		# Sort the outputs to find the best point that will be the initial point of the optimizer
		Y0 <- PbDefinition$ObjFunc(PbDefinition$x0)
		IndexMin <- which.min(Y0[,1,drop=F])
	}

	solution <- optim(unlist(PbDefinition$x0[IndexMin,]), PbDefinition$ObjFunc, method = "SANN", control=append(ParametersValues,AdvancedParametersValues))
	
	Outputs <- list(solution$par)
    return(Outputs)	
}

SANN.description <- function(){

	DisplayName="optim SANN"

	Description="Simulated-annealing belongs to the class of stochastic global optimization methods. It uses only function values but is relatively slow. It will also work for non-differentiable functions. This implementation uses the Metropolis function for the acceptance probability."
	
	OptimTags=list(constraints=FALSE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	fnscale=list(default=1,lower=-Inf, upper=Inf,type="float",description="An overall scaling to be applied to the value of fn and gr during optimization. If negative, turns the problem into a maximization problem. Optimization is performed on fn(par)/fnscale."),
	abstol=list(default=1.e-8,lower=1.e-18,upper=1,type="float",description="The absolute convergence tolerance. Only useful for non-negative functions, as a tolerance for reaching zero."),
	reltol=list(default=1.e-8,lower=1.e-18,upper=1,type="float",description="Relative convergence tolerance. The algorithm stops if it is unable to reduce the value by a factor of reltol * (abs(val) + reltol) at a step."),
	maxit=list(default=500,lower=0,upper=Inf,type="integer",description="The maximum number of iterations."),
	trace=list(default=0,enum=c(0,1,2,3,4,5,6),type="integer",description="If positive, tracing information on the progress of the optimization is produced. Higher values may produce more tracing information.")
	)

	AdvancedParameters=list(
	REPORT=list(default=100,lower=0,upper=Inf,type="integer",description="The frequency of reports for SANN methods Parameters$trace is positive."),
	temp=list(default=10,lower=0,upper=Inf,type="float",description="Starting temperature for the cooling schedule."),
	tmax=list(default=10,lower=0,upper=Inf,type="integer",description="Number of function evaluations at each temperature.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}