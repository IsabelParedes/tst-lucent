# NLOPT optimization library
# NELDER-MEAD : Local Derivative Free optimizer

neldermead.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(nloptr)
	
  opts <- append(append(list("algorithm"= "NLOPT_LN_NELDERMEAD"),ParametersValues), AdvancedParametersValues)

	IndexMin = 1
	if (nrow(PbDefinition$x0) > 1) {
		# Sort the outputs to find the best point that will be the initial point of the optimizer
		Y0 <- PbDefinition$ObjFunc(PbDefinition$x0)
		IndexMin <- which.min(Y0[,1,drop=F])
	}

	res <- nloptr(unlist(PbDefinition$x0[IndexMin,]), PbDefinition$ObjFunc, lb = PbDefinition$lb, ub = PbDefinition$ub, opts=opts, isNewIteration=TRUE)
    
	Outputs <- list(xsol=res$solution)
  return(Outputs)
}

neldermead.description <- function(){
# Display Name: Short name of the optimizer
#
# Description: short description of the optimization method
#
# OptimTags=list(constraints=FALSE, categorical=FALSE, multiobj=FALSE, derivative=FALSE)
# 
# Warnings=list( nd=list(lower=, upper=, message=""), ncat=list(lower=, upper=, message="") )
# 	Warnings on optimizer application perimeter : ranges of values for problem entries
#   Filter the optimizers: check the optimizers that are compatible with the type of problems
#
# Parameters=list( 	param1=list(default=10, lower=NA, upper=NA, enum=c(1,5,10), type=, description=""),
#                  	param2=list(default=10, lower=1, upper=10, type="integer"/float/string/enum, description="" ),
#					param3=list(default=1.1)  ) 
# 	list of optimizer parameters the user can modify with the default values, the restriction, the description 
#
# Advanced parameters: same structure
#
  DisplayName="NLOPT NELDER-MEAD"
  
  Description="Nelder-Mead simplex algorithm, a derivative-free local optimizer"
  
  OptimTags=list(constraints=FALSE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)
  
  Warnings=list()
  
  Parameters=list(
  stopval=list(default=-Inf,lower=-Inf,upper=Inf,type="float",description="Stop minimization when an objective value <= stopval is found.Setting stopval to -Inf disables this stopping criterion (default)."),
	ftol_rel=list(default=0,lower=0,upper=Inf,type="float",description="Stop when an optimization step (or an estimate of the optimum) changes the objective function value by less than ftol_rel multiplied by the absolute value of the function value. If there is any chance that your optimum function value is close to zero, you might want to set an absolute tolerance with ftol_abs as well. Criterion is disabled if ftol_rel is non-positive (default)."),
	xtol_rel=list(default=1.e-4,lower=0,upper=Inf,type="float",description="Stop when an optimization step (or an estimate of the optimum) changes every parameter by less than xtol_rel multiplied by the absolute value of the parameter. If there is any chance that an optimal parameter is close to zero, you might want to set an absolute tolerance with xtol_abs as well. Criterion is disabled if xtol_rel is non-positive."),
	maxeval=list(default=100,lower=0,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval. This is not a strict maximum: the number of function evaluations may exceed maxeval slightly, depending upon the algorithm. Criterion is disabled if maxeval is non-positive.")
	)
	
  AdvancedParameters=list(
	ftol_abs=list(default=0,lower=0,upper=Inf,type="float",description="Stop when an optimization step (or an estimate of the optimum) changes the function value by less than ftol_abs. Criterion is disabled if ftol_abs is non-positive (default)."),
	maxtime=list(default=0,lower=-1,upper=Inf,type="float",description="Stop when the optimization time (in seconds) exceeds maxtime. This is not a strict maximum: the time may exceed maxtime slightly, depending upon the algorithm and on how slow your function evaluation is. Criterion is disabled if maxtime is non-positive (default)."),
	print_level=list(default=0,enum=c(0,1,2,3),type="integer",description="The option print_level controls how much output is shown during the	optimization process. Possible values: 0 (default): no output; 1: show iteration number and value of objective function; 2: 1 + show value of (in)equalities; 3: 2 + show value of controls.")
	)
  
  return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
