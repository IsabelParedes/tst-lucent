# SCIPY optimization library
# COBYQA : Local Derivative Free optimizer with black-box constraints
# 
# Requirement:  package reticulate
# to specify the version of python to be used : 
# library(reticulate);use_python(Sys.which("python"))
#

cobyqa.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(reticulate)
  
  np = reticulate::import("numpy", delay_load = TRUE)
  optimize = reticulate::import("scipy.optimize", delay_load = TRUE)
  cobyqa = reticulate::import("cobyqa", delay_load = TRUE)

  opts <- dict(append(ParametersValues, AdvancedParametersValues), convert = TRUE)
  
  nd <- ncol(PbDefinition$x0)
  ncons <- PbDefinition$ncons

  # scipy optimizers require separated OF and constraints computations: 
  # call twice the simulationLauncher with the same x (simulationLauncher detects duplicated points -> only one simulation !)

  ObjFunc <- function(x) { 
    Y <- PbDefinition$ObjFunc(x)
    return(Y[1])
  }

  bounds = optimize$Bounds(as.vector(PbDefinition$lb), as.vector(PbDefinition$ub))
  bounds = t(rbind(PbDefinition$lb, PbDefinition$ub))

  IndexMin = 1
  if (nrow(PbDefinition$x0) > 1) {
  # Sort the outputs to find the best feasible point that will be the initial point of the optimizer
    Y0 <- PbDefinition$ObjFunc(PbDefinition$x0)
    if (ncons > 0){
      indexFeas = which(rowSums(pmax(Y0[,2:(1+ncons),drop=F],0))==0) #consider only active constraints (C<0)
      if (length(indexFeas)>0){ #best OF among feasible initial points
        IndexMin <- indexFeas[which.min(Y0[indexFeas,1,drop=F])]
      }else{ #best compromise (OF,Constraints)
        Ytemp = cbind(Y0[,1,drop=F],pmax(Y0[,2:(1+ncons),drop=F],0)) #consider only active constraints (C<0)
        IndexMin <- which.min(rowSums(Ytemp[,,drop=FALSE]))
      }
    }else IndexMin <- which.min(Y0[,1,drop=F])
  }

  if (ncons > 0) {
    ConsFunc <- function(x) {
      Y <- PbDefinition$ObjFunc(x, FALSE)
      return(-Y[2:(1 + ncons)])
    }
    constraints = optimize$NonlinearConstraint(ConsFunc, replicate(ncons,-np$inf), replicate(ncons,0))
    res = optimize$minimize(ObjFunc, as.vector(unlist(PbDefinition$x0[IndexMin,])), bounds = bounds, constraints=constraints, method="COBYQA", options=opts)
  }else{
    res = optimize$minimize(ObjFunc, as.vector(unlist(PbDefinition$x0[IndexMin,])), bounds = bounds, method="COBYQA", options=opts)
  }
  
  Outputs <- list(xsol=res$x)
  return(Outputs)
}

cobyqa.description <- function(){

	DisplayName="SCIPY COBYQA"

	Description="COBYQA is an algorithm for local derivative-free optimization with nonlinear inequality constraints using iteratively constructed quadratic approximations for the objective function and the constraints."

	OptimTags=list(constraints=TRUE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()
	Parameters=list(
	final_tr_radius=list(default=1.e-4,lower=0,upper=Inf,type="float",description="Final accuracy in the optimization (not precisely guaranteed). This is a lower bound on the size of the trust region.."),
	maxfev=list(default=100,lower=5,upper=Inf,type="integer",description="Maximum number of function evaluations."),
	feasibility_tol=list(default=2.e-4,lower=0,upper=Inf,type="float",description="Tolerance (absolute) for constraint violations.")
	)

	AdvancedParameters=list(
	initial_tr_radius=list(default=0.5,lower=0,upper=Inf,type="float",description="Reasonable initial changes to the variables."),
	disp=list(default=FALSE,type="logical",description="Set to TRUE to print convergence messages. If FALSE, verbosity is ignored as set to 0.")
	)
	  
	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}

