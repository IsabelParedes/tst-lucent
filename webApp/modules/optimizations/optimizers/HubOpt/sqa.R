# HubOpt optimization library
# SQA : Local Derivative Free optimizer with derivative based and black-box constraints 

sqa.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(HubOpt4R) #IFPEN library
  source("optimizers/HubOpt/CreateDefaultXML.R")
  
  ParametersValues$solver = "SQA"
  CreateDefaultXML(ParametersValues,AdvancedParametersValues,HOpath=PbDefinition$savepath)
  
  nd <- PbDefinition$nd
  ncons <- PbDefinition$ncons
  
  # HubOpt4R package requires separated OF and constraints computations: 
  # call twice the simulationLauncher with the same x (simulationLauncher detects duplicated points -> only one simulation !)
  ObjFunc <- function(x) { nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ; 
                           Y <- PbDefinition$ObjFunc(x) ; return(matrix(t(Y[,1,drop=F]),nrow=nsim,ncol=1,byrow=F))}
  
  if (ncons > 0) ConsFunc <- function(x) { 
     nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ; 
     Y <- PbDefinition$ObjFunc(x, FALSE) 
     return(matrix(t(Y[ , 2:(1+ncons), drop=F]),nrow=nsim,ncol=ncons,byrow=F)) 
     } else ConsFunc = NULL
  
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

  resHO <- HubOpt(list(ObjFunc), unlist(PbDefinition$x0[IndexMin,]), nd, PbDefinition$lb, PbDefinition$ub, nx0=1, nobj=1, 
                  DFCON=list(ConsFunc), nid=ncons, ld=rep(-Inf,ncons), ud=rep(0,ncons), HOPATH=PbDefinition$savepath)  
  
  Outputs <- list(xsol=resHO[[2]])
  return(Outputs)
}

sqa.description <- function(){

	DisplayName="HubOpt SQA"

	Description="SQA performs local derivative-free constrained optimization using an iteratively constructed quadratic approximation for the objective function."

	OptimTags=list(constraints=TRUE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	dx_init=list(default=1,lower=0,upper=Inf,type="float",description="Initial trust region = reasonable relative initial changes to the variables."),
	dx_min=list(default=1.e-4,lower=0,upper=Inf,type="float",description="Stop when an optimization step (or an estimate of the optimum) changes every parameter by less than xtol_rel multiplied by the absolute value of the parameter. If there is any chance that an optimal parameter is close to zero, you might want to set an absolute tolerance with xtol_abs as well. Criterion is disabled if xtol_rel is non-positive."),
	maxeval=list(default=100,lower=0,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval.")
	)

	AdvancedParameters=list(
	npoints=list(default=0,enum=c(-3,-2,-1,0),type="integer",description="Number of interpolation points used to build quadratic models in [nx+2,(nx+1)(nx+2)/2] : 0 default value=(2nx+1), -1 min value=(nx+2), -2 max value=0.5(nx+1)(nx+2), -3 mean value=0.25(nx_+2)(nx_+3)."),
	weightMerit=list(default=100,lower=0,upper=Inf,type="float",description="Weight for merit function in case of derivative free constraints."),
	print_level=list(default=0,enum=c(0,10,20,30),type="integer",description="Display level: 0 mute, 1 display error, 10 display warnings, 20 display info, 30 display debug.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
  