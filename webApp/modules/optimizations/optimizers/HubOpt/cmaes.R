# HubOpt optimization library
# CMAES : Global Derivative Free optimizer with bound constraints 

cmaes.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(HubOpt4R) #IFPEN library
  source("optimizers/HubOpt/CreateDefaultXML.R")
  
  ParametersValues$solver = "CMAES"
  CreateDefaultXML(ParametersValues,AdvancedParametersValues,HOpath=PbDefinition$savepath)
  
  nd <- PbDefinition$nd
  ncons <- PbDefinition$ncons
  
  ObjFunc <- function(x) { nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ; 
                           Y <- PbDefinition$ObjFunc(x) ; 
                           return(matrix(t(Y[,1,drop=F]),nrow=nsim,ncol=1,byrow=F)) }
  
  IndexMin = 1
  if (nrow(PbDefinition$x0) > 1) IndexMin <- which.min(Y0[,1,drop=F])

  resHO <- HubOpt(list(ObjFunc), unlist(PbDefinition$x0[IndexMin,]), nd, PbDefinition$lb, PbDefinition$ub, nx0=0, nobj=1, HOPATH=PbDefinition$savepath)
   
  Outputs <- list(xsol=resHO[[2]]) 
  return(Outputs)
}

cmaes.description <- function(){

	DisplayName="HubOpt CMAES"

	Description="CMAES performs local derivative-free optimization based on Covariance Matrix Adapting Evolutionary Strategy."
	
	OptimTags=list(constraints=FALSE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	maxeval=list(default=100,lower=0,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval.")
	)

	AdvancedParameters=list(
	npoints=list(default=0,lower=100,upper=Inf,type="integer",description="Size of the generation, if value=0: round(4 + 3*log(nx))."),
	seed=list(default=42,lower=0,upper=100000,type="integer",description="Random seed (if <> 0 the seed is fixed)"),
	print_level=list(default=0,enum=c(0,10,20,30),type="integer",description="Display level: 0 mute, 1 display error, 10 display warnings, 20 display info, 30 display debug.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
  