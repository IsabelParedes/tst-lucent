# HubOpt optimization library
# Multi-start : multiple start of local optimizer 

multistart.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(HubOpt4R) #IFPEN library
  source("optimizers/HubOpt/CreateDefaultXML.R")
  
  ParametersValues$solver = "MULTI-START"

 # The number of initial points (ParametersValues$nx0) is passed to HubOpt via the XML HubOpt input file
 # It can be larger than the size of selected x0, HubOpt completes with random points
  nselectedx0 = nrow(PbDefinition$x0)
  print(c(nrow(PbDefinition$x0),ncol(PbDefinition$x0)))
  if (ParametersValues$nx0 < nselectedx0){
     ParametersValues$nx0 = nselectedx0
  }
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
  
  if (nselectedx0 > 0){
     x0 = PbDefinition$x0
     nDOEfixed <- length(x0)/nd
     #Y0 <- matrix(t(PbDefinition$ObjFunc(x0)),nrow=nDOEfixed,ncol=ncons+1,byrow=F)
     Y0 <- PbDefinition$ObjFunc(x0)
     XDOEfixed = cbind(x0,Y0)
  }else{
     XDOEfixed=NULL
     nDOEfixed=0
  }

  resHO <- HubOpt(list(ObjFunc), 
                  NULL,
                  #unlist(PbDefinition$x0[1,]), 
                  nd, PbDefinition$lb, PbDefinition$ub, nobj=1, 
                  XDOEfixed=as.vector(t(XDOEfixed)), nDOEfixed=nDOEfixed,
                  DFCON=list(ConsFunc), nid=ncons, ld=rep(-Inf,ncons), ud=rep(0,ncons), HOPATH=PbDefinition$savepath)
  
  Outputs <- list(xsol=resHO[[2]])
  return(Outputs)
}

multistart.description <- function(){

	DisplayName="HubOpt Multi-Start"

	Description="Multi-Start method runs multiple local derivative-free optimization methods (SQA or SQPAL)."

	OptimTags=list(constraints=TRUE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	nx0=list(default=10,lower=0,upper=Inf,type="integer",description="Total number of initial points for the multiple local optimizations (selected and/or chosen by the algorithm)."),
	localsolver=list(default="SQA",enum=c("SQA","SQPAL"),type="string",description="Local solver: SQA or SQPAL"),
	dx_init=list(default=1,lower=0,upper=Inf,type="float",description="Initial perturbation of x."),
	dx_min=list(default=1.e-4,lower=0,upper=Inf,type="float",description="Minimal perturbation of x."),
	maxeval=list(default=100,lower=0,upper=Inf,type="integer",description="Stop the local solver when the number of function evaluations exceeds maxeval.")
	)
	
	AdvancedParameters=list(
	seed=list(default=42,lower=0,upper=100000,type="integer",description="Random seed for initial sampling (if <> 0 the seed is fixed)."),
	npoints=list(default=0,enum=c(-3,-2,-1,0),type="integer",description="Number of interpolation points used to build quadratic models in [nx+2,(nx+1)(nx+2)/2] : 0 default value=(2nx+1), -1 min value=(nx+2), -2 max value=0.5(nx+1)(nx+2), -3 mean value=0.25(nx_+2)(nx_+3)."),
	weightMerit=list(default=100,lower=0,upper=Inf,type="float",description="Weight for merit function in case of derivative free constraints."),
	print_level=list(default=0,enum=c(0,10,20,30),type="integer",description="Display level: 0 mute, 1 display error, 10 display warnings, 20 display info, 30 display debug.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
  