# HubOpt optimization library
# NOMAD : Derivative Free optimizer with constraints
 
nomad.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(HubOpt4R) #IFPEN library
  source("optimizers/HubOpt/CreateDefaultXML.R")
  
  ParametersValues$solver = "NOMAD"
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
  
  indCat = NULL
  if (PbDefinition$tags$categorical) {
    #identify categorical/integer variables by their indices
    indint = which( PbDefinition$inputflag == "I")
    indCat = which( PbDefinition$inputflag == "Cat")
    ny = length(c(indint,indCat))
    ycategorical = 0 #no categorical adapted poll step for categorical (except when coupled with EGO)
    yindex = c(indint,indCat)-1
  }else{
    ny=0
	yindex=NULL
	ycategorical=0
  }
  
  print(paste0("nomad with ",ny," integer variables of index "))
  print(yindex)
  
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
                  DFCON=list(ConsFunc), nid=ncons, ld=rep(-Inf,ncons), ud=rep(0,ncons), 
                  ny=ny, yindex=yindex, ycategorical=ycategorical,
                  HOPATH=PbDefinition$savepath)
  
  Outputs <- list(xsol=resHO[[2]])
  return(Outputs)
}

nomad.description <- function(){

	DisplayName="HubOpt NOMAD"

	Description="NOMAD performs global derivative-free constrained optimization with a Mesh Adaptive Direct Search algorithm (MADS)."

	OptimTags=list(constraints=TRUE, categorical=TRUE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	dx_init=list(default=1,lower=0,upper=Inf,type="float",description="Initial mesh size."),
	dx_min=list(default=1.e-6,lower=0,upper=Inf,type="float",description="Minimal mesh size."),
	maxeval=list(default=100,lower=0,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval.")
	)

	AdvancedParameters=list(
	print_level=list(default=0,enum=c(0,10,20,30),type="integer",description="Display level: 0 mute, 1 display error, 10 display warnings, 20 display info, 30 display debug.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
  