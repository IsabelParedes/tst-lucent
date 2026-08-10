# HubOpt optimization library
# EGO : Derivative Free optimizer with constraints

ego.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(HubOpt4R) #IFPEN library
  source("optimizers/HubOpt/CreateDefaultXML.R")
  
  ParametersValues$solver = "EGO"

  CreateDefaultXML(ParametersValues,AdvancedParametersValues,HOpath=PbDefinition$savepath)
  
  nd <- PbDefinition$nd
  ncons <- PbDefinition$ncons
  
  ObjFunc <- function(x) { nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ; 
                           Y <- PbDefinition$ObjFunc(x) ; 
                           return(matrix(t(Y[,1,drop=F]),nrow=nsim,ncol=1,byrow=F)) }
  
  if (PbDefinition$tags$categorical) {
    #identify categorical/integer variables by their indices
    indint = which( PbDefinition$inputflag == "I")
    indCat = which( PbDefinition$inputflag == "Cat")
    ny = length(c(indint,indCat))
    ycategorical = 1
    yindex = c(indint,indCat)-1
  }
  else{
    ny=0
	yindex=NULL
	ycategorical=0
  }
  
  if (nrow(PbDefinition$x0) > 1){ #User has selected more than one point => specified an initial DOE
     x0 = PbDefinition$x0
     nDOEfixed <- length(x0)/nd
     # Run the simulations for the specified DOE (if already simulated, the results are read in the folder)
     #Y0 <- matrix(t(PbDefinition$ObjFunc(x0)),nrow=nDOEfixed,ncol=1,byrow=F)
     Y0 <- PbDefinition$ObjFunc(x0)
     XDOEfixed = cbind(x0,Y0)
     XDOEfixed = as.vector(t(XDOEfixed))
  }else{
     XDOEfixed=NULL
     nDOEfixed=0
  }
  
  resHO <- HubOpt(list(ObjFunc), 
                  NULL, #unlist(PbDefinition$x0[1,]), 
                  nd, PbDefinition$lb, PbDefinition$ub, nobj=1,
                  nx0=0, XDOEfixed=XDOEfixed, nDOEfixed=nDOEfixed,
                  ny=ny, yindex=yindex, ycategorical=ycategorical,
                  HOPATH=PbDefinition$savepath)
  
  #resHO<- HubOpt(list(func), x0, nx, lb, ub, nx0=1, 
  #               XDOEfixed=NULL, nDOEfixed=0, 
  #               ny=0, yindex=NULL, ycategorical=0,
  #               nobj=1, A=NULL, nil=0, B=NULL, Aeq=NULL, nel=0, Beq=NULL, 
  #               NLCON="", ninl=0, nenl=0, 
  #               DFCON="", nid=0, ld=NULL, ud=NULL,  
  #               xn=NULL, XMLFILE='./HO.xml', HOPATH='./')
  
  Outputs <- list(xsol=resHO[[2]])
  return(Outputs)
  
}

ego.description <- function(){

	DisplayName="HubOpt EGO"

	Description="EGO performs global derivative-free unconstrained optimization based on Gaussian Process surrogate models."

	OptimTags=list(constraints=FALSE, categorical=TRUE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	maxeval=list(default=100,lower=5,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval."),
	nx0=list(default=5,lower=5,upper=Inf,type="integer",description="Number of initial points."),
	nx_iter=list(default=1,lower=0,upper=Inf,type="integer",description="Maximal number of points per iteration.")
	)

	AdvancedParameters=list(
	seed=list(default=42,lower=0,upper=100000,type="integer",description="Random seed for initial sampling (if <> 0 the seed is fixed)."),
	dx_min=list(default=1.e-6,lower=0,upper=Inf,type="float",description="Minimal distance between simulated points."),
	print_level=list(default=0,enum=c(0,10,20,30),type="integer",description="Display level: 0 mute, 1 display error, 10 display warnings, 20 display info, 30 display debug.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
  