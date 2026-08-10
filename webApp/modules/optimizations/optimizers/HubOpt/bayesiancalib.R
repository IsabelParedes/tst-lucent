# HubOpt optimization library
# Bayesian Calibration based on Gaussian processes

bayesiancalib.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
  library(HubOpt4R) #IFPEN library
  source("optimizers/HubOpt/CreateDefaultXML.R")
  
  ParametersValues$solver = "MCMC"
  CreateDefaultXML(ParametersValues,AdvancedParametersValues,HOpath=PbDefinition$savepath)
  
  nd <- PbDefinition$nd
  
  ObjFunc <- function(x) { nsim <- length(x)/nd ; x = matrix(x,nrow=nsim,ncol=nd,byrow=TRUE) ; 
                           Y <- PbDefinition$ObjFunc(x) ; 
                           return(matrix(t(Y[,1,drop=F]),nrow=nsim,ncol=1,byrow=F)) }
  
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

  resHO <- HubOpt(list(ObjFunc), NULL, #unlist(PbDefinition$x0[1,]), 
                  nd, PbDefinition$lb, PbDefinition$ub, nobj=1,
                  nx0=0, XDOEfixed=XDOEfixed, nDOEfixed=nDOEfixed,
                  HOPATH=PbDefinition$savepath)

  
  paramQuantiles <- t(matrix(resHO[[3]],nrow=3,ncol=nd))
  for (i in 1:3) {
     paramQuantiles[i,] <- PbDefinition$lb_orig + paramQuantiles[i,] * (PbDefinition$ub_orig - PbDefinition$lb_orig)  
  }
  if (file.exists(file.path(PbDefinition$savepath,"Sample.txt"))){
     sample <- read.table(file.path(PbDefinition$savepath,"Sample.txt"))
     for (i in 1:nrow(sample)) {
         sample[i,1:nd] <- PbDefinition$lb_orig + sample[i,1:nd] * (PbDefinition$ub_orig - PbDefinition$lb_orig)  
     }
     sample[,1+nd] <- -sample[,1+nd]
     colnames(sample) = c(colnames(PbDefinition$x0),"OF")
  }
  
  # save in a csv file sampling of last estimate of posterior law in order to analyze it in Lagun 
  postUncer <- list(paramQuantiles=paramQuantiles, postSample=sample)
  write.table(x = postUncer$postSample, file = file.path(PbDefinition$savepath,"PostSample.txt"),
              row.names = F, quote = FALSE, col.names = T, sep = ";", na = "")
  write.table(x = postUncer$postSample[,1:nd], file = file.path(PbDefinition$savepath,"PostSample_X.txt"),
              row.names = F, quote = FALSE, col.names = T, sep = ";", na = "")
  
  Outputs <- list(xsol=resHO[[2]])

  # save in rds file (to be read with readRDS) Quantiles of last estimate of posterior law, sampling of this law
  postUncer$xsol = resHO[[2]]
  #saveRDS(postUncer,file.path(PbDefinition$savepath,"Sample.RDS")) #could not be zip ?!?
  
  return(Outputs)
  
}

bayesiancalib.description <- function(){

	DisplayName="HubOpt Bayesian Calibration"

	Description="Bayesian calibration performs a global optimization of a least-square function derived from a calibration problem. It is based on Gaussian Process models. Posterior density is saved in a csv file."

	OptimTags=list(constraints=FALSE, categorical=FALSE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	maxeval=list(default=100,lower=5,upper=Inf,type="integer",description="Stop when the number of function evaluations exceeds maxeval."),
	nx0=list(default=5,lower=5,upper=Inf,type="integer",description="Number of initial points."),
	nx_iter=list(default=5,lower=0,upper=Inf,type="integer",description="Maximal number of points per iteration.")
	)

	AdvancedParameters=list(
	seed=list(default=42,lower=0,upper=100000,type="integer",description="Random seed (if <> 0 the seed is fixed)"),
	nEI_iter=list(default=2,lower=0,upper=Inf,type="integer",description="Number of additionnal points at each iter according to the EIG criterion."),
	nSampling=list(default=10000,lower=0,upper=100000,type="integer",description="Number of sampling points."),
    nTheta=list(default=10,lower=0,upper=50,type="integer",description="Size of the LHS used to compute the kriging parameters with the multi-start optimization."),
    dx_min=list(default=1.e-6,lower=0,upper=Inf,type="float",description="Minimal distance between simulated points."),
	print_level=list(default=0,enum=c(0,10,20,30),type="integer",description="Display level: 0 mute, 1 display error, 10 display warnings, 20 display info, 30 display debug.")
	
  )

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
  