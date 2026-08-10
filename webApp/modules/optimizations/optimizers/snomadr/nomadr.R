# nomadr (crs package) : direct search method

nomadr.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
    
	library(crs)
	
  nd <- PbDefinition$nd
  ncons <- PbDefinition$ncons
  
  
	bbin <- rep(0, nd)
	scale <- rep(1, nd)
	lb <-  PbDefinition$lb
	ub <-  PbDefinition$ub

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

	x0 <- unlist(PbDefinition$x0[IndexMin,])
	
	if (PbDefinition$tags$categorical) {
		#identify categorical variables by their indices
		indCat = which( PbDefinition$inputflag == "Cat")
		indcont = which( PbDefinition$inputflag == "C")
		if (length(indCat) > 0) bbin[indCat] = 1
		# To get around the unique value for MIN_POLL_SIZE in case of mixed categorical variables:
		# scaling of continuous values by MIN_POLL_SIZE : xoptim = x * scale avec scale = MIN_POLL_SIZE
		scale[indcont] <- AdvancedParametersValues$MIN_POLL_SIZE
		AdvancedParametersValues$MIN_POLL_SIZE <- 1
	}
	
	ParametersValues$random_seed <- NULL
	opts <- append(ParametersValues, AdvancedParametersValues)
	
	lb <- PbDefinition$lb / scale
	ub <- PbDefinition$ub / scale
	x0 <- x0 / scale
	
	#Extra parameters to be passed to ObjFunc
	params <- list(PbDefinition=PbDefinition, scale=scale)
	
	ObjFunc <- function(x,params) {
	           nsim <- length(x)/params$PbDefinition$nd ; x = matrix(x,nrow=nsim,ncol=params$PbDefinition$nd,byrow=T) ; 
	           return(params$PbDefinition$ObjFunc(x*params$scale))}
	
	
	
	# types of outputs: 0:objective, 1/2/3:constraints (Extreme barrier, Filter, Progressive Barrier)
	bbout <- c(0, rep(3,ncons))

	
	ret <- snomadr(eval.f=ObjFunc, n=nd, x0=x0, bbin=bbin, bbout=bbout, lb=lb, ub=ub, random.seed = ParametersValues$random_seed, opts=opts, params=params)

  # ret$status <- solution$status
  # ret$message <- solution$message
  # ret$bbe <- solution$bbe
  # ret$iterations <- solution$iterations
  # ret$objective <- solution$objective
  # ret$solution <- solution$solution
	
	Outputs <- list(xsol=ret$solution * scale)
  return(Outputs)
}


nomadr.description <- function(){

	DisplayName="snomadr from crs package"

	Description="R interface to NOMAD (Nonsmooth Optimization by Mesh Adaptive Direct Search) that performs global derivative-free constrained optimization with a Mesh Adaptive Direct Search algorithm (MADS)."
	
	OptimTags=list(constraints=TRUE, categorical=TRUE, monoobj=TRUE, multiobj=FALSE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	random_seed = list(default=0,lower=0, upper=100, type="integer", description="when it is not missing and not equal to 0, the initial points will be generated using this seed when nmulti > 0"),
	MAX_BB_EVAL=list(default=100, lower=5, upper=10000, type="integer", description="Stop when the number of function evaluations exceeds maxeval")
	)

	AdvancedParameters=list(
	INITIAL_MESH_SIZE=list(default=1, lower=1.e-15, upper=1, type="float", description="Initial mesh size"),
	MIN_MESH_SIZE=list(default=1.0e-10, lower=1.e-15, upper=1, type="float", description="Minimal mesh size"),
	MIN_POLL_SIZE= list(default=1.e-3, lower=1.e-15, upper=1, type="float", description="Minimal poll size for continuous variables")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}