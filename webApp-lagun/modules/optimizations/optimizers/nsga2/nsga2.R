# mco package
# nsga2 : Multi-Objective optimization evolutionary algorithm

nsga2.run <- function(PbDefinition, ParametersValues, AdvancedParametersValues) {
    library(mco)
	
	nd <- length(PbDefinition$lb)
	nobj <- PbDefinition$nobj 
	ncons <- PbDefinition$ncons 
	
	ObjFunc <- function(x) { Y <- t(PbDefinition$ObjFunc(x)) ;   #should be transposed for NSGA2 
	                         return(Y[1:nobj,, drop=FALSE]) }
	
	ConsFunc <- function(x) { Y <- t(PbDefinition$ObjFunc(x, FALSE)) ;   #should be transposed for NSGA2
	                          return(-Y[(nobj+1):(nobj+ncons), , drop=FALSE]) }  #constraints C(x) >=0 in NSGA2
	
    set.seed(1)
	res <- nsga2(ObjFunc, nd, PbDefinition$nobj, lower.bounds=PbDefinition$lb, 
	             upper.bounds=PbDefinition$ub, 
	             constraints=ConsFunc, cdim=ncons, 
	             popsize=ParametersValues$popsize, generations=ParametersValues$generations, 
	             cprob=AdvancedParametersValues$cprob, cdist=AdvancedParametersValues$cdist, 
	             mprob=AdvancedParametersValues$mprob, mdist=AdvancedParametersValues$mdist, vectorized=TRUE)
    
	Outputs <- list(xsol=res$bestX)
    return(Outputs)
}


nsga2.description <- function(){

	DisplayName="mco nsga2"

	Description="The NSGA-II algorithm minimizes a multidimensional function to approximate its Pareto front and Pareto set. It does this by successive sampling of the search space."
	
	OptimTags=list(constraints=TRUE, categorical=FALSE, monoobj=FALSE, multiobj=TRUE, derivative=FALSE)

	Warnings=list()

	Parameters=list(
	popsize=list(default=12,lower=0,upper=Inf,type="integer",description="Size of population."),
	generations=list(default=5,lower=1,upper=Inf,type="integer",description="Number of generations to breed.")
	)

	AdvancedParameters=list(
	cprob=list(default=0.7,lower=0,upper=Inf,type="float",description="Crossover probability."),
	cdist=list(default=5,lower=0,upper=Inf,type="integer",description="Crossover distribution index."),
    mprob=list(default=0.2,lower=0,upper=Inf,type="float",description="Mutation probability."),
	mdist=list(default=10,lower=0,upper=Inf,type="integer",description="Mutation distribution index.")
	)

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}

