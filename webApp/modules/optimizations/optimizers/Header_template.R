#
# Template for optimizer : 1 folder for each solver (ex. nloptr), 1 file for each method (COBYLA, NELDER-MEAD ...)
#
# listSolvers.R: contains the list of solver files .R associated with the methods 
# if this file does not exist, all the files *.R in the folder are considered
#
# each file associated with a method should contain:
# - a function *method*.description with *method* = name of the method (same as the name of the file)
# - a function *method*.run = run the method on the defined problem with the values of the Paramaters and return the filled Outputs list
#


method.run <- function(PbDefinition, ParametersValues,AdvancedParametersValues){
# Run the optimization and returns the Outputs list
#
# ParametersValues and AdvancedParametersValues contain the values entered in GUI by the user
# e.g. ParametersValues = list(par1=10, par2=0, par3=TRUE ...)
#
	library(...)
	
	#Define options list from Parameters list, for instance:
	opts <- append(list(ParametersValues, AdvancedParametersValues))
	
	#Define inputs from PbDefinition, for instance: x0, ObjFunc, lb, ub 
	#Run optimization method
    temp <- method(unlist(PbDefinition$x0[1,]), PbDefinition$ObjFunc, lb = PbDefinition$lb, ub = PbDefinition$ub, opts=opts)
	
	#Build the Output list from method outputs
	#Outputs=list( xsol, f@sol, constr@sol, distribution, warnings, stopcriteria, neval, niter) ... to be continued
    Outputs <- list(...)
	
    return(Outputs)
}

method.description <- function(){
# Returns the characteristics and parameters of the optimization method
#
# Display Name: OPTIM
#
# OptimTags=list( "Mixed Categorical", "Constrained", "Multi-objective", "Provided Derivatives" ) 
# 
# Warnings=list( nd=list(lower=, upper=, message=""), ncat=list(lower=, upper=, message=""))
# 	warnings sur l'utilisation du solveur pour certaines plages de valeurs des entrées du problème
# 
# Filter the optimizers: check the optimizers that are compatible with the type of problems
#
# Parameters=list( param1=list(default=10, lower=NA, upper=NA, enum=c(1,5,10), type=, description=""),
#                   param2=list(default=10, lower=1, upper=10, type=int/float/string, description="" ),
#					param3=list(default=1.1)) 
# 	list of optimizer parameters the user can modify with the default values, the restriction, the description 
#
# Advanced parameters: same structure
#
#
	DisplayName=""
	OptimTags=list(constraints=FALSE, categorical=FALSE, multiobj=FALSE, derivative=FALSE, inversion=FALSE)
	Warnings=list()
	Parameters=list(
		param1=list(default=1.e-4,lower=0,upper=Inf,type="float",description="blabla"),
		param2=list(default=100,lower=0,upper=Inf,type="integer",description="blibli"),
		param3=list(default=0,enum=c(0,1,2,3),type="integer",description="bloblo"),
		param4=list(default="ch0",enum=c("ch1","ch2","ch3"),type="string",description="blabli")
	)
	AdvancedParameters=list(param1=list(default=1.e-4,lower=0,upper=Inf,type="float",description="blabla"),
		param2=list(default=100,lower=0,upper=Inf,type="integer",description="blibli"),
		param3=list(default=0,enum=c(0,1.4,2.6,3),type="float",description="bloblo"),
		param4=list(default=FALSE,type="logical",description="blablo")
	)
	Outputs=list()

	return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))
}
