#set default values of Parameters and AdvancedParameters to ParametersValues and AdvancedParametersValues

setDefaultParam <- function(Parameters, AdvancedParameters){
  
  ParametersValues = list()
  for (i in seq(1,length(Parameters))) {
  ParametersValues[names(Parameters)[i]]= Parameters[[i]]$default
  }
  
  AdvancedParametersValues = list()
  for (i in seq(1,length(AdvancedParameters))) {
  AdvancedParametersValues[names(AdvancedParameters)[i]]= AdvancedParameters[[i]]$default
  }
  
  return(list(param=ParametersValues, advparam=AdvancedParametersValues))
}