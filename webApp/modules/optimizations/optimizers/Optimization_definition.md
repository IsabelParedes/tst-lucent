# Optimization problem definition and associated optimizers

Depending on the optimization problem definition, the list of adapted optimizers is updated thanks to defined tags: `pbDefinition$tags` and `OptimTags` that should be defined for each optimizer.

## Problem definition

### Problem definition inputs filled automatically from user choices by current GUI 
3 **boolean tags** variables defining the type of problem that is addressed :
- `pbDefinition$tags$monoobj`
- `pbDefinition$tags$multiobj`
- `pbDefinition$tags$constraints`

Two additionnal tags for problem which respectively deals with categorical variables and problem which provides derivatives of the responses: `pbDefinition$tags$categorical`,  `pbDefinition$tags$derivatives`.
  
Parameters defining precisely the optimized responses :
- `pbDefinition$nobj` = number of objectives to be minimized or maximized
- `pbDefinition$ncons` = number of objectives to be constrained
- `pbDefinition$indmin` = indices of responses to be minimized in the total array of responses
- `pbDefinition$indmax` = indices of responses to be maximized in the total array of responses
- `pbDefinition$indcons` = indices of responses to be minimized in the total array of responses </br> **Convention: responses are constrained to be negative**
- `pbDefinition$ObjFunc` = function that computes the responses to be minimized and constrained from simulations results
  </br> **Convention: returns a matrix with, by line, the objective functions first, the constraints after, each line being associated with one simulation. Objectives to be maximized are transformed into their opposites to be minimized (-obj).**

### Problem definition inputs fixed *in hard* in the code (from now)
- `pbDefinition$x0` = initial input values
- `pbDefinition$lb, $ub` = lower and upper bounds for input parameters
- `pbDefinition$tags$categorical` = boolean that indicates if at least one of the input is categorical 
- `pbDefinition$inputflag[1:nd]` = "C" or "I" or "Cat" (resp. Continuous, Integer or Categorical) </br>
*if pbDefinition\$categorical is set to FALSE, all the variables are considered as continuous*

## Optimizer description
Different optimizers are available in *optimizers* folder, sorted by optimization library (e.g. folders nlopt, optim, HubOpt, scipy, ...).
In each libray folder, there is one file for each optimization method (e.g. cobyla.R, neldermead.R, nsga2.R).

These folders and files are parsed by GUI and theirs tags are compared with the current `pbDefinition$tags` to eliminate not adapted optimizers.

Optionnal files `listSolverFolders.R` and `listSolvers.R` (in each library folder) can be provided in order to select a reduced list of folders or files.

Each optimizer file contains 2 functions: 
- **optimizer_name.description()** that returns a list of 6 fields for the optimizer description and its parameters that can be tuned by the user in the GUI.</br>
*`optimizer_name.description <- function(){ ... return(list(dispname=DisplayName, descr=Description, tags=OptimTags, warn=Warnings, param=Parameters, advparam=AdvancedParameters))}`*
    - `dispname` contains the name of the optimizer that is used in the optimizer list of the GUI: it should be a short string that indicates for instance the library and the optimization method (e.g. *optim Nelder-Mead*)
   - `descr` is a longer string that describes the main aspects of the optimization method (displayed in the GUI as tooltips)
   - `warnings` is a string that describes some possible limitations of the optimizer (e.g. in terms of size problem) that can be displayed in GUI as tooltips
   - `tags` are composed of the same tags available in the list `pbDefinition$tags` that should be adapted for the optimizer. These 2 lists of tags are used to do the association between a given problem and the adapted optimization methods that can solve it.
   - `Parameters` and `AdvancedParameters` is the list of the standard parameters of the optimizer and the advanced ones (2 separated menus in the GUI). These 2 lists has this structure used in the GUI </br>
   *`Parameters / AdvancedParameters=list(`* </br>
	    *`param1=list(default=1,lower=-Inf, upper=Inf, type="float", description="blabla ..."),`*</br>
	    *`param2=list(default=0,enum=c(0,1,2,3,4,5,6),type="integer",description="blabla ..."))`* 
        - `type` can be 'integer', 'float', 'logical', 'string'
        - `description` is a (long) string that describes the parameter (displayed in the GUI as tooltips)

  
- **optimizer_name.run()** that runs the selected optimization method on a given problem defined in `pbDefinition` and with the values of the optimizer parameters given in `ParametersValues` `AdvancedParametersValues` filled in GUI.</br>
*`optimizer_name.run <- function(pbDefinition, ParametersValues, AdvancedParametersValues) { ... return(OptimizedParametersValues) }`*

