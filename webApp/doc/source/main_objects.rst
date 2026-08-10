Main objects
############


DOE
***

.. list-table:: DOE parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - Xopt
      - data.frame
      - *n_estimations* × *nX*
      - generated DOE points, *n_estimations* is the sample size defined by the user
   
   *  - Xinfos
      - object
      - *7 fields*
      - input variable information
        (check :ref:`table Xinfos` for details)
   
   *  - Yinfos
      - object
      - *7 fields*
      - output variable information
        (check :ref:`table Yinfos` for details)
   
   *  - XY
      - data.frame
      - *nobs* × *(nX + nY)*
      - input and output data
   
   *  - X
      - data.frame
      - *nobs* × *nX*
      - input data
   
   *  - Y
      - data.frame
      - *nobs* × *nY*
      - output data
   
   *  - nobs
      - numeric
      - *NA*
      - sample size
   
   *  - nX
      - numeric
      - *NA*
      - number of input variables
   
   *  - nY
      - numeric
      - *NA*
      - number of output variables
   
   *  - xnames
      - list
      - *nX* elements
      - input variable names
   
   *  - ynames
      - list
      - *nY* elements
      - output variable names
   
   *  - xnamesvisu
      - list
      - *nX* elements
      - input variable names processed for plots
   
   *  - ynamesvisu
      - list
      - *nY* elements
      - output variable names processed for plots
   
   *  - xnamesmenu
      - list
      - *nX* elements
      - input variable names processed for menus   
   
   *  - ynamesmenu
      - list
      - *nY* elements
      - output variable names processed for menus
   
   *  - adapt.visu
      - logical
      - NA
      - informs whether the plots should be adapted to the window
   
   *  - idref
      - list
      - *0 to nobs* elements
      - indices of rows to keep, specified by user selection
   
   *  - idon
      - list
      - *0 to (nX + nY)* elements
      - indices of columns to keep, specified by user selection 
	  
   *  - compositeInfos
      - list of objects
      - *number of composite functions*
      - composite functions information (check :ref:`table compositeInfos` for details)

   *  - Fnames
      - list
      - *nY* elements
      - functional output variable names

   *  - Fnamesvisu
      - list
      - *nY* elements
      - functional output variable names processed for plots

   *  - idF
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each experimental functional output in DOE$Y

   *  - nZ
      - list
      - *number of functional outputs*
      - vector containing the length of each simulation functional output

   *  - discF
      - list
      - *number of functional outputs*
      - for each functional output, data.frame containing discretization values

   *  - Z
      - data.frame
      - *1* × *nY*
      - experimental data values

   *  - idZ
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each simulation functional output in DOE$Y

   *  - nZ
      - list
      - *number of functional outputs*
      - vector containing the length of each experimental functional output

   *  - discZ
      - list
      - *number of functional outputs*
      - for each functional output, data.frame containing discretization values

   *  - idZY
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each functional output, used to map simulation and experimental data when the discretization is different

   *  - OF
      - data.frame
      - *nobs* × *number of functional outputs*
      - objective function values for each functional output

   *  - OFtot
      - data.frame
      - *nobs* × *1*
      - total sum of objective function values

.. _table Xinfos:

.. list-table:: Xinfos parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - name
      - character
      - *NA* 
      - variable name
   
   *  - namevisu
      - character
      - *NA*
      - variable name processed for plots
   
   *  - namemenu
      - character
      - *NA*
      - variable name processed for menus
   
   *  - type
      - character
      - *NA*
      - variable type (numeric or categorical)
   
   *  - bounds
      - list
      - *2* elements
      - lower bound and upper bound if type is numeric
   
   *  - nlevels
      - numeric
      - *NA*
      - number of levels if type is categorical
   
   *  - levels
      - list
      - *nlevels* elements
      - list of levels if type is categorical


.. _table Yinfos:

.. list-table:: Yinfos parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - all.ids
      - list
      - *nY* elements
      - output group (Interest, Control, Status, Constant or Functional)
   
   *  - int.ids
      - list
      - *0 to nY* elements
      - indices of outputs of interest
   
   *  - control.ids
      - list
      - *0 to nY* elements
      - indices of control outputs
   
   *  - const.ids
      - list
      - *0 to nY* elements
      - indices of constant outputs
   
   *  - status.ids
      - list
      - *0 to nY* elements
      - indices of status outputs
   
   *  - visu.ids
      - list
      - *0 to nY* elements
      - indices of outputs for visualizations
      
   *  - func.ids
      - list
      - *0 to nY* elements
      - indices of functional outputs
   
   *  - nY
      - numeric
      - *NA*
      - number of output variables
   
   *  - type
      - list
      - *nY* elements
      - output type (numeric or categorical)

.. _table compositeInfos:

.. list-table:: compositeInfos object description
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - name
      - character
      - *NA*
      - name of the composite output (entered by the user)
   
   *  - id
      - numeric
      - *NA*
      - id of the composite output
   
   *  - type
      - character
      - *NA*
      - variable type (numeric or categorical)
   
   *  - levels
      - list
      - *1 to n* elements
      - list of levels if type is categorical
	  
   *  - usedY
      - list
      - *1 to n* elements
      - name(s) of the used input(s)
	  
   *  - formula
      - character
      - *NA*
      - formula entered by the user
   
   *  - modelMode
      - character
      - *NA*
      - informs whether a model should be trained or combine the models from the used parameters ("Train" or "Combine")
   
   *  - dfNewCol
      - dataframe
      - *nobs* × 1
      - output result using the current DOE


surrogate
*********

.. list-table:: surrogate parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - listmodels
      - object
      - *11* fields
      - model information
        (check :ref:`table listmodels` for details)
   
   *  - simulations
      - object
      - *5* fields
      - simulation information
        (check :ref:`table simulations` for details)

.. _table listmodels:

.. list-table:: listmodels parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description

   *  - names_surrogatemodel
      - list
      - *12* elements
      - names of the available surrogate models

   *  - models
      - list
      - *12* elements
      - computed models

   *  - tableQ2loo
      - data.frame
      - *12* × *(nY + 3)*
      - leave-one-out validation results

   *  - tableQ2test
      - data.frame
      - *12* × *(nY + 3)*
      - test validation results

   *  - bestQ2loo
      - list
      - *nY* × *2*
      - list of best model for each output with respect to the Q2loo criterion

   *  - bestQ2test
      - list 
      - *nY* × *2*
      - list of selected model for each output with respect to the Q2test criterion

   *  - selected
      - list
      - *nY* × *2*
      - list of selected model for each output, along with the Q2 criterion

   *  - trainedModels
      - list
      - *0 to 12* elements
      - IDs of trained models

   *  - finalpredfun
      - function
      - *NA*
      - computes prediction with the selected surrogate model

   *  - categorical
      - list
      - *0 to nX* elements
      - indices of categorical input variables

   *  - levels.models
      - list
      - *0 to nX* elements
      - levels of categorical input variables

   *  - withsdmodels
      - list
      - *12* elements
      - list of boolean values, TRUE if a standard deviation model is available

   *  - currentlyUsed
      - list
      - *0 to nobs* elements
      - indices of rows used to build models

.. _table simulations:

.. list-table:: simulations parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description

   *  - Xadd
      - data.frame
      - *n_points* × *nX*
      - extra points to refine the model, *n_points* is specified by the user

   *  - mode.manual 
      - logical
      - *NA*
      - send the additional simulations to the "importDOE" panel where they can be manually launched

   *  - mode.automatic
      - logical
      - *NA*
      - automatically launch the additional simulations with the current simulator settings

   *  - tagDOE
      - character
      - *NA*
      - tag DOE information

   *  - nRefine
      - numeric
      - *NA*
      - refine threshold


UQParams
********

.. list-table:: UQParam parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - typeDistr
      - character
      - *NA*
      - type of distribution
   
   *  - P1Distr
      - numeric
      - *NA*
      - distribution parameter, depends on typeDistr
   
   *  - P2Distr
      - numeric
      - *NA*
      - distribution parameter, depends on typeDistr
   
   *  - P3Distr
      - numeric
      - *NA*
      - distribution parameter, depends on typeDistr
   
   *  - P4Distr
      - numeric
      - *NA*
      - distribution parameter, depends on typeDistr
   
   *  - levels
      - list
      - *number of levels*
      - levels for categorical variables
   
   *  - weights
      - list
      - *number of levels*
      - weights of the levels
   

calibration
***********

The data structures in this section are specific to the calibration module, they are not used throughout the application. 
The needed information for the calibration is stored in the object DOE. 

.. list-table:: calibDOE parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description
   
   *  - Z
      - data.frame
      - *1* × *nY*
      - experimental data values
   
   *  - sigZ
      - data.frame
      - *1* × *nY*
      - experimental data standard deviations

   *  - nZ
      - list
      - *number of functional outputs*
      - vector containing the length of each experimental functional output

   *  - idZ
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each functional output

   *  - idZY
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each functional output, used to map simulation and experimental data when the discretization is different

   *  - discZ
      - list
      - *number of functional outputs*
      - for each functional output, data.frame containing discretization values

   *  - OF
      - data.frame
      - *nobs* × *number of functional outputs*
      - objective function values for each functional output

   *  - OFtot
      - data.frame
      - *nobs* × *1*
      - total sum of objective function values

.. _table objFunc:

.. list-table:: objFunc parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description

   *  - norm
      - character
      - *NA* 
      - normalization type ("L1" or "L2")

   *  - weightsTemp
      - vector
      - *number of functional outputs*
      - weight associated to each functional output (in "Save OF Weights" modal)

   *  - weights
      - vector
      - *number of functional outputs*
      - weight associated to each functional output

   *  - OF
      - data.frame
      - *nobs* × *number of functional outputs*
      - objective function values for each functional output

   *  - OFtot
      - data.frame
      - *nobs* × *1*
      - total sum of objective function values

.. _table importDiscF:

.. list-table:: importDiscF parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description

   *  - discFtemp
      - data.frame
      - *discretization count* × *1*
      - discretization values for some functional outputs in "Import Output Discretization" modal
        (depends of selected output: "All" or one of the functional outputs)

   *  - discF
      - list
      - *number of functional outputs*
      - for each functional output, data.frame containing discretization values

   *  - saveMessage
      - character
      - *NA*
      - displayed message which indicates discretization saving status

.. _table xpData:

.. list-table:: xpData parameters
   :header-rows: 1
   :widths: 10 10 10 70

   *  - Parameter
      - Type
      - Dimension
      - Description

   *  - Z
      - data.frame
      - *1* × *nY*
      - experimental data values
   
   *  - sigZ
      - data.frame
      - *1* × *nY*
      - experimental data standard deviations

   *  - nZ
      - list
      - *number of functional outputs*
      - vector containing the length of each experimental functional output

   *  - idZ
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each functional output

   *  - idZY
      - list
      - *number of functional outputs*
      - vector containing indices of columns for each functional output, used to map simulation and experimental data when the discretization is different

   *  - discZ
      - list
      - *number of functional outputs*
      - data.frame containing discretization values for each functional output













 