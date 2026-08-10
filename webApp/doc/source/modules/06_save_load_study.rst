saveLoadStudy
#############

loadStudy.R
***********

``source("modules/saveLoadStudy/saveStudy.R", local = TRUE)``

.. py:data:: SAVE_MODULE_MIN_VER

   Minimum version of the saved file allowed.


UI and Server functions
=======================


.. py:function:: loadStudyUI(id)

   This function adds the menu "Load Study" to the user interface

   :param character id: namespace of the module

.. py:function:: loadStudyServer(id)
   
   Allows to check the selected file and load the study
   
   :param character id: namespace of the module


saveStudy.R
***********

.. py:data:: SAVE_MODULE_VER

   Current version of the save file

UI and Server functions
=======================

.. py:function:: saveStudyUI(id)
   
   This function adds the menu "Save Study" to the user interface

   :param character id: namespace of the module

.. py:function:: saveStudyServer(id, DOE, listmodels, sensitivityAnalysis, UQparams, UQres, UQproba, unconstrOptim, constrOptim)

   :param character id: namespace of the module
   :param object DOE: stores the point values and the DOE information (e.g. bounds...)
   :param list listmodels: list of trained models
   :param list sensitivityAnalysis: Sensitivity Analysis results (First & Total Sobol Indices, Higher-Order Sobol Indices, Shapley Indices, Optimization Indices)
   :param list UQparams: Uncertainty Quantification parameters
   :param list UQres: Uncertainty Quantification propagation results
   :param list UQproba: Uncertainty Quantification probability results
   :param list unconstrOptim: Unconstrained Optimization results and Xinfos used for the optimization
   :param list constrOptim: Constraint formulation, Constrained Optimization results and Xinfos used for the optimization
   
   .. rubric:: Main reactives:

   * **output$savedStudies**: list of existing studies and action buttons to download and delete each study
   * **output$download**: downloadable file containing the study


Main functions
==============

.. py:function:: buildSaveList(eltsToSave, DOE, listmodels, sensitivityAnalysis, UQparams, UQres, UQproba, unconstrOptim, constrOptim)
   
   Builds a list containig the elements the user wishes to save.
   
   :param list eltsToSave: elements to save selected by the user
   :param object DOE: stores the point values and the DOE information (e.g. bounds...)
   :param list listmodels: list of trained models
   :param list sensitivityAnalysis: Sensitivity Analysis results (First & Total Sobol Indices, Higher-Order Sobol Indices, Shapley Indices, Optimization Indices)
   :param list UQparams: Uncertainty Quantification parameters
   :param list UQres: Uncertainty Quantification propagation results
   :param list UQproba: Uncertainty Quantification probability results
   :param list unconstrOptim: Unconstrained Optimization results and Xinfos used for the optimization
   :param list constrOptim: Constraint formulation, Constrained Optimization results and Xinfos used for the optimization
   
   :return: a list containing the elements to save
   :rtype: list


.. py:function:: saveData(fileName, study)
   
   Saves the study with the specified filename, overwrites existing files with the same name
   
   :param character fileName: filename
   :param list study: list containing the elements to save





Secondary functions
===================

   .. py:function:: bytesToHumanSize(sizes)
   
   Converts sizes in bytes to units easily readable by a user
   
   :param list sizes: list of sizes in bytes

