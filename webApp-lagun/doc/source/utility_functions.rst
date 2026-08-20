Utility Functions
#################

utilityFunctions.R
******************

.. py:function:: disableActionButton(id, session)

   Sets the property named *disabled* to `FALSE` in the javascript form

   :param character id: namespace of the module
   :param object session: environment that can be used to access information and functionality related to the session

.. py:function:: enableActionButton(id, session)

   Sets the property named *disabled* to `TRUE` in the javascript form

   :param character id: namespace of the module
   :param object session: environment that can be used to access information and functionality related to the session

.. py:function:: check.header(DOE, datapath, separator, decimal, nY = 0)

   Extracts column names from file and checks if they are unique and consistent with the DOE definition

   :param object DOE: stores the point values and the DOE information (e.g. bounds...)
   :param character datapath: path to data file
   :param character separator: delimiter used to separate values in the file
   :param character decimal: decimal separator
   :param numeric nY: number of outputs (default=0)

.. py:function:: check.new.data(nX, Xinfos, newData, nY = 0)

   Checks if the data is consistent with the DOE definiton

   :param add_type nX: add_description
   :param object Xinfos: input variable information
   :param data.frame newData: data
   :param numeric nY: number of outputs (default=0)

.. py:function:: get.new.data.from.file(DOE, datapath, separator, decimal, nY = 0)

   Reads the data from the spectified file

   :param object DOE: stores the point values and the DOE information (e.g. bounds...)
   :param character datapath: path to data file
   :param character separator: delimiter used to separate values in the file
   :param character decimal: decimal separator
   :param numeric nY: number of outputs (default=0)

.. py:function:: dchi2LHS(x, n, min, max)

   Computes Chi2 distance to LHS

   :param data.frame x: data
   :param numeric n: size of the sample
   :param numeric min: min value
   :param numeric max: max value

.. py:function:: discrepancyL2centered(X)

   Computes discrepency criterion

   :param data.frame X: data

.. py:function:: maxprocrit(x)

   Computes MaxPro criterion

   :param data.frame X: data

.. py:function:: generateUQ(UQparams, n, DOE)

   Generates uncertainty quntification sample

   :param list UQparams: Uncertainty Quantification parameters
   :param numeric n: sample size
   :param object DOE: stores the point values and the DOE information (e.g. bounds...)

.. py:function:: generateXtest(Xbounds, n, DOE)

   Generates a Sobol sample

   :param list Xbounds: lower and upper bound
   :param numeric n: sample size
   :param object DOE: stores the point values and the DOE information (e.g. bounds...)

.. py:function:: rbf_hsic(x, param)

   Computes the Hilbert-Schmidt Independence Criterion

   :param data.frame x: data
   :param numeric param: normalization parameter (e.g. median)

.. py:function:: MMD(X, Y, kernel, param, ...)

   Computes Maximum Mean Discrepency

   :param data.frame X: input variables
   :param data.frame Y: output variables
   :param character kernel: kernel method to use
   :param numeric param: normalization parameter (e.g. median)
   :param - ....: ellipsis

.. py:function:: paretoFilterFast2obj(d)

   Computes fast pareto filtering with two objectives

   :param data.frame d: data
