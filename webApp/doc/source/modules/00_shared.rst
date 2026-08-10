Shared
######


dynamicSelect.R
***************


.. py:function:: dynamicSelect.ui(id)

   This function displays a select list wich uses ID **select**

   :param character id: namespace of the module


.. py:function::   dynamicSelect.server(input, output, session, label, choices, multiple = FALSE, selected.ind = 1)

   This function creates a select list that can be used to choose a single or multiple items from a list of values
   
   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param character label: display label for the menu
   :param list choices: list of values to select from
   :param logical multiple: informs whether the selection of multiple items is allowed (default=FALSE)
   :param numeric selected.ind: initially selected value's index (default=1)



dynamicSelectpicker.R
*********************


.. py:function:: dynamicSelectpicker.ui(id)

   This function displays a select list wich uses ID **select**

   :param character id: namespace of the module

.. py:function::   dynamicSelectpicker.server(input, output, session, label.title, choices, multiple = TRUE, label.window = NULL, selected = "All", idon = NULL, livesearch = FALSE, maxOptions = NULL, abox = TRUE)

   This function creates a select list (with additional options) that can be used to choose a single or multiple items from a list of values
   
   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param character label.title: display label for the menu
   :param list choices: list of values to select from
   :param logical multiple: informs whether the selection of multiple items is allowed (default=TRUE)
   :param character label.window: option title, the default title for the selectpicker (default=NULL)
   :param list selected: initially selected items (default=All)
   :param list idon: values to keep in the selected list (filtered by user) (default=NULL)
   :param logical livesearch: when set to true, adds a search box to the top of the selectpicker dropdown (default=FALSE)
   :param numeric maxOptions: the number of selected options cannot exceed the given value (default=NULL)
   :param logical abox: option actionBox, when set to true, adds two buttons to the top of the dropdown menu (Select All & Deselect All) (default=TRUE)

missingValChange.R
******************


.. py:function:: missingValChange.ui(id, label = "Define Missing Values", width=NULL)

   This function displays a button that calls a modal window in order to specify how missing values are defined 

   :param character id: namespace of the module
   :param character label: label of the button (default="Define Missing Values")
   :param numeric width: width of the button (default=NULL)

.. py:function:: missingValChange.server(input, output, session, initialNA)

   This function allows to interact with the modal window

   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param object initialNA: contains NA defaults for each 


XactiveChange.R
***************


.. py:function:: XactiveChange.ui(id, label = "Change Outputs Type", width=NULL)

   This function displays a button that calls a modal window in order to change input activation

   :param character id: namespace of the module
   :param character label: label of the button (default="Change Outputs Type")
   :param numeric width: width of the button (default=NULL)

.. py:function:: XactiveChange.server(input, output, session, Xinfos)

   This function allows to interact with the modal window

   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param object Xinfos: input variable information


XinfosChange.R
**************


.. py:function:: get.Xinfos(line, decimal, header)

   Returns an object containing the input variable information of a given line

   :param character line: line of imported file
   :param character decimal: decimal separator
   :param logical header: informs whether there is a header containing the variable name

.. py:function:: check.Xinfos.file(nX, lines, Xinfos.temp, separator, decimal, data, edit.disable, header)

   Checks the validity of the imported file

   :param numeric nX: number of inputs
   :param vector lines: lines read from file
   :param object Xinfos.temp: temporary Xinfos, input variable information
   :param character separator: delimiter used to separate values in the file
   :param character decimal: decimal separator
   :param object data: contains data points
   :param logical edit.disable: informs whether the edition should be disabled
   :param logical header: informs whether there is a header containing the variable name

.. py:function:: get.Xinfos.from.file(nX, lines.split, Xinfos.temp, header, separator, decimal, data, edit.disable)

   Returns an object containing the input variable information of every variable from a file
   (returns error message if wrong file format)

   :param numeric nX: number of inputs
   :param vector lines.split: lines read from file
   :param object Xinfos.temp: temporary Xinfos, input variable information
   :param logical header: informs whether there is a header containing the variable name
   :param character separator: delimiter used to separate values in the file
   :param character decimal: decimal separator
   :param object data: contains data points
   :param logical edit.disable: informs whether the edition should be disabled

.. py:function:: get.Xinfos.from.input(input, initialXinfos, Xinfos.temp, edit.disable = FALSE)

   Returns an object containing the input variable information from what the user specified in the UI

   :param list-like-object input: stores the widgets' values.
   :param object initialXinfos: initial input variable information
   :param object Xinfos.temp: temporary Xinfos, input variable information
   :param logical edit.disable: informs whether the edition should be disabled (default=FALSE)

.. py:function:: update.type(input, Xinfos, data, error.msg, session)

   Updates Xinfos with the types specified in the UI by the user
   (returns error message if a type change is not possible)

   :param list-like-object input: stores the widgets' values.
   :param object Xinfos: input variable information
   :param object data: contains data points
   :param object error.msg: error messages
   :param object session: environment that can be used to access information and functionality relating to the session.

.. py:function:: Xinfos.check(Xinfos.temp, data, edit.disable)

   Checks the validity of the object Xinfos

   :param object Xinfos.temp: temporary Xinfos, input variable information
   :param object data: contains data points
   :param logical edit.disable: informs whether the edition should be disabled

.. py:function:: get.initialXinfos(ind)

   Returns a default Xinfos object (a numeric variable, bounded by 0 and 1)

   :param numeric ind: index of the variable

.. py:function:: get.bounds(Xinfos)

   Returns the bounds contained in Xinfos, for numerical variables

   :param object Xinfos: input variable information

.. py:function:: get.levels(Xinfos, var.cat.list)

   Returns the levels contained in Xinfos, for categorical variables

   :param object Xinfos: input variable information
   :param list var.cat.list: name list of the catgorical variables

.. py:function:: get.nb.num(Xinfos)

   Returns the number of numerical variables in Xinfos

   :param object Xinfos: input variable information

.. py:function:: get.Xinfos.df(Xinfos, ncolumns)

   Returns Xinfos as a data.frame

   :param object Xinfos: input variable information
   :param numeric ncolumns: expected number of columns

.. py:function:: get.Xinfos.download(Xinfos, separator, decimal)

   Shapes an Xinfos data.frame for file export

   :param object Xinfos: input variable information
   :param character separator: delimiter used to separate values in the file
   :param character decimal: decimal separator

.. py:function:: Xinfos.row.ui(i, ns, input, Xinfos, initialXinfos, data, error.msg, edit.disable)

   Displays Xinfos for one variable in the UI, allows the user to edit the information
   
   :param numeric i: index of the variable
   :param character ns: namespaced ID
   :param list-like-object input: stores the widgets' values.
   :param object Xinfos: input variable information
   :param object initialXinfos: initial input variable information
   :param object data: contains data points
   :param object error.msg: error messages
   :param logical edit.disable: informs whether the edition should be disabled

.. py:function:: XinfosChange.ui(id, label = "Change Inputs", width = NULL)

   This function displays a button that calls a modal window in order
   to import a file with the input variable information or to manually change the information

   :param character id: namespace of the module
   :param character label: label of the button (default="Change Inputs")
   :param numeric width: width of the button (default=NULL)

.. py:function:: XinfosChange.ui.preview(id, simple = FALSE)

   This function displays the input variable information as a table, alongside with an export button if the *simple* parameter is set to FALSE

   :param character id: namespace of the module
   :param logical simple: when TRUE, displays a table, when FALSE, displays a button to download the data(default=FALSE)

.. py:function:: XinfosChange.server(input, output, session, initialXinfos, data = NULL, edit.disable = FALSE, nvalues = NULL, verbose = FALSE)

   This function allows to interact with the modal windows

   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param object initialXinfos: initial input variable information
   :param object data: contains data points (default=NULL)
   :param logical edit.disable: informs whether the edition should be disabled (default=FALSE)
   :param numeric nvalues: number of values for the variable (default=NULL)
   :param logical verbose: option to provide additional details during the process (default=FALSE)



XYnamesChange.R
***************


.. py:function:: XYnamesChange.ui(id, label = "Check Variables Names", width = NULL)

   This function displays a button that calls a modal window in order to change input and output variable names

   :param character id: namespace of the module
   :param character label: label of the button (default="Check Variables Names")
   :param numeric width: width of the button (default=NULL)

.. py:function:: XYnamesChange.server(input, output, session, XYnames.init, max.char = 40)

   This function allows to interact with the modal window
   and processes the names for visualisations and menus if needed.

   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param object XYnames.init: initial names for input and output variables
   :param numeric max.char: maximum length allowed for a name (default=40)


YinfosChange.R
**************

.. py:function:: get.Yinfos.from.file(datapath, ynames, Yinfos, separator)

   This funtion reads a file and updates Yinfos with its content

   :param character datapath: file to read
   :param list ynames: initial output variable names
   :param object Yinfos: initial output variable information
   :param character separator: delimiter used to separate values in the file

.. py:function:: YinfosChange.ui(id, label = "Change Outputs Type", width = NULL)

   This function displays a button that calls a modal window in order to change output variable information

   :param character id: namespace of the module
   :param character label: label of the button (default="Change Outputs Type")
   :param numeric width: width of the button (default=NULL)

.. py:function:: YinfosChange.server(input, output, session, initialYinfos, hideFunctional)

   This function allows to interact with the modal window

   :param list-like-object input: stores the widgets' values.
   :param list-like-object output: stores the instructions to build R objects in the app.
   :param object session: environment that can be used to access information and functionality relating to the session.
   :param object initialYinfos: initial output variable information
   :param logical hideFunctional: if TRUE, hide the ability to attribute functional group to outputs
