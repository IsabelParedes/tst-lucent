#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module dynamicSelectpicker
dynamicSelectpicker.ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("select"))
}

dynamicSelectpicker.server <- function(input, output, session, label.title, choices, 
                                       multiple = TRUE, label.window = NULL, selected = "All", idon = NULL, livesearch = FALSE, maxOptions = NULL, abox = TRUE) {
  ns <- session$ns
  
  output$select <- renderUI({
    req(choices())
    
    if (is.null(label.window)){
      textformat <- "count > 3"
    }else{
      textformat = "static"
    }
    
    if(typeof(selected)!="closure"){
      selected <- reactiveVal(selected)
    }
    
    if (length(selected())==1){
      if (selected()=="All"){
        selected(choices())
      }
    }
    if (!is.null(idon)){
      selected(selected()[idon()])
    }

    pickerInput(inputId = ns("choice"), 
                label = label.title, 
                choices = choices(),
                selected = selected(),
                options = list(`actions-box` = abox,`selected-text-format` = textformat,style = "btn-primary",
                               title = label.window,`live-search` = livesearch, "max-options" = maxOptions), 
                multiple = multiple)
  })
  
  choiceVal <- reactiveVal(NULL)
  
  choice <- reactive({
    req(choices())
    input$choice
  })
  
  observeEvent(choice(), ignoreNULL = FALSE, {
      choiceVal(choice())
  })

  return(choiceVal)
}
