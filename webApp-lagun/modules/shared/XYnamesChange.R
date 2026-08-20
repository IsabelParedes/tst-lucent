#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module XYnamesChange

XYnamesChange.ui <- function(id, label = "Check Variables Names", width = NULL) {
  ns <- NS(id)
  
  modalContent <- tagList(
    fluidRow(
      column(3,""),
      column(6,
             uiOutput(ns("change.names.ui"))),
      column(3,"")
    )
  )
  
  tagList(
    fluidRow(
      column(6, actionButton(ns("change"), label = label, class = "btn-primary", width = width), offset = 6)
    ),
    bsModal(ns("modal"), "Check Variables Names", NULL, size = "large", modalContent)
  )
}

XYnamesChange.server <- function(input, output, session, XYnames.init, max.char = 40) {
  
  ns <- session$ns
  
  nXY <- reactive(length(XYnames.init$xynames))
  
  # newXYnames is updated only when the user saves
  newXYnames <- reactiveValues(namesvisu = NULL, namesmenu = NULL, adapt.visu = FALSE)

  # we reinitialize newXYnames whenever XYnames.init changes
  observeEvent(XYnames.init$xynames, {
    req(XYnames.init$xynames)
    names.init <- XYnames.init$xynames
    # First delete quotes if there are any
    names.init <- stri_replace_all_fixed(names.init,'\"','')
    # Step 0: namesmenu contains initial names
    newXYnames$namesmenu <- names.init
    # We modify the initial names
    # Step 1: trim the names if they are longer than max.char
    namestemp <- unlist(stri_sub_all(names.init,1,max.char))
    # Step 2: split the strings thar are longer than max.char/2 and add a HTML line break, then feed namesvisu
    split.temp <- smart.split(namestemp,width=max.char/2)
    idsplit <- which(stri_length(namestemp) > max.char/2)
    namestemp[idsplit] <- unlist(lapply(split.temp[idsplit],paste0,sep="",collapse="<br>"))
    newXYnames$namesvisu <- namestemp
    if (length(idsplit)){
      # If we had to split strings, then subsequent visu will be modified
      newXYnames$adapt.visu <- TRUE
    }else{
      newXYnames$adapt.visu <- FALSE
    }
  })
  
  # We populate the switches with newXYnames
  output$change.names.ui <- renderUI({
    req(XYnames.init$xynames,newXYnames,nXY())
    tl <- tagList(
      fluidRow(
        column(6,h4("Initial Names")),
        column(6,h4("New Names"))
      )
    )
    tl <- tagAppendChildren(tl,
                            lapply(1:nXY(),function(i) {
                              fluidRow(
                                column(6,h5(HTML(XYnames.init$xynames[i])),align="left"),
                                column(6,h5(HTML(newXYnames$namesvisu[i])),align="right")
                              )
                            }
                            )
    )
  })
  
  observeEvent(input$change, {
    toggleModal(session, "modal", toggle = "open")
  })
  
  
  return(newXYnames)
}