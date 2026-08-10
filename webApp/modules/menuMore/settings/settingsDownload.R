#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settingsDownload
settingsDownload.ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    uiOutput(ns("dynui_ddl"))
  )
}

settingsDownload.server <- function(input, output, session, listmodels, writefile) {
  
  ns <- session$ns
  
  exported.surrogate <- reactiveValues(models=NULL, filepath=NULL)
  
  observe({
    req(listmodels$finalpredfun,writefile())
    if (all(!is.na(listmodels$selected$id))){
      showModal(modalDialog(HTML(paste(
        "Currently creating your file for download.", 
        "If you close this window it is not advised to navigate in other panels until the file is ready.",
        "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
        size = 'l')
      )
      ny <- length(listmodels$selected$id)
      models <- list()
      for (i in 1:ny){
        models[[i]] <- listmodels$models[[listmodels$selected$id[i]]][[i]]
      }
    }
    tmp_file <- paste0(tempfile(), ".RData")
    save(models, file = tmp_file)
    exported.surrogate$filepath <- tmp_file
    removeModal()
  })
  
  output$dynui_ddl <- renderUI({
    req(exported.surrogate$filepath)
    downloadButton(ns("downloadSurrogateFileR"), "Export Surrogate Model R File")
  })
  
  output$downloadSurrogateFileR <- downloadHandler(
    filename = 'LagunSurrogate.RData',
    content = function(file) {
      filepath <- exported.surrogate$filepath
      file.copy(filepath, file)
    }
  )
}