#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# utilities for development
createTemplateModule <- function(moduleName) {
  file.name <- paste0("modules/",moduleName,".R")
  if (file.exists(file.name)) stop("module ", moduleName, " already exists")
  
  header <- paste0("# module ", moduleName)
  ui.code <- paste(
    paste0(moduleName,".ui <- function(id) {"),
    "  ns <- NS(id)",
    paste0("  h1(\"TODO ", moduleName,"\")"),
    "}",
    sep = "\n"
  )
  server.code <- paste(
    paste0(moduleName,".server <- function(input, output, session) {"),
    paste0("  print(\"TODO ", moduleName,"\")"),
    "}",
    sep = "\n"
  )
  
  cat(header, file = file.name, sep = "\n")
  cat(ui.code, server.code, file = file.name, sep = "\n", append = TRUE)
}
