#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module about
about.ui <- function(id) {
  ns <- NS(id)
  includeMarkdown("modules/menuMore/about/about.md")
}

about.server <- function(input, output, session) {
}
