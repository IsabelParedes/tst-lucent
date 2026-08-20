#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module settings
source("modules/menuMore/settings/settingsDOE.R", local = TRUE)
source("modules/menuMore/settings/settingsSurrogate.R", local = TRUE)
source("modules/menuMore/settings/settingsGSA.R", local = TRUE)
source("modules/menuMore/settings/settingsUQ.R", local = TRUE)
source("modules/menuMore/settings/settingsExploration.R", local = TRUE)
source("modules/menuMore/settings/settingsOptimization.R", local = TRUE)
source("modules/menuMore/settings/settingsDownload.R", local = TRUE)
source("modules/menuMore/settings/settingsRobust.R", local = TRUE)

collapsible.ui <- function(title, ui, id = NULL, value = NULL) {
  column(
    6, "",
    bsCollapse(
      id = id,
      multiple = TRUE,
      # see https://github.com/ebailey78/shinyBS/issues/50
      # in bsCollaspePanel we must specify the value when using a HTML element for the title
      # otherwise it raises a warning    
      bsCollapsePanel(h4(title), ui, value = value)
    )
  )
}

settings.ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      collapsible.ui(
        title = "DOE & Preliminary Exploration",
        settingsDOE.ui(id = ns("settingsDOE")),
        id = ns("collapseSettingsDOE"),
        value = "collapseSettingsDOE"
      ),
      collapsible.ui(
        title = "Surrogate Models",
        settingsSurrogate.ui(id = ns("settingsSurrogate")),
        id = ns("collapseSettingsSurrogate"),
        value = "collapseSettingsSurrogate"
      )
    ),
    fluidRow(
      collapsible.ui(
        title = "GSA",
        settingsGSA.ui(id = ns("settingsGSA")),
        id = ns("collapseSettingsGSA"),
        value = "collapseSettingsGSA"
      ),
      collapsible.ui(
        title = "Exploration",
        settingsExploration.ui(id = ns("settingsExploration")),
        id = ns("collapseSettingsExploration"),
        value = "collapseSettingsExploration"
      )
    ),
    fluidRow(
      collapsible.ui(
        title = "Optimization",
        settingsOptimization.ui(id = ns("settingsOptimization")),
        id = ns("collapseSettingsOptimization"),
        value = "collapseSettingsOptimization"
      ),
      collapsible.ui(
        title = "Download Interface Files",
        settingsDownload.ui(id = ns("settingsDownload")), 
        id = ns("panelDownload"),
        value = "panelDownload"
      )
    ),
    fluidRow(
      collapsible.ui(
        title = "UQ",
        settingsUQ.ui(id = ns("settingsUQ")),
        id = ns("collapseSettingsUQ"),
        value = "collapseSettingsUQ"
      ),
      collapsible.ui(
        title = "Robust Optimization",
        settingsRobust.ui(id = ns("settingsRobust")),
        id = ns("collapseSettingsRobust"),
        value = "collapseSettingsRobust"
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("activeCollapseSettings"),
        label = "Active Panel:",
        choices = c("",
                    "collapseSettingsDOE",
                    "collapseSettingsSurrogate",
                    "collapseSettingsGSA",
                    "collapseSettingsExploration",
                    "collapseSettingsOptimization",
                    "panelDownload",
                    "collapseSettingsUQ",
                    "collapseSettingsRobust"),
        selected = ""
      )
    ),
    conditionalPanel(
      condition = "false",
      selectInput(
        ns("desactiveCollapseSettings"),
        label = "Desactive Panel:",
        choices = c("",
                    "collapseSettingsDOE",
                    "collapseSettingsSurrogate",
                    "collapseSettingsGSA",
                    "collapseSettingsExploration",
                    "collapseSettingsOptimization",
                    "panelDownload",
                    "collapseSettingsUQ",
                    "collapseSettingsRobust"),
        selected = ""
      )
    )
  )
}

settings.server <- function(input, output, session, listmodels) {
  
  writefile <- reactive({
    if (is.null(input$panelDownload)){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  observeEvent(input$activeCollapseSettings,
               {
                 updateCollapse(session,
                                input$activeCollapseSettings,
                                open = input$activeCollapseSettings)
               })
  observeEvent(input$desactiveCollapseSettings,
               {
                 updateCollapse(session,
                                input$desactiveCollapseSettings,
                                close = input$desactiveCollapseSettings)
               })
  
  callModule(settingsDownload.server, "settingsDownload", listmodels, writefile)
  
  settings <- reactiveValues()
  callModule(settingsDOE.server, "settingsDOE", settings)
  callModule(settingsSurrogate.server, "settingsSurrogate", settings)
  callModule(settingsUQ.server, "settingsUQ", settings)
  callModule(settingsGSA.server, "settingsGSA", settings)
  callModule(settingsExploration.server, "settingsExploration", settings)
  callModule(settingsOptimization.server, "settingsOptimization", settings)
  callModule(settingsRobust.server, "settingsRobust", settings)
  
  return(settings)
}
