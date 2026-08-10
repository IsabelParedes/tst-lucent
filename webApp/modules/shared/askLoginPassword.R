#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module askLoginPassword

askLoginPassword.ui <- function(id) {
  ns <- NS(id)
  
  modalContent <- tagList(
    fluidRow(
      column(12,
             uiOutput(ns("askLoginPassword.ui")))
    )
  )
  
  tagList(
    bsModal(ns("modal"), "Login/Password", NULL, size = "large", modalContent)
  )
}

askLoginPassword.server <- function(input, output, session, askLoginPassword, simulatorHostAndPort, doeProblemDef) {
  
  ns <- session$ns

  credential <- reactiveValues(ok = NULL, action = NULL)

  observeEvent(askLoginPassword(), {
    req(askLoginPassword() != 0)
    credential$ok <- NULL
    # Send a message to know if a login/password must be asked to the user (retrieved through reactive 'input$needed')
    simulationsLauncher$loginPasswordNeeded(session, list(logPwdNeededArgs = list(host = simulatorHostAndPort()$host, port = simulatorHostAndPort()$port), neededInputId = ns("needed")))
  })
  
  # Retrieve answer to the message 'loginPasswordNeeded'
  observeEvent(input$needed, {
    if (input$needed$success) {
      if (input$needed$answer) {
        showModal(dataModal())
      }
      else {
        credential$ok <- FALSE
      }
    }
    else {
      logger$print(input$needed$answer)
      credential$ok <- FALSE
    }
  })
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      div(tags$b("Be careful:", style = "font-size: 15px; color: red;")),
      p(HTML('use this feature only on a secure network, authentication data is transmitted unencrypted.<br><br>'), style = "font-size: 15px"),
      textInput(ns('login'), label = "Login"),
      passwordInput(ns('password'), label = "Password"),

      if (failed) div(tags$b("Invalid login/password", style = "color: red;")),

      title = paste("Login/Password for", doeProblemDef$simulatorName),
      footer = tagList(
        actionButton(ns("ok"), "OK"),
        actionButton(ns("cancel"), "Cancel")
      )
    )
  }

  observeEvent(input$cancel, {
    removeModal()
    credential$ok <- FALSE
  })

  observeEvent(input$ok, {
    logPwdArgs = list(
        host = simulatorHostAndPort()$host,
        port = simulatorHostAndPort()$port,
        loginPassword = list(
            login = input$login, 
            password = input$password
        )
    );
    # Send a message to try a login/password (retrieve answer through reactive 'input$validLoginPwd')
    simulationsLauncher$addLoginPassword(session, list(logPwdArgs = logPwdArgs, validLoginPwdInputId = ns("validLoginPwd")))
  })
  
  # Retrieve answer to the message 'addLoginPassword'
  observeEvent(input$validLoginPwd, {
    if (input$validLoginPwd$success) {
      credential$ok <- TRUE
      removeModal()
    }
    else {
      showNotification(input$validLoginPwd$answer)
      logger$print(input$validLoginPwd$answer)
      showModal(dataModal(failed = TRUE))
    }
  })
  
  return(credential)
}