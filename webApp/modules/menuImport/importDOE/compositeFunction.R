#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

findAllFunctions <- function(formula, funcName, nbToFind = NULL, res = NULL){

  if (is.null(nbToFind)) nbToFind <- stringr::str_count(formula, funcName)

  if (nbToFind > 0){
    pattern <- "\\((?:[^()]+|(?R))*+\\)"

    matches_raw <- gregexpr(pattern, formula, perl = TRUE)[[1]]
    matches <- regmatches(rep(formula, length(matches_raw)), matches_raw)

    for(i in seq(matches)){
      prevChars <- stringr::str_sub(formula, 
                                   matches_raw[i]-nchar(funcName), 
                                   matches_raw[i]-1)
      if(prevChars==funcName){
        fullFunc <- paste0(prevChars, matches[i])
        content <- stringr::str_sub(matches[i], 2, -2)
        
        if(!(fullFunc %in% unlist(res))){
          newRes <- list(list(fullFunc = fullFunc, content = content))
          if(is.null(res)){
            res <- newRes
          }else{
            res <- append(res, newRes)
          }
        }
        nbToFind = nbToFind - 1
      }
    }
    
    subFormulas <- stringr::str_sub(matches, 2, -2)
    
    for(subFormula in subFormulas){
      res <- findAllFunctions(subFormula, funcName, nbToFind, res)
    }
  }
  
  decOrder <- order(sapply(res, function(x) nchar(x$fullFunc)), decreasing = TRUE)
  return(res[decOrder])
}


latexify <- function(formula, name){
  
  latex <- formula
  
  # Superscript ----
  
  # without parentheses
  
  sup <- stringr::str_extract_all(latex, "\\^[^=+*\\/\\(\\)><-]+")
  
  for(elem in sup[[1]]){
    latex <- gsub(elem, paste0("^{", substring(elem, 2), "}"), latex, fixed = TRUE)
  }
  
  # with parentheses
  
  allSup <- findAllFunctions(latex, "^")
  
  for(elem in allSup){
    latex <- gsub(elem$fullFunc, paste0("^{", elem$content, "}"), latex, fixed = TRUE)
  }
  
  
  # Operators ----
  
  latex <- gsub("!=", " \\neq ", latex, fixed = TRUE)
  latex <- gsub("==", " = ", latex, fixed = TRUE)
  latex <- gsub("*", " \\times ", latex, fixed = TRUE)
  
  # log / ln / sin / cos / tan / exp ----
  
  latex <- stringr::str_replace_all(latex, "(?<=[^a-z]|^)(log\\()", "\\\\ln(")
  latex <- gsub("log10(", "\\log(", latex, fixed = TRUE)
  
  fns <- c("sin(", "cos(", "tan(", "exp(")
  
  for(f in fns){
    latex <- gsub(f, paste0("\\", f), latex, fixed = TRUE)
  }
  
  # Absolute value  ----
  
  allAbs <- findAllFunctions(latex, "abs")
  
  for(abs in allAbs){
    latex <- gsub(abs$fullFunc, paste0("\\lvert ", abs$content, " \\rvert"), latex, fixed = TRUE)
  }
  
  # Squared root ----
  
  allSqrt <- findAllFunctions(latex, "sqrt")
  
  for(sqrt in allSqrt){
    latex <- gsub(sqrt$fullFunc, paste0("\\sqrt{", sqrt$content, "}"), latex, fixed = TRUE)
  }
  
  # Indicator function ----
  
  allIndic <- findAllFunctions(latex, "indicator")
  
  for(indic in allIndic){
    indicTex <- paste0("\\mathbf{1}_A: \\left\\{\\begin{matrix} 1\\; if\\; ", 
                        indic$content,
                      " \\\\ 0\\; otherwise \\end{matrix}\\right.")
    
    latex <- gsub(indic$fullFunc, indicTex, latex, fixed = TRUE)
  }
  
  # levels in italic ----
  
  lev <- stringr::str_extract_all(latex, "\"(.*?)\"")
  
  for(l in lev[[1]]){
    levelContent <- stringr::str_extract(l, "(?<=\")(.*?)(?=\")")
    
    levelTex <- paste0("\\textit{", levelContent, "}")
    
    latex <- gsub(l, levelTex, latex, fixed = TRUE)
  }
  
  # ----
  
  if(name != ""){
    name_esc <- stringr::str_replace_all(name, "_", "\\\\_") 
    latex <- paste0("$$", name_esc, " = ", latex, "$$")
  }else{
    latex <- paste0("$$", latex, "$$")
  }
  return(latex)
}


getUsedParams <- function(formula, ynames){
  usedParams <- c()
  ynames <- ynames[order(sapply(ynames, nchar), decreasing=TRUE)]
  
  for (yname in ynames){
    if(grepl(yname, formula, fixed = TRUE)){
      usedParams <- append(usedParams, yname)
      formula <- stringr::str_remove_all(formula, yname)
    }
  }
  
  return(usedParams)
}


checkName <- function(name, xnames, ynames){

  if(name == ""){
    "Name must not be empty"
  }else if(name %in% c(xnames, ynames)){
    "Name must be unique"
  }else if (!stringr::str_detect(name, "^[A-Za-z0-9\\._]+$")){
    "Name should only contain letters, numbers, \"_\", and \".\"" 
  }else{
    NULL
  }
}


checkFormula <- function(formula, usedParams, modelMode, type, Y){
  
  resParse <- tryCatch(parse(text = formula), error = function(e) e)

  if (is.expression(resParse)){
    resEval <- tryCatch(with(Y, eval(resParse)),
                        error = function(e) e,
                        warning = function(w) w)
  }
  
  containsLogOrSqrt <- FALSE
  if(modelMode == "Combine"){
    containsLogOrSqrt <- stringr::str_detect(formula, "(?<=[^a-z]|^)(sqrt\\(|log\\(|log10\\()")
  }
  
  if(formula == ""){
    "Formula must not be empty"
  }else if (is.null(usedParams)){
    "Formula must contain at least one output"
  }else if (!is.expression(resParse)){
    paste("Incorrect formula:", resParse$message)
  }else if (is.list(resEval)){
    paste("Problem evaluating formula:", resEval$message, resEval$call)
  }else if (all(is.na(resEval))){
    "Invalid output, only NAs are produced"
  }else if (type == "numeric" & !is.list(resEval)){
    numValues <- grepl("^-?[0-9.]+$", resEval)
    if (!all(numValues) | is.factor(resEval)){
      if(!all(is.na(resEval[!numValues])))
        "Output not consitent with numeric type"
    }
  }else if (containsLogOrSqrt){
    "Cannot use log, ln or squared root in combine mode"
  }else{
    NULL
  }
}


indicator <- function(expr){
  return(ifelse(expr, 1, 0))
}


pln <- function(y){
  return(sign(y)*log(1+sign(y)*y))
}


plog <- function(y){
  return(sign(y)*log10(1+sign(y)*y))
}


compositeFunctionUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$style(
      HTML(paste0("
      
            #", ns("modalCompFunc"), " .modal-footer{
              display: none;
            }
            
            #", ns("modalCompFunc"), " .modal-header{
              display: none;
            }
            
            #", ns("modalCompFunc"), " .modal-body{
              min-height: auto;
            }
            
            #latexFormula {
              min-height: 50px;
              border: 1px lightgray solid;
            }
            
            #existingComp {
              border-top: 1px lightgray solid;
              padding-top: 5px;
              padding-bottom: 5px;
            }
            
           "))
    ),
    tagList(
      actionButton(ns("createCompFunc"), 
                   label = p(HTML("Create composite output"), 
                             style = "font-size: 15px"), 
                   class = "btn-primary", width = "40%"),
      bsModal(ns("modalCompFunc"), "Composite Output", NULL, size = "large",
              footer = NULL,
              uiOutput(ns("modalAddCompFunc")))
    ),
    
    uiOutput(ns("existingCompFunc")),
    
    br(),
    hr(),
    br(),
    
    h4(HTML("Note: 
             <ul>
                <li> In combine mode, if there is an existing model for the used outputs, the combination will be computed automatically </li>
                <li> In train mode, you must train the model manually </li>
                <li> Make sure to check the quality of the models in the surrogate model tab before proceeding with your work </li>
             </ul> "), 
       align = "left")
    
    
  )
}


compositeFunctionServer <- function(id, DOE) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      
      output$existingCompFunc <- renderUI({
        
        
        if(!is.null(DOE$compositeInfos)){
          tagList(
            useShinyjs(),
            withMathJax(),
            hr(),
            h4("Existing composite outputs"),
            
            lapply(seq(DOE$compositeInfos), function(i){
              div(id = "existingComp",
                  tagList(
                    fluidRow(
                      column(4, disabled(textInput(paste0("name", i), label = "Name", DOE$compositeInfos[[i]]$name))),
                      column(2, disabled(textInput(paste0("type", i), label = "Type", DOE$compositeInfos[[i]]$type))),
                      column(4, disabled(textInput(paste0("usedY", i), label = "Used Outputs", paste(unlist(DOE$compositeInfos[[i]]$usedY), collapse = ", ")))),
                      column(2, disabled(textInput(paste0("modelMode", i), label = "Metamodel", DOE$compositeInfos[[i]]$modelMode)))
                    ),
                    
                    fluidRow(
                      column(11, withMathJax(latexify(DOE$compositeInfos[[i]]$formula, DOE$compositeInfos[[i]]$name))),
                      column(1, style = "padding-top: 5px;",
                             tagList(
                               actionButton(ns(paste0("delCompFunc_", i)), 
                                            label = "", icon = icon("trash"), 
                                            class = "btn-danger", width = "100%"), 
                               hidden(list(
                                 p(id = ns(paste0("confirm", i)), HTML("Are you sure?")),
                                 actionButton(ns(paste0("yes", i)), label = "Yes", width = "45%"),
                                 actionButton(ns(paste0("no", i)), label = "No", width = "45%")
                               ))
                             )
                      )
                    )
                  )
              )
            })
          )
        }
      })
      
      output$modalAddCompFunc <- renderUI({
        fluidPage(
          useShinyjs(),
          withMathJax(),
          tagList(
            fluidRow(
              column(3, textInput(ns("name"), label = "Name")),
              column(2, pickerInput(inputId = ns("type"),
                                    label = "Type",
                                    choices = c("numeric", "categorical"))),
              column(2, pickerInput(inputId = ns("modelMode"),
                                    label = "Metamodel",
                                    choices = c("Train", "Combine"))),
              column(5, disabled(textInput(ns("formula"), label = "Formula"))),
            ),
            
            div(id = "latexFormula", uiOutput(ns("latexFormula"))),
            
            br(),
            
            fluidRow(
              # Calculator
              column(3, uiOutput(ns("buttonsOutputs"))),
              column(5, uiOutput(ns("buttonsNumbers"))),
              column(4, uiOutput(ns("buttonsFunctions")))

            ),
            
            br(),
            
            htmlOutput(ns("error")),
            fluidRow(
              column(3, actionButton(ns("addCompFunc"), label = "Add composite function", 
                                     class = "btn-warning",
                                     width = '100%'), offset = 2),
              column(3, actionButton(ns("closeCompFunc"), label = "Cancel", 
                                     class = "btn-secondary",
                                     width = '100%'), offset = 2)
            )
          )
        )
      })
      
      observeEvent(input$type, {
        
        if(input$type=="categorical"){
          updatePickerInput(session, "modelMode", 
                            choices = c("Train"),
                            selected = "Train")
        }else{
          updatePickerInput(session, "modelMode", 
                            choices = c("Train", "Combine"), 
                            selected = "Train")
        }
      })
      
      observeEvent(input$modelMode, {
        
        if(input$modelMode=="Combine"){
          shinyjs::disable("fnLog")
          shinyjs::disable("fnLn")
          shinyjs::disable("fnSqrt")
        }else{
          shinyjs::enable("fnLog")
          shinyjs::enable("fnLn")
          shinyjs::enable("fnSqrt")
        }
      })
      
      observeEvent(input$createCompFunc, {
        toggleModal(session, "modalCompFunc", toggle = "open")
      })
      
      
      observeEvent(input$closeCompFunc, {
        toggleModal(session, "modalCompFunc", toggle = "close")
        
        # Reset inputs
        updateTextInput(session, "formula", value = "")
        updateTextInput(session, "name", value = "")
        updatePickerInput(session, "type", selected = "numeric")
        updatePickerInput(session, "modelMode", selected = "Train")
        output$error <- NULL
      })
      
      delButtons <- reactiveVal(NULL)
      
      observeEvent(DOE$compositeInfos, {
        # Update compositeInfos when study is loaded
        if (length(compositeInfos$CInfos) 
            != length(DOE$compositeInfos)
                      & length(compositeInfos$CInfos) == 0){
          compositeInfos$CInfos <- DOE$compositeInfos
        }
      })
      
      observe({
        lapply(seq(compositeInfos$CInfos), function(i){
          
          delCompId <- paste0("delCompFunc_", i)
          delCompYes <- paste0("yes", i)
          delCompNo <- paste0("no", i)
          
          if (!(delCompId %in% delButtons())){
            
            delButtons(c(delButtons(), delCompId))
            
            observeEvent(input[[delCompId]], {
              toggleElement(paste0("confirm", i), time = 0.5, anim = TRUE, animType = "slide")
              toggleElement(paste0("yes", i), time = 0.25, anim = TRUE, animType = "slide")
              toggleElement(paste0("no", i), time = 0.25, anim = TRUE, animType = "slide")
            })
            
            observeEvent(input[[delCompNo]], {
              hideElement(paste0("confirm", i), time = 0.5, anim = TRUE, animType = "slide")
              hideElement(paste0("yes", i), time = 0.25, anim = TRUE, animType = "slide")
              hideElement(paste0("no", i), time = 0.25, anim = TRUE, animType = "slide")
            })
            
            observeEvent(input[[delCompYes]], {
              currName <- compositeInfos$CInfos[[i]]$name
              usingCurrentComp <- unlist(sapply(seq(compositeInfos$CInfos), 
                                                function(j){
                                                  if(currName %in% compositeInfos$CInfos[[j]]$usedY) 
                                                    j
                                                }))
              if (is.null(usingCurrentComp)){
                idDeleted <- compositeInfos$CInfos[[i]]$id
                compositeInfos$CInfos <- compositeInfos$CInfos[-i]
                for(j in seq(compositeInfos$CInfos)){
                  if (compositeInfos$CInfos[[j]]$id > idDeleted){
                    compositeInfos$CInfos[[j]]$id <- compositeInfos$CInfos[[j]]$id - 1
                  }
                }
              }else{
                showModal(
                  modalDialog(
                    title = "Unable to delete",
                    "Delete dependent outputs first"
                  )
                )
              }
            })
            
          }
        })
      })

      compositeInfos <- reactiveValues(CInfos = list())
      
      observeEvent(input$addCompFunc, {
        
        usedParams <- getUsedParams(input$formula, DOE$ynames)

        validName <- checkName(input$name, DOE$xnames, DOE$ynames)
        validFormula <- checkFormula(input$formula, usedParams, input$modelMode, input$type, DOE$Y)
        
        if(is.null(validName) & is.null(validFormula)){

          dfNewCol <- data.frame(with(DOE$Y, eval(parse(text = input$formula))))
          colnames(dfNewCol) <- input$name
          if (input$type == "categorical"){
            dfNewCol[, 1] <- as.factor(dfNewCol[, 1])
          }
          
          idComposite <- DOE$nY + 1
          
          newComposite <- list(name = input$name,
                               id = idComposite,
                               type = input$type, 
                               levels = ifelse(input$type == "categorical", 
                                               levels(dfNewCol[, 1]), NA),
                               usedY = usedParams, 
                               formula = input$formula,
                               modelMode = input$modelMode,
                               dfNewCol = dfNewCol)
          
          compositeInfos$CInfos <- append(compositeInfos$CInfos, list(newComposite))

          toggleModal(session, "modalCompFunc", toggle = "close")
          
          # Reset inputs
          updateTextInput(session, "formula", value = "")
          updateTextInput(session, "name", value = "")
          updatePickerInput(session, "type", selected = "numeric")
          updatePickerInput(session, "modelMode", selected = "Train")
          output$error <- NULL
          
        }else{
          output$error <- renderText({
            HTML(paste0("<hr />", validName, "<br />", validFormula, "<hr />"))
          })
        }
      })
      
      
      output$latexFormula <- renderUI({
        if(input$formula != ""){
          withMathJax(latexify(input$formula, input$name))
        }
      })

      YNames <- reactive({
        req(DOE$Yinfos)
        ids <- c(DOE$Yinfos$int.ids, DOE$Yinfos$const.ids, DOE$Yinfos$control.ids)
        return(DOE$ynames[ids])
      })
      
      output$buttonsOutputs <- renderUI({
        fluidPage(
          fluidRow(
            column(7, pickerInput(inputId = ns("chooseOutput"),
                                  label = "Outputs",
                                  choices = YNames(),
                                  selected = YNames()[1],
                                  options = pickerOptions(liveSearch = TRUE))),
            column(2, style = "margin-top: 25px;", 
                   actionButton(ns("insertOutput"), label = "Insert"))
          ),
          uiOutput(ns("buttonsLevels"))
        )
      })
      
      output$buttonsLevels <- renderUI({
        req(input$chooseOutput)
        
        outputType <- DOE$Yinfos$type[match(input$chooseOutput, DOE$ynames)]
        
        if(outputType == "categorical"){
          
          lvls <- with(DOE$Y, levels(eval(parse(text = input$chooseOutput))))
          
          fluidRow(
            column(7, pickerInput(inputId = ns("chooseLevel"),
                                  label = "Levels",
                                  choices = lvls,
                                  selected = lvls[1],
                                  options = pickerOptions(liveSearch = TRUE))),
            column(2, style = "margin-top: 25px;", 
                   actionButton(ns("insertLevel"), label = "Insert"))
          )
        }
        
        
        
      })
      
      observeEvent(input$insertOutput, {
        updateTextInput(session, "formula", value = paste0(input$formula, 
                                                           input$chooseOutput))
      })
      
      observeEvent(input$insertLevel, {
        updateTextInput(session, "formula", value = paste0(input$formula, "\"",
                                                           input$chooseLevel, "\""))
      })
      
      # Calc - output + observeEvents - Numbers ----
      
      output$buttonsNumbers <- renderUI({
        
        fluidPage(
          fluidRow(
            column(2, actionButton(ns("nb7"), label = "7", width = "150%")),
            column(2, actionButton(ns("nb8"), label = "8", width = "150%")),
            column(2, actionButton(ns("nb9"), label = "9", width = "150%")),
            column(1, ""),
            column(2, actionButton(ns("nbDEL"), label = HTML("&larr;"), width = "150%")),
            column(2, actionButton(ns("nbCLR"), label = "C", width = "150%"))
          ),
          br(),
          
          fluidRow(
            column(2, actionButton(ns("nb4"), label = "4", width = "150%")),
            column(2, actionButton(ns("nb5"), label = "5", width = "150%")),
            column(2, actionButton(ns("nb6"), label = "6", width = "150%")),
            column(1, ""),
            column(2, actionButton(ns("nbOP"), label = "(", width = "150%")),
            column(2, actionButton(ns("nbCP"), label = ")", width = "150%"))
          ),
          br(),
          
          fluidRow(
            column(2, actionButton(ns("nb1"), label = "1", width = "150%")),
            column(2, actionButton(ns("nb2"), label = "2", width = "150%")),
            column(2, actionButton(ns("nb3"), label = "3", width = "150%")),
            column(1, ""),
            column(2, actionButton(ns("nbDivide"), label = HTML("&div;"), width = "150%")),
            column(2, actionButton(ns("nbMultiply"), label = HTML("&times;"), width = "150%"))
          ),
          br(),
          
          fluidRow(
            column(4, actionButton(ns("nb0"), label = "0", width = "120%")),
            column(2, actionButton(ns("nbPoint"), label = ".", width = "150%")),
            column(1, ""),
            column(2, actionButton(ns("nbAdd"), label = "+", width = "150%")),
            column(2, actionButton(ns("nbSubstract"), label = "-", width = "150%"))
          )
        )
      })
      
      observeEvent(input$nb0, {
        updateTextInput(session, "formula", value = paste0(input$formula, "0"))
      })
      
      observeEvent(input$nb1, {
        updateTextInput(session, "formula", value = paste0(input$formula, "1"))
      })
      
      observeEvent(input$nb2, {
        updateTextInput(session, "formula", value = paste0(input$formula, "2"))
      })
      
      observeEvent(input$nb3, {
        updateTextInput(session, "formula", value = paste0(input$formula, "3"))
      })
      
      observeEvent(input$nb4, {
        updateTextInput(session, "formula", value = paste0(input$formula, "4"))
      })
      
      observeEvent(input$nb5, {
        updateTextInput(session, "formula", value = paste0(input$formula, "5"))
      })
      
      observeEvent(input$nb6, {
        updateTextInput(session, "formula", value = paste0(input$formula, "6"))
      })
      
      observeEvent(input$nb7, {
        updateTextInput(session, "formula", value = paste0(input$formula, "7"))
      })
      
      observeEvent(input$nb8, {
        updateTextInput(session, "formula", value = paste0(input$formula, "8"))
      })
      
      observeEvent(input$nb9, {
        updateTextInput(session, "formula", value = paste0(input$formula, "9"))
      })
      
      observeEvent(input$nbDEL, {
        updateTextInput(session, "formula", 
                        value = stringr::str_remove(input$formula, 
                                                    "([^\\^=+*\\/\\(\\)><-]+|[\\^+*\\/\\(\\)><-]|[=]{1,2}|!=)$"))
      })
      
      observeEvent(input$nbCLR, {
        updateTextInput(session, "formula", value = "")
      })
      
      observeEvent(input$nbOP, {
        updateTextInput(session, "formula", value = paste0(input$formula, "("))
      })
      
      observeEvent(input$nbCP, {
        updateTextInput(session, "formula", value = paste0(input$formula, ")"))
      })
      
      observeEvent(input$nbDivide, {
        updateTextInput(session, "formula", value = paste0(input$formula, "/"))
      })
      
      observeEvent(input$nbMultiply, {
        updateTextInput(session, "formula", value = paste0(input$formula, "*"))
      })
      
      observeEvent(input$nbPoint, {
        updateTextInput(session, "formula", value = paste0(input$formula, "."))
      })
      
      observeEvent(input$nbAdd, {
        updateTextInput(session, "formula", value = paste0(input$formula, "+"))
      })
      
      observeEvent(input$nbSubstract, {
        updateTextInput(session, "formula", value = paste0(input$formula, "-"))
      })
      
      # Calc - output + observeEvents - Functions ----
      
      output$buttonsFunctions <- renderUI({
        
        fluidPage(
          fluidRow(
            column(4, actionButton(ns("fnSin"), label = "sin", width = "130%")),
            column(4, actionButton(ns("fnCos"), label = "cos", width = "130%")),
            column(4, actionButton(ns("fnTan"), label = "tan", width = "130%"))
          ),
          br(),
          
          fluidRow(
            column(4, actionButton(ns("fnExp"), label = HTML("e<sup>x</sup>"), width = "130%")),
            column(4, actionButton(ns("fnLn"), label = "ln", width = "130%")),
            column(4, actionButton(ns("fnLog"), label = "log", width = "130%"))
          ),
          br(),
          
          fluidRow(
            column(4, actionButton(ns("fnAbs"), label = "|x|", width = "130%")),
            column(4, actionButton(ns("fnPln"), label = "pln", width = "130%")),
            column(4, actionButton(ns("fnPlog"), label = "plog", width = "130%"))
          ),
          br(),
          
          fluidRow(
            column(4, actionButton(ns("fnSqrt"), label = HTML("&radic;"), width = "130%")),
            column(4, actionButton(ns("fnPower"), label = HTML("x<sup>y</sup>"), width = "130%")),
            column(4, actionButton(ns("fnIndic"), label = HTML("<b>1</b><sub>A</sub>"), width = "130%"))
          ),
          br(),
          
          fluidRow(
            column(3, actionButton(ns("fnLt"), label = "<", width = "150%")),
            column(3, actionButton(ns("fnGt"), label = ">", width = "150%")),
            column(3, actionButton(ns("fnEq"), label = "=", width = "150%")),
            column(3, actionButton(ns("fnNeq"), label = HTML("&ne;"), width = "150%"))
          ),
          
          
        )
      })
      
      
      
      observeEvent(input$fnSin, {
        updateTextInput(session, "formula", value = paste0(input$formula, "sin("))
      })
      
      observeEvent(input$fnCos, {
        updateTextInput(session, "formula", value = paste0(input$formula, "cos("))
      })
      
      observeEvent(input$fnTan, {
        updateTextInput(session, "formula", value = paste0(input$formula, "tan("))
      })
      
      observeEvent(input$fnExp, {
        updateTextInput(session, "formula", value = paste0(input$formula, "exp("))
      })
      
      observeEvent(input$fnLn, {
        updateTextInput(session, "formula", value = paste0(input$formula, "log("))
      })
      
      observeEvent(input$fnLog, {
        updateTextInput(session, "formula", value = paste0(input$formula, "log10("))
      })
      
      observeEvent(input$fnAbs, {
        updateTextInput(session, "formula", value = paste0(input$formula, "abs("))
      })
      
      observeEvent(input$fnPln, {
        updateTextInput(session, "formula", value = paste0(input$formula, "pln("))
      })
      
      observeEvent(input$fnPlog, {
        updateTextInput(session, "formula", value = paste0(input$formula, "plog("))
      })
      
      observeEvent(input$fnSqrt, {
        updateTextInput(session, "formula", value = paste0(input$formula, "sqrt("))
      })
      
      observeEvent(input$fnPower, {
        updateTextInput(session, "formula", value = paste0(input$formula, "^"))
      })
      
      observeEvent(input$fnIndic, {
        updateTextInput(session, "formula", value = paste0(input$formula, "indicator("))
      })
      
      observeEvent(input$fnLt, {
        updateTextInput(session, "formula", value = paste0(input$formula, "<"))
      })
      
      observeEvent(input$fnGt, {
        updateTextInput(session, "formula", value = paste0(input$formula, ">"))
      })
      
      observeEvent(input$fnEq, {
        
        lastChar <- ""
        
        if(input$formula != ""){
          n <- nchar(input$formula)
          lastChar <- substr(input$formula, n, n)
        }
        
        eq <- ifelse(lastChar %in% c("<", ">"), "=", "==")
        
        updateTextInput(session, "formula", value = paste0(input$formula, eq))
      })
      
      observeEvent(input$fnNeq, {
        updateTextInput(session, "formula", value = paste0(input$formula, "!="))
      })
      
      return(compositeInfos)
    }
  )
}
