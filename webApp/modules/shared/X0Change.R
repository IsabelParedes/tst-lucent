#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module X0change

X0.check <- function(Xinfos, X0.temp) {
    error.msg <- list()

    # check Xinfos is not null
    if (is.null(X0.temp)) {
        error.msg$null <- "Empty inputs."
    } else {
        # check if the initial points are inside the bounds
        for (i in seq_len(nrow(X0.temp))) {
            valid.initial.inside <- all(unlist(lapply(1:ncol(X0.temp), function(i) {
                if (Xinfos[[i]]$type == "numeric") {
                    (X0.temp[[1, i]] >= Xinfos[[i]]$bounds[1]) & (X0.temp[[1, i]] <= Xinfos[[i]]$bounds[2])
                } else if (Xinfos[[i]]$type == "categorical") {
                    X0.temp[[1, i]] %in% Xinfos[[i]]$levels
                } else {
                    TRUE
                }
            })))
            if (!valid.initial.inside) {
                error.msg$initial.inside <- paste("Initial point", i, "is outside the bounds.")
                break
            }
        }
    }
    return(list(valid = (length(error.msg) == 0), error.msg = error.msg))
}

X0Change.ui <- function(id, label = "Change Initial Inputs Values", width = NULL) {
    ns <- NS(id)

    modalContent <- tagList(
        fluidRow(
            column(8, fileInput(ns('file'), 'Select File', accept = c('.txt','.dat','.csv'))),
            column(4, "")
        ),
        uiOutput(ns("sepdec.dynui")),
        uiOutput(ns("rangeInputs")),
        hr(),
        uiOutput(ns("footer"))
    )

    tagList(
        actionButton(ns("change"), label = label, class = "btn-primary", width = width),
        bsModal(ns("modal"), "Change Initial Inputs Values", NULL,
            modalContent,
            tags$head(tags$style(paste0(
                "#", ns("modal"), " .modal-footer{display:none}"
            )))
        )
    )
}

X0Change.server <- function(input, output, session, Xinfos, initialXVal, DOE) {
    ns <- session$ns

    # X0.temp temporally stores all modifications when the modal is open
    # initialXVal is set to X0.temp when the user saves the modifications and returned
    X0.temp <- reactiveVal(NULL)
    # Warning message for wrong updates of variable types
    error.msg <- reactiveValues(type = list(), save = NULL, file = NULL, file.names = NULL)

    file.to.load <- reactiveValues(datapath = NULL)
    
    observeEvent(input$file,{
        file.to.load$datapath <- input$file$datapath
    })

    # Once we have the file path, load it and try to autodetect separator, header and decimal
    
    header <- reactiveValues(bool = TRUE)
    separator <- reactiveValues(char= ",")
    decimal <- reactiveValues(char = ".")
    firstguessfile <- reactiveValues(finished = FALSE)
    
    observe({
        req(file.to.load$datapath)
        # Investigate the second line (to prevent a false detection if there is a header with points in variable names)
        line2 <- readLines(file.to.load$datapath, n = 2)
        if (length(line2) > 0) {
            if (length(line2) >= 2) {
                numLine <- 2
            }
            else {
                numLine <- 1
            }

            # Try all possible separators
            count.comma <- stri_count_fixed(line2, ",")[numLine]
            count.semicolon <- stri_count_fixed(line2, ";")[numLine]
            count.tab <- stri_count_fixed(line2, "\t")[numLine]
            if (count.semicolon > 0) {
                separator$char <- ";"
                if (count.comma > 0) {
                    decimal$char <- ","
                }
                else {
                    decimal$char <- "."
                }
            }
            else {
                if (count.tab > 0) {
                    separator$char <- "\t"
                    if (count.comma > 0) {
                        decimal$char <- ","
                    }
                    else {
                        decimal$char <- "."
                    }
                }
                else {
                    separator$char <- ","
                    decimal$char <- "."
                }
            }
            # Then use the separator to detect if there is a header
            line1 <- readLines(file.to.load$datapath, n = 1)
            xynames <- unlist(strsplit(line1, separator$char))
            xynames <- gsub(paste0('[', decimal$char,']'), '.',  xynames)
            header$bool <- suppressWarnings(all(is.na(as.numeric(xynames))))
        }
        else {
            header$bool <- F
        }
        firstguessfile$finished <- TRUE
    })
    
    # Initialize separator and decimal UI with first guess
    # Separator and decimal UI now longer accessible once import is confirmed
    output$sepdec.dynui <- renderUI({
        req(firstguessfile$finished)
        tagList(
            hr(),
            h5("Header, Separator and Decimal have been auto-detected."),
            h5("Please change values if not correct."),
            fluidRow(
                column(4, radioButtons(ns("separator"), "Separator",
                                    choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"), selected=separator$char)),
                column(4, radioButtons(ns("decimal"), "Decimal",
                                    choices = list(". (point)" = ".", ", (comma)" = ","), selected=decimal$char))
            ),
            hr(),
            bsModal(
                ns("modalduplicate"), "Information", NULL, size = "large",
                uiOutput(outputId = ns("alertduplicate"))
            )
        )
    })
    
    # Now read file with appropriate settings
    file.data <- reactive({
        req(firstguessfile$finished, file.to.load$datapath, input$separator != input$decimal)
        # consistency check
        head.lines.consist <- length(unique(lapply(readLines(file.to.load$datapath), function(line) {
            stringi::stri_count(line, fixed = input$separator)
        })))
        if (head.lines.consist == 1) {
            df <- read.csv(file.to.load$datapath, header = header$bool, sep = input$separator,
                            dec = input$decimal)
            if (ncol(df) >= length(Xinfos$Xinfos)) {
                df <- df[,seq_len(length(Xinfos$Xinfos)), drop=F]
                df.temp <- df
                colnames(df.temp) <- NULL
                if (anyDuplicated(df.temp) > 0) {
                    toggleModal(session, "modalduplicate", toggle = "open")
                }
                return(df)
            }
        }
        return(NULL)
    })

    observeEvent(file.data(), {
        req(file.data())
        X0.temp(file.data())
    })

    # Alert the user if there are duplicate rows
    output$alertduplicate <- renderUI({
        tagList(
            h3("Identical simulations have been detected in the DOE."),
            h3("We strongly suggest you clean your file and reload it.")
        )
    })
    
    # we reinitialize X0.temp when initialXVal has changed
    observe({
        X0.temp(initialXVal())
    })

    observeEvent(input$change, {
        toggleModal(session, "modal", toggle = "open")
    })

    observeEvent(input$resetInitValues, {
        x0 <- lapply(seq_len(length(Xinfos$Xinfos)), function(i) {
          var <- Xinfos$Xinfos[[i]]
          if (var$type == "numeric") {
            return((var$bounds[1] + var$bounds[2]) / 2)
          } else if (var$type == "categorical") {
            return(var$levels[[1]])
          }
          return(NaN)
        })

        X0.temp(as.data.frame(x0))
    })

    observeEvent(input$useExistingPoint, {
        showModal(dataModal())
    })
    
    dataModal <- function(errorMessage = NULL) {
        modalDialog(
            if (is.null(DOE$XY))
                HTML("In this run set, there is no available points for the selected simulator"),
            if (!is.null(DOE$XY))
                DTOutput(ns("simulationsView")),
            if (!is.null(errorMessage))
                div(tags$b(errorMessage, style = "color: red;")),
    
            footer = tagList(
                modalButton("Cancel"),
                actionButton(ns("ok"), "OK")
            ),
            title = "Select one or several line(s) as initial value(s)"
        )
    }

    output$simulationsView <- DT::renderDT({
        if (length(DOE$Yinfos$int.ids) > 0) {
            data <- cbind(DOE$X, DOE$Y[,DOE$Yinfos$int.ids,drop=F])
        }
        else {
            if (ncol(DOE$XY) > 100) {
                data <- DOE$X
            }
            else {
                data <- DOE$XY
            }
        }
        DT::datatable(
            as.data.frame(data),
            extensions = c("FixedColumns","Scroller"),
            selection = "multiple",
            options = list(
                dom = "t",
                scrollX = T, scrollY = 400, scroller = TRUE, fixedColumns = list(leftColumns = 1),
                autoWidth = F
            )
        )
    })

    checkSelectedExistingPoint <- function(selectedRowIndex) {
        if (is.null(selectedRowIndex)) {
            return("Please, select a row.")
        }

        insideBounds <- all(unlist(lapply(1:length(Xinfos$Xinfos), function(i) {
            var <- Xinfos$Xinfos[[i]]
            varValue <- DOE$X[var$name][selectedRowIndex, ]
            if (var$type == "numeric") {
                (varValue >= var$bounds[1]) & (varValue <= var$bounds[2])
            } else if (var$type == "categorical") {
                varValue %in% var$levels
            } else {
                TRUE
            }
        })))

        if (!insideBounds) {
            return("Selected point is out of the optimization bounds.")
        }

        return(NULL)
    }

    observeEvent(input$ok, {
        selectedRowIndex = input$simulationsView_rows_selected
        # Check that selected point is in the optimization bounds
        errorMessage <- checkSelectedExistingPoint(selectedRowIndex)
        if (is.null(errorMessage)) {
            X0.temp(DOE$X[selectedRowIndex,])

            removeModal()
        } else {
            showModal(dataModal(errorMessage = errorMessage))
        }
    })

    observeEvent(input$save, {
        req(Xinfos)

        X0.validation <- X0.check(Xinfos$Xinfos, X0.temp())
        if (X0.validation$valid) {
            initialXVal(X0.temp())
            error.msg$save <- NULL
            error.msg$file <- NULL
            error.msg$file.names <- NULL
            error.msg$type <- list()
            toggleModal(session, "modal", toggle = "close")
        } else {
            error.msg$save <- X0.validation$error.msg
        }
    })

    observeEvent(input$close, {
        toggleModal(session, "modal", toggle = "close")

        error.msg$file <- NULL
        error.msg$file.names <- NULL
        error.msg$save <- NULL

        X0.temp(initialXVal())
    })

    output$rangeInputs <- renderUI({
        req(Xinfos, initialXVal)
        Xinfos.df.names <- sapply(Xinfos$Xinfos, function(row) {
          return(row[['namevisu']])
        })
        X.init <- X0.temp()
        colnames(X.init) <- Xinfos.df.names
        tagList(
            DT::datatable(
                X.init,
                extensions = c("FixedColumns","Scroller"),
                selection = "single",
                options = list(
                    dom = "t",
                    scrollX = T, scrollY = 400, scroller = TRUE, fixedColumns = list(leftColumns = 1),
                    autoWidth = F
                )
            ),
            fluidRow(
                column(
                    6,
                    actionButton(ns("resetInitValues"),
                        label = "Reset to central point", class = "btn-primary",
                        width = "100%"
                    ),
                    style = "margin-top: 15px;"
                ),
                column(
                    6,
                    actionButton(ns("useExistingPoint"),
                        label = "Set to existing points", class = "btn-primary",
                        width = "100%"
                    ),
                    style = "margin-top: 15px;"
                )
            )
        )
    })

    output$footer <- renderUI({
        list(
            column(
                12,
                if (!is.null(error.msg$save)) {
                    list(
                        h4(strong("Error !")),
                        HTML(paste(paste(error.msg$save, collapse = "<br/>"), "<br/> <br/>"))
                    )
                } else {
                    NULL
                }
            ),
            fluidRow(
                column(3, actionButton(ns("save"),
                    label = "Save and Close", class = "btn-warning",
                    width = "100%"
                ), offset = 2),
                column(3, actionButton(ns("close"),
                    label = "Dismiss", class = "btn-secondary",
                    width = "100%"
                ), offset = 2)
            )
        )
    })

    return(Xinfos)
}