#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module buildsurrogate
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)

# load all surrogate functions
source("modules/surrogate/surrogate_functions.R", local = TRUE)

updatebestselected <- function(listmodels, type) {
  idTrained <- listmodels$trainedModels
  dims <- ncol(listmodels$tableQ2loo)

  allQ2 <- listmodels$tableQ2loo[, 4:dims, drop = FALSE]
  idYFailModels <- apply(allQ2, 2, allNA)
  allQ2[is.na(allQ2)] <- -Inf
  bestQ2 <- apply(allQ2, 2, max, na.rm = TRUE)
  idbestQ2 <- apply(allQ2, 2, which.max)
  idbestQ2[idYFailModels] <- NA

  if (type == "LOO") {
    listmodels$bestQ2loo$id <- listmodels$selected$id <- idbestQ2
    listmodels$bestQ2loo$Q2 <- listmodels$selected$Q2 <- bestQ2
    listmodels <- updateFinalpredfun(listmodels)
  }

  if (type == "Test") {
    listmodels$bestQ2test$id <- listmodels$selected$id <- idbestQ2
    listmodels$bestQ2test$Q2 <- listmodels$selected$Q2 <- bestQ2
    listmodels <- updateFinalpredfun(listmodels)
  }

  return(listmodels)
}

updateFinalpredfun <- function(listmodels) {
  listmodels$finalpredfun <- function(x, j) {
    obj <- listmodels$models[[listmodels$selected$id[[j]]]][[j]]
    if (is.null(obj$predfunForm)) {
      predict.metamodel(obj, x, computesd = FALSE)$mean
    } else {
      eval(parse(text = obj$predfunForm))
    }
  }
  return(listmodels)
}

computeQ2test <- function(ypredtest, ytest) {
  if (is.character(ypredtest)) {
    # This means this is a categorical output
    Q2test <- sum(ypredtest == as.character(ytest)) / length(ypredtest)
  } else {
    Q2test <- 1 - sum((ytest - ypredtest)^2) / sum((ytest - mean(ytest))^2)
  }
  return(Q2test)
}

compute.lasso.model <- function(DOE, idY, categorical, levels, models, callback) {
  dimY <- length(idY)
  idComposites <- sapply(DOE$compositeInfos, function(x) x$id)
  lassomodels <- list()

  for (i in 1:dimY) {
    yy <- DOE$Y[, idY[i]]
    Ytype <- DOE$Yinfos$type[idY[i]]

    train <- TRUE

    if (idY[i] %in% idComposites) {
      cInfoIdx <- sapply(DOE$compositeInfos, function(x) idY[i] %in% x$id)
      cInfoIdx <- match(TRUE, cInfoIdx)
      currentCompositeInfos <- DOE$compositeInfos[[cInfoIdx]]

      if (currentCompositeInfos$modelMode == "Combine") {
        train <- FALSE
      }
    }

    if (train) {
      modelTemp <- try(build.metamodel(DOE$X, yy, Ytype, type.metamodel = "Lasso", categorical = categorical, levels = levels))
      if (class(modelTemp) == "try-error") {
        lassomodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      } else {
        lassomodels[[i]] <- modelTemp
        models[[idY[i]]] <- modelTemp
      }
    } else {
      idxToCombine <- sapply(match(currentCompositeInfos$usedY, DOE$ynames), function(j) {
        which(1:length(models) == j)
      })
      modelsToCombine <- lapply(idxToCombine, function(x) models[[x]])
      # If one model has not converged (is NULL), the combined model must be NULL
      bool_converge <- TRUE
      for (j in 1:length(modelsToCombine)) {
        if (is.null(modelsToCombine[[j]])) {
          bool_converge <- FALSE
        }
      }
      if (bool_converge) {
        combinedModel <- combineMetamodels(yy, modelsToCombine, idxToCombine, currentCompositeInfos)
        lassomodels[[i]] <- combinedModel
        models[[idY[i]]] <- combinedModel
      } else {
        lassomodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      }
    }
    callback(i)
  }

  return(lassomodels)
}

compute.acosso.model <- function(DOE, idY, order, categorical, levels, models, vars = NULL, callback) {
  type.metamodel <- switch(order,
    "Acosso1",
    "Acosso2"
  )
  dimX <- DOE$nX
  dimY <- length(idY)
  if (is.null(vars)) vars <- rep(list(NULL), dimY)
  acossomodels <- list()

  idComposites <- sapply(DOE$compositeInfos, function(x) x$id)

  for (i in 1:dimY) {
    yy <- DOE$Y[, idY[i]]

    train <- TRUE

    if (idY[i] %in% idComposites) {
      cInfoIdx <- sapply(DOE$compositeInfos, function(x) idY[i] %in% x$id)
      cInfoIdx <- match(TRUE, cInfoIdx)
      currentCompositeInfos <- DOE$compositeInfos[[cInfoIdx]]

      if (currentCompositeInfos$modelMode == "Combine") {
        train <- FALSE
      }
    }

    if (train) {
      modelTemp <- try(build.metamodel(DOE$X, yy,
        type.metamodel = type.metamodel,
        categorical = categorical, levels = levels, acosso2.selvar = vars[[i]]
      ))
      if (class(modelTemp) == "try-error") {
        acossomodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      } else {
        acossomodels[[i]] <- modelTemp
        models[[idY[i]]] <- modelTemp
      }
    } else {
      idxToCombine <- sapply(match(currentCompositeInfos$usedY, DOE$ynames), function(j) {
        which(1:length(models) == j)
      })
      modelsToCombine <- lapply(idxToCombine, function(x) models[[x]])
      # If one model has not converged (is NULL), the combined model must be NULL
      bool_converge <- TRUE
      for (j in 1:length(modelsToCombine)) {
        if (is.null(modelsToCombine[[j]])) {
          bool_converge <- FALSE
        }
      }
      if (bool_converge) {
        combinedModel <- combineMetamodels(yy, modelsToCombine, idxToCombine, currentCompositeInfos)
        acossomodels[[i]] <- combinedModel
        models[[idY[i]]] <- combinedModel
      } else {
        acossomodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      }
    }

    callback(i)
  }
  return(acossomodels)
}

compute.kriging.model <- function(DOE, idY, categorical, levels, models, vars, trend, trendobj, interpolate, multi, callback) {
  type.metamodel <- "Kriging"
  kriging.nugget <- !(interpolate)
  dimX <- DOE$nX
  dimY <- length(idY)
  krigingmodels <- list()

  idComposites <- sapply(DOE$compositeInfos, function(x) x$id)

  for (i in 1:dimY) {
    yy <- DOE$Y[, idY[i]]
    Ytype <- DOE$Yinfos$type[idY[i]]

    train <- TRUE

    if (idY[i] %in% idComposites) {
      cInfoIdx <- sapply(DOE$compositeInfos, function(x) idY[i] %in% x$id)
      cInfoIdx <- match(TRUE, cInfoIdx)
      currentCompositeInfos <- DOE$compositeInfos[[cInfoIdx]]

      if (currentCompositeInfos$modelMode == "Combine") {
        train <- FALSE
      }
    }

    if (train) {
      modelTemp <- try(build.metamodel(DOE$X, yy, Ytype,
        type.metamodel = type.metamodel,
        categorical = categorical, levels = levels,
        kriging.trend = trend, kriging.selvar = vars[[i]],
        trendobj = trendobj[[i]], kriging.nugget = kriging.nugget, kriging.multi = multi
      ))
      if (class(modelTemp) == "try-error") {
        krigingmodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      } else {
        krigingmodels[[i]] <- modelTemp
        models[[idY[i]]] <- modelTemp
      }
    } else {
      idxToCombine <- sapply(match(currentCompositeInfos$usedY, DOE$ynames), function(j) {
        which(1:length(models) == j)
      })
      modelsToCombine <- lapply(idxToCombine, function(x) models[[x]])
      # If one model has not converged (is NULL), the combined model must be NULL
      bool_converge <- TRUE
      for (j in 1:length(modelsToCombine)) {
        if (is.null(modelsToCombine[[j]])) {
          bool_converge <- FALSE
        }
      }
      if (bool_converge) {
        combinedModel <- combineMetamodels(yy, modelsToCombine, idxToCombine, currentCompositeInfos)
        krigingmodels[[i]] <- combinedModel
        models[[idY[i]]] <- combinedModel
      } else {
        krigingmodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      }
    }

    callback(i)
  }
  return(krigingmodels)
}




compute.surrogate.model <- function(DOE, idY, args, categorical, levels, models, SurrogateEnv, vars = NULL, callback) {
  type.metamodel <- "UserDefined"
  dimX <- DOE$nX
  dimY <- length(idY)
  if (is.null(vars)) vars <- rep(list(NULL), dimY)
  userdefinedmodels <- list()
  idComposites <- sapply(DOE$compositeInfos, function(x) x$id)
  for (i in 1:dimY) {
    # yy <- DOE$Y[, idY[i]]
    # To keep the column name :
    yy <- matrix( DOE$Y[, idY[i]], dimnames=list(NULL, colnames(DOE$Y)[idY[i]]) )

    Ytype <- DOE$Yinfos$type[idY[i]]

    train <- TRUE

    if (idY[i] %in% idComposites) {
      cInfoIdx <- sapply(DOE$compositeInfos, function(x) idY[i] %in% x$id)
      cInfoIdx <- match(TRUE, cInfoIdx)
      currentCompositeInfos <- DOE$compositeInfos[[cInfoIdx]]

      if (currentCompositeInfos$modelMode == "Combine") {
        train <- FALSE
      }
    }

    if (train) {
      modelTemp <- try(build.metamodel(DOE$X, yy, Ytype,
        type.metamodel = type.metamodel,
        categorical = categorical, levels = levels, udefined.selvar = vars[[i]], args=args
      ))
      if (class(modelTemp) == "try-error") {
        logger$print(attr(modelTemp, "condition"))
        userdefinedmodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      } else {
        userdefinedmodels[[i]] <- modelTemp
        models[[idY[i]]] <- modelTemp
      }
    } else {
      idxToCombine <- sapply(match(currentCompositeInfos$usedY, DOE$ynames), function(j) {
        which(1:length(models) == j)
      })
      modelsToCombine <- lapply(idxToCombine, function(x) models[[x]])
      # If one model has not converged (is NULL), the combined model must be NULL
      bool_converge <- TRUE
      for (j in 1:length(modelsToCombine)) {
        if (is.null(modelsToCombine[[j]])) {
          bool_converge <- FALSE
        }
      }
      if (bool_converge) {
        combinedModel <- combineMetamodels(yy, modelsToCombine, idxToCombine, currentCompositeInfos)
        userdefinedmodels[[i]] <- combinedModel
        models[[idY[i]]] <- combinedModel
      } else {
        userdefinedmodels[i] <- list(NULL)
        models[idY[i]] <- list(NULL)
      }
    }
    callback(i)
  }
  return(userdefinedmodels)
}

buildsurrogate.ui <- function(id) {
  ns <- NS(id)

  Q2Modal <- bsModal(
    ns("modalQ2"), "Q2", NULL,
    uiOutput(ns("ui.Q2visu")),
    size = "large"
  )

  Q2testfileModal <- bsModal(
    ns("modalQ2testfile"), "Import test file for Q2 validation", NULL,
    tagList(
      fluidRow(
        column(2, radioButtons(ns("separator"), "Separator",
          choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t")
        )),
        column(2, radioButtons(ns("decimal"), "Decimal",
          choices = list(". (point)" = ".", ", (comma)" = ",")
        )),
        column(
          7,
          fileInput(ns("fileQ2test"), "Select Test File", accept = c(".txt", ".dat", ".csv")),
          tags$script(paste0('$( "#', ns("fileQ2test"), '" ).on( "click", function() { this.value = null; });')),
          uiOutput(ns("error.file"))
        )
      ),
      DT::dataTableOutput(ns("Q2testfilecontents")),
      br(),
      actionButton(ns("computeQ2test"), "Compute Q2 on test file for all metamodels already built", class = "btn-primary")
    ),
    size = "large"
  )

  Acosso1Modal <- bsModal(
    ns("modalAcosso1"), "Acosso1 Settings", NULL,
    tagList(
      h4("You can choose to train the surrogate only on the outputs for which the Q2 of a previous surrogate was not sufficient enough. When the number of outputs is large and previous surrogates are of good quality,
       this accelerates the search of the best surrogate."),
      br(),
      fluidRow(
        column(3, ""),
        column(6,
          radioGroupButtons(ns("acosso1outputs"),
            label = "Choose Outputs",
            choices = list("All" = 1, "Selected Outputs" = 2),
            selected = 1,
            status = "primary",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"),
              no = icon("remove", lib = "glyphicon")
            )
          ),
          align = "center"
        ),
        column(
          3,
          numericInput(ns("outQ2thresholdacosso1"), "Choose Q2 LOO threshold", 0.95, min = 0, max = 1),
          h5("Only the outputs with the best surrogate so far less than the Q2 LOO threshold will be considered.")
        )
      ),
      br(),
      actionButton(ns("buildacosso1"), "Train Acosso 1", class = "btn-primary", width = "100%")
    ),
    size = "large"
  )

  Acosso2Modal <- bsModal(
    ns("modalAcosso2"), "Acosso2 Settings", NULL,
    tagList(
      fluidRow(
        column(3, ""),
        column(6,
          dynamicSelect.ui(ns("chooseAcosso2var")),
          align = "center"
        ),
        column(3, "")
      ),
      h4("If the number of inputs is large, the number of interactions to identify increases drastically. For problems with more than 20 inputs, a first reasonable try consists in restricting the search to interactions
       between the inputs which have a main effect only (i.e. those identified by Acosso 1). For problems with more than 50 inputs, this is the only reasonable approach for computational reasons."),
      hr(),
      h4("You can choose to train the surrogate only on the outputs for which the Q2 of a previous surrogate was not sufficient enough. When the number of outputs is large and previous surrogates are of good quality,
       this accelerates the search of the best surrogate."),
      br(),
      fluidRow(
        column(3, ""),
        column(6,
          radioGroupButtons(ns("acosso2outputs"),
            label = "Choose Outputs",
            choices = list("All" = 1, "Selected Outputs" = 2),
            selected = 1,
            status = "primary",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"),
              no = icon("remove", lib = "glyphicon")
            )
          ),
          align = "center"
        ),
        column(
          3,
          numericInput(ns("outQ2thresholdacosso2"), "Choose Q2 LOO threshold", 0.95, min = 0, max = 1),
          h5("Only the outputs with the best surrogate so far less than the Q2 LOO threshold will be considered.")
        )
      ),
      br(),
      actionButton(ns("buildacosso2"), "Train Acosso 2", class = "btn-primary", width = "100%")
    ),
    size = "large"
  )

  KrigingModal <- bsModal(
    ns("modalKriging"), "Kriging Settings", NULL,
    tagList(
      fluidRow(
        column(3, ""),
        column(6,
          dynamicSelect.ui(ns("chooseKrigingvar")),
          dynamicSelect.ui(ns("chooseKrigingtrend")),
          align = "center"
        ),
        column(
          3,
          checkboxInput(ns("interpolate"), "Interpolate ?", value = T),
          h6("This option only applies to kriging surrogates. When the outputs are subjected to noise (i.e. experimental data),
                it is highly recommended to turn off the interpolation."),
          checkboxInput(ns("krig.multistart"), "MultiStart Estim.", value = F)
        )
      ),
      h4("This model is the most flexible one and can model complex input-outputs relationships. But if the number of inputs is larger than 15-20, its flexibility implies that its calibration may be hard and result in a poor surrogate."),
      h4("For such situations, when activating the expert mode, it is possible to break down the calibration by using a trend given by one of the previously trained surrogate, and try to estimate the residuals only.
       To go further, one can also limit the variables involved in the kriging model."),
      hr(),
      h4("You can choose to train the surrogate only on the outputs for which the Q2 of a previous surrogate was not sufficient enough. When the number of outputs is large and previous surrogates are of good quality,
       this accelerates the search of the best surrogate."),
      br(),
      fluidRow(
        column(3, ""),
        column(6,
          radioGroupButtons(ns("krigingoutputs"),
            label = "Choose Outputs",
            choices = list("All" = 1, "Selected Outputs" = 2),
            selected = 1,
            status = "primary",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"),
              no = icon("remove", lib = "glyphicon")
            )
          ),
          align = "center"
        ),
        column(
          3,
          numericInput(ns("outQ2thresholdkriging"), "Choose Q2 LOO threshold", 0.95, min = 0, max = 1),
          h5("Only the outputs with the best surrogate so far less than the Q2 LOO threshold will be considered.")
        )
      ),
      br(),
      actionButton(ns("buildkriging"), "Train Kriging Model", class = "btn-primary", width = "100%")
    ),
    size = "large"
  )

  UserDefinedSurrogateModal <- bsModal(
    ns("modalUserDefined"), "Surrogate Settings", NULL,
    tagList(
      h4("You can choose to train the surrogate only on the outputs for which the Q2 of a previous surrogate was not sufficient enough. When the number of outputs is large and previous surrogates are of good quality,
       this accelerates the search of the best surrogate."),
      br(),
      fluidRow(
        column(3, ""),
        column(6,
          dynamicSelect.ui(ns("chooseSurrogatevar")),
          dynamicSelect.ui(ns("chooseSurrogateModel")),
          align = "center"
        ),
        column(3, "")
      ),
      fluidRow(
        column(6,
          radioGroupButtons(ns("surrogateoutputs"),
            label = "Choose Outputs",
            choices = list("All" = 1, "Selected Outputs" = 2),
            selected = 1,
            status = "primary",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"),
              no = icon("remove", lib = "glyphicon")
            )
          ),
          align = "center"
        ),
        column(
          3,
          numericInput(ns("outQ2thresholdsurrogate"), "Choose Q2 LOO threshold", 0.95, min = 0, max = 1),
          h5("Only the outputs with the best surrogate so far less than the Q2 LOO threshold will be considered.")
        )
      ),
      br(),
      actionButton(ns("builduserdefined"), "Train Surrogate", class = "btn-primary", width = "100%")
    ),
    size = "large"
  )

  tagList(
    fluidRow(
      column(
        2,
        selectInput(
          ns("surrogatemode"),
          label = h5("Choose Normal/Expert Mode", id = ns("surrogatemodeLabel")),
          choices = list("Normal Mode", "Expert Mode"),
          selected = "Normal Mode"
        ),
        h6("Switching to expert mode enables more complex surrogate combinations to handle difficult problems.")
      ),
      column(2,
        align = "center",
        wellPanel(
          actionButton(ns("buildlasso"), "Train Lasso Model", class = "btn-primary"),
          h6("It is highly recommended to try first this linear model. For some outputs it may be predictive enough and for the others you can check how nonlinear the output may be.")
        )
      ),
      uiOutput(ns("dynui_normalmode")),
      column(1, ""),
      column(1,
        switchInput(ns("retrain"), value = F, label = "Re-training allowed", size = "mini"),
        align = "center"
      ),
      Acosso1Modal,
      Acosso2Modal,
      KrigingModal,
      UserDefinedSurrogateModal
    ),
    hr(),
    hr(),
    fluidRow(
      column(
        4,
        h4("Click on a cell in the Q2 table below to display the prediction quality for each output and each metamodel.")
      ),
      column(4,
        align = "center",
        dynamicSelect.ui(ns("Q2mode"))
      ),
      column(4,
        align = "center",
        actionButton(ns("loadQ2testfile"), "Load Test File", class = "btn-primary")
      ),
      Q2Modal,
      Q2testfileModal,
      DT::dataTableOutput(ns("tableQ2"))
    )
  )
}

buildsurrogate.server <- function(id, DOE, listmodels, settings, surrogate.clicked, SurrogateEnv, persistence) {
  
  SurrogatesNumber <- function() {
      return(length(listmodels$names_surrogatemodel))
  }

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Store latest metamodel information for crash pop up
      latest_model <- reactiveValues(
        name = NULL,
        convergence = NULL,
        trained_outputs = NULL
      )

      # Store which metamodels for which outputs have been trained
      whatwastrained <- reactiveValues(id = NULL)
      # React to what is asked
      askedtrained <- reactiveValues(id = NULL)
      # start afresh when training data changed
      observeEvent(list(DOE$X, DOE$Y, DOE$xnames, DOE$ynames),
        {
          req(DOE$nY > 0)
          whatwastrained$id <- matrix(FALSE, SurrogatesNumber(), DOE$nY)
        },
        ignoreNULL = FALSE
      )
      # if a new output from Yinfos is available for surrogate modelling, reset askedtrained
      observeEvent(list(DOE$X, DOE$Y, DOE$xnames, DOE$ynames, DOE$Yinfos), {
        req(DOE$nY > 0, all(DOE$Yinfos$surrogate.ids <= DOE$nY))
        askedtrained$id <- matrix(FALSE, SurrogatesNumber(), DOE$nY)
        askedtrained$id[1, DOE$Yinfos$surrogate.ids] <- TRUE
        whatwastrained$id <- matrix(FALSE, SurrogatesNumber(), DOE$nY)
      })

      observe({
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, all(DOE$Yinfos$surrogate.ids <= ncol(askedtrained$id)),
          input$acosso1outputs, input$outQ2thresholdacosso1, DOE$Yinfos
        )

        if (input$acosso1outputs == 1) {
          askedtrained$id[2, DOE$Yinfos$surrogate.ids] <- TRUE
        } else {
          id <- which(listmodels$bestQ2loo$Q2[DOE$Yinfos$surrogate.ids] < input$outQ2thresholdacosso1)
          askedtrained$id[2, DOE$Yinfos$surrogate.ids[id]] <- TRUE
          askedtrained$id[2, setdiff(1:DOE$nY, DOE$Yinfos$surrogate.ids[id])] <- FALSE
        }
      })
      observe({
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, all(DOE$Yinfos$surrogate.ids <= ncol(askedtrained$id)),
          input$acosso2outputs, input$outQ2thresholdacosso2, acosso2var(), DOE$Yinfos
        )

        if (input$acosso2outputs == 1) {
          if (acosso2var() == "All Vars") {
            askedtrained$id[3, DOE$Yinfos$surrogate.ids] <- TRUE
            askedtrained$id[4, ] <- FALSE
          } else {
            askedtrained$id[4, DOE$Yinfos$surrogate.ids] <- TRUE
            askedtrained$id[3, ] <- FALSE
          }
        } else {
          id <- which(listmodels$bestQ2loo$Q2[DOE$Yinfos$surrogate.ids] < input$outQ2thresholdacosso2)
          if (acosso2var() == "All Vars") {
            askedtrained$id[3, DOE$Yinfos$surrogate.ids[id]] <- TRUE
            askedtrained$id[3, setdiff(1:DOE$nY, DOE$Yinfos$surrogate.ids[id])] <- FALSE
            askedtrained$id[4, ] <- FALSE
          } else {
            askedtrained$id[4, DOE$Yinfos$surrogate.ids[id]] <- TRUE
            askedtrained$id[4, setdiff(1:DOE$nY, DOE$Yinfos$surrogate.ids[id])] <- FALSE
            askedtrained$id[3, ] <- FALSE
          }
        }
      })
      observe({
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, all(DOE$Yinfos$surrogate.ids <= ncol(askedtrained$id)),
          input$krigingoutputs, input$outQ2thresholdkriging, idkrigingselected(), DOE$Yinfos
        )

        if (input$krigingoutputs == 1) {
          askedtrained$id[idkrigingselected(), DOE$Yinfos$surrogate.ids] <- TRUE
          askedtrained$id[setdiff(5:12, idkrigingselected()), ] <- FALSE
        } else {
          id <- which(listmodels$bestQ2loo$Q2[DOE$Yinfos$surrogate.ids] < input$outQ2thresholdkriging)
          askedtrained$id[idkrigingselected(), DOE$Yinfos$surrogate.ids[id]] <- TRUE
          askedtrained$id[idkrigingselected(), setdiff(1:DOE$nY, DOE$Yinfos$surrogate.ids[id])] <- FALSE
          askedtrained$id[setdiff(5:12, idkrigingselected()), ] <- FALSE
        }
      })

      observe({
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, all(DOE$Yinfos$surrogate.ids <= ncol(askedtrained$id)),
          input$surrogateoutputs, input$outQ2thresholdsurrogate, surrogatevar(), DOE$Yinfos
        )
        if (input$surrogateoutputs == 1) {
            askedtrained$id[idsurrogateselected(), DOE$Yinfos$surrogate.ids] <- TRUE
            askedtrained$id[setdiff(13:SurrogatesNumber(), idsurrogateselected()), ] <- FALSE
        } else {
          id <- which(listmodels$bestQ2loo$Q2[DOE$Yinfos$surrogate.ids] < input$outQ2thresholdsurrogate)
          askedtrained$id[idsurrogateselected(), DOE$Yinfos$surrogate.ids[id]] <- TRUE
          askedtrained$id[idsurrogateselected(), setdiff(1:DOE$nY, DOE$Yinfos$surrogate.ids[id])] <- FALSE
          askedtrained$id[setdiff(13:SurrogatesNumber(), idsurrogateselected()), ] <- FALSE
          }
      })

      # Dynamic UI for metamodels options
      choicesAcosso2var <- reactive({
        if ((2 %in% listmodels$trainedModels)) {
          return(c("All Vars", "Acosso 1 Vars"))
        } else {
          return("All Vars")
        }
      })

      acosso2var <- callModule(
        dynamicSelect.server, "chooseAcosso2var",
        label = "Choose Variables", choicesAcosso2var
      )

      choicesvar <- reactive({
        req(input$surrogatemode)
        listvar <- "All Vars"
        if (input$surrogatemode == "Expert Mode" || (input$surrogatemode == "User Defined")) {
          if (1 %in% listmodels$trainedModels) {
            listvar <- c(listvar, "Lasso Vars")
          }
          if (2 %in% listmodels$trainedModels) {
            listvar <- c(listvar, "Acosso1 Vars")
          }
        }
        return(listvar)
      })

      krigingvar <- callModule(
        dynamicSelect.server, "chooseKrigingvar",
        label = "Choose Variables", choicesvar
      )

      surrogatevar <- callModule(
        dynamicSelect.server, "chooseSurrogatevar",
        label = "Choose Variables", choicesvar
      )

      choicesKrigingtrend <- reactive({
        req(krigingvar(), input$surrogatemode)
        if (length(listmodels$categorical) == 0) {
          if (input$surrogatemode == "Expert Mode" || (input$surrogatemode == "User Defined")) {
            if (krigingvar() == "Lasso Vars") {
              listtrend <- c("Constant", "Linear")
            }
            if (krigingvar() == "Acosso1 Vars") {
              listtrend <- c("Constant", "Acosso 1")
            }
            if (krigingvar() == "All Vars") {
              listtrend <- c("Constant", "Linear")
              if (1 %in% listmodels$trainedModels) {
                listtrend <- c(listtrend, "Lasso")
              }
              if (2 %in% listmodels$trainedModels) {
                listtrend <- c(listtrend, "Acosso 1")
              }
            }
          } else {
            listtrend <- c("Constant", "Linear")
          }
        } else {
          if (input$surrogatemode == "Expert Mode" || (input$surrogatemode == "User Defined")) {
            if (krigingvar() == "Lasso Vars") {
              listtrend <- c("Constant", "Linear")
            }
            if (krigingvar() == "Acosso1 Vars") {
              listtrend <- c("Constant", "Acosso 1")
            }
            if (krigingvar() == "All Vars") {
              listtrend <- c("Constant")
              if (1 %in% listmodels$trainedModels) {
                listtrend <- c(listtrend, "Lasso")
              }
              if (2 %in% listmodels$trainedModels) {
                listtrend <- c(listtrend, "Acosso 1")
              }
            }
          } else {
            listtrend <- c("Constant")
          }
        }
        return(listtrend)
      })

      krigingtrend <- callModule(
        dynamicSelect.server, "chooseKrigingtrend",
        label = "Choose Trend", choicesKrigingtrend
      )

      idkrigingselected <- reactive({
        req(krigingtrend(), krigingvar())
        switch(paste(sub(" ", "_", krigingvar()), sub(" ", "_", krigingtrend()), sep = "_"),
          All_Vars_Constant = 5,
          All_Vars_Linear = 6,
          All_Vars_Lasso = 7,
          All_Vars_Acosso_1 = 8,
          Lasso_Vars_Constant = 9,
          Lasso_Vars_Linear = 10,
          Acosso1_Vars_Constant = 11,
          Acosso1_Vars_Acosso_1 = 12
        )
      })

################################################################################
################################# USER DEFINED #################################
#################################################################################

      SurrogateDescrs <- function() {
        return(SurrogateEnv$SurrogateDescrs)
      }

      SurrogateDispNames <- function() {
        choices <- unlist(lapply(seq_len(length(SurrogateDescrs())), function(i) {
          SurrogateDescrs()[[i]]$dispname
        }))
        return(choices)
      }

      choicesUdefinedSurrogate <- reactive({
        req(length(SurrogateDescrs()) != 0, surrogatevar(), input$surrogatemode)
        choices <- SurrogateDispNames()
        return(choices)
      })

      UdefinedSurrogate <- callModule(
        dynamicSelect.server, "chooseSurrogateModel",
        label = "Choose User-defined Surrogate", choicesUdefinedSurrogate
      )

      surrogateDescrsLength <- reactive({
        return(length(SurrogateDescrs()))
      })

      observeEvent(surrogateDescrsLength(), {
        if (surrogateDescrsLength() != 0) {
          updateSelectInput(
            session,
            inputId = "surrogatemode",
            choices = list("Normal Mode", "Expert Mode", "User Defined")
          )
          shinyjs::html("surrogatemodeLabel", "Choose Normal/Expert Mode/User Defined")
        }
      })

      idsurrogateselected <- reactive({
        req(surrogatevar(), UdefinedSurrogate())

        l <- list(surrogates=SurrogateDispNames(), vars=c("All Vars", "Lasso Vars", "Acosso1 Vars"))
        result.df <- rev(expand.grid(rev(l)))
        list_alternatives <- paste(sub(" ", "_", result.df[,1]), sub(" ", "_", result.df[,2]), sep="_")
     
        matching_id <- match(paste(sub(" ", "_", UdefinedSurrogate()), sub(" ", "_", surrogatevar()), sep = "_"),
        list_alternatives)

        return(matching_id+12)
      })

      observeEvent(UdefinedSurrogate(), {
        req(length(SurrogateDescrs()) != 0)
        optimIndex <- which(unlist(lapply(seq_len(length(SurrogateDescrs())), function(i) {
          SurrogateDescrs()[[i]]$dispname
        })) == UdefinedSurrogate())
        req(length(SurrogateDescrs()) >= optimIndex)
        SurrogateEnv$SurrogateFileName <- SurrogateDescrs()[[optimIndex]]$Filename
      })

###############################################################################

      choicesQ2validation <- reactive({
        if (testfileloaded$bool) {
          return(c("Leave-One-Out", "Separate Test File"))
        } else {
          return("Leave-One-Out")
        }
      })
      Q2validationselect <- callModule(
        dynamicSelect.server, "Q2mode",
        label = "Choose Validation", choicesQ2validation
      )
  
      # Dynamic UI for normal mode
      output$dynui_normalmode <- renderUI({
        uilist <- list()

        kigring_panel <- column(2,
          align = "center",
          wellPanel(
            actionButton(ns("krigingsettings"), "Train Kriging Model", class = "btn-primary"),
            h6("This model is the most flexible one and can model complex input-outputs relationships. But if the number of inputs is larger than 15-20, its calibration may be hard.")
          )
        )

        udefined_panel <- column(2,
          align = "center",
          wellPanel(
            actionButton(ns("udefinedsettings"), "Train Surrogate Model", class = "btn-primary"),
            h6("This option allows the User to add User-defined model with a R/Python Interface.")
          )
        )

        if (any(DOE$Yinfos$type == "numeric")) {
          if ((1 %in% listmodels$trainedModels) || (input$surrogatemode == "Expert Mode") || (input$surrogatemode == "User Defined")) {
            uilist[[1]] <- column(2,
              align = "center",
              wellPanel(
                actionButton(ns("acosso1settings"), "Train Acosso 1", class = "btn-primary"),
                h6("This is the first generalization of the Lasso model: it is additive but nonlinear. If the number of inputs is large you should try it before training a kriging model.
                                   Not available for classification.")
              )
            )
          } else {
            uilist[[1]] <- column(2)
          }

          if ((2 %in% listmodels$trainedModels || (input$surrogatemode == "Expert Mode") || (input$surrogatemode == "User Defined"))) {
            uilist[[2]] <- column(2,
              align = "center",
              wellPanel(
                actionButton(ns("acosso2settings"), "Train Acosso 2", class = "btn-primary"),
                h6("This model generalizes Acosso 1st order by adding nonlinear interactions between the variables. If the number of inputs is large it may take a long time to compute.
                                   Not available for classification.")
              )
            )
            uilist[[3]] <- kigring_panel
          } else {
            uilist[[2]] <- column(2)
          }
          if ((input$surrogatemode == "User Defined")) {
            uilist[[4]] <- udefined_panel
          }
        } else {
          # Acosso is not displayed when all outputs are categorical
          if ((1 %in% listmodels$trainedModels) || (input$surrogatemode == "Expert Mode") || (input$surrogatemode == "User Defined")) {
            uilist[[1]] <- kigring_panel
            if ((input$surrogatemode == "User Defined")) {
              uilist[[2]] <- udefined_panel
            }
          }
        }

        return(uilist)
      })

      ###################################################################################################################
      ##################################### Surrogate Models ############################################################
      ###################################################################################################################

      # 1 - Lasso
      observeEvent(input$buildlasso, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, (length(intersect(which(askedtrained$id[1, ]), which(!whatwastrained$id[1, ]))) > 0 |
          (input$retrain & length(which(askedtrained$id[1, ])) > 0)))

        latest_model$name <- "lasso"
        # Check first if some constant outputs need to be trained
        idY <- DOE$Yinfos$const.ids
        idconst.totrain <- which(!whatwastrained$id[1, idY])
        if (length(idconst.totrain) > 0) {
          idY <- idY[idconst.totrain]
          dimY <- length(idY)
          callback <- function(i) {
            print(paste("Constant Output", i, "/", dimY))
          }
          lassomodels <- compute.lasso.model(DOE, idY,
            categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
          )
          indModels <- !sapply(lassomodels, is.null)
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          whatwastrained$id[1, idY] <- TRUE
        }

        if (input$retrain) {
          idY <- which(askedtrained$id[1, ])
        } else {
          idY <- intersect(which(askedtrained$id[1, ]), which(!whatwastrained$id[1, ]))
        }
        latest_model$trained_outputs <- idY
        dimY <- length(idY)

        if (dimY > 0) {
          callback <- function(i) {
            incProgress(1 / dimY, detail = paste("Output", i, "/", dimY))
          }
          withProgress(message = "Building Metamodels...", value = 0, {
            lassomodels <- compute.lasso.model(DOE, idY,
              categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
            )
          })
          indModels <- !sapply(lassomodels, is.null)
          latest_model$convergence <- indModels
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Do the same with Q2 test if a file has been loaded
          if (testfileloaded$bool) {
            dimX <- DOE$nX
            df <- data.Q2test$XY
            Xtest <- df[, 1:dimX]
            # Compute Q2 test
            for (i in seq_len(dimY)) {
              predtest <- predict.metamodel(lassomodels[[i]], Xtest, computesd = FALSE)
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + i)]), ncol = 1)

              lassomodels[[i]]$ytest <- ytest
              lassomodels[[i]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              lassomodels[[i]]$Q2test <- Q2test
              listmodels$tableQ2test[1, 3 + idY[i]] <- Q2test
              if (DOE$Yinfos$type[i] == "categorical") {
                lassomodels[[i]]$maxprobapredtest <- apply(predict.metamodel(lassomodels[[i]], Xtest,
                  computesd = FALSE, computeProba = TRUE
                )$mean, 1, max)
              }
            }
            # Update best and selected model with test set
            listmodels <- updatebestselected(listmodels, type = "Test")
          }

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          listmodels$trainedModels <- c(listmodels$trainedModels, 1)
          whatwastrained$id[1, idY] <- askedtrained$id[1, idY]
        }
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "buildsurrogate-buildlasso"
      })
      # 2 - Acosso 1
      observeEvent(input$buildacosso1, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, (length(intersect(which(askedtrained$id[2, ]), which(!whatwastrained$id[2, ]))) > 0 |
          (input$retrain & length(which(askedtrained$id[2, ])) > 0)))
        toggleModal(session, "modalAcosso1", toggle = "close")

        showModal(modalDialog(
          HTML(paste(
            "Depending on the number of simulations and outputs the computation may take a while.",
            "If you close this window it is not advised to navigate in other panels until the computation is done.",
            "This window will close automatically when the task is finished.",
            sep = "<br/>"
          )),
          title = "Warning",
          size = "l"
        ))

        latest_model$name <- "acosso 1"

        # Check first if some constant outputs need to be trained
        idY <- DOE$Yinfos$const.ids
        idconst.totrain <- which(!whatwastrained$id[1, idY])
        if (length(idconst.totrain) > 0) {
          idY <- idY[idconst.totrain]
          dimY <- length(idY)
          callback <- function(i) {
            print(paste("Constant Output", i, "/", dimY))
          }
          lassomodels <- compute.lasso.model(DOE, idY,
            categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
          )
          indModels <- !sapply(lassomodels, is.null)
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          whatwastrained$id[1, idY] <- TRUE
        }

        if (input$retrain) {
          idY <- which(askedtrained$id[2, ])
        } else {
          idY <- intersect(which(askedtrained$id[2, ]), which(!whatwastrained$id[2, ]))
        }
        # acosso is only fit for numerical outputs
        idY <- intersect(idY, which(DOE$Yinfos$type == "numeric"))
        latest_model$trained_outputs <- idY
        dimY <- length(idY)

        if (dimY > 0) {
          callback <- function(i) {
            incProgress(1 / dimY, detail = paste("Output", i, "/", dimY))
          }

          withProgress(message = "Building Metamodels...", value = 0, {
            acossomodels <- compute.acosso.model(DOE, idY,
              order = 1,
              categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[2]], callback = callback
            )
          })
          indModels <- !sapply(acossomodels, is.null)
          latest_model$convergence <- indModels
          idY <- idY[indModels]
          acossomodels <- acossomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[2, 3 + idY[i]] <- acossomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Do the same with Q2 test if a file has been loaded
          if (testfileloaded$bool) {
            dimX <- DOE$nX
            df <- data.Q2test$XY
            Xtest <- df[, 1:dimX]
            # Compute Q2 test
            for (i in seq_len(dimY)) {
              predtest <- predict.metamodel(acossomodels[[i]], Xtest, computesd = FALSE)
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + i)]), ncol = 1)

              acossomodels[[i]]$ytest <- ytest
              acossomodels[[i]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              acossomodels[[i]]$Q2test <- Q2test
              listmodels$tableQ2test[2, 3 + idY[i]] <- Q2test
            }
            # Update best and selected model with test set
            listmodels <- updatebestselected(listmodels, type = "Test")
          }

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[2]][[idY[i]]] <- acossomodels[[i]]
          }
          listmodels$trainedModels <- c(listmodels$trainedModels, 2)
          whatwastrained$id[2, idY] <- askedtrained$id[2, idY]
        }
        removeModal()
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "buildsurrogate-buildacosso1"
      })
      # 3 - Acosso 2 with all vars
      observeEvent(input$buildacosso2, {
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0,
          (length(intersect(which(askedtrained$id[3, ]), which(!whatwastrained$id[3, ]))) > 0 |
            (input$retrain & length(which(askedtrained$id[3, ])) > 0)), acosso2var() == "All Vars"
        )
        toggleModal(session, "modalAcosso2", toggle = "close")

        showModal(modalDialog(
          HTML(paste(
            "Depending on the number of simulations and outputs the computation may take a while.",
            "If you close this window it is not advised to navigate in other panels until the computation is done.",
            "This window will close automatically when the task is finished.",
            sep = "<br/>"
          )),
          title = "Warning",
          size = "l"
        ))

        latest_model$name <- "acosso 2"

        # Check first if some constant outputs need to be trained
        idY <- DOE$Yinfos$const.ids
        idconst.totrain <- which(!whatwastrained$id[1, idY])
        if (length(idconst.totrain) > 0) {
          idY <- idY[idconst.totrain]
          dimY <- length(idY)
          callback <- function(i) {
            print(paste("Constant Output", i, "/", dimY))
          }
          lassomodels <- compute.lasso.model(DOE, idY,
            categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
          )
          indModels <- !sapply(lassomodels, is.null)
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          whatwastrained$id[1, idY] <- TRUE
        }

        if (input$retrain) {
          idY <- which(askedtrained$id[3, ])
        } else {
          idY <- intersect(which(askedtrained$id[3, ]), which(!whatwastrained$id[3, ]))
        }
        # acosso is only fit for numerical outputs
        idY <- intersect(idY, which(DOE$Yinfos$type == "numeric"))
        latest_model$trained_outputs <- idY
        dimY <- length(idY)

        if (dimY > 0) {
          callback <- function(i) {
            incProgress(1 / dimY, detail = paste("Output", i, "/", dimY))
          }
          withProgress(message = "Building Metamodels...", value = 0, {
            acossomodels <- compute.acosso.model(DOE, idY,
              order = 2,
              categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[3]], callback = callback
            )
          })
          indModels <- !sapply(acossomodels, is.null)
          latest_model$convergence <- indModels
          idY <- idY[indModels]
          acossomodels <- acossomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[3, 3 + idY[i]] <- acossomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Do the same with Q2 test if a file has been loaded
          if (testfileloaded$bool) {
            dimX <- DOE$nX
            df <- data.Q2test$XY
            Xtest <- df[, 1:dimX]
            # Compute Q2 test
            for (i in seq_len(dimY)) {
              predtest <- predict.metamodel(acossomodels[[i]], Xtest, computesd = FALSE)
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + i)]), ncol = 1)

              acossomodels[[i]]$ytest <- ytest
              acossomodels[[i]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              acossomodels[[i]]$Q2test <- Q2test
              listmodels$tableQ2test[3, 3 + idY[i]] <- Q2test
            }
            # Update best and selected model with test set
            listmodels <- updatebestselected(listmodels, type = "Test")
          }

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[3]][[idY[i]]] <- acossomodels[[i]]
          }
          listmodels$trainedModels <- c(listmodels$trainedModels, 3)
          whatwastrained$id[3, idY] <- askedtrained$id[3, idY]
        }
        removeModal()
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "buildsurrogate-buildacosso2-AllVars"
      })
      # 4 - Acosso 2 with vars selected with Acosso 1
      observeEvent(input$buildacosso2, {
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0,
          (length(intersect(which(askedtrained$id[4, ]), which(!whatwastrained$id[4, ]))) > 0 |
            (input$retrain & length(which(askedtrained$id[4, ])) > 0)), acosso2var() == "Acosso 1 Vars"
        )
        toggleModal(session, "modalAcosso2", toggle = "close")

        showModal(modalDialog(
          HTML(paste(
            "Depending on the number of simulations and outputs the computation may take a while.",
            "If you close this window it is not advised to navigate in other panels until the computation is done.",
            "This window will close automatically when the task is finished.",
            sep = "<br/>"
          )),
          title = "Warning",
          size = "l"
        ))

        latest_model$name <- "acosso 2"

        # Check first if some constant outputs need to be trained
        idY <- DOE$Yinfos$const.ids
        idconst.totrain <- which(!whatwastrained$id[1, idY])
        if (length(idconst.totrain) > 0) {
          idY <- idY[idconst.totrain]
          dimY <- length(idY)
          callback <- function(i) {
            print(paste("Constant Output", i, "/", dimY))
          }
          lassomodels <- compute.lasso.model(DOE, idY,
            categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
          )
          indModels <- !sapply(lassomodels, is.null)
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          whatwastrained$id[1, idY] <- TRUE
        }

        if (input$retrain) {
          idY <- which(askedtrained$id[4, ])
        } else {
          idY <- intersect(which(askedtrained$id[4, ]), which(!whatwastrained$id[4, ]))
        }
        # acosso is only fit for numerical outputs
        idY <- intersect(idY, which(DOE$Yinfos$type == "numeric"))
        # fit only for outputs with a trained acosso 1
        idY <- intersect(idY, which(whatwastrained$id[2, ]))

        latest_model$trained_outputs <- idY

        dimY <- length(idY)
        if (dimY > 0) {
          vars <- list()
          for (i in 1:dimY) {
            vars[[i]] <- listmodels$models[[2]][[idY[i]]]$selvar
          }
          callback <- function(i) {
            incProgress(1 / dimY, detail = paste("Output", i, "/", dimY))
          }
          withProgress(message = "Building Metamodels...", value = 0, {
            acossomodels <- compute.acosso.model(DOE, idY,
              order = 2, vars = vars,
              categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[4]], callback = callback
            )
          })
          indModels <- !sapply(acossomodels, is.null)
          latest_model$convergence <- indModels
          idY <- idY[indModels]
          acossomodels <- acossomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[4, 3 + idY[i]] <- acossomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Do the same with Q2 test if a file has been loaded
          if (testfileloaded$bool) {
            dimX <- DOE$nX
            df <- data.Q2test$XY
            Xtest <- df[, 1:dimX]
            # Compute Q2 test
            for (i in seq_len(dimY)) {
              predtest <- predict.metamodel(acossomodels[[i]], Xtest, computesd = FALSE)
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + i)]), ncol = 1)

              acossomodels[[i]]$ytest <- ytest
              acossomodels[[i]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              acossomodels[[i]]$Q2test <- Q2test
              listmodels$tableQ2test[4, 3 + idY[i]] <- Q2test
            }
            # Update best and selected model with test set
            listmodels <- updatebestselected(listmodels, type = "Test")
          }

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[4]][[idY[i]]] <- acossomodels[[i]]
          }
          listmodels$trainedModels <- c(listmodels$trainedModels, 4)
          whatwastrained$id[4, idY] <- askedtrained$id[4, idY]
        }
        removeModal()
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "buildsurrogate-buildacosso2-Acosso1Vars"
      })
      # 5/6/7/8/9/10/11/12 - Kriging
      observeEvent(input$buildkriging, {
        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0,
          (length(intersect(which(askedtrained$id[idkrigingselected(), ]), which(!whatwastrained$id[idkrigingselected(), ]))) > 0 |
            (input$retrain & length(which(askedtrained$id[idkrigingselected(), ])) > 0))
        )
        toggleModal(session, "modalKriging", toggle = "close")

        showModal(modalDialog(
          HTML(paste(
            "Depending on the number of simulations and outputs the computation may take a while.",
            "If you close this window it is not advised to navigate in other panels until the computation is done.",
            "This window will close automatically when the task is finished.",
            sep = "<br/>"
          )),
          title = "Warning",
          size = "l"
        ))

        latest_model$name <- "kriging"

        # Check first if some constant outputs need to be trained
        idY <- DOE$Yinfos$const.ids
        idconst.totrain <- which(!whatwastrained$id[1, idY])
        if (length(idconst.totrain) > 0) {
          idY <- idY[idconst.totrain]
          dimY <- length(idY)
          callback <- function(i) {
            print(paste("Constant Output", i, "/", dimY))
          }
          lassomodels <- compute.lasso.model(DOE, idY,
            categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
          )
          indModels <- !sapply(lassomodels, is.null)
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          whatwastrained$id[1, idY] <- TRUE
        }

        if (input$retrain) {
          idY <- which(askedtrained$id[idkrigingselected(), ])
        } else {
          idY <- intersect(which(askedtrained$id[idkrigingselected(), ]), which(!whatwastrained$id[idkrigingselected(), ]))
        }
        # fit only for outputs with required Lasso/acosso models
        if (idkrigingselected() %in% c(7, 9, 10)) {
          idY <- intersect(idY, which(whatwastrained$id[1, ]))
        }
        if (idkrigingselected() %in% c(8, 11, 12)) {
          idY <- intersect(idY, which(whatwastrained$id[2, ]))
        }
        # non constant trend only for numeric outputs
        if (!idkrigingselected() %in% c(5, 9, 11)) {
          idY <- intersect(idY, which(DOE$Yinfos$type == "numeric"))
        }
        latest_model$trained_outputs <- idY
        dimY <- length(idY)

        if (dimY > 0) {
          dimX <- DOE$nX
          kv <- krigingvar()
          if (kv == "All Vars") {
            # vars <- rep(list(1:dimX),dimY)
            vars <- rep(list(NULL), dimY)
          } else {
            if (kv == "Lasso Vars") {
              idv <- 1
            }
            if (kv == "Acosso1 Vars") {
              idv <- 2
            }
            vars <- list()
            for (i in 1:dimY) {
              vars[[i]] <- listmodels$models[[idv]][[idY[i]]]$selvar
            }
          }
          kt <- krigingtrend()
          if (kt == "Constant" || kt == "Linear") {
            trendobj <- rep(list(NULL), dimY)
          } else {
            if (kt == "Lasso") {
              idt <- 1
            }
            if (kt == "Acosso 1") {
              idt <- 2
            }
            trendobj <- list()
            for (i in 1:dimY) {
              trendobj[[i]] <- listmodels$models[[idt]][[idY[i]]]
            }
          }
          idk <- isolate(idkrigingselected())
          callback <- function(i) {
            incProgress(1 / dimY, detail = paste("Output", i, "/", dimY))
          }
          withProgress(message = "Building Metamodels...", value = 0, {
            krigingmodels <- compute.kriging.model(DOE, idY,
              categorical = listmodels$categorical, levels = listmodels$levels.models,
              models = listmodels$models[[idk]],
              vars = vars, trend = sub(" ", "", krigingtrend()), trendobj = trendobj,
              interpolate = input$interpolate, multi = input$krig.multistart, callback
            )
          })
          indModels <- !sapply(krigingmodels, is.null)
          latest_model$convergence <- indModels
          idY <- idY[indModels]
          krigingmodels <- krigingmodels[indModels]
          dimY <- length(idY)

          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[idk, 3 + idY[i]] <- krigingmodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Do the same with Q2 test if a file has been loaded
          if (testfileloaded$bool) {
            dimX <- DOE$nX
            df <- data.Q2test$XY
            Xtest <- df[, 1:dimX]
            # Compute Q2 test
            for (i in seq_len(dimY)) {
              predtest <- predict.metamodel(krigingmodels[[i]], Xtest, computesd = FALSE)
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + i)]), ncol = 1)

              krigingmodels[[i]]$ytest <- ytest
              krigingmodels[[i]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              krigingmodels[[i]]$Q2test <- Q2test
              listmodels$tableQ2test[idk, 3 + idY[i]] <- Q2test
              if (DOE$Yinfos$type[i] == "categorical") {
                krigingmodels[[i]]$maxprobapredtest <- apply(predict.metamodel(krigingmodels[[i]], Xtest,
                  computesd = FALSE, computeProba = TRUE
                )$mean, 1, max)
              }
            }
            # Update best and selected model with test set
            listmodels <- updatebestselected(listmodels, type = "Test")
          }

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[idk]][[idY[i]]] <- krigingmodels[[i]]
          }
          listmodels$trainedModels <- c(listmodels$trainedModels, idk)
          whatwastrained$id[idk, idY] <- askedtrained$id[idk, idY]
          persistence$autoSavingCount <- persistence$autoSavingCount + 1
          persistence$autoSavingCaller <- paste("buildsurrogate-buildkriging", kv, kt, sep = "-")
        }
        removeModal()
      })

      # 13- ....  - User defined surrogate

      observeEvent(input$builduserdefined, {

        req(
          DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, 
          (length(intersect(which(askedtrained$id[idsurrogateselected(), ]), which(!whatwastrained$id[idsurrogateselected(), ]))) > 0 |
            (input$retrain & length(which(askedtrained$id[idsurrogateselected(), ])) > 0))
        )
        
        toggleModal(session, "modalUserDefined", toggle = "close")
       
        showModal(modalDialog(
          HTML(paste(
            "Depending on the number of simulations and outputs the computation may take a while.",
            "If you close this window it is not advised to navigate in other panels until the computation is done.",
            "This window will close automatically when the task is finished.",
            sep = "<br/>"
          )),
          title = "Warning",
          size = "l"
        ))

        SurrogateIndex <- which(unlist(lapply(seq_len(length(SurrogateDescrs())), function(i) {
          SurrogateDescrs()[[i]]$Filename
        })) == SurrogateEnv$SurrogateFileName)

          SurrogateDescrs <- SurrogateDescrs()[[SurrogateIndex]]
          SurrogateBaseName <- SurrogateDescrs$name
          SurrogateBuildName <- paste0(SurrogateBaseName, ".build")
          SurrogatePredictName <- paste0(SurrogateBaseName, ".predict")
          SurrogateUpdateName <- paste0(SurrogateBaseName, ".update") 
          SurrogateModelName <- paste0(SurrogateBaseName, ".model") 
          args <- list(
            SurrogateModelName = SurrogateModelName,
            SurrogateFileName = SurrogateDescrs$Filename,
            SurrogateBuildName = SurrogateBuildName,
            SurrogatePredictName = SurrogatePredictName,
            SurrogateUpdateName = SurrogateUpdateName
          )

        latest_model$name <- SurrogateBaseName 
         # Check first if some constant outputs need to be trained
        idY <- DOE$Yinfos$const.ids
        idconst.totrain <- which(!whatwastrained$id[1, idY])
        if (length(idconst.totrain) > 0) {
          idY <- idY[idconst.totrain]
          dimY <- length(idY)
          callback <- function(i) {
            print(paste("Constant Output", i, "/", dimY))
          }
          lassomodels <- compute.lasso.model(DOE, idY,
            categorical = listmodels$categorical, levels = listmodels$levels.models, models = listmodels$models[[1]], callback
          )
          indModels <- !sapply(lassomodels, is.null)
          idY <- idY[indModels]
          lassomodels <- lassomodels[indModels]
          dimY <- length(idY)
          
          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[1, 3 + idY[i]] <- lassomodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[1]][[idY[i]]] <- lassomodels[[i]]
          }
          whatwastrained$id[1, idY] <- TRUE
        }

        if (input$retrain) {
          idY <- which(askedtrained$id[idsurrogateselected(), ])
        } else {
          idY <- intersect(which(askedtrained$id[idsurrogateselected(), ]), which(!whatwastrained$id[idsurrogateselected(), ]))
        }
  
        # check if model fit only numerical outputs
        if(isFALSE(SurrogateDescrs$tags$classification) ||is.null(SurrogateDescrs$tags$classification)){
          idY <- intersect(idY, which(DOE$Yinfos$type == "numeric"))
          latest_model$trained_outputs <- idY
          dimY <- length(idY)
        }
          # check if model fit only categorical outputs
        if(isFALSE(SurrogateDescrs$tags$regression)){
          idY <- intersect(idY, which(DOE$Yinfos$type == "categorical"))
          latest_model$trained_outputs <- idY
          dimY <- length(idY)
        }
   
        # fit only for outputs with required Lasso/acosso models
        if (surrogatevar() == "Lasso Vars") {
          idY <- intersect(idY, which(whatwastrained$id[1, ]))
        }
        if (surrogatevar() == "Acosso1 Vars") {
          idY <- intersect(idY, which(whatwastrained$id[2, ]))
        }
        latest_model$trained_outputs <- idY
        dimY <- length(idY)

        if (dimY > 0) {
          dimX <- DOE$nX
          sv <- surrogatevar()
          if (sv == "All Vars") {
            # vars <- rep(list(1:dimX),dimY)
            vars <- rep(list(NULL), dimY)
          } else {
            if (sv == "Lasso Vars") {
              idv <- 1
            }
            if (sv == "Acosso1 Vars") {
              idv <- 2
            }
            vars <- list()
            for (i in 1:dimY) {
              vars[[i]] <- listmodels$models[[idv]][[idY[i]]]$selvar
            }
          }
          
          ids <- isolate(idsurrogateselected())
          callback <- function(i) {
            incProgress(1 / dimY, detail = paste("Output", i, "/", dimY))
          }
          withProgress(message = "Building Metamodels...", value = 0, {
            userdefinedmodels <- compute.surrogate.model(DOE, idY, args=args,
              categorical = listmodels$categorical, vars = vars, levels = listmodels$levels.models, models = listmodels$models[[ids]], callback = callback
            )
          })
          indModels <- !sapply(userdefinedmodels, is.null)
          latest_model$convergence <- indModels
          idY <- idY[indModels]
          userdefinedmodels <- userdefinedmodels[indModels]
          dimY <- length(idY)
          
          # Store LOO results
          for (i in seq_len(dimY)) {
            listmodels$tableQ2loo[ids, 3 + idY[i]] <- userdefinedmodels[[i]]$Q2loo
          }
          # Update best and selected model with LOO
          listmodels <- updatebestselected(listmodels, type = "LOO")

          # Do the same with Q2 test if a file has been loaded
          if (testfileloaded$bool) {
            dimX <- DOE$nX
            df <- data.Q2test$XY
            Xtest <- df[, 1:dimX]
            # Compute Q2 test
            for (i in seq_len(dimY)) {
              predtest <- predict.metamodel(userdefinedmodels[[i]], Xtest, computesd = FALSE)              
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + i)]), ncol = 1)

              userdefinedmodels[[i]]$ytest <- ytest
              userdefinedmodels[[i]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              userdefinedmodels[[i]]$Q2test <- Q2test
              listmodels$tableQ2test[ids, 3 + idY[i]] <- Q2test
              if (DOE$Yinfos$type[i] == "categorical") {
                userdefinedmodels[[i]]$maxprobapredtest <- apply(predict.metamodel(userdefinedmodels[[i]], Xtest,
                  computesd = FALSE
                )$mean, 1, max)
              }
            }
            # Update best and selected model with test set
            listmodels <- updatebestselected(listmodels, type = "Test")
          }          
          # Store metamodels
          for (i in seq_len(dimY)) {
            listmodels$models[[ids]][[idY[i]]] <- userdefinedmodels[[i]]
          }
          listmodels$trainedModels <- c(listmodels$trainedModels, ids)
          whatwastrained$id[ids, idY] <- askedtrained$id[ids, idY]
        }

        removeModal()
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- paste("buildsurrogate-builduserdefined", sv, sep = "-")
      })

###################################################################################################################
###################################################################################################################

      observeEvent(list(latest_model$name, latest_model$convergence, latest_model$trained_outputs), {
        req(latest_model$trained_outputs)

        # If some models did not converge, show an error modal dialog
        if (sum(latest_model$convergence) < length(latest_model$convergence)) {
          showModal(modalDialog(
            HTML(paste("A fitting of ", latest_model$name, " model was tried for ",
              paste(DOE$ynames[latest_model$trained_outputs], collapse = ", "), " and failed for ",
              paste(DOE$ynames[latest_model$trained_outputs[!(latest_model$convergence)]], collapse = ", "),
              ". <br> Therefore, no Q2 is computed for ",
              paste(DOE$ynames[latest_model$trained_outputs[!(latest_model$convergence)]], collapse = ", "), ".",
              sep = ""
            )),
            title = "Model training has failed"
          ))
        }
      })

      header <- reactiveValues(bool = TRUE)
      testfileloaded <- reactiveValues(bool = FALSE)

      observe({
        req(input$fileQ2test$datapath)
        xynames <- unlist(strsplit(readLines(input$fileQ2test$datapath, n = 1), input$separator))
        xynames <- gsub(paste0("[", input$decimal, "]"), ".", xynames)
        header$bool <- suppressWarnings(all(is.na(as.numeric(xynames))))
        testfileloaded$bool <- TRUE
      })

      data.Q2test <- reactiveValues(XY = NULL)
      error.msg <- reactiveValues(file = NULL)
      # start afresh when training data changed
      observeEvent(list(DOE$X, DOE$Y, DOE$xnames, DOE$ynames, DOE$Yinfos), {
        data.Q2test$XY <- NULL
      })
      observeEvent(input$fileQ2test$datapath, {
        validation.header <- check.header(DOE, input$fileQ2test$datapath, input$separator, input$decimal, DOE$nY)
        if (validation.header$valid) {
          newData <- get.new.data.from.file(DOE, input$fileQ2test$datapath, input$separator, input$decimal, DOE$nY)
          validation.newData <- check.new.data(DOE$nX, DOE$Xinfos, newData, DOE$nY)
          if (validation.newData$valid) {
            data.Q2test$XY <- newData
            error.msg$file <- NULL
          } else {
            error.msg$file <- validation.newData$error.msg
          }
        } else {
          error.msg$file <- validation.header$error.msg
        }
      })

      output$error.file <- renderUI({
        req(error.msg$file)
        list(
          h4(strong("Error !")),
          HTML(paste(paste(error.msg$file, collapse = "<br/>"), "<br/> <br/>"))
        )
      })

      output$Q2testfilecontents <- DT::renderDataTable({
        req(data.Q2test$XY)
        dimd <- ncol(data.Q2test$XY)
        DT::datatable(
          data.Q2test$XY,
          extensions = c("FixedColumns", "Scroller", "Buttons"), filter = "top",
          options = list(
            dom = "Brtip",
            buttons = list(list(extend = "colvis", columns = 1:dimd)),
            scrollX = TRUE, scrollY = 400, scroller = TRUE, fixedColumns = TRUE
          )
        )
      })

      output$tableQ2 <- DT::renderDataTable({
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0, input$surrogatemode, Q2validationselect(), DOE$Yinfos)
        if (Q2validationselect() == "Leave-One-Out") {
          df1 <- listmodels$tableQ2loo
          df2 <- as.data.frame(matrix(listmodels$bestQ2loo$Q2, nrow = 1))
        } else {
          df1 <- listmodels$tableQ2test
          df2 <- as.data.frame(matrix(listmodels$bestQ2test$Q2, nrow = 1))
        }
        colnames(df2) <- DOE$ynamesvisu
        df3 <- data.frame(Type = "BestQ2", Var = NA, Trend = NA)
        df <- rbind(cbind(df3, df2), df1)
        dimd <- ncol(df)
        for (i in 1:DOE$nY) {
          df[, 3 + i] <- signif(df[, 3 + i], 4)
        }
        if (input$surrogatemode == "Normal Mode") {
          if (length(listmodels$categorical) == 0) {
            idQ2display <- 1:7
          } else {
            idQ2display <- 1:6
          }
        } 
        else if (input$surrogatemode == "Expert Mode") {
          if (length(listmodels$categorical) == 0) {
            idQ2display <- 1:13
          } else {
            idQ2display <- c(1:6, 8:13)
          }
        }
        else {
          if (length(listmodels$categorical) == 0) {
            idQ2display <- 1:(SurrogatesNumber()+1)
          } else {
            idQ2display <- c(1:6, 8:(SurrogatesNumber()+1))
          }
        }
        hidecol <- setdiff(1:DOE$nY, DOE$Yinfos$surrogate.ids)
        DT::datatable(df[idQ2display, ],
          rownames = FALSE, extensions = "FixedColumns", selection = list(mode = "single", target = "cell"), escape = FALSE,
          options = list(
            dom = "t", columnDefs = list(
              list(className = "dt-left", targets = "_all"),
              list(visible = FALSE, targets = 2 + hidecol)
            ),
            ordering = FALSE, pageLength = (length(BUILTIN_SURROGATE_NAMES) + 3 * length(SurrogateEnv$AllSurrogateDescrs) + 1), scrollX = TRUE, fixedColumns = list(leftColumns = 3)
          )
        ) %>%
          formatStyle(names(df), backgroundColor = styleInterval(c(0.5, 0.7, 0.9), c("red", "yellow", "yellowgreen", "lightgreen"))) %>%
          formatStyle(
            "Type",
            target = "row",
            fontWeight = styleEqual("BestQ2", "bold")
          )
      })

      observeEvent(input$computeQ2test, {
        req(!is.null(listmodels$trainedModels), data.Q2test$XY)
        dimY <- DOE$nY
        dimX <- DOE$nX
        df <- data.Q2test$XY
        Xtest <- df[, 1:dimX]
        withProgress(message = "Computing Q2 on test file...", value = 0, {
          for (j in 1:dimY) {
            for (idtrained in which(whatwastrained$id[, j])) {
              surrogatemodel <- listmodels$models[[idtrained]][[j]]
              predtest <- predict.metamodel(surrogatemodel, Xtest, computesd = FALSE)
              ypredtest <- predtest$mean
              idrowok <- predtest$idrowok
              ytest <- matrix(as.matrix(df[idrowok, (dimX + j)]), ncol = 1)

              listmodels$models[[idtrained]][[j]]$ytest <- ytest
              listmodels$models[[idtrained]][[j]]$ypredtest <- ypredtest
              Q2test <- computeQ2test(ypredtest, ytest)
              listmodels$models[[idtrained]][[j]]$Q2test <- Q2test
              listmodels$tableQ2test[idtrained, 3 + j] <- Q2test
              if (DOE$Yinfos$type[j] == "categorical") {
                listmodels$models[[idtrained]][[j]]$maxprobapredtest <- apply(predict.metamodel(surrogatemodel, Xtest,
                  computesd = FALSE, computeProba = TRUE
                )$mean, 1, max)
              }
            }
            incProgress(1 / dimY, detail = paste("Output", j, "/", dimY))
          }
        })
        listmodels <- updatebestselected(listmodels, type = "Test")
        persistence$autoSavingCount <- persistence$autoSavingCount + 1
        persistence$autoSavingCaller <- "buildsurrogate-computeQ2test"
        toggleModal(session, "modalQ2testfile", toggle = "close")
      })

      observeEvent(input$acosso1settings, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0)
        toggleModal(session, "modalAcosso1", toggle = "open")
      })
      observeEvent(input$acosso2settings, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0)
        toggleModal(session, "modalAcosso2", toggle = "open")
      })
      observeEvent(input$krigingsettings, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0)
        toggleModal(session, "modalKriging", toggle = "open")
      })
      observeEvent(input$udefinedsettings, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0)
        toggleModal(session, "modalUserDefined", toggle = "open")
      })

      tableQ2id <- reactiveValues(idsurrogate = NULL, idoutput = NULL)
      observeEvent(input$tableQ2_cell_clicked, { # 'tableQ2' has cell selection enabled by 'target = "cell"' in its 'selection' argument
        if (length(listmodels$categorical) == 0) {
          idQ2display <- 1:(SurrogatesNumber() + 1)
        } else {
          idQ2display <- c(1:6, 8:(SurrogatesNumber() + 1))
        }
        clickedQ2Id <- idQ2display[input$tableQ2_cell_clicked$row]
        req(input$tableQ2_cell_clicked$col > 2, whatwastrained$id[clickedQ2Id - 1, input$tableQ2_cell_clicked$col - 2])
        tableQ2id$idsurrogate <- clickedQ2Id - 1
        tableQ2id$idoutput <- input$tableQ2_cell_clicked$col - 2
        toggleModal(session, "modalQ2", toggle = "open")
      })

      output$ui.Q2visu <- renderUI({
        Ytype <- DOE$Yinfos$type[tableQ2id$idoutput]
        if (Ytype == "numeric") {
          t <- fluidRow(
            pickerInput(ns("chooseQ2visu"),
              label = "Compare Surrogate(s)",
              choices = choicesQ2visu(), selected = choicesQ2visu()[1], multiple = TRUE,
              options = list(
                `actions-box` = TRUE, `selected-text-format` = "count > 3",
                style = "btn-primary", `live-search` = TRUE
              )
            )
          )
        } else {
          t <- fluidRow(
            column(
              4,
              pickerInput(ns("chooseQ2visu"),
                label = "Compare Surrogate(s)",
                choices = choicesQ2visu(), selected = choicesQ2visu()[1], multiple = TRUE,
                options = list(
                  `actions-box` = TRUE, `selected-text-format` = "count > 3",
                  style = "btn-primary", `live-search` = TRUE
                )
              )
            ),
            column(
              4,
              pickerInput(ns("categoricalVisu"),
                label = "Choose Categorical Visualization",
                choices = c("Class Predictions", "Class Probabilities"), selected = "Class Predictions",
                multiple = FALSE, options = list(style = "btn-primary"),
              )
            )
          )
        }
        tagList(
          t,
          plotlyOutput(ns("plotQ2"), height = 500) %>% withSpinner()
        )
      })

      # Choices available in Q2 modal for visualization
      choicesQ2visu <- reactive({
        req(tableQ2id$idsurrogate, tableQ2id$idoutput)
        idvisu <- which(whatwastrained$id[, tableQ2id$idoutput])
        # Re-order the list so that the clicked surrogate appears first
        idvisu <- c(tableQ2id$idsurrogate, setdiff(idvisu, tableQ2id$idsurrogate))
        return(listmodels$names_surrogatemodel[idvisu])
      })

      observeEvent(input$loadQ2testfile, {
        req(DOE$X, DOE$Y, DOE$nX > 0, DOE$nY > 0)
        toggleModal(session, "modalQ2testfile", toggle = "open")
      })

      output$plotQ2 <- renderPlotly({
        req(tableQ2id$idsurrogate, tableQ2id$idoutput, input$chooseQ2visu, Q2validationselect(), cancelOutput = TRUE)
        names_surrogatemodel <- listmodels$names_surrogatemodel
        ids <- which(names_surrogatemodel %in% input$chooseQ2visu)
        nids <- length(ids)
        nameplot <- switch(Q2validationselect(),
          "Leave-One-Out" = "LOO",
          "Test"
        )
        yname <- DOE$ynames[tableQ2id$idoutput]
        Ypred <- NULL
        Ytrue <- NULL
        Ysurrogatename <- NULL
        Ytype <- DOE$Yinfos$type[tableQ2id$idoutput]
        for (i in 1:nids) {
          surrogatemodel <- listmodels$models[[ids[i]]][[tableQ2id$idoutput]]
          ypredtemp <- surrogatemodel[[switch(Q2validationselect(),
            "Leave-One-Out" = "yloo",
            "ypredtest"
          )]]
          if (Ytype == "categorical") {
            if (input$categoricalVisu == "Class Predictions") {
              ypredtemp <- surrogatemodel[[switch(Q2validationselect(),
                "Leave-One-Out" = "yloo",
                "ypredtest"
              )]]
            } else {
              ypredtemp <- surrogatemodel[[switch(Q2validationselect(),
                "Leave-One-Out" = "maxprobaloo",
                "maxprobapredtest"
              )]]
            }
          }
          Ypred <- c(Ypred, ypredtemp)
          ytruetemp <- switch(Q2validationselect(),
            "Leave-One-Out" = DOE$Y[surrogatemodel$idYok, yname],
            surrogatemodel$ytest
          )
          if (Ytype == "categorical") {
            ytruetemp <- as.character(ytruetemp)
          }
          Ytrue <- c(Ytrue, ytruetemp)
          Ysurrogatename <- c(Ysurrogatename, rep(names_surrogatemodel[ids[i]], length(ypredtemp)))
        }
        mm <- min(c(Ypred, Ytrue))
        MM <- max(c(Ypred, Ytrue))
        dm <- data.frame(x = c(mm, MM), y = c(mm, MM))
        colnames(dm) <- c("x", "y")
        if (Ytype == "numeric") {
          df <- data.frame(x = Ytrue, y = Ypred, namesurrogate = Ysurrogatename, numexp = rep(1:length(Ypred), nids))
          plotQ2out <- plot_ly(df,
            x = ~x, y = ~y, split = ~namesurrogate, text = ~ paste("Exp nb:", numexp),
            hoverinfo = "x+y+text", mode = "markers", showlegend = TRUE, type = "scatter"
          ) %>%
            add_trace(x = dm$x, y = dm$y, mode = "lines", name = "Perfect Match", showlegend = FALSE, type = "scatter", inherit = FALSE) %>%
            layout(
              title = paste("Q2", nameplot, "for", DOE$ynamesmenu[tableQ2id$idoutput]),
              xaxis = list(title = "True Value"), yaxis = list(title = "Predicted Value"),
              legend = list(orientation = "h")
            )
        }
        if (Ytype == "categorical") {
          if (input$categoricalVisu == "Class Predictions") {
            df <- data.frame(x = as.factor(Ytrue), y = as.factor(Ypred), namesurrogate = Ysurrogatename, numexp = rep(1:length(Ypred), nids))
            nx <- length(levels(df$x))
            ny <- length(levels(df$y))
            plotQ2out <- plot_ly(df,
              x = ~ jitter(as.numeric(x)), y = ~ jitter(as.numeric(y)), color = ~namesurrogate,
              type = "scatter", mode = "markers", showlegend = TRUE, alpha = 0.5
            ) %>%
              layout(
                xaxis = list(title = "True Class", tickvals = 1:nx, ticktext = levels(df$x)),
                yaxis = list(title = "Predicted Class", tickvals = 1:ny, ticktext = levels(df$y)),
                legend = list(orientation = "h")
              )
          } else {
            df <- data.frame(x = Ytrue, y = Ypred, namesurrogate = Ysurrogatename, numexp = rep(1:length(Ypred), nids))
            plotQ2out <- plot_ly(df,
              x = ~x, y = ~y, split = ~namesurrogate, text = ~ paste("Exp nb:", numexp),
              hoverinfo = "x+y+text", showlegend = TRUE, type = "violin", box = list(visible = T), points = "all", jitter = 0.3
            ) %>%
              layout(
                title = paste("Q2", nameplot, "for", DOE$ynamesmenu[tableQ2id$idoutput]),
                xaxis = list(title = "True Class"), yaxis = list(title = "Predicted Probability"),
                legend = list(orientation = "h")
              )
          }
        }

        plotQ2out
      })

      observeEvent(list(DOE$compositeInfos, listmodels$selected$id), {
        req(
          !all(is.na(listmodels$selected$id)) & any(is.na(listmodels$selected$id)),
          DOE$nY == length(listmodels$selected$id), DOE$compositeInfos
        )

        idLastCreated <- length(listmodels$selected$id)
        cInfoIdx <- sapply(DOE$compositeInfos, function(x) idLastCreated %in% x$id)
        cInfoIdx <- match(TRUE, cInfoIdx)
        currentCompositeInfos <- DOE$compositeInfos[[cInfoIdx]]

        if (!is.null(currentCompositeInfos)) {
          if (currentCompositeInfos$modelMode == "Combine") {
            for (trainedModel in listmodels$trainedModels) {
              idxToCombine <- match(currentCompositeInfos$usedY, DOE$ynames)
              modelsToCombine <- lapply(idxToCombine, function(x) listmodels$models[[trainedModel]][[x]])
              modelsExist <- sapply(modelsToCombine, function(x) !is.null(x))

              if (all(modelsExist)) {
                yy <- DOE$Y[, idLastCreated]
                combinedModel <- combineMetamodels(yy, modelsToCombine, idxToCombine, currentCompositeInfos)
                listmodels$tableQ2loo[trainedModel, 3 + idLastCreated] <- combinedModel$Q2loo
                listmodels <- updatebestselected(listmodels, type = "LOO")
                listmodels$models[[trainedModel]][[idLastCreated]] <- combinedModel
              }
            }
          }
        }
      })

      observeEvent(persistence$updatingStep, {
        if (persistence$updatingStep == "buildsurrogate-surrogateMode") {
          logger$print(paste("Loaded study, updating",  persistence$updatingStep))

          if (!is.null(listmodels$trainedModels)) {
            # Try to update the selected element of the 'surrogatemode' selector
            if (any(listmodels$trainedModels > 12)) {
              updateSelectInput(session, inputId = "surrogatemode", selected = "User Defined")
            }
            else if (any(listmodels$trainedModels > 6)) {
              updateSelectInput(session, inputId = "surrogatemode", selected = "Expert Mode")
            }
            # Update 'whatwastrained'
            whatwastrained$id <- !is.na(listmodels$tableQ2loo[,4:ncol(listmodels$tableQ2loo), drop = F])
            if (!is.null(listmodels$tableQ2test) && !all(is.na(listmodels$tableQ2test[,4:ncol(listmodels$tableQ2test)]))) {
              testfileloaded$bool = TRUE
            }
          }
          progressToNextStep(persistence)
        }
      }, priority = -1) # Reduce priority to execute later than OF updating

      return(listmodels)
    }
  )
}
