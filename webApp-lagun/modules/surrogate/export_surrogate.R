#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)

prepareJSON <- function(selectedSurrogates, listmodels, DOE){

  # For the moment, composite outputs with 'combine' mode are not exportable (detected via empty name)
  exportableOutputIds <- DOE$Yinfos$surrogate.ids[Filter(function(i) {
    nchar(selectedSurrogates[i]) != 0
  }, seq_along(selectedSurrogates))]

  models <- vector(mode="list", length=length(exportableOutputIds))
  
  for(i in seq_along(exportableOutputIds)) {
    model_name <- selectedSurrogates[i]
    model_nb <- switch(model_name, 
                       "Lasso" = 1, 
                       "Kriging with constant trend and all vars" = 5,
                       "Kriging with linear trend and all vars" = 6)
    m <- listmodels$models[[model_nb]][[exportableOutputIds[i]]]
    targetType <- DOE$Yinfos$type[exportableOutputIds[i]]
    
    if(m$type.metamodel == "Lasso"){
      
      if(length(m$levels)>0){
        extra_X <- sum(sapply(m$levels, function(x) length(x)-2))
      }else{
        extra_X <- 0
      }
      
      
      if (targetType=="numeric"){
        
        ic <- as.vector(m$model$coefficients)
        inter <- ic[1]
        coeff <- rep(0, DOE$nX+extra_X)
        coeff[m$selvar] <- ic[2:length(ic)] 
        
        models[[i]] <- list(type = "Lasso",
                            package = "glmnet",
                            package_version = paste(packageVersion("glmnet")), 
                            target_name = DOE$ynames[exportableOutputIds[i]],
                            parameters = list(
                              intercept = inter,
                              coefficients = I(coeff)
                            ),
                            Q2_LOO = m$Q2loo)
        
      }else if (targetType=="categorical"){
        
        ic <- as.vector(m$model@coefficients)
        nb_coef <- length(m$Ylevels)-1
        inter <- ic[1:nb_coef]
        coeff <- matrix(0, ncol=DOE$nX+extra_X, nrow=nb_coef)
        coeff[,m$selvar] <- matrix(ic[nb_coef+1:(nb_coef*length(m$selvar))], 
                                   nrow=nb_coef)
        
        models[[i]] <- list(type = "Lasso",
                            package = "VGAM",
                            package_version = paste(packageVersion("VGAM")),
                            target_name = DOE$ynames[exportableOutputIds[i]],
                            parameters = list(
                              intercept = I(inter),
                              coefficients = I(coeff),
                              classes = I(m$Ylevels)
                            ),
                            Q2_LOO = m$Q2loo)
      }
    }else if(m$type.metamodel == "Kriging"){
      if (targetType=="numeric"){
        
        models[[i]] <- list(type = "Kriging",
                            package = "RobustGaSP",
                            package_version = paste(packageVersion("RobustGaSP")),
                            target_name = DOE$ynames[exportableOutputIds[i]],
                            parameters = list(
                              trend = m$trend,
                              kernel = m$model@kernel_type[1],
                              theta_hat = m$model@theta_hat,
                              sigma2_hat = m$model@sigma2_hat,
                              range_parameters = I(1/m$model@beta_hat),
                              nugget = m$model@nugget
                            ),
                            Q2_LOO = m$Q2loo)
        
      }else if (targetType=="categorical"){
        
        models[[i]] <- list(type = "Kriging",
                            package = "VBMP (Lagun)",
                            package_version = paste(packageVersion("VBMP")), #custom VBMP version number ?
                            target_name = DOE$ynames[exportableOutputIds[i]],
                            parameters = list(
                              kernel = m$model$sKernelType,
                              method = m$model$con$method,
                              theta = I(m$model$THETA[nrow(m$model$THETA),]),
                              y_tild = I(m$model$Y),
                              classes = I(m$Ylevels)
                            ),
                            Q2_LOO = m$Q2loo)
      }
    }
    
  }
  
  feat <- vector(mode="list", length=DOE$nX)
  
  for(j in seq_len(length(feat))){
    feat[[j]] <- list(name = DOE$Xinfos[[j]]$name, 
                      type = DOE$Xinfos[[j]]$type)
    
    if(DOE$Xinfos[[j]]$type == "categorical"){
      feat[[j]] <- c(feat[[j]], list(levels = DOE$Xinfos[[j]]$levels))
    }
    
  }
  
  targ <- vector(mode="list", length=length(exportableOutputIds))
  target_names <- DOE$ynames[exportableOutputIds]
  target_types <- DOE$Yinfos$type[exportableOutputIds]
  
  for(j in seq_along(exportableOutputIds)){
    targ[[j]] <- list(name = target_names[j], 
                      type = target_types[j])
    
    if(target_types[[j]] == "categorical"){
      targ[[j]] <- c(targ[[j]], 
                     list(levels = levels(as.factor(DOE$Y[[target_names[j]]]))))
    }
    
  }
  
  data = list(features = feat,
              targets = targ,
              features_data = as.matrix(DOE$X),
              targets_data = as.matrix(DOE$Y[exportableOutputIds]))
  
  j <- list(
    creation_date = format(Sys.time()),
    r_version = R.Version()["version.string"][[1]],
    OS = Sys.info()["sysname"][[1]],
    models = models,
    data = data
  )
  
  return(j)
}

exportSurrogateUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(8, 
           wellPanel(
             uiOutput(ns("chooseExportSurrogate")),
             style = "background-color: #ffffff")
    ),
    column(4,
           wellPanel(
             downloadButton(ns("exportSurrogate"), label = "Export Surrogate", 
                          class = "btn-primary",
                          width = '100%')
           )
    )
  )
  
}

exportSurrogateServer <- function(id, DOE, listmodels) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      output$chooseExportSurrogate <- renderUI({
        req(!is.null(listmodels$trainedModels), DOE$Yinfos$surrogate.ids)
        ns <- session$ns
        ynames <- DOE$ynames
        ynamesvisu <- DOE$ynamesvisu
        lmodels <- listmodels

        availableModelsForExport <- c(1, 5, 6)

        # Determine which outputs have some trained and exportable surrogate models
        surrogate.ids <- Filter(function(i) {
          idTrainedModels <- which(!is.na(listmodels$tableQ2loo[, 3 + i]))
          return(length(intersect(idTrainedModels, availableModelsForExport)) > 0)
        }, DOE$Yinfos$surrogate.ids)

        # For the moment, composite outputs with 'combine' mode are not exportable
        combineCompositeInfos <- Filter(function(compositeInfo) compositeInfo$modelMode == "Combine", DOE$compositeInfos)
        idCombineComposites <- sapply(combineCompositeInfos, function(x) x$id)
        surrogate.ids <- Filter(function(i) !(i %in% idCombineComposites), surrogate.ids)

        # build gui for each output having exportable models
        lapply(surrogate.ids, function(i){
          idTrainedModels <- which(!is.na(listmodels$tableQ2loo[, 3 + i]))
          exportableModels <- intersect(idTrainedModels, availableModelsForExport)
          bestQ2Id <- which.max(listmodels$tableQ2loo[, 3 + i][exportableModels])
          fluidRow(
            column(2, h4(HTML(ynamesvisu[i]))),
            column(4,
                   selectInput(
                     inputId = ns(paste0('SelSurrogate', i)),
                     label = "Choose Surrogate to export",
                     choices = lmodels$names_surrogatemodel[exportableModels],
                     selected = lmodels$names_surrogatemodel[exportableModels][bestQ2Id]
                   )
            )
          )
        })
      })
      
      
      output$exportSurrogate <- downloadHandler(
        filename = function() {
          paste0("surrogate_export_", 
                format(Sys.time(), "%Y%m%d_%H%M%S"), 
                ".json")
        },
        content = function(file) {
          
          selectedSurrogates <- sapply(DOE$Yinfos$surrogate.ids, function(i){
            selectedSurrogate <- input[[paste0('SelSurrogate', i)]]
            # For the moment, composite outputs with 'combine' mode are not exportable => use empty name
      			ifelse(is.null(selectedSurrogate), "", selectedSurrogate)
          })
          
          json_data <- prepareJSON(selectedSurrogates, listmodels, DOE)
          
          jsonlite::write_json(json_data, auto_unbox = TRUE, pretty = TRUE, file)
        }
      )
    }
  )
}