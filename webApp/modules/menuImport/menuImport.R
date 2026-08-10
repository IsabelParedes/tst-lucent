#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module menuImport
source("modules/menuImport/importDOE/importDOE.R", local = TRUE)
source("modules/menuImport/calibration/defineCalibration.R", local = TRUE)
source("modules/menuImport/exploreDOE/exploreDOE.R", local = TRUE)

menuImport.ui <- function(id) {
  ns <- NS(id)
  navbarMenu(
    "Problem Definition",
    tabPanel("Import DOE", importDOE.ui(id = ns("importDOE")), icon = icon("table"), value=ns("tabimportDOE")),
    tabPanel("Define Calibration", defineCalibration.ui(id = ns("defineCalibration")), icon = icon("table"), value=ns("tabdefineCalib")),
    tabPanel("Preliminary Exploration", exploreDOE.ui(id = ns("exploreDOE")), icon = icon("chart-area"), value=ns("tabprelimExplo")),
    icon = icon("chart-area")
  )
}

menuImport.server <- function(input, output, session, DOEX, Xadd, XaddUQ, XaddSeqOptim, XaddUnconstOptim, XaddConstOptim, persistence, settings, import.clicked, prelimexplo.clicked, window.dimension) {
  OutputimportDOE <- callModule(importDOE.server, "importDOE", DOEX, Xadd, XaddUQ, XaddSeqOptim, XaddUnconstOptim, XaddConstOptim, persistence, settings, import.clicked, window.dimension)
  importDOE <- OutputimportDOE$DOE
  advance.importDOE <- OutputimportDOE$advance.simu
  doeProblemDef <- OutputimportDOE$problemDef
  directOptim <- OutputimportDOE$directOptim
  calibDOE <- callModule(defineCalibration.server, "defineCalibration", importDOE, persistence, settings)
  ML <- callModule(exploreDOE.server, "exploreDOE", filteredDOE, settings, prelimexplo.clicked, advance.importDOE, doeProblemDef, window.dimension)
  filteredDOE <- reactiveValues(idref=NULL,nobs=NULL,nX=NULL,nY=NULL,nYsurrogate=NULL,X=NULL,Xopt=NULL,xnames=NULL,
                                Xinfos=NULL,Y=NULL,ynames=NULL,Yinfos=NULL,XY=NULL, xnamesvisu = NULL, ynamesvisu = NULL, xnamesmenu = NULL, ynamesmenu = NULL,
                                compositeInfos = NULL,
                                discF = NULL, nF = NULL, idF = NULL, Fnames = NULL, Fnamesvisu = NULL,
                                Z = NULL, sigZ = NULL, nZ = NULL, idZ = NULL, idZY = NULL, discZ = NULL,
                                OF = NULL, OFtot = NULL)
  
  observeEvent(list(calibDOE$OF, calibDOE$OFtot), {
    req(calibDOE$OF, calibDOE$OFtot)
    # Append OF to DOE$Y
    DOEOF <- cbind(calibDOE$OF, calibDOE$OFtot)
    if (length(intersect(filteredDOE$ynames, colnames(DOEOF))) == 0) {
      filteredDOE$Yinfos$all.ids <- c(importDOE$Yinfos$all.ids, rep('Interest', ncol(DOEOF)))
      filteredDOE$Yinfos$int.ids <- c(importDOE$Yinfos$int.ids, importDOE$nY + 1:ncol(DOEOF))
      filteredDOE$Yinfos$surrogate.ids <- c(importDOE$Yinfos$surrogate.ids, importDOE$nY + 1:ncol(DOEOF))
      filteredDOE$Yinfos$type <- c(importDOE$Yinfos$type, rep('numeric', ncol(DOEOF)))
      filteredDOE$nY <- importDOE$nY + ncol(calibDOE$OF) + 1
      filteredDOE$Y <- as.data.frame(cbind(importDOE$Y, DOEOF))
      OFnames <- colnames(DOEOF)
      names(OFnames) <- OFnames
      filteredDOE$ynames <- c(importDOE$ynames, OFnames)
      filteredDOE$ynamesvisu <- c(importDOE$ynamesvisu, OFnames)
      filteredDOE$ynamesmenu <- c(importDOE$ynamesmenu, OFnames)
      # Add calibration data structures
      filteredDOE$discF <- importDOE$discF
      filteredDOE$nF <- importDOE$nF
      filteredDOE$idF <- importDOE$idF
      filteredDOE$Fnames <- importDOE$Fnames
      filteredDOE$Fnamesvisu <- importDOE$Fnamesvisu
      filteredDOE$Z <- calibDOE$Z
      filteredDOE$sigZ <- calibDOE$sigZ
      filteredDOE$nZ <- calibDOE$nZ
      filteredDOE$idZ <- calibDOE$idZ
      filteredDOE$idZY <- calibDOE$idZY
      filteredDOE$discZ <- calibDOE$discZ
      filteredDOE$OF <- calibDOE$OF
      filteredDOE$OFtot <- calibDOE$OFtot
    }
    else {
      filteredDOE$Y <- as.data.frame(cbind(importDOE$Y, DOEOF))
    }
  })
  
  observe({
    req(importDOE$idon, importDOE$X, importDOE$Y)
    if(all(importDOE$idon <= importDOE$nX)){
      if (is.null(calibDOE$OF)){
        req(nrow(importDOE$X) == nrow(importDOE$Y))
      }
      else {
        req(nrow(importDOE$X) == nrow(filteredDOE$Y))
      }
      idon <- importDOE$idon
      filteredDOE$idref <- importDOE$idref
      filteredDOE$nobs <- importDOE$nobs
      filteredDOE$nX <- length(idon)
      filteredDOE$nYsurrogate <- importDOE$nYsurrogate
      filteredDOE$X <- importDOE$X[,idon,drop=FALSE]
      filteredDOE$Xopt <- importDOE$Xopt[,idon,drop=FALSE]
      filteredDOE$xnames <- importDOE$xnames[idon]
      filteredDOE$xnamesvisu <- importDOE$xnamesvisu[idon]
      filteredDOE$xnamesmenu <- importDOE$xnamesmenu[idon]
      filteredDOE$adapt.visu <- importDOE$adapt.visu
      filteredDOE$Xinfos <- importDOE$Xinfos[idon]
      if (is.null(calibDOE$OF)){
        filteredDOE$nY <- importDOE$nY
        filteredDOE$Y <- importDOE$Y
        filteredDOE$ynames <- importDOE$ynames
        filteredDOE$ynamesvisu <- importDOE$ynamesvisu
        filteredDOE$ynamesmenu <- importDOE$ynamesmenu
        filteredDOE$Yinfos <- importDOE$Yinfos
      }
      filteredDOE$XY <- cbind(filteredDOE$X, filteredDOE$Y)
      filteredDOE$idon <- idon
      filteredDOE$compositeInfos <- importDOE$compositeInfos
      filteredDOE$discF <- importDOE$discF
      filteredDOE$nF <- importDOE$nF
      filteredDOE$idF <- importDOE$idF
      filteredDOE$Fnames <- importDOE$Fnames
      filteredDOE$Fnamesvisu <- importDOE$Fnamesvisu
    }
  })

  import <- list(DOE.manual = OutputimportDOE$DOE.manual, DOE = filteredDOE, calibration = calibDOE, ML = ML, advance.importDOE = advance.importDOE, doeProblemDef = doeProblemDef, directOptim = directOptim)

  return(import)
}
