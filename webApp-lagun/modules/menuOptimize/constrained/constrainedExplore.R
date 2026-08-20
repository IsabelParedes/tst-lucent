#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module constrainedExplore

source("modules/shared/spmExport.R", local = TRUE)
source("modules/shared/pcpExport.R", local = TRUE)

CONTINUOUS_CS <- c(
  "Viridis", "Inferno", "Magma", "Plasma", "Warm", "Cool",
  "Rainbow", "CubehelixDefault", "Blues", "Greens", "Greys",
  "Oranges", "Purples", "Reds", "BuGn", "BuPu", "GnBu", "OrRd",
  "PuBuGn", "PuBu", "PuRd", "RdBu", "RdPu", "YlGnBu", "YlGn",
  "YlOrBr", "YlOrRd"
)

CATEGORIAL_CS <- c("Category10", "Accent", "Dark2", "Paired", "Set1")

ARRANGE_METHODS <- c("fromLeft", "fromRight", "fromBoth", "fromNone")

getParcoordsCOExplo <- function(DOE, predfun, lb, ub, COformulation, resCoptim, Yinfos, nobsparcoords) {
  idO <- COformulation$idO
  idC <- COformulation$idC
  nvar <- get.nb.num(DOE$Xinfos)
  nobs <- nobsparcoords
  dimx <- DOE$nX
  
  if (nvar < dimx){
    nlevels.all <- sapply(DOE$Xinfos, function(Xinfos){Xinfos$nlevels})
    nlevels.all <- nlevels.all[!is.na(nlevels.all)]
    nslices <- prod(nlevels.all)
    nobs.slide <- floor(nobs/nslices)
    nobs <- nobs.slide*nslices
    Xvisu <- lapply(1:nslices, function(ind, nobs.slide, nvar, lb, ub){
      Xvisu.temp <- matrix(runif(nobs.slide*nvar),nobs.slide, nvar)
      Xvisu.temp <- repmat(lb, nobs.slide, 1) + repmat(ub - lb, nobs.slide, 1)*Xvisu.temp},
      nobs.slide = nobs.slide, nvar = nvar, lb = lb, ub = ub)
    Xvisu <- do.call(rbind, Xvisu)
    levels <- expand.grid(lapply(nlevels.all, function(nlevel){1:nlevel}))
    levels <- levels[sort(rep(1:nslices, nobs.slide)),]
    rownames(levels) <- NULL
    num.index <- unlist(sapply(1:dimx, function(ind, Xinfos){
      if (Xinfos[[ind]]$type=='numeric') {ind}
    }, Xinfos = DOE$Xinfos))
    index <- 1:dimx
    cat.index <- index[!index %in% num.index]
    index <- c(num.index, cat.index)
    Xvisu <- cbind(Xvisu, levels)
    Xvisu <- as.data.frame(Xvisu[,order(index)])
    colnames(Xvisu) <- DOE$xnames
  }else{
    Xvisu <- runif.sobol(nobs, dimx)
    Xvisu <- repmat(lb, nobs, 1) + repmat(ub - lb, nobs, 1)*Xvisu
  }
  if (nvar < dimx){
    Xvisu[,cat.index] <- as.data.frame(sapply(cat.index, function(ind, Xinfos, data){
      as.factor(unlist(Xinfos[[ind]]$levels)[data[,ind]])
    }, Xinfos = DOE$Xinfos, data = Xvisu))
  }

  Yvisu <- matrix(NA, nrow = nobs, ncol = DOE$nY)
  for (j in Yinfos$visu.ids) {
    Yvisu[,j] <- predfun(Xvisu, j)
  }
  OFids <- match(colnames(resCoptim$OF), DOE$ynamesmenu)
  d <- cbind(Xvisu,Yvisu[,c(idO,idC, OFids)])
  datanorm <- as.data.frame(d)
  colnames(datanorm) <- c(DOE$xnames,DOE$ynames[c(idO,idC, OFids)])
  datanorm <- cbind(datanorm,Select = rep("Explo",nobs))
  nO <- length(idO)
  m <- matrix(as.matrix(COformulation$COobj),ncol = nO)
  bO <- matrix(rep(": Max",nO),ncol = nO)
  idmin <- which(m == -1)
  bO[idmin] <- ": Min"
  nC <- length(idC)
  threshconstraints <- matrix(as.matrix(COformulation$COt),ncol = nC)
  signconstraints <- matrix(as.matrix(COformulation$COsign),ncol = nC)
  bC <- matrix(NA,ncol = nC)
  for (j in 1:nC) {
    idsign <- (as.numeric(COformulation$COsign[j]) + 3)/2
    bC[j] <- paste0(tablesign[idsign],threshconstraints[j])
  }
  colnames(datanorm) <- c(DOE$xnamesvisu,paste0(DOE$ynamesvisu[idO],bO),paste0(DOE$ynamesvisu[idC],bC),DOE$ynamesmenu[OFids], "Select")
  exporttemp <- resCoptim$export
  colnames(exporttemp) <- colnames(datanorm)
  # Add optimization results
  datavisu <- rbind(datanorm,exporttemp)
  return(datavisu)
}

constrainedExplore.ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      column(1,br(),
        dropdownButton(
          radioGroupButtons(
            inputId = ns("layout"),
            label = tags$h4("Layout"), 
            choices = c("Vertical", "Horizontal"),
            status = "primary"
          ),
          hr(),
          tags$h4("Palette Colors"),
          selectInput(ns("choose.palette.num"),
            "Choose Palette for Numeric Columns",
            choices = CONTINUOUS_CS,
            selected = CONTINUOUS_CS[9]
          ),
          selectInput(ns("choose.palette.cat"),
            "Choose Palette for Categorical Columns",
            choices = CATEGORIAL_CS,
            selected = CATEGORIAL_CS[4]
          ),
          hr(),
          tags$h3("Parallel Coordinate Plot"),
          selectInput(
            ns("arrange.method"),
            "Arrange Method in Category Boxes",
            choices = ARRANGE_METHODS,
            selected = ARRANGE_METHODS[2]
          ),
          pcpExport.ui(ns("pcpExport")),
          hr(),
          tags$h3("Scatter Plot Matrix"),
          selectInput(ns("corrPlotType"),
            "Correlation Plot Type",
            choices = list("Text" = "Text", "AbsText" = "AbsText"),
            selected = "Text"
          ),
          selectInput(ns("corrPlotCs"),
            "Correlation Plot Palette",
            choices = CONTINUOUS_CS,
            selected = CONTINUOUS_CS[22] # RdBu
          ),
          selectInput(ns("distribType"),
            "Distribution:",
            choices = list("Histogram" = 2, "Density Plot" = 1),
            selected = 1
          ),
          spmExport.ui(ns("spmExport")),
          circle = TRUE,
          icon = icon("cog"), status = "primary", right = FALSE,
          tooltip = tooltipOptions(title = "Click for advanced settings")
        ), align="left"
      )
    ),
    div(id = ns("pcpspm"),
      parallelPlotOutput(ns("parcoords")),
      scatterPlotMatrixOutput(ns("scatterPlotMatrix"))
    )
  )
}

constrainedExplore.server <- function(input, output, session, DOE, listmodels, COformulation, resCoptim, settings) {
  
  ns <- session$ns

  datavisu <- reactive({
    req(resCoptim$table)
    predfun <- listmodels$finalpredfun
    Xbounds <- get.bounds(DOE$Xinfos)
    lb <- Xbounds[1,,drop=F]
    ub <- Xbounds[2,,drop=F]

    idO <- COformulation$idO
    idC <- COformulation$idC

    return(getParcoordsCOExplo(DOE, predfun, lb, ub, COformulation, resCoptim, Yinfos, settings$nobsparcoords))
  })

  observeEvent(input$layout, {
    if (input$layout == "Vertical") {
      shinyjs::runjs(paste0(
        "$('#", ns("pcpspm"), ">.scatterPlotMatrix').attr('align', 'center');",
        "$('#", ns("pcpspm"), "').css('display', 'block');",
        "$('#", ns("pcpspm"), ">.parallelPlot').css('width', '100%');",
        "$('#", ns("pcpspm"), ">.scatterPlotMatrix').css('width', '100%').trigger('shown');"
      ))
    }
    if (input$layout == "Horizontal") {
      shinyjs::runjs(paste0(
        "$('#", ns("pcpspm"), "').css('display', 'flex');",
        "$('#", ns("pcpspm"), ">.parallelPlot').css('width', '55%');",
        "$('#", ns("pcpspm"), ">.scatterPlotMatrix').css('width', '45%').trigger('shown');"
      ))
    }
  })
  
  # Update output types for the visualization only if the surrogate models are updated
  Yinfos <- reactiveValues(int.ids=NULL, control.ids=NULL, const.ids=NULL, visu.ids=NULL, nY=NULL)
  observeEvent(list(listmodels$bestQ2loo$id, DOE$nY), {
    
    YwithSelectedModel <- seq(DOE$nY)
    
    if (!is.null(listmodels$selected))
      YwithSelectedModel <- YwithSelectedModel[sapply(listmodels$selected$id, function(x) !is.na(x[1]))]
    
    Yinfos$int.ids <- intersect(DOE$Yinfos$int.ids, YwithSelectedModel)
    Yinfos$control.ids <- intersect(DOE$Yinfos$control.ids, YwithSelectedModel)
    Yinfos$const.ids <- intersect(DOE$Yinfos$const.ids, YwithSelectedModel)
    Yinfos$visu.ids <- c(Yinfos$int.ids, Yinfos$control.ids, Yinfos$const.ids)
    Yinfos$nY <- length(Yinfos$visu.ids)
  })

  output$parcoords <- renderParallelPlot({
    req(resCoptim$table)
    predfun <- listmodels$finalpredfun
    Xbounds <- get.bounds(DOE$Xinfos)
    lb <- Xbounds[1,,drop=F]
    ub <- Xbounds[2,,drop=F]
    idO <- COformulation$idO
    idC <- COformulation$idC
    categorical <- lapply(1:DOE$nX, function(i) {
      if (DOE$Xinfos[[i]]$type == "categorical") {
        return(as.character(DOE$Xinfos[[i]]$levels))
      }
      return(NULL)
    })
    categorical <- c(categorical, vector('list', ncol(datavisu()) - length(categorical)))
    categorical[[ncol(datavisu())]] <- unique(datavisu()$Select)
    isolate({
      parallelPlot(
        data = datavisu(),
        categorical= categorical,
        arrangeMethod = input$arrange.method,
        rotateTitle = DOE$adapt.visu,
        columnLabels = NULL,
        refColumnDim = "Select",
        keptColumns = NULL,
        histoVisibility = NULL,
        refRowIndex = NULL,
        continuousCS = input$choose.palette.num,
        categoricalCS = input$choose.palette.cat,
        cutoffs = NULL,
        controlWidgets = NULL,
        eventInputId = ns("pcpEvent")
      )
    })
  })
  
  output$scatterPlotMatrix <- renderScatterPlotMatrix({
    req(resCoptim$table)
    predfun <- listmodels$finalpredfun
    Xbounds <- get.bounds(DOE$Xinfos)
    lb <- Xbounds[1,,drop=F]
    ub <- Xbounds[2,,drop=F]
    idO <- COformulation$idO
    idC <- COformulation$idC
    categorical <- lapply(1:DOE$nX, function(i) {
      if (DOE$Xinfos[[i]]$type == "categorical") {
        return(as.character(DOE$Xinfos[[i]]$levels))
      }
      return(NULL)
    })
    categorical <- c(categorical, vector('list', ncol(datavisu()) - length(categorical)))
    categorical[[ncol(datavisu())]] <- unique(datavisu()$Select)
    isolate({
      scatterPlotMatrix(
        data = datavisu(),
        categorical= categorical,
        rotateTitle = DOE$adapt.visu,
        columnLabels = NULL,
        zAxisDim = "Select",
        keptColumns = NULL,
        distribType = as.numeric(input$distribType),
        corrPlotType = as.character(input$corrPlotType),
        corrPlotCS = as.character(input$corrPlotCs),
        continuousCS = input$choose.palette.num,
        categoricalCS = input$choose.palette.cat,
        cutoffs = NULL,
        controlWidgets = NULL,
        cssRules = list(
          ".jitterZone" = "fill: white"
        ),
        plotProperties = list(
          noCatColor = "#1F78B4",
          point = list(
            alpha = 0.8,
            radius = 5
          )
        ),
        slidersPosition = list(
          dimCount = 5
        ),
        eventInputId = ns("spmEvent")
      )
    })
  })

  callModule(pcpExport.server,
    "pcpExport",
    parallelPlotId = ns("parcoords"),
    datavisu = datavisu
  )

  callModule(spmExport.server,
    "spmExport",
    scatterPlotMatrixId = ns("scatterPlotMatrix"),
    datavisu = datavisu
  )
  
  observeEvent(input$spmEvent, {
    if (input$spmEvent$type == "zAxisChange") {
      parallelPlot::setRefColumnDim(ns("parcoords"), input$spmEvent$value)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "refColumnDimChange") {
      scatterPlotMatrix::setZAxis(ns("scatterPlotMatrix"), input$pcpEvent$value$refColumnDim)
    }
  })

  observeEvent(input$spmEvent, {
    if (input$spmEvent$type == "hlPointEvent") {
      parallelPlot::highlightRow(ns("parcoords"), input$spmEvent$value$pointIndex)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "hlRowEvent") {
      scatterPlotMatrix::highlightPoint(ns("scatterPlotMatrix"), input$pcpEvent$value$rowIndex)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "cutoffChange" && !input$pcpEvent$value$adjusting) {
      ppCutoffs <- input$pcpEvent$value$cutoffs

      updatedDim <- input$pcpEvent$value$updatedDim
      if (ppCutoffs[updatedDim] == "NULL") {
        ppCutoffs[updatedDim] <- list(NULL)
      }

      setSpmCutoffsFromPP(ppCutoffs)
    }
  })

  appendPPCutoff <- function(ppCutoff, curCutoff, categories) {
    if (is.null(categories)) {
      return(append(ppCutoff, curCutoff))
    }
    else {
      keptCategories <- categories
      if (!is.null(curCutoff)) {
        sorted <- sort(unlist(curCutoff)) + 1
        if (ceiling(sorted[1]) <= floor(sorted[2])) {
          keptCategories <- categories[ceiling(sorted[1]):floor(sorted[2])]
        }
      }
      return(union(ppCutoff, list(keptCategories)))
    }
  }

  observeEvent(input$spmEvent, {
    req(input$spmEvent)
    if (input$spmEvent$type == "cutoffChange" && !input$spmEvent$value$adjusting) {
      spmCutoffs <- input$spmEvent$value$cutoffs
      ppCutoffs <- NULL
      if (!is.null(spmCutoffs)) {
        dimNames <- colnames(datavisu())
        ppCutoffs <- list()
        for (dimName in dimNames) {
          ppCutoffs[dimName] <- list(NULL)
        }
        for (i in seq_along(spmCutoffs)) {
          xDim <- spmCutoffs[[i]]$xDim
          if (!is.vector(ppCutoffs[[xDim]])) {
            ppCutoffs[[xDim]] <- vector()
          }

          yDim <- spmCutoffs[[i]]$yDim
          if (!is.vector(ppCutoffs[[yDim]])) {
            ppCutoffs[[yDim]] <- vector()
          }

          for (xyCutoff in spmCutoffs[[i]]$xyCutoffs) {
            ppCutoffs[[xDim]] <- appendPPCutoff(ppCutoffs[[xDim]], xyCutoff[1], levels(datavisu()[[which(dimNames == xDim)]]))
            ppCutoffs[[yDim]] <- appendPPCutoff(ppCutoffs[[yDim]], xyCutoff[2], levels(datavisu()[[which(dimNames == yDim)]]))
          }
        }
      }
      parallelPlot::setCutoffs(ns("parcoords"), ppCutoffs)
    }
  })

  observeEvent(input$pcpEvent, {
    req(input$pcpEvent)
    if (input$pcpEvent$type == "cutoffChange" && !input$pcpEvent$value$adjusting) {
      ppCutoffs <- input$pcpEvent$value$cutoffs

      updatedDim <- input$pcpEvent$value$updatedDim
      if (ppCutoffs[updatedDim] == "NULL") {
        ppCutoffs[updatedDim] <- list(NULL)
      }

      setSpmCutoffsFromPP(ppCutoffs)
    }
  })

  setSpmCutoffsFromPP <- function(ppCutoffs) {
    spmCutoffs <- NULL
    if (is.list(ppCutoffs)) {
      categorical <- lapply(1:DOE$nX, function(i) {
        if (DOE$Xinfos[[i]]$type == "categorical") {
          return(as.character(DOE$Xinfos[[i]]$levels))
        }
        return(NULL)
      })
      categorical <- c(categorical, vector('list', ncol(datavisu()) - length(categorical)))
      categorical[[ncol(datavisu())]] <- unique(datavisu()$Select)
      dimNames <- colnames(datavisu())
      spmCutoffs <- vector()
      for (dimName in names(ppCutoffs)) {
        ppCutoff <- ppCutoffs[[dimName]]
        if (!is.null(ppCutoff)) {
          spCutoff <- list(xDim = dimName, yDim = dimName)
          if (!is.null(categorical[[which(dimNames == dimName)]])) {
            ppCutoff <- Filter(function(e) { return(e %in% unique(datavisu()[[dimName]]))}, ppCutoff) # Workaround (bug in 'spm.setCutoffs' when a category is not used in data)
            categories <- categorical[[which(dimNames == dimName)]]
            spCutoff$xyCutoffs <- sapply(ppCutoff, function(cat) {
              catIndex <- which(cat == categories)
              list(list(NULL, c(catIndex - 1 - 1 / 8, catIndex - 1 + 1 / 8)))
            })
          }
          else {
            xyCutoffs <- list()
            for (cutoff in ppCutoff) {
              xyCutoffs <- append(xyCutoffs, list(list(NULL, rev(cutoff))))
            }
            spCutoff$xyCutoffs <- xyCutoffs
          }
          spmCutoffs <- append(spmCutoffs, list(spCutoff))
        }
      }
    }
    scatterPlotMatrix::setCutoffs(ns("scatterPlotMatrix"), spmCutoffs)
  }

  # If continuous palette has been changed ...
  observeEvent(input$choose.palette.num, {
    parallelPlot::setContinuousColorScale(ns("parcoords"), input$choose.palette.num)
    scatterPlotMatrix::setContinuousColorScale(ns("scatterPlotMatrix"), input$choose.palette.num)
  })
  
  # If categorical palette has been changed ...
  observeEvent(input$choose.palette.cat, {
    parallelPlot::setCategoricalColorScale(ns("parcoords"), input$choose.palette.cat)
    scatterPlotMatrix::setCategoricalColorScale(ns("scatterPlotMatrix"), input$choose.palette.cat)
  })
  
  # If arrange method has been changed ...
  observeEvent(input$arrange.method, {
    parallelPlot::setArrangeMethod(ns("parcoords"), input$arrange.method)
  })
  
  # If 'corrPlotType' has been changed ...
  observeEvent(input$corrPlotType, {
    scatterPlotMatrix::setCorrPlotType(
      ns("scatterPlotMatrix"),
      input$corrPlotType
    )
  })

  # If 'corrPlotCs' has been changed ...
  observeEvent(input$corrPlotCs, {
    scatterPlotMatrix::setCorrPlotCS(
      ns("scatterPlotMatrix"),
      input$corrPlotCs
    )
  })

  # If 'distribType' has been changed ...
  observeEvent(input$distribType, {
    scatterPlotMatrix::setDistribType(
      ns("scatterPlotMatrix"),
      input$distribType
    )
  })
  
}