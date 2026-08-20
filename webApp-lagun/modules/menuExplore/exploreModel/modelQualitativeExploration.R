#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module modelQualitativeExploration
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/shared/dynamicSelectpicker.R", local = TRUE)
source("modules/shared/XinfosChange.R", local = TRUE)
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

sampleInputs  <- function(DOE, nobs, Xinfos) {

  dimx <- DOE$nX
  nvar <- get.nb.num(DOE$Xinfos)
  Xbounds <- get.bounds(Xinfos)
  lb <- Xbounds[1,,drop=F]
  ub <- Xbounds[2,,drop=F]
  if (nvar < dimx){
    nlevels.all <- sapply(Xinfos, function(Xinfos){Xinfos$nlevels})
    nlevels.all <- nlevels.all[!is.na(nlevels.all)]
    levels <- expand.grid(lapply(nlevels.all, function(nlevel){1:nlevel}))
    if (nvar > 0){  
      nslices <- prod(nlevels.all)
      nobs.slide <- floor(nobs/nslices)
      nobs <- nobs.slide*nslices
      levels <- levels[sort(rep(1:nslices, nobs.slide)),]
      rownames(levels) <- NULL
      Xvisu <- lapply(1:nslices, function(ind, nobs.slide, nvar, lb, ub){
        Xvisu.temp <- matrix(runif(nobs.slide*nvar),nobs.slide, nvar)
        Xvisu.temp <- repmat(lb, nobs.slide, 1) + repmat(ub - lb, nobs.slide, 1)*Xvisu.temp},
        nobs.slide = nobs.slide, nvar = nvar, lb = lb, ub = ub)
      Xvisu <- do.call(rbind, Xvisu)
      num.index <- unlist(sapply(1:dimx, function(ind, Xinfos){
        if (Xinfos[[ind]]$type=='numeric') {ind}
      }, Xinfos = DOE$Xinfos))
      index <- 1:dimx
      cat.index <- index[!index %in% num.index]
      index <- c(num.index, cat.index)
      Xvisu <- cbind(Xvisu, levels)
      Xvisu <- as.data.frame(Xvisu[,order(index)])
    }else{
      Xvisu <- levels
      cat.index <- 1:dimx
      nobs <- nrow(Xvisu)
    }
    Xvisu[,cat.index] <- as.data.frame(sapply(cat.index, function(ind, Xinfos, data){
      as.factor(unlist(Xinfos[[ind]]$levels)[data[,ind]])
    }, Xinfos = Xinfos, data = Xvisu))
  }else{
    Xvisu <- runif.sobol(nobs, dimx)
    Xvisu <- repmat(lb, nobs, 1) + repmat(ub - lb, nobs, 1)*Xvisu
    Xvisu <- as.data.frame(Xvisu)
  }
  colnames(Xvisu) <- DOE$xnames
  return(Xvisu)
  
}

getParcoordsData  <- function(DOE, nobs, predfunc, Xinfos, Yinfos, callback, idY = NULL) {

  Xvisu <- sampleInputs(DOE, nobs, Xinfos)
  if (is.null(idY)){idY <- Yinfos$visu.ids}
  Yvisu <- as.data.frame(matrix(NA, nrow = nrow(Xvisu), ncol = DOE$nY))
  for (j in 1:length(idY)) {
    Yvisu[, idY[j]] <- predfunc(Xvisu, idY[j])
    callback(j)
  }
  d <- cbind(Xvisu, Yvisu)
  datanorm <- as.data.frame(d)
  colnames(datanorm) <- c(DOE$xnames, DOE$ynames)
  return(datanorm)

}

getParcoordsDataFromFile  <- function(DOE, PCPsample, predfunc, Yinfos, callback) {

  Xvisu <- PCPsample
  colnames(Xvisu) <- DOE$xnames
  nobs <- nrow(PCPsample)
  dimx <- DOE$nX
  dimy <- length(Yinfos$visu.ids)
  Yvisu <- matrix(NA, nrow = nobs, ncol = DOE$nY)
  for (j in 1:dimy) {
    Yvisu[, Yinfos$visu.ids[j]] <- predfunc(Xvisu, Yinfos$visu.ids[j])
    callback(j)
  }
  d <- cbind(Xvisu, Yvisu)
  datanorm <- as.data.frame(d)
  colnames(datanorm) <- c(DOE$xnames, DOE$ynames)
  return(datanorm)
  
}

#######################################################
##      SMC functions

# initial sampling on the hypercube
unrestricted = function(N, range) { # initial sample on the hypercube
  D = nrow(range)
  samp = NULL
  for (d in 1:D) samp = cbind(samp, runif(N, range[d,1], range[d,2]))
  return(samp)
}

# log-posterior / equivalent to the constraint if a uniform sample is drawn
logpost_vec = function(sample, nu_t, constraint) { 
  term = constraint(sample)
  return(rowSums(pnorm(- term / nu_t, log = T)))
}

# sampling at each time step
Gibbs_vec = function(x, q, d, nu_t, lpdent = lpdent, N, constraint) { 
  delta = rnorm(N,0,q[d])
  newx = x
  newx[,d] = newx[,d] + delta
  lpnum = logpost_vec(newx, nu_t, constraint)
  ratio = lpnum - lpdent
  prob = pmin(1, exp(ratio))
  u = runif(N)
  idchange <- which(u <= prob)
  x[idchange,] <- newx[idchange,]
  lpdent[idchange] <- lpnum[idchange]
  return(list(x = x, lpdent = lpdent))
}

# adaptive specification of the constraint parameter
adapt_seq_vec = function(nu, nu0, N, term=term, Wt) { 
  cons1 <- rowSums(pnorm(- term / nu, log = T))
  cons2 <- rowSums(pnorm(- term / nu0, log = T))
  wt <- cons1 - cons2
  Wt = Wt * exp(wt)
  Wt = Wt / sum(Wt)
  ESS = ifelse(sum(is.na(Wt)) == N, 0, 1 / sum(Wt ^ 2))
  return(ESS - (N / 2))
}

# Get constraint function from selected parcoords
get.constraintfunction <- function(dfparcoords,DOE,Xinfos,predfunc){
  # Constraint function
  dimx <- DOE$nX
  allnames.selected <- colnames(dfparcoords)
  idY <- intersect(allnames.selected,DOE$ynames)
  nY <- length(idY)
  Xlowerupper <- get.lowerupper(dfparcoords,DOE,Xinfos)
  minY <- as.matrix(dfparcoords[1,idY,drop=FALSE])
  maxY <- as.matrix(dfparcoords[2,idY,drop=FALSE])
  
  constraint <-function(sample){
    ns <- nrow(sample)
    tempY <- matrix(NA,ns,nY)
    for (j in 1:nY) {
      idtemp <- which(DOE$ynames==idY[j])
      tempY[,j] <- predfunc(sample, idtemp)
    }
    tempXlb <- Xlowerupper[1,,drop=FALSE]
    tempXub <- Xlowerupper[2,,drop=FALSE]
    tempYmin <- repmat(minY,ns,1) - tempY
    tempYmax <- tempY - repmat(maxY,ns,1)
    return(cbind(tempYmin,tempYmax,repmat(tempXlb,ns,1)-sample,sample-repmat(tempXub,ns,1)))
  }
  
  return(constraint)
}

# Get lower and upper
get.lowerupper <- function(dfparcoords,DOE,Xinfos){
  Xbounds <- get.bounds(Xinfos)
  dimx <- ncol(Xbounds)
  allnames.selected <- colnames(dfparcoords)
  idX <- intersect(allnames.selected,DOE$xnames)
  notidX <- setdiff(DOE$xnames,idX)
  bounds <- matrix(NA,2,dimx)
  colnames(bounds) <- colnames(Xbounds) <- DOE$xnames
  bounds[1,idX] <- as.matrix(dfparcoords[1,idX])
  bounds[2,idX] <- as.matrix(dfparcoords[2,idX])
  bounds[1,notidX] <- Xbounds[1,notidX]
  bounds[2,notidX] <- Xbounds[2,notidX]
  return(bounds)
}

SCMC_withcallback <- function(dfparcoords,DOE,Xinfos,predfunc,nsampleSMC,iterSMC,callback){
  N <- nsampleSMC
  M <- 10
  L <- iterSMC
  qt <- 1
  nuseq_T <- 1e-3
  Xlowerupper <- get.lowerupper(dfparcoords,DOE,Xinfos)
  lower <- Xlowerupper[1,]
  upper <- Xlowerupper[2,]
  constraint <- get.constraintfunction(dfparcoords,DOE,Xinfos,predfunc)
  
  rge = cbind(lower,upper)
  D = nrow(rge)
  t = 1
  ESS = NULL
  nuseq = c(Inf)
  b = seq(1.5,.1,length = L)
  nuseq_0 = c(Inf, b^7)
  samplet = array(dim=c(N, 1, D))
  samplet[,1,] = unrestricted(N, range=rge)
  lpdent <- matrix(logpost_vec(samplet[, t,],nuseq[t],constraint),ncol=1)
  Wt = matrix(rep(1 / N, N),ncol=1)
  withProgress(message = 'Inverse Sampling...', value = 0, {
    repeat {
      callback(t)
      t = t+1
      newsample = samplet[, t-1,]
      newlpdent = lpdent[, t- 1]
      newWt = Wt[, t - 1]
      term <- constraint(newsample)
      if (adapt_seq_vec(nu = nuseq_T, nu0 = nuseq[t-1], N = N, term=term, Wt = newWt) > 0){
        nuseq[t] = nuseq_T
      }
      if (adapt_seq_vec(nu = ifelse(t < 3, min(.1, nuseq_0[t]), nuseq_T), nu0 = nuseq[t-1], N = N, term=term, Wt = newWt) > 0){
        nuseq[t] = nuseq_T
      }else{
        nuseq[t] = uniroot(adapt_seq_vec, interval = c(ifelse(t < 3, min(.1, nuseq_0[t]), nuseq_T), ifelse(t != 2, nuseq[t-1], 1e5)), nu0 = nuseq[t-1], N = N, term = term, Wt = newWt)$root
      }
      constraint1 <- rowSums(pnorm(- term / nuseq[t], log = T))
      constraint2 <- rowSums(pnorm(- term / nuseq[t-1], log = T))
      wt <- constraint1 - constraint2
      newWt = newWt * exp(wt)
      newWt = newWt / sum(newWt)
      if (anyNA(newWt)){t <- t-1; break}
      ESS[t] = 1 / sum(newWt ^ 2)
      index = sample(1:N, N, prob = newWt, replace = T)
      newsample = newsample[index,] 
      newlpdent = newlpdent[index]
      newWt = rep(1/N, N)
      q = apply(newsample, 2, sd) / (t^qt)
      for (j in 1:M){
        for (d in 1:D){
          out <- Gibbs_vec(newsample, q, d, nuseq[t], lpdent = newlpdent, N = N, constraint)
          newsample = out$x
          newlpdent = out$lpdent
        }
      }
      
      samplet = abind(samplet, newsample, along = 2)
      lpdent = abind(lpdent,newlpdent,along = 2)
      Wt = abind(Wt, newWt, along = 2)
      if (nuseq[t] <= nuseq_T | (t+1)>L) break
    }
  })
  t_final = t
  sample = samplet[, t_final,]
  
  idok <- which(apply(constraint(sample)<=0,1,all))
  nok <- length(idok)
  sample <- sample[idok,]
  return(list(sample=sample,nsample=nok))
}

#######################################################
##      END SMC functions

modelQualitativeExploration.ui <- function(id) {
  ns <- NS(id)
  
  SMCModal <- bsModal(
    ns("modalSMC"), "Inverse Sampling Settings", NULL, 
    tagList(
      fluidRow(
        column(12, align="center", DT::dataTableOutput(ns("tableselectedParCoords2")))
      ),
      fluidRow(
        column(6, align="center", 
               numericInput(ns("nSMCsample"), "Sample Size", 1000, min = 1)
        ),
        column(6, align="center", 
               numericInput(ns("nSMCiter"), "Iteration Number", 25, min = 1)
        )
      ),
      uiOutput(ns("SMCtext")),
      actionButton(ns("launchSMC"), "Launch Inverse Sampling", class = "btn-primary", width = '100%')
    ),
    size="large"
  )
  
  PCPfileModal <- bsModal(
    ns("modalPCPfile"), "Import file for Parallel Coordinate Plot", NULL,
    tagList(
      fluidRow(
        column(2, radioButtons(ns("separator"), "Separator",
                               choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"))),
        column(2, radioButtons(ns("decimal"), "Decimal",
                               choices = list(". (point)" = ".", ", (comma)" = ","))
        ),
        column(7,
               fileInput(ns('filePCP'), 'Select File', accept = c('.txt','.dat','.csv')),
               tags$script(paste0('$( "#', ns('filePCP'), '" ).on( "click", function() { this.value = null; });')),
               uiOutput(ns('error.file'))
        )
      ),
      DT::dataTableOutput(ns('PCPfilecontents')),
      br(),
      fluidRow(
        column(6,
               actionButton(ns("loadPCPfile"), "Display Sample from File", class = "btn-primary",width='100%'), align = "center"
        ),
        column(6,
               actionButton(ns("initPCP"), "Display Sample from Monte-Carlo (Reset)", class = "btn-primary",width='100%'), align = "center"
        )
      )
    ),
    size="large"
  )
  
  BoundfileModal <- bsModal(
    ns("modalBoundfile"), "Import file for Bounds", NULL,
    tagList(
      fluidRow(
        column(2, radioButtons(ns("boundseparator"), "Separator",
                               choices = list(", (comma)" = ",", "; (semi-colon)" = ";", "Tab" = "\t"))),
        column(2, radioButtons(ns("bounddecimal"), "Decimal",
                               choices = list(". (point)" = ".", ", (comma)" = ","))
        ),
        column(7,
               fileInput(ns('fileBound'), 'Select File', accept = c('.txt','.dat','.csv')),
               tags$script(paste0('$( "#', ns('fileBound'), '" ).on( "click", function() { this.value = null; });'))
        )
      ),
      DT::dataTableOutput(ns('Boundfilecontents')),
      fluidRow(
        column(3, actionButton(ns("boundsave"), label = "Save and Close", class = "btn-warning",
                               width = '100%'), offset = 2),
        column(3, actionButton(ns("boundclose"), label = "Dismiss", class = "btn-secondary",
                               width = '100%'), offset = 2)
      )
    ),
    size="large",tags$head(tags$style(paste0("#", ns("modalBoundfile")," .modal-footer{display:none}")))
  )
  
  tagList(
    useShinyjs(),
    br(),
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
                 selected = CONTINUOUS_CS[1]
               ),
               selectInput(ns("choose.palette.cat"),
                 "Choose Palette for Categorical Columns",
                 choices = CATEGORIAL_CS,
                 selected = CATEGORIAL_CS[1]
               ),
               hr(),
               tags$h3("Parallel Coordinate Plot"),
               fluidRow(
                 column(6,tags$h5("Bounds"))
               ),
               fluidRow(
                 column(6,"",align="center"),
                 column(6,switchInput(ns("keepbounds"), label = "Keep bounds", size="small"),align="center")
               ),
               hr(),
               selectInput(
                 ns("arrange.method"),
                 "Arrange Method in Category Boxes",
                 choices = ARRANGE_METHODS,
                 selected = ARRANGE_METHODS[2]
               ),
               hr(),
               pcpExport.ui(ns("pcpExport")),
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
             ),align="left"
      ),
      column(2,dynamicSelectpicker.ui(ns("chooseX"))),
      column(2,dynamicSelectpicker.ui(ns("chooseY"))),
      column(2,dynamicSelectpicker.ui(ns("chooseHistParcoords"))),
      column(2,dynamicSelectpicker.ui(ns("chooseBoundsParcoords"))),
      column(1,
             actionButton(ns("Loadsampling"), HTML(paste("Sampling","from File",sep='<br>')), class = "btn-primary", width="100%"), align = "center"
      ),
      column(1,
             XinfosChange.ui(ns("bounds"), label = HTML(paste("Refine","Sampling",sep='<br>')), width="100%"), align = "center"
      ),
      column(1,
             actionButton(ns("SMCsettings"), HTML(paste("Inverse","Sampling",sep='<br>')), class = "btn-primary", width="95%"), align = "center"
      )
    ),
    fluidRow(
      column(12,uiOutput(ns("dynui_bounds")),align="center")
    ),
    tags$script(paste0('$( "#', ns('chooseX'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXPCPclosed'),'", 1, {priority: "event"}); });')),
    tags$script(paste0('$( "#', ns('chooseY'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYPCPclosed'),'", 1, {priority: "event"}); });')),
    tags$script(paste0('$( "#', ns('chooseHistParcoords'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseHistPCPclosed'),'", 1, {priority: "event"}); });')),
    div(id = ns("pcpspm"),
      parallelPlotOutput(ns("parcoords")),
      scatterPlotMatrixOutput(ns("scatterPlotMatrix"), height = "1000px")
    ),
    br(),
    fluidRow(
      column(9,""),
      column(1,
             switchInput(ns("display"), value = F, label = "Display Data",size = "mini"), align="right"),
      column(2,"If you change cutoffs, you need to reactive display to update the table", align="right")
    ),
    uiOutput(ns("dataView")),
    SMCModal,
    PCPfileModal,
    BoundfileModal
  )
}

modelQualitativeExploration.server <- function(id, DOE, listmodels, window.dimension, settings) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

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
      Yinfos <- reactiveValues(all.ids = NULL, int.ids = NULL, control.ids = NULL, const.ids = NULL, status.ids = NULL, visu.id = NULL, nY = NULL, type = NULL)
      observeEvent(list(listmodels$selected$id, DOE$nY), {

        YwithSelectedModel <- seq(length(listmodels$selected$id))
        
        if (!is.null(listmodels$selected))
          YwithSelectedModel <- YwithSelectedModel[sapply(listmodels$selected$id, function(x) !is.na(x[1]))]

        Yinfos$all.ids <- DOE$Yinfos$all.ids
        Yinfos$int.ids <- intersect(DOE$Yinfos$int.ids, YwithSelectedModel)
        Yinfos$control.ids <- intersect(DOE$Yinfos$control.ids, YwithSelectedModel)
        Yinfos$const.ids <- intersect(DOE$Yinfos$const.ids, YwithSelectedModel)
        Yinfos$status.ids <- DOE$Yinfos$status.ids
        Yinfos$visu.ids <- c(Yinfos$int.ids, Yinfos$control.ids, Yinfos$const.ids)
        Yinfos$nY <- length(Yinfos$visu.ids)
        Yinfos$type <- DOE$Yinfos$type
      })
      
      Xcat <- reactive({
        req(DOE$Xinfos)
        cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
        return(DOE$xnamesmenu[cat])
      })
      
      Ycat <- reactive({
        req(Yinfos)
        cat <- which(Yinfos$type == "categorical")
        return(DOE$ynamesmenu[cat])
      })
      
      Allcat <- reactive({
        req(DOE$Xinfos,Yinfos)
        categorical <- sapply(1:DOE$nX, function(i) {
          if (DOE$Xinfos[[i]]$type == "categorical") {
            return(as.character(DOE$Xinfos[[i]]$levels))
          }
          return(NULL)
        })
        categorical <- c(categorical, sapply(1:DOE$nY,function(i){
          if (Yinfos$type[i] == "categorical") {
            return(as.character(unique(DOE$Y[,i])))
          }
          return(NULL)
        }))
        names(categorical) <- c(DOE$xnamesmenu,DOE$ynamesmenu)
        return(categorical)
      })
      
      Allb <- reactive({
        req(DOE$Xinfos,Yinfos)
        bounds <- lapply(1:DOE$nX, function(i) {
          if (DOE$Xinfos[[i]]$type == "numeric") {
            return(DOE$Xinfos[[i]]$bounds)
          }
          return(NULL)
        })
        bounds <- c(bounds, lapply(1:DOE$nY,function(i){
          if (Yinfos$type[i] == "numeric") {
            return(range(DOE$Y[,i]))
          }
          return(NULL)
        }))
        names(bounds) <- c(DOE$xnamesmenu,DOE$ynamesmenu)
        return(bounds)
      })
      
      data.PCP <- reactiveValues(X = NULL, bounds = NULL)
      error.msg <- reactiveValues(file = NULL)
      
      choicesX <- reactive({
        req(DOE$xnamesmenu)
        DOE$xnamesmenu
      })

      xname <- callModule(
        dynamicSelectpicker.server, "chooseX", label.title = "Choose Input(s) to Visualize", choices = choicesX,
        multiple = TRUE, selected = choicesX(), livesearch = TRUE
      )
      
      choicesY <- reactive({
        req(DOE$ynamesmenu,Yinfos)
        l <- list()
        if (length(Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[Yinfos$int.ids])
        if (length(Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[Yinfos$control.ids])
        if (length(Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[Yinfos$const.ids])
        return(l)
      })
      
      selectedY <- reactive({
        req(DOE$ynamesmenu,Yinfos)
        DOE$ynamesmenu[Yinfos$int.ids]
      })
      
      yname <- callModule(
        dynamicSelectpicker.server, "chooseY", label.title = "Choose Output(s) to Visualize", choices = choicesY,
        multiple = TRUE,  livesearch = TRUE, selected = selectedY
      )
      
      choicesHistParcoords <- reactive({
        req(choicesX(),choicesY())
        return(c(choicesX(),choicesY()))
      })
      
      histnameParcoords <- callModule(
        dynamicSelectpicker.server, "chooseHistParcoords", label.title =  "Visualize Histograms",
        choices = choicesHistParcoords, livesearch = TRUE, selected = "None"
      )
      
      choicesBoundsParcoords <- reactive({
        req(choicesX(),choicesY(),BoundsParcoords$names)
        return(BoundsParcoords$names)
      })
      
      BoundsParcoords <- reactiveValues(names="No cutoff")
      observe({
        if (length(Xcat()) | length(Ycat())){
          BoundsParcoords$names <- c(Xcat(),Ycat())
        }
      })
      
      
      boundsnameParcoords <- callModule(
        dynamicSelectpicker.server, "chooseBoundsParcoords", label.title =  "Manual Bounds",
        choices = choicesBoundsParcoords, livesearch = TRUE, selected = "None", maxOptions = 1, abox = FALSE
      )
      
      menuson <- reactiveValues(on = FALSE)
      observe({
        req(xname(),yname())
        menuson$on <- TRUE
      })
      
      # initialize with bounds coming from DOE reactiveValues
      Xinfos <- callModule(XinfosChange.server, "bounds", DOE, data = DOE, edit.disable = TRUE)
      
      observeEvent(listmodels$finalpredfun, {
        parcoords.SMCsample$sample <- NULL
        parcoords.SMCsample$nsample <- 0
        parcoords.SMCsample$textmodal <- ''
      })
      
      observeEvent(input$Loadsampling, {
        req(!is.null(listmodels$trainedModels), Xinfos$Xinfos)
        toggleModal(session, "modalPCPfile", toggle = "open")
      })
      
      observeEvent(input$filePCP$datapath, {
        validation.header <- check.header(DOE, input$filePCP$datapath, input$separator, input$decimal)
        if (validation.header$valid){
          newData <- get.new.data.from.file(DOE, input$filePCP$datapath, input$separator, input$decimal)
          validation.newData <- check.new.data(DOE$nX, DOE$Xinfos, newData)
          if (validation.newData$valid){
            data.PCP$X <- newData
            error.msg$file <- NULL
          }else{
            error.msg$file <- validation.newData$error.msg
          }
        }else{
          error.msg$file <- validation.header$error.msg
        }
      })
      
      output$error.file <- renderUI({
        req(error.msg$file)
        list(h4(strong("Error !")), 
             HTML(paste(paste(error.msg$file, collapse = '<br/>'), '<br/> <br/>')))
      })
      
      output$PCPfilecontents <- DT::renderDataTable({
        req(data.PCP$X)
        dimd <- ncol(data.PCP$X)
        DT::datatable(
          data.PCP$X,
          extensions = c('FixedColumns','Scroller','Buttons'),filter = 'top',
          options = list(
            dom = 'Brtip',
            buttons = list(list(extend = 'colvis', columns = 1:dimd)),
            pageLength = 10, scrollX = TRUE,scrollY = 200,scroller = TRUE,fixedColumns = TRUE
          ))
      })
      
      
      PCPsample <- reactiveValues(fromMC = TRUE, fromSample = FALSE)
      
      observeEvent(input$loadPCPfile,{
        req(data.PCP$X)
        PCPsample$fromMC <- FALSE
        PCPsample$fromSample <- TRUE
        toggleModal(session, "modalPCPfile", toggle = "close")
      })
      observeEvent(input$initPCP,{
        PCPsample$fromMC <- TRUE
        PCPsample$fromSample <- FALSE
        toggleModal(session, "modalPCPfile", toggle = "close")
      })
      
      parcoordsData <- eventReactive(list(listmodels$selected, Xinfos$Xinfos, Yinfos$visu.ids, menuson$on, 
                                          parcoords.SMCsample$sample, PCPsample$fromSample, PCPsample$fromMC), {
                                            req(!is.null(listmodels$trainedModels), Xinfos$Xinfos, menuson$on, Yinfos$visu.ids)
                                            dimY <- Yinfos$nY
                                            callback <- function(i) {
                                              incProgress(1/dimY, detail = paste("Computing Prediction for Output", i,"/",dimY))
                                            }
                                            showModal(modalDialog(HTML(paste(
                                              "Depending on the number of inputs/outputs the computation may take a while.", 
                                              "If you close this window it is not advised to navigate in other panels until the computation is done.",
                                              "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
                                              size = 'l')
                                            )
                                            withProgress(message = 'Propagating through Surrogates...', value = 0, {
                                              if (PCPsample$fromSample){
                                                req(data.PCP$X)
                                                df <- try(getParcoordsDataFromFile(DOE, data.PCP$X, listmodels$finalpredfun, Yinfos, callback))
                                              }else{
                                                df <- try(getParcoordsData(DOE, settings$nobsparcoords, listmodels$finalpredfun, Xinfos$Xinfos, Yinfos, callback))
                                              }
                                            })
                                            if (!inherits(df, 'try-error')){
                                              nf <- nrow(df)
                                              if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0)){
                                                if (ncol(parcoords.SMCsample$sample) == ncol(df)){
                                                  dataSMC <- parcoords.SMCsample$sample
                                                  colnames(dataSMC) <- colnames(df)
                                                  dim <- length(colnames(dataSMC))
                                                  df <- rbind(df,dataSMC)
                                                  df$Select <- c(rep("Random",nf),rep("Inverse",nrow(dataSMC)))
                                                  df <- df[,c(dim+1,1:dim)]
                                                }
                                              }
                                            }else{
                                              logger$print(attr(df, "condition"))
                                              df <- NULL
                                            }
                                            removeModal()
                                            return(df)
                                          })
      
      selectedParCoords <- reactive({
        req(parcoordsData())
        cutoffs <- storeCutoffs$cutoffs
        if (length(cutoffs) > DOE$nX+DOE$nY){
          cutoffs <- cutoffs[-1]
        }
        allis.selected <- !unlist(lapply(cutoffs,is.null))
        if (any(allis.selected)){
          allids.selected <- which(allis.selected)
          allnames.selected <- c(DOE$xnames,DOE$ynames)[allids.selected]
          dfparcoords <- sapply(storeCutoffs$cutoffs[allids.selected],function(l) l[[1]])
          colnames(dfparcoords) <-  allnames.selected
        }else{
          dfparcoords <- NULL
        }
        return(dfparcoords)
      })
      
      output$dynui_bounds <- renderUI({
        req(boundsnameParcoords(),!is.null(Xcat()),!is.null(Ycat()),Allcat(),Allb())
        xynames <- c(DOE$xnames,DOE$ynames)
        id <- which(boundsnameParcoords()==c(DOE$xnamesmenu,DOE$ynamesmenu))
        if (length(id)){
          if (boundsnameParcoords() %in% c(Xcat(),Ycat())){
            # Categorical widget
            choices.widget <- Allcat()[[boundsnameParcoords()]]
            selected.widget <- storeCutoffs$cutoffs[[id]]
            if (!is.list(selected.widget)){
              # cutoff is null, meaning all are selected
              selected.widget <- choices.widget
            }else{
              selected.widget <- unlist(selected.widget)
            }
            t <- tagList(
              fluidRow(
                column(2,""),
                column(8, checkboxGroupButtons(inputId = ns("manualcat"), label="", choices = choices.widget, 
                                               selected = selected.widget, justified = TRUE, status = "primary", individual = TRUE,
                                               checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                                                no = icon("remove", lib = "glyphicon"))),align="center"),
                column(2,"")
              )
            )
          }else{
            # Numeric widget
            cc <- storeCutoffs$cutoffs[[id]]
            bounds <- Allb()[[boundsnameParcoords()]]
            if (is.null(cc)){
              # No cutoff
              t <- NULL
            }else{
              nc <- min(length(cc),5) # max 5 cutoffs for one column
              t <- lapply(1:nc,function(i){
                bounds.sel <- sort(unlist(cc[[i]]))
                fluidRow(
                  column(2,""),
                  column(8, sliderInput(inputId = ns(paste0('manualbounds', i)), label = "", min = bounds[1], 
                                        max = bounds[2], value = bounds.sel, width='100%'), align = "center"),
                  column(2,br(),br(),actionButton(ns(paste0('remove', i)),label="Remove", class = "btn-danger"), align = "center")
                )
              })
            }
          }
        }else{
          t <- NULL
        }
        return(t)
      })
      
      observeEvent(!is.null(input[["manualcat"]]),{
        xynames <- c(DOE$xnames,DOE$ynames)
        choices.widget <- Allcat()[[boundsnameParcoords()]]
        id <- which(boundsnameParcoords()==c(DOE$xnamesmenu,DOE$ynamesmenu))
        selected.widget <- input[["manualcat"]]
        # Update storeCutoffs
        if (all(choices.widget %in% selected.widget)){
          storeCutoffs$cutoffs[id] <- list(NULL)
        }else{
          storeCutoffs$cutoffs[[id]] <- as.list(selected.widget)
        }
        # Update pcp and spm cutoffs
        if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0)){
          updatePcpSpmCutoffs(c(list(NULL),storeCutoffs$cutoffs))
        }else{
          updatePcpSpmCutoffs(storeCutoffs$cutoffs)
        }
      })
      
      observe({ # We assume a maximum of 5 cutoffs have been applied on a column
        lapply(1:5, function(i){
          observeEvent(input[[paste0('remove', i)]], {
            xynamesmenu <- c(DOE$xnamesmenu,DOE$ynamesmenu)
            # Update storeCutoffs
            id <- which(boundsnameParcoords()==xynamesmenu)
            nc <- length(storeCutoffs$cutoffs[[id]])
            if (nc>1){
              storeCutoffs$cutoffs[[id]][i] <- NULL
            }else{
              storeCutoffs$cutoffs[id] <- list(NULL)
              # Update choices in bound picker input
              BoundsParcoords$names <- setdiff(BoundsParcoords$names,xynamesmenu[id])
              if (length(BoundsParcoords$names)==0){
                BoundsParcoords$names <- "No cutoff"
              }
              
            }
            # Update pcp and spm cutoffs
            if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0)){
              updatePcpSpmCutoffs(c(list(NULL),storeCutoffs$cutoffs))
            }else{
              updatePcpSpmCutoffs(storeCutoffs$cutoffs)
            }
          })
        })
      })
      
      observe({ # We assume a maximum of 5 cutoffs have been applied on a column
        lapply(1:5, function(i){
          observeEvent(input[[paste0('manualbounds', i)]], {
            selected.widget <- input[[paste0('manualbounds', i)]]
            # Update storeCutoffs
            id <- which(boundsnameParcoords()==c(DOE$xnamesmenu,DOE$ynamesmenu))
            storeCutoffs$cutoffs[[id]][[i]] <- as.list(selected.widget)
            # Update pcp and spm cutoffs
            if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0)){
              updatePcpSpmCutoffs(c(list(NULL),storeCutoffs$cutoffs))
            }else{
              updatePcpSpmCutoffs(storeCutoffs$cutoffs)
            }
          })
        })
      })
      
      updatePcpSpmCutoffs <- function(ppCutoffs) {
        parallelPlot::setCutoffs(ns("parcoords"), ppCutoffs)

        dimNames <- colnames(dataPCP$data)
        ppCutoffsByName <- list()
        for (i in seq_along(dataPCP$data)) {
          if (!is.null(ppCutoffs[[i]])) {
            ppCutoffsByName[[dimNames[i]]] <- ppCutoffs[[i]]
          }
        }

        setSpmCutoffsFromPP(ppCutoffsByName)
      }

      datavisu <- reactiveVal(NULL)

      dataPCP <- reactiveValues(data=NULL,categorical=NULL,rotateTitle=NULL,columnLabels=NULL,
                                zAxisDim=NULL, refColumnDim=NULL, keptColumns=NULL, histoVisibility=NULL,
                                refRowIndex=NULL,cutoffs=NULL,init=FALSE)
      storeCutoffs <- reactiveValues(cutoffs=NULL)
      
      # Initialize parallel plot data
      observe({
        req(menuson$on, parcoordsData(), 
            isolate(length(DOE$ynames)==DOE$nY))
        
        isolate({
          xynames <- c(DOE$xnames,DOE$ynames)
          xynamesvisu <- c(DOE$xnamesvisu,DOE$ynamesvisu)
          xynamesmenu <- c(DOE$xnamesmenu,DOE$ynamesmenu)
          idrows <- 1:nrow(DOE$XY)
          datanorm <- DOE$XY[idrows,xynames]
          categorical <- lapply(1:DOE$nX, function(i) {
            if (DOE$Xinfos[[i]]$type == "categorical") {
              return(as.character(DOE$Xinfos[[i]]$levels))
            }
            return(NULL)
          })
          categorical <- c(categorical, lapply(1:DOE$nY,function(i){
            if (DOE$Yinfos$type[i] == "categorical" && !(i %in% DOE$Yinfos$const.ids)) {
              return(as.character(unique(DOE$Y[,i])))
            }
            return(NULL)
          }))
        })
        
        names(categorical) <- xynames
        
        idvisu <- xynamesmenu %in% isolate(c(choicesX(),selectedY()))
        idhist <- xynamesmenu %in% isolate(histnameParcoords())

        df <- parcoordsData()
        
        if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0) ){
          categorical <- c(list(Select=c("Random","Inverse")),categorical)
          categorical.final <- categorical[c("Select",xynames)]
          xynamesvisu.final <- c("Select",xynamesvisu)
          refColumnDim <- "Select"
          zAxisDim <- "Select"
          idvisu <- c(TRUE,idvisu)
          idhist <- c(FALSE,idhist)
        }else{
          categorical.final <- categorical[xynames]
          xynamesvisu.final <- xynamesvisu
          refColumnDim <- NULL
          zAxisDim <- NULL
        }
        names(categorical.final) <- NULL
        names(xynamesvisu.final) <- NULL
        refRowIndex <- NULL
        if (isolate(input$keepbounds)){
          cutoffs <- storeCutoffs$cutoffs
          if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0)){
            cutoffs <- c(vector('list',1),cutoffs)
          }
        }else{
          cutoffs <- NULL
        }
        idNosurrogate <- isolate(DOE$nX + as.numeric(which(is.na(listmodels$selected$id))))
        categorical.final[idNosurrogate] <- list(NULL)
        datavisu(df)
        dataPCP$data <- df
        dataPCP$categorical <- categorical.final
        dataPCP$arrangeMethod <- isolate(input$arrange.method)
        dataPCP$rotateTitle <- DOE$adapt.visu
        dataPCP$columnLabels <- xynamesvisu.final
        dataPCP$refColumnDim <- refColumnDim
        dataPCP$zAxisDim <- zAxisDim
        dataPCP$keptColumns <- idvisu
        dataPCP$histoVisibility <- idhist
        dataPCP$refRowIndex <- refRowIndex
        dataPCP$continuousCS <- isolate(input$choose.palette.num)
        dataPCP$categoricalCS <- isolate(input$choose.palette.cat)
        dataPCP$cutoffs <- cutoffs
        dataPCP$init <- TRUE
      })
      
      # Initialize parallel plot
      output$parcoords <- renderParallelPlot({
        req(dataPCP$init,dataPCP$data)
        isolate({
          parallelPlot(
            data = datavisu(),
            categorical= dataPCP$categorical,
            arrangeMethod = dataPCP$arrangeMethod,
            rotateTitle = dataPCP$rotateTitle,
            columnLabels = dataPCP$columnLabels,
            refColumnDim = dataPCP$refColumnDim,
            keptColumns = dataPCP$keptColumns,
            histoVisibility = dataPCP$histoVisibility,
            refRowIndex = dataPCP$refRowIndex,
            continuousCS = dataPCP$continuousCS,
            categoricalCS = dataPCP$categoricalCS,
            cutoffs = dataPCP$cutoffs,
            controlWidgets = NULL,
            eventInputId = ns("pcpEvent")
          )
        })
      })
      
      # Initialize scatterPlotMatrix
      output$scatterPlotMatrix <- renderScatterPlotMatrix({
        req(dataPCP$init,dataPCP$data)
        isolate({
          scatterPlotMatrix(
            data = datavisu(),
            categorical = dataPCP$categorical,
            rotateTitle = DOE$adapt.visu,
            columnLabels = dataPCP$columnLabels,
            zAxisDim = dataPCP$zAxisDim,
            keptColumns = dataPCP$keptColumns,
            distribType = as.numeric(isolate(input$distribType)),
            corrPlotType = as.character(isolate(input$corrPlotType)),
            corrPlotCS = as.character(isolate(input$corrPlotCs)),
            continuousCS = input$choose.palette.num,
            categoricalCS = input$choose.palette.cat,
            cutoffs = dataPCP$cutoffs,
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
    
      # If selected columns have been changed ...
      observeEvent(c(input$chooseXPCPclosed,input$chooseYPCPclosed), {
        req(length(union(isolate(xname()), isolate(yname())))>1)
        id <- c(DOE$xnamesmenu,DOE$ynamesmenu) %in% c(isolate(xname()), isolate(yname()))
        if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0) ){
          id <- c(TRUE,id)
        }
        if (any(id!=dataPCP$keptColumns)){
          dataPCP$keptColumns <- id
          parallelPlot::setKeptColumns(ns("parcoords"), id)
          scatterPlotMatrix::setKeptColumns(ns("scatterPlotMatrix"), id)
        }
      })
      
      # If selected histograms have been changed ...
      observeEvent(c(input$chooseHistPCPclosed), {
        id <- c(DOE$xnamesmenu,DOE$ynamesmenu) %in% isolate(histnameParcoords())
        if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0) ){
          id <- c(TRUE,id)
        }
        if (any(id!=dataPCP$histoVisibility)){
          dataPCP$histoVisibility <- id
          parallelPlot::setHistoVisibility(ns("parcoords"), id)
        }
      })
      
      # If continuous palette has been changed ...
      observeEvent(input$choose.palette.num, {
        dataPCP$continuousCS <- input$choose.palette.num
        parallelPlot::setContinuousColorScale(ns("parcoords"), input$choose.palette.num)
        scatterPlotMatrix::setContinuousColorScale(ns("scatterPlotMatrix"), input$choose.palette.num)
      })
      
      # If categorical palette has been changed ...
      observeEvent(input$choose.palette.cat, {
        dataPCP$categoricalCS <- input$choose.palette.cat
        parallelPlot::setCategoricalColorScale(ns("parcoords"), input$choose.palette.cat)
        scatterPlotMatrix::setCategoricalColorScale(ns("scatterPlotMatrix"), input$choose.palette.cat)
      })
      
      # If arrange method has been changed ...
      observeEvent(input$arrange.method, {
        dataPCP$arrangeMethod <- input$arrange.method
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
      
      # Store cutoffs (may be necessary)
      observe({
        req(DOE$XY,parcoordsData())
        if (is.null(isolate(input$keepbounds)) || !input$keepbounds){
          ll <- vector('list',DOE$nX+DOE$nY)
          storeCutoffs$cutoffs <- ll
        }
      })
      
      observeEvent(input$pcpEvent,{
        if (!is.null(input$pcpEvent) && input$pcpEvent$type == "cutoffChange"){
          updatedDim <- input$pcpEvent$value$updatedDim
          if (updatedDim != "Select"){
            xynamesmenu <- c(DOE$xnamesmenu,DOE$ynamesmenu)
            id <- which(updatedDim==c(DOE$xnames,DOE$ynames))
            cutoffs.temp <- input$pcpEvent$value$cutoffs[[updatedDim]]
            nc <- length(cutoffs.temp)
            if (nc == 0){
              if (is.list(cutoffs.temp)){
                # All categories disabled
                storeCutoffs$cutoffs[[id]] <- list()
              }else{
                # All categories selected or cutoff removed on a numeric column
                storeCutoffs$cutoffs[id] <- list(NULL)
                if (!(xynamesmenu[id]%in%c(Xcat(),Ycat()))){
                  BoundsParcoords$names <- setdiff(BoundsParcoords$names,xynamesmenu[id])
                  if (length(BoundsParcoords$names)==0){
                    BoundsParcoords$names <- "No cutoff"
                  }
                }
              }
            }else{
              ncj <- length(cutoffs.temp[[1]])
              if (ncj==1){
                # Categorical
                cutoffs <- cutoffs.temp
              }else{
                cutoffs <- lapply(1:nc,function(j) sort(unlist(cutoffs.temp[[j]])))
                if ("No cutoff" %in% BoundsParcoords$names){
                  BoundsParcoords$names <- xynamesmenu[id]
                }else{
                  BoundsParcoords$names <- union(BoundsParcoords$names,xynamesmenu[id])
                }
              }
              storeCutoffs$cutoffs[[id]] <- cutoffs
            }
          }
        }
      })
      
      observeEvent(input$display,{
        # Assign selected traces to brushedData reactive
        parallelPlot::getValue(ns("parcoords"), "SelectedTraces", ns("brushedData"))
      })
      
      
      output$brushed <- DT::renderDataTable({
        req(input$brushedData, input$display)
        df <- parcoordsData()[input$brushedData, dataPCP$keptColumns]
        dimd <- ncol(df)
        dfNames <- c(DOE$xnamesvisu, DOE$ynamesvisu)
        if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0) ){
          dfNames <- c("Select", dfNames)
        }
        colnames(df) <- dfNames[dataPCP$keptColumns]
        DT::datatable(
          df, escape = FALSE,
          extensions = c('FixedColumns','Scroller','Buttons'),
          filter = 'top',
          options = list(
            dom = 'Btip',
            buttons = list(list(extend = 'colvis', columns = 1:dimd)),
            scrollX = TRUE,scrollY = 400,scroller = TRUE, fixedColumns = TRUE
          )
        )
      })
      
      output$tableselectedParCoords <- DT::renderDataTable(selectedParCoords(),rownames = TRUE,escape=FALSE,options = list(
        dom = 't'))
      output$tableselectedParCoords2 <- DT::renderDataTable(selectedParCoords(),rownames = TRUE,escape=FALSE,options = list(
        dom = 't'))
      
      output$download <- downloadHandler(
        filename = 'Parcoords.csv',
        content = function(con) {
          df <- parcoordsData()[input$brushedData, dataPCP$keptColumns]
          dimd <- ncol(df)
          dfNames <- c(DOE$xnamesvisu, DOE$ynamesvisu)
          if (!is.null(parcoords.SMCsample$sample) & (parcoords.SMCsample$nsample >0) ){
            dfNames <- c("Select", dfNames)
          }
          colnames(df) <- dfNames[dataPCP$keptColumns]
          write.table(x = df, file = con, row.names = F, col.names = T, sep = ",")
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
            dimNames <- colnames(dataPCP$data)
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
                ppCutoffs[[xDim]] <- appendPPCutoff(ppCutoffs[[xDim]], xyCutoff[1], levels(dataPCP$data[[which(dimNames == xDim)]]))
                ppCutoffs[[yDim]] <- appendPPCutoff(ppCutoffs[[yDim]], xyCutoff[2], levels(dataPCP$data[[which(dimNames == yDim)]]))
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
          dimNames <- colnames(dataPCP$data)
          spmCutoffs <- vector()
          for (dimName in names(ppCutoffs)) {
            ppCutoff <- ppCutoffs[[dimName]]
            if (!is.null(ppCutoff)) {
              spCutoff <- list(xDim = dimName, yDim = dimName)
              if (!is.null(dataPCP$categorical[[which(dimNames == dimName)]])) { #
                ppCutoff <- Filter(function(e) { return(e %in% unique(dataPCP$data[[dimName]]))}, ppCutoff) # Workaround (bug in 'spm.setCutoffs' when a category is not used in data)
                categories <- dataPCP$categorical[[which(dimNames == dimName)]]
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

      output$dataView <- renderUI({
        if (!input$display) {return(NULL)}
        req(input$brushedData)
        tagList(
          DT::dataTableOutput(ns("brushed")),
          br(),
          downloadButton(ns("download"), label = "Export Data", class = "btn-primary")
        )
      })
      
      observeEvent(input$SMCsettings, {
        req(parcoordsData(),!all(unlist(lapply(storeCutoffs$cutoffs,is.null))),length(listmodels$categorical)==0)
        parcoords.SMCsample$text <- ""
        toggleModal(session, "modalSMC", toggle = "open")
      })
      
      observeEvent(list(listmodels$categorical, storeCutoffs$cutoffs), {
        idOutputs <- which(!sapply(storeCutoffs$cutoffs, is.null)) - DOE$nX
        idOutputs <- idOutputs[idOutputs > 0]
        any_categorical_constraints <- any(Yinfos$type[idOutputs] == 'categorical')
        if (length(listmodels$categorical) > 0 || any_categorical_constraints){
          disable('SMCsettings')
        }else{
          enable('SMCsettings')
        }
      })
      
      parcoords.SMCsample <- reactiveValues(sample = NULL, nsample = 0, textmodal='')
      observeEvent(input$launchSMC,{
        iterSMC <- input$nSMCiter
        nsampleSMC <- input$nSMCsample
        callback <- function(i) {
          incProgress(1/iterSMC, detail = paste("Iteration", i,"/",iterSMC))
        }
        showModal(modalDialog(HTML(paste(
          "Depending on the problem difficulty and the number of constraints the computation may take a while.", 
          "If you close this window it is not advised to navigate in other panels until the computation is done.",
          "This window will close automatically when the task is finished.", sep = '<br/>')), title = "Warning",
          size = 'l')
        )
        
        allnames.selected <- colnames(selectedParCoords())
        idY <- intersect(allnames.selected,DOE$ynames)
        nY <- length(idY)

        if (nY>0){
          sampleSMC <- try(SCMC_withcallback(selectedParCoords(),DOE,Xinfos$Xinfos,listmodels$finalpredfun,nsampleSMC,iterSMC,callback))
          if (class(sampleSMC) != 'try-error'){
            if (sampleSMC$nsample > 1){
              sampleSMC.x <- as.data.frame(sampleSMC$sample)
              sampleSMC.y <- as.data.frame(matrix(NA, nrow(sampleSMC.x), DOE$nY))
              for (j in Yinfos$visu.ids){
                sampleSMC.y[, j] <- listmodels$finalpredfun(sampleSMC.x, j)
              }
              parcoords.SMCsample$sample <- cbind(sampleSMC.x,sampleSMC.y)
              parcoords.SMCsample$nsample <- sampleSMC$nsample
              parcoords.SMCsample$text <- ""
              toggleModal(session, "modalSMC", toggle = "close")
            }else{
              parcoords.SMCsample$nsample <- sampleSMC$nsample
              parcoords.SMCsample$text <- "Sorry, the inverse sampling algorithm did not find any combination 
                satisfying all constraints. Try increasing the sample size and/or the number of iterations."
            }
          }else{
            parcoords.SMCsample$sample <- NULL
            parcoords.SMCsample$nsample <- 0
            parcoords.SMCsample$text <- "Sorry, the inverse sampling algorithm did not converge. Try increasing the sample size and/or the number of iterations."
          }
        }else{
          Xlowerupper <- get.lowerupper(selectedParCoords(),DOE,Xinfos$Xinfos)
          lower <- Xlowerupper[1,]
          upper <- Xlowerupper[2,]
          rge <- cbind(lower,upper)
          N <- 1000
          sampleSMC.x <- as.data.frame(unrestricted(N, range=rge))
          sampleSMC.y <- as.data.frame(matrix(NA, nrow(sampleSMC.x), DOE$nY))
          for (j in Yinfos$visu.ids){
            sampleSMC.y[, j] <- listmodels$finalpredfun(sampleSMC.x, j)
          }
          parcoords.SMCsample$sample <- cbind(sampleSMC.x,sampleSMC.y)
          parcoords.SMCsample$nsample <- N
          parcoords.SMCsample$text <- ""
          toggleModal(session, "modalSMC", toggle = "close")
        }
        removeModal()
      })
      
      output$SMCtext<- renderUI({
        req(parcoords.SMCsample$text)
        h3(parcoords.SMCsample$text)
      })
    }
  )
}
