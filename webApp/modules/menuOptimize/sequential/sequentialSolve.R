#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module sequentialSolve
source("modules/shared/XinfosChange.R", local = TRUE)
source("modules/shared/dynamicSelect.R", local = TRUE)
source("modules/menuOptimize/sequential/sequentialOptimizers.R", local = TRUE)

respect.constraints <- function(Y,signconstr,thconstr){
  return(apply(Y,1,function(x) {x*signconstr > signconstr*thconstr}))
}

getlbub <- function(signs,thresholds){
  lb <- -Inf
  ub <- Inf
  id.neg <- which(signs==-1)
  if (length(id.neg)){
    ub <- min(thresholds[id.neg])
  }
  id.pos <- which(signs==1)
  if (length(id.pos)){
    lb <- max(thresholds[id.pos])
  }
  return(c(lb,ub))
}

update.plots.pred <- function(DOE,COformulation,seq,simulations,nitermax,session,npts){

  npred <- DOE$nobs+(1:npts)
  ypred <- simulations$Ypredadd[npred,COformulation$idO]

  xAdd <- as.character(seq$ninit + rep((DOE$nobs - seq$ninit) / npts + 1, npts))
  if (npts == 1) {
    xAdd <- paste0(xAdd, "_")
  }
  else {
    xAdd <- paste0(xAdd, "_", c(1, npts))
  }

  if (seq$nconstr>0){
    rpred <- respect.constraints(isolate(simulations$Ypredadd[npred,COformulation$idC,drop=FALSE]),COformulation$COsign,COformulation$COt)
    if (seq$nconstr>1){
      allrpred <- apply(rpred,2,all)
    }else{
      allrpred <- rpred
    }
    ind0 <- xAdd[allrpred]
    if (length(ind0) > 0){
      # Extend trace with Pred - Feasible
      plotlyProxy("plotObj", session) %>%
        plotlyProxyInvoke(
          "extendTraces", 
          list(
            y = list(as.list(ypred[allrpred])), 
            x = list(as.list(ind0))
          ), 
          list(2)
        )
    }
    ind1 <- xAdd[!allrpred]
    if (length(ind1) > 0){
      # Extend trace with Pred - Non Feasible
      plotlyProxy("plotObj", session) %>%
        plotlyProxyInvoke(
          "extendTraces", 
          list(
            y = list(as.list(ypred[!allrpred])), 
            x = list(as.list(ind1))
          ), 
          list(3)
        )
    }
  }else{
    # Extend trace with Pred
    plotlyProxy("plotObj", session) %>%
      plotlyProxyInvoke(
        "extendTraces", 
        list(
          y = list(as.list(ypred)), 
          x = list(as.list(xAdd))
        ), 
        list(1)
      )
  }
}

update.plots <- function(DOE,COformulation,seq,simulations,nitermax,session,npts){

  ally <- DOE$Y[1:DOE$nobs,COformulation$idO]
  n <- DOE$nobs - npts + (1:npts)

  xAdd <- as.character(seq$ninit + rep((DOE$nobs - seq$ninit) / npts, npts))
  if (npts == 1) {
    xAdd <- paste0(xAdd, "_")
  }
  else {
    xAdd <- paste0(xAdd, "_", c(1, npts))
  }

  if (seq$nconstr>0){
    r <- respect.constraints(isolate(DOE$Y[1:DOE$nobs,COformulation$idC,drop=FALSE]),COformulation$COsign,COformulation$COt)
    if (seq$nconstr>1){
      allr <- apply(r,2,all)
    }else{
      allr <- r
    }
    ind0 <- n[allr[n]]
    if (length(ind0) > 0){
      # Extend trace with True - Feasible
      plotlyProxy("plotObj", session) %>%
        plotlyProxyInvoke(
          "extendTraces", 
          list(
            y = list(as.list(ally[ind0])), 
            x = list(as.list(xAdd[allr[n]]))
          ), 
          list(0)
        )
    }
    ind1 <- n[!allr[n]]
    if (length(ind1) > 0){
      # Extend trace with True - Non Feasible
      plotlyProxy("plotObj", session) %>%
        plotlyProxyInvoke(
          "extendTraces", 
          list(
            y = list(as.list(ally[ind1])), 
            x = list(as.list(xAdd[!allr[n]]))
          ), 
          list(1)
        )
    }
    
    # Constraints heatmap
    rhp <- ifelse(r, 1, 0)
    rhp <- matrix(as.numeric(r), ncol = seq$ninit + seq$niter * npts, byrow = FALSE)

    # Hack to emulate a categorical color scale
    colors <- c("red", "green")
    if (all(r == 1)) {
      colors <- c("green", "green")
    }
    else if (all(r == 0)) {
      colors <- c("red", "red")
    }

    textz <- matrix(rep("", length(COformulation$idC) * (seq$ninit + nitermax * npts)), nrow = length(COformulation$idC))
    textz[rhp==0] <- "Non-Feasible"
    textz[rhp==1] <- "Feasible"
    plotlyProxy("plotConstr", session) %>%
      plotlyProxyInvoke(
        "restyle",
        list(
          z = list(rhp),
          text = list(textz),
          colors = colors
        ),
        list(0)
      )
  }else{
    # Extend trace with True
    plotlyProxy("plotObj", session) %>%
      plotlyProxyInvoke(
        "extendTraces", 
        list(
          y = list(as.list(ally[n])), 
          x = list(as.list(xAdd))
        ), 
        list(0)
      )
  }

  # Q2 plot
  plotlyProxy("plotQ2", session) %>%
    plotlyProxyInvoke(
      "extendTraces", 
      list(
        y = lapply(as.vector(seq$idmodels),function (i){return(list(seq$Q2[i,seq$ninit+seq$niter]))}), 
        x = lapply(as.vector(seq$idmodels),function (i){return(list(seq$ninit+seq$niter))})
      ), 
      as.list(0:(length(seq$idmodels)-1))
    )
}

computeDistToDOE <- function(DOE, Xadd){
  numInd <- sapply(DOE$Xinfos, function(x){x$type}) == 'numeric'
  min(apply(DOE$X[, numInd, drop=F], 1, function(x){
    min(apply(Xadd[, numInd, drop=F], 1, function(z){
      sqrt(sum((x - z)^2))
    }))
  }))/sqrt(sum(apply(get.bounds(DOE$Xinfos), 2, function(x){(x[1] - x[2])^2})))
}


sequentialSolve.ui <- function(id) {
  ns <- NS(id)
  
  IterModal <- bsModal(
    ns("modalIter"), "Iteration Analysis", NULL,
    fluidRow(
      column(12,
             uiOutput(ns('sliderIter.dynui')),
             align="center")
    ),
    hr(),
    h5("Constraint Visualization: DOE quantiles, thresholds and current iteration. Constraints are normalized for easier comparison."),
    plotlyOutput(ns("plotRadar")),
    hr(),
    fluidRow(
      column(12,
             h5("Selected Point"),
             align="center")
    ),
    DT::dataTableOutput(ns('Xaddcontents'))
    ,size="large"
  )
  
  SettingsModal <- bsModal(
    ns("modalSettings"), "Advanced Optimizer Settings", NULL,
    uiOutput(ns('settings.dynui'))
    ,size="large"
  )
  
  convergedOptimModal <- bsModal(
    ns("modalConvergedOptim"), "Warning: optimization Converged !", NULL,
    fluidRow(
      column(12, h4("The next point to simulate is very close to the current DOE : the optimization may have converged."))
    )
    ,size="large"
  )
  
  failOptimModalStep1 <- bsModal(
    ns("modalfailOptimStep1"), "Error: fail to find next points !", NULL,
    fluidRow(
      column(12, h4("Cannot find next points to simulate. If multiple points by iteration, 
                    this error may come from convergence problems of the metamodel update between iterations.
                    Try to restart optimization with only one point by iteration."))
    )
    ,size="large"
  )
  
  failOptimModalStep3 <- bsModal(
    ns("modalfailOptimStep3"), "Error: metamodel cannot be updated !", NULL,
    fluidRow(
      column(12, h4("Cannot update metamodel with the new simulated points : the optimization may have converged."))
    )
    ,size="large"
  )
  
  
  panel1 <- fluidRow(
    column(3, dynamicSelect.ui(ns("chooseOptimAlgo")), align="center"),
    column(3, numericInput(ns("nitermax"),"Choose Max. Iterations",1,min=1), align="center"),
    column(3, numericInput(ns("nptsiter"),"Choose Nb Pts / Iter",1,min=1), align="center"),
    column(3, actionButton(ns("optimsettings"), label = "Optimizer Advanced Settings", class = "btn-primary"), align="center")
  )
  
  panel2 <- wellPanel(
    fluidRow(
      tags$head(
        tags$script(
          HTML("
               Shiny.addCustomMessageHandler ('print',function (message) {
               $('#'+message.selector).html(message.html);
               console.log(message);
               });
               ")
        )
      ),
      column(2, 
             br(), uiOutput(ns("launchButtonUI")), br(), br(), uiOutput(ns("tagOptimUI"))
      ),
      column(2, 
             br(),
             actionButton(ns("stopOptim"), label = HTML(paste("Stop Sequential","Optimization",sep='<br>')), class = "btn-warning", icon = icon("stop-circle"),
                          width = '100%'),
             br()
      ), 
      column(2, 
             br(),
             actionButton(ns("restartOptim"), label = HTML(paste("Restart Sequential","Optimization",sep='<br>')), class = "btn-warning", icon = icon("stop-circle"),
                          width = '100%'),
             br()
      ), 
      column(6,
             textOutput(ns("text_iterations")),
             br(),
             textOutput(ns("text_step")),
             br(),
             actionButton(ns("studyIter"), label = "Iteration Analysis", class = "btn-primary",width = '100%')
             , align="center"
      )
    )
  )
  
  rowSetting <- tagList(
    panel1,
    panel2,
    IterModal,
    SettingsModal,
    convergedOptimModal,
    failOptimModalStep1,
    failOptimModalStep3
  )
  
  tagList(
    rowSetting,br(),uiOutput(ns("rowvisu.dynui"))
  )
}

sequentialSolve.server <- function(input, output, session, DOE, listmodels, Xinfos, COformulation, advance.importDOE, settings, doeProblemDef) {
  
  ns <- session$ns
  
  use_simulator <- reactive({
    bool <- FALSE
    if (!is.null(doeProblemDef$choice)){
      bool <- (doeProblemDef$choice != 1)
    }
    return(bool)
  })

  # Advanced Optim settings
  listsettings.init <- reactiveValues(settings=NULL,n=0)
  listsettings <- reactiveValues(settings=NULL)
  
  output$settings.dynui <- renderUI({
    l <- lapply(1:listsettings.init$n, function(i){
      if (listsettings.init$settings[[i]]$type=="choice"){
        return(
          fluidRow(
            column(12,selectInput(ns(paste0('defsettings', i)), label=listsettings.init$settings[[i]]$name, choices = listsettings.init$settings[[i]]$choices,
                                  selected = listsettings.init$settings[[i]]$default.value))
          )
        )
      }
      if (listsettings.init$settings[[i]]$type=="switch"){
        return(
          fluidRow(
            column(12,switchInput(ns(paste0('defsettings', i)),label=listsettings.init$settings[[i]]$name,value=listsettings.init$settings[[i]]$default.value))
          )
        )
      }
      if (listsettings.init$settings[[i]]$type=="numeric"){
        return(
          fluidRow(
            column(12,numericInput(ns(paste0('defsettings', i)), label=listsettings.init$settings[[i]]$name, value=listsettings.init$settings[[i]]$default.value, min=1))
          )
        )
      }
    })
  })
  
  manual.trigger.settings <- reactive({
    req(algoname(),listsettings.init$n>0)
    lapply(1:listsettings.init$n, function(i){
      input[[paste0('defsettings', i)]]
    })
  })
  
  observeEvent(manual.trigger.settings(), {
    req(algoname(),listsettings.init$n>0)
    l <- list()
    for (i in 1:listsettings.init$n){
      if (!is.null(input[[paste0('defsettings', i)]])){
         l[[i]] <- input[[paste0('defsettings', i)]]
      }else{
        l[[i]] <- listsettings.init$settings[[i]]$default.value
      }
      listsettings$settings <- l
    }
  })
  
  observe({
    req(algoname())
    listsettingstemp <- do.call(paste0(algoname(),".seq.settings"),list())
    n <- length(listsettingstemp)
    if (n>0){
      listsettings.init$n <- n
      listsettings.init$settings <- listsettingstemp
    }
  })
  
  observeEvent(input$optimsettings, {
    toggleModal(session, "modalSettings", toggle = "open")
  })
  
  # Settings for iteration selection to study
  studyiter <- reactiveValues(id = NULL)
  
  observeEvent(input$studyIter, {
    studyiter$id <- isolate(seq$niter)
    toggleModal(session, "modalIter", toggle = "open")
  })
  
  output$sliderIter.dynui <- renderUI({
    req(listmodels$finalpredfun, Xinfos$Xinfos, seq$ninit, studyiter$id, 
        COformulation$idO, COformulation$idC,seq$ninit,input$nitermax,input$nptsiter)
    # Can only study finished iterations
    maxselect <- (studyiter$id - 1) * input$nptsiter
    choices <- addedCategories()
    sliderTextInput(
      inputId = ns("selected.iter"),
      label = "Choose Iteration:",
      choices = choices,
      selected = choices[maxselect],
      from_min = choices[1],
      from_max = choices[maxselect]
    )
  })
  
  observeEvent(input$selected.iter,{
    req(input$selected.iter)
    # Update Radar plot
    idtrace <- nrow(rplot$quantiles) + 2
    selectedIndex <- match(input$selected.iter, addedCategories())
    init.df <- isolate(DOE$Y[seq$ninit + selectedIndex, COformulation$idC, drop = FALSE])
    colmarkers <- c("red","green")[1+as.numeric(init.df * COformulation$COsign > COformulation$COt * COformulation$COsign)]
    df <- (init.df - rplot$lb)/(rplot$ub-rplot$lb)
    df <- as.matrix(df)
    plotlyProxy("plotRadar", session) %>%
      plotlyProxyInvoke(
        "deleteTraces",
        list(idtrace)
      )
    plotlyProxy("plotRadar", session) %>%
      plotlyProxyInvoke(
        "addTraces",
        list(
          r = c(df, df[1]),
          theta = c(rplot$cnames, rplot$cnames[1]),
          type = "scatterpolar",
          fill = "toself",
          fillcolor = "transparent",
          mode = "markers+lines",
          name = paste("Iteration", input$selected.iter),
          line = list(color = "grey"),
          marker = list(symbol = "square", color = c(colmarkers, colmarkers[1]))
        ),
        list(idtrace)
      )
  })
  
  
  # Choice of seq optim algorithm
  choicesOptimAlgo <- reactive({
    available.seq.optimizers()
  })
  algoname <- callModule(dynamicSelect.server, "chooseOptimAlgo", label = "Choose Optimizer", choicesOptimAlgo)
  
  simulations <- reactiveValues(Xadd = NULL, Ypredadd = NULL, launch.simu = FALSE, is.running = FALSE,
                                nOptim = 1, tagOptim = NULL, tagLaunchBtn = HTML(paste("Launch Sequential","Optimization",sep='<br>')))
  
  seq <- reactiveValues(ninit = 0, niter = 0, idmodels = NULL, models = NULL, idconstr = NULL, nconstr = 0, 
                        Q2 = NULL, stop.asked = FALSE, do.step1 = FALSE, do.step2 = FALSE, do.step3 = FALSE, finish = FALSE)
  
  rplot <- reactiveValues(quantiles = NULL, seuils = NULL, cnames = NULL, lb = NULL, ub = NULL, range = NULL)
  
  observeEvent(list(DOE$Xinfos, DOE$Yinfos), {
    COformulation$idC <- NULL
    COformulation$idO <- NULL
    COformulation$COsign <- NULL
    COformulation$COt <- NULL
    COformulation$COobj <- NULL
    COformulation$thresholds <- NULL
    COformulation$optimTypes <- NULL
  })
  
  # reinitialize seq optim when Xinfos or COformulation is modified or user hit restartOptim button
  observeEvent(list(input$restartOptim, Xinfos$Xinfos, COformulation$idC, COformulation$idO, 
                    COformulation$COsign, COformulation$COt, COformulation$COobj,
                    COformulation$thresholds, COformulation$optimTypes), {
    seq$niter <- 0
    enableActionButton(ns("goOptim"),session)
    simulations$tagOptim <- paste("Optim", simulations$nOptim)
    updateTextInput(session, ns("tagOptim"), value = simulations$tagOptim)
    enableActionButton(ns("nitermax"),session)
    enableActionButton(ns("nptsiter"),session)
    simulations$tagLaunchBtn <- HTML(paste("Launch Sequential","Optimization",sep='<br>'))
  })

  observe({
    req(listmodels$models, listmodels$selected, length(listmodels$selected$id) == DOE$nY)
    seq$models <- lapply(1:DOE$nY, function (i){
      if (length(listmodels$selected$id[[i]])){
        return(listmodels$models[[listmodels$selected$id[[i]]]][[i]])
      }else{
        return(NULL)
      }
    })
  })

  observe({
    # Initialize before lauching seq optim
    req(listmodels$finalpredfun, Xinfos$Xinfos, seq$niter==0)
    disableActionButton(ns("stopOptim"),session)
    disableActionButton(ns("restartOptim"),session)
    disableActionButton(ns("studyIter"),session)
    seq$ninit <- DOE$nobs
  })
  
  observe({
    # Initialize once optim settings are given
    req(listmodels$finalpredfun, Xinfos$Xinfos, seq$niter==0, COformulation$idO,seq$ninit,input$nitermax,input$nptsiter, use_simulator())
    signs <- COformulation$COsign
    thresholds <- COformulation$COt
    idO <- COformulation$idO
    idOinC <- which(idO==COformulation$idC)
    if (length(idOinC) > 0){
      # Objective appears in constraints
      # First get the bounds on the objective
      boundsO <- getlbub(signs[idOinC],thresholds[idOinC])
      # Then get the bounds on the constraints
      idC <- COformulation$idC[!idOinC]
    }else{
      boundsO <- c(-Inf,Inf)
      idC <- COformulation$idC
    }
    # Get the bounds on the constraints
    idC.unique <- unique(c(idC))
    nu <- length(idC.unique)
    boundsC <- matrix(NA,2,nu)
    for (i in seq_len(nu)){
      ids <- which(idC==idC.unique[i])
      boundsC[,i]<-getlbub(signs[ids],thresholds[ids])
    }
    
    # Variables used in the seq optimizer
    seq$idmodels <- c(idO,idC.unique)
    seq$idconstr <- 1+seq_len(nu)
    seq$boundsO <- boundsO
    seq$boundsC <- boundsC
    # Variables used in the displays
    seq$nconstr <- length(COformulation$idC)
    Q2init <- unlist(lapply(1:DOE$nY,function (i){
      if (is.null(seq$models[[i]])) NA else seq$models[[i]]$Q2loo
    }))
    seq$Q2 <- cbind(matrix(Q2init,DOE$nY,seq$ninit),matrix(NA,DOE$nY,input$nitermax))
    # Initialize predictions
    Ypredadd <- matrix(NA,seq$ninit,DOE$nY)
    for (i in c(COformulation$idO,COformulation$idC)){
      p <- predict.metamodel(seq$models[[i]],isolate(DOE$X))
      Ypredadd[,i] <- p$mean
    }
    simulations$Ypredadd <- Ypredadd
    
    # Initialize constraint quantiles with DOE for radar plot
    nC <- length(COformulation$idC)
    if (nC){
      df <- DOE$Y[,COformulation$idC,drop=F]
      rplot$cnames <- paste0(DOE$ynamesmenu[COformulation$idC],tablesign[(as.numeric(COformulation$COsign) + 3)/2],COformulation$COt)
      seuils <- COformulation$COt
      ub <- apply(df,2,max)
      lb <- apply(df,2,min)
      dd <- scale(df,center=lb,scale=(ub-lb))
      news <- (seuils-lb)/(ub-lb)
      newub <- rep(1,nC); newlb <- rep(0,nC)
      idup <- which(news>1); newub[idup] <- 2 * news[idup] 
      idlow <- which(news<0); newlb[idlow] <- 2 * news[idlow] - 1 
      rplot$range <- c(min(newlb),max(newub))
      qq <- apply(dd,2,quantile,probs=c(0.25,0.5,0.75))
      rplot$quantiles <- qq
      rplot$seuils <- news
      rplot$lb <- lb
      rplot$ub <- ub
    }
    
    session$sendCustomMessage(type = 'print', message = list(selector = ns('text_iterations'),
                                                             html = paste0("Iteration 0/",input$nitermax)))
    session$sendCustomMessage(type = 'print', message = list(selector = ns('text_step'),
                                                             html = "Optimization not launched"))
  })
  
  output$launchButtonUI <- renderUI({column(12,
      actionButton(ns("goOptim"), label = simulations$tagLaunchBtn, class = "btn-primary", icon = icon("play-circle"),
                   width = '100%'))
  })

  output$plotRadar <- renderPlotly({
    req(rplot$quantiles, rplot$seuils, cancelOutput = TRUE)
    qq <- rplot$quantiles
    qrnames <- paste("DOE", rownames(qq))
    news <- rplot$seuils
    cnames <- rplot$cnames
    nqq <- nrow(qq)
    p <- plot_ly(
      type = "scatterpolar",
      fill = "toself", fillcolor = "transparent",
      mode = "lines"
    )
    for (i in 1:nqq) {
      p <- add_trace(p,
        r = c(qq[i, ], qq[i, 1]),
        theta = c(cnames, cnames[1]),
        name = qrnames[i],
        marker = list(symbol = "x", color = "black"),
        line = list(color = "black", dash = "dash"),
        mode = "markers+lines"
      )
    }
    p <- add_trace(p,
      r = c(news, news[1]),
      theta = c(cnames, cnames[1]),
      name = "Threshold",
      marker = list(color = "orange"),
      line = list(color = "orange"),
      mode = "markers+lines"
    )
    p <- layout(p,
      polar = list(
        radialaxis = list(
          visible = F,
          range = c(rplot$range[1], rplot$range[2])
        )
      ),
      showlegend = TRUE
    )
    return(p)
  })

  output$tagOptimUI <- renderUI({column(12,
      textInput(ns("tagOptim"), label = 'Tag DOE Info', value = simulations$tagOptim, width = '100%')
    )
  })

  output$rowvisu.dynui <- renderUI({
    req(seq$nconstr, cancelOutput = TRUE)
    if (seq$nconstr>0){
      tl <- tagList(
        fluidRow(
          column(6, plotlyOutput(ns("plotObj"))),
          column(6, plotlyOutput(ns("plotConstr")))
        ),
        br(),
        fluidRow(
          column(6, plotlyOutput(ns("plotQ2"))),
          column(6, "")
        )
      )
    }else{
      tl <- tagList(
        fluidRow(
          column(6, plotlyOutput(ns("plotObj"))),
          column(6, plotlyOutput(ns("plotQ2")))
        )
      )
    }
    return(tl)
  })
  
  observeEvent(input$goOptim, {
    # Initialize once seq optim is launched
    req(listmodels$finalpredfun, Xinfos$Xinfos, COformulation$idO, algoname())
    simulations$is.running <- TRUE
    disableActionButton(ns("goOptim"),session)
    enableActionButton(ns("stopOptim"),session)
    disableActionButton(ns("optimsettings"),session)
    disableActionButton(ns("nitermax"),session)
    disableActionButton(ns("nptsiter"),session)
    seq$finish <- FALSE
    simulations$tagOptim <- input$tagOptim
    seq$niter <- seq$niter + 1
    simulations$nOptim <- simulations$nOptim + 1
    seq$do.step1 <- TRUE
  })
  
  observeEvent(input$stopOptim, {
    disableActionButton(ns("stopOptim"),session)
    if (seq$niter < input$nitermax){
      enableActionButton(ns("goOptim"),session)
      simulations$tagLaunchBtn <- HTML(paste("Continue Sequential","Optimization",sep='<br>'))
      enableActionButton(ns("optimsettings"),session)
    }
    seq$stop.asked <- TRUE
  })
  
  observeEvent(seq$do.step1, {
    req(isTRUE(seq$do.step1))
    print("Step 1.1")
    session$sendCustomMessage(type = 'print', message = list(selector = ns('text_iterations'),
                                                             html = paste0("Iteration ",seq$niter,"/",input$nitermax)))
    session$sendCustomMessage(type = 'print', message = list(selector = ns('text_step'),
                                                             html = "Step 1/3: Identifying next simulation"))
  },priority = 2)
  
  observeEvent(seq$do.step1, {
    req(isTRUE(seq$do.step1))
    print("Step 1.2")

    # Step 1: compute next point to simulate Xadd
    stopOptim <- FALSE
    simulationsTemp <- try(do.call(paste0(algoname(),".seq.optimize"),list(models=seq$models[seq$idmodels],Xinfos=Xinfos, DOE=DOE, npts = input$nptsiter, 
                                                                        idobj=1, minimize=(COformulation$COobj == -1), idconstr=seq$idconstr,
                                                                        boundsO=seq$boundsO,boundsC=seq$boundsC,settings=listsettings$settings)))
    if (class(simulationsTemp)=="try-error"){
      toggleModal(session, "modalfailOptimStep1", toggle = "open")
      stopOptim <- TRUE
    }else{
      simulations$Xadd <- simulationsTemp
    
      # check next point is valid
      distLimit <- listsettings$settings[[4]]
      distToDOE <- computeDistToDOE(DOE, simulations$Xadd)
      validXadd <- distToDOE > distLimit
      print(simulations$Xadd)
      print(paste("Distance to DOE", distToDOE))
      
      if (!validXadd){
        Xbounds <- get.bounds(DOE$Xinfos)
        lb <- Xbounds[1,,drop=FALSE]
        ub <- Xbounds[2,,drop=FALSE]
        ntest <- 100*DOE$nX
        Xtest <- runif.sobol(ntest, DOE$nX, scrambling = 1, init = F)
        Xtest <- repmat(lb, ntest, 1) + repmat(ub - lb, ntest, 1)*Xtest
        ei <- computeEI(Xtest = Xtest, dimx = DOE$nX, objs = seq$models[seq$idmodels], boundsO=seq$boundsO, boundsC=seq$boundsC,
                              sdreweightedloo = as.logical(listsettings$settings[[3]]))
        isnullEI <- all(ei == 0)
        print(paste("EI is null", isnullEI))
        settingOptim <- listsettings$settings[[1]]
        if (isnullEI){
          UCB <- TRUE # only for BOBYQA
        }else{
          UCB <- FALSE
          listsettings$settings[[1]] <- "MO"
        }
        # try to find next points to simulate with other criterion/optimizer
        simulationsTemp <- try(do.call(paste0(algoname(),".seq.optimize"),list(models=seq$models[seq$idmodels],Xinfos=Xinfos, DOE=DOE, npts = input$nptsiter, 
                                                                            idobj=1, minimize=(COformulation$COobj == -1),idconstr=seq$idconstr,
                                                                            boundsO=seq$boundsO,boundsC=seq$boundsC,settings=listsettings$settings, UCB = UCB)))
        # reset default parameters
        listsettings$settings[[1]] <- settingOptim
        UCB <- FALSE
        # check if new point is valid
        if (class(simulationsTemp)=="try-error"){
          toggleModal(session, "modalfailOptimStep1", toggle = "open")
          stopOptim <- TRUE
        }else{
          simulations$Xadd <- simulationsTemp
          distToDOE <- computeDistToDOE(DOE, simulations$Xadd)
          if (distToDOE < distLimit){
            toggleModal(session, "modalConvergedOptim", toggle = "open")
            stopOptim <- TRUE
          }
        }
      }
    
    }
    
    if (!stopOptim){
    
      colnames(simulations$Xadd) <- DOE$xnames
      print(simulations$Xadd)
      print("Yadd")
      Ypredadd <- rep(list(matrix(NA,3,input$nptsiter)), DOE$nY)
      for (i in seq$idmodels){

        p <- predict.metamodel(seq$models[[i]],simulations$Xadd)
        Ypredadd[[i]][1,] <- p$mean
        Ypredadd[[i]][2,] <- p$mean-2*p$sd
        Ypredadd[[i]][3,] <- p$mean+2*p$sd
      }
      print(Ypredadd)
      simulations$Ypredadd <- rbind(simulations$Ypredadd,
                                    t(do.call('rbind', lapply(Ypredadd, function(x){x[1,,drop=F]}))))
      
      # We update the plots with the predictions
      update.plots.pred(DOE,COformulation,seq,simulations,input$nitermax,session,input$nptsiter)
      
      seq$do.step1 <- FALSE
      session$sendCustomMessage(type = 'print', message = list(selector = ns('text_step'),
                                                               html = "Step 2/3: Computing next simulation"))
      seq$do.step2 <- TRUE
      
    }else{
      
      seq$do.step1 <- FALSE
      seq$finish <- TRUE
      print("Finish !")
      enableActionButton(ns("restartOptim"),session)
      disableActionButton(ns("stopOptim"),session)
      enableActionButton(ns("optimsettings"),session)
      enableActionButton(ns("studyIter"),session)
      seq$stop.asked <- FALSE
      simulations$is.running <- FALSE
      simulations$tagOptim <- NULL
      
    }

  })
  
  observeEvent(seq$do.step2, {
    req(isTRUE(seq$do.step2))
    print("Step 2.1")
    # Step 2.1: compute next simulation
    simulations$launch.simu <- TRUE
    seq$do.step2 <- FALSE
  })
  
  observeEvent(list(DOE$nobs, advance.importDOE$status, DOE$Y), {
    req(
      isTRUE(simulations$launch.simu),
      all(advance.importDOE$status == 'ended'),
      all(apply(DOE$Y, c(1,2), function(x) { !is.na(x) || is.nan(x) })),
      seq$ninit + seq$niter*input$nptsiter == DOE$nobs
    )
    print("Step 2.2")
    # Step 2.2: detect end of simulation
    session$sendCustomMessage(type = 'print', message = list(selector = ns('text_step'),
                                                             html = "Step 3/3: Updating surrogate models"))
    simulations$launch.simu <- FALSE
    seq$do.step3 <- TRUE
  })
  
  observeEvent(seq$do.step3, {
    req(isTRUE(seq$do.step3))
    print("Step 3")
    # Step 3: update surrogate models
    stopOptim <- FALSE
    for (i in seq$idmodels){

      modelTemp <- try(build.metamodel(X=DOE$X[1:DOE$nobs,,drop=F], Y=DOE$Y[1:DOE$nobs,i], type.metamodel=seq$models[[i]]$type.metamodel,
                                        categorical=seq$models[[i]]$categorical,levels=seq$models[[i]]$levels,acosso2.selvar=seq$models[[i]]$selvar,
                                        kriging.trend=seq$models[[i]]$trend,kriging.cov=c("Matern32","Matern52","Gauss"),kriging.selvar=seq$models[[i]]$selvar,
                                        kriging.nugget=FALSE,kriging.estim="MLE",trendobj=seq$models[[i]]$trendobj,tag.failY="NA"))
      if (class(modelTemp)=="try-error"){
        stopOptim <- TRUE
      }else{
        seq$models[[i]] <- modelTemp
        seq$Q2[i,seq$ninit+seq$niter] <- seq$models[[i]]$Q2loo
      }
    }
    # We update the plots with new values
    update.plots(DOE,COformulation,seq,simulations,input$nitermax,session,input$nptsiter)
    if (stopOptim){
      toggleModal(session, "modalfailOptimStep3", toggle = "open")
    }
    seq$do.step3 <- FALSE
    # Detect if we reached max number of iterations or if stop was asked
    if (seq$niter < input$nitermax & !seq$stop.asked & !stopOptim){
      seq$niter <- seq$niter + 1
      if (seq$niter==2) enableActionButton(ns("studyIter"),session)
      seq$do.step1 <- TRUE
    }else{
      seq$finish <- TRUE
      print("Finish !")
      enableActionButton(ns("restartOptim"),session)
      disableActionButton(ns("stopOptim"),session)
      enableActionButton(ns("optimsettings"),session)
      enableActionButton(ns("studyIter"),session)
      seq$stop.asked <- FALSE
      simulations$is.running <- FALSE
      simulations$tagOptim <- NULL
    }
  })

  initCategories <- function() {
    paste0(1:seq$ninit, "_")
  }

  addedCategories <- function() {
    xAdd <- paste0(seq$ninit + rep(seq_len(input$nitermax), each = input$nptsiter), "_")
    if (input$nptsiter > 1) {
      xAdd <- paste0(xAdd, 1:input$nptsiter)
    }
    xAdd
  }

  categoriesArray <- function() {
    if (input$nitermax * input$nptsiter == 0) {
      return (initCategories())
    }
    return (c(initCategories(), addedCategories()))
  }

  output$plotObj <- renderPlotly({
    req(isolate(DOE$Y), seq$ninit > 0, COformulation$idO,
        input$nitermax,input$nptsiter, cancelOutput = TRUE)
    
    df1 <- cbind(data.frame(x=initCategories()),isolate(DOE$Y[1:seq$ninit,COformulation$idO]))
    df2 <- cbind(data.frame(x=initCategories()),isolate(simulations$Ypredadd[1:seq$ninit,COformulation$idO]))
    colnames(df1) <- colnames(df2) <- c("x","y")

    if (seq$nconstr>0){
      # If there are constraints, plot with colors dependending on feasibility
      r <- respect.constraints(isolate(DOE$Y[1:seq$ninit,COformulation$idC,drop=FALSE]),COformulation$COsign,COformulation$COt)
      rpred <- respect.constraints(isolate(simulations$Ypredadd[1:seq$ninit,COformulation$idC,drop=FALSE]),COformulation$COsign,COformulation$COt)
      if (seq$nconstr>1){
        allr <- apply(r,2,all)
        allrpred <- apply(rpred,2,all)
      }else{
        allr <- r
        allrpred <- rpred
      }
      # Trace 1: True values feasible (add false point to initialize it)
      p <- plot_ly(
        x = as.character(c(NULL, df1$x[allr])),
        y = c(NULL, df1$y[allr]),
        name = "True - Feasible",
        type = "scatter",
        mode = "markers",
        marker = list(color = toRGB("green"), symbol = "circle")
      )
      # Trace 2: True values non-feasible (add false point to initialize it)
      p <- add_trace(p,
        x = as.character(c(NULL, df1$x[!allr])),
        y = c(NULL,df1$y[!allr]),
        name = "True - Non Feasible",
        type = "scatter",
        mode = "markers",
        marker = list(color = toRGB("red"), symbol="circle"),
        inherit = FALSE
      )
      # Trace 3: Pred values feasible (add false point to initialize it)
      p <- add_trace(p, 
        x = as.character(c(NULL, df2$x[allrpred])),
        y = c(NULL, df2$y[allrpred]),
        name = "Pred - Feasible",
        type = "scatter",
        mode = "markers",
        marker = list(color = toRGB("green"), symbol="circle-open"),
        inherit = FALSE
      )
      # Trace 4: Pred values non-feasible (add false point to initialize it)
      p <- add_trace(p, 
        x = as.character(c(NULL, df2$x[!allrpred])),
        y = c(NULL, df2$y[!allrpred]),
        name = "Pred - Non Feasible",
        type = "scatter",
        mode = "markers",
        marker = list(color = toRGB("red"), symbol="circle-open"),
        inherit = FALSE
      )
      
      p <- layout(
        p,
        xaxis = list(
          title = "Iteration_Simulation", 
          categoryorder = "array",
          categoryarray = categoriesArray()
        ),
        yaxis = list(
          title = isolate(DOE$ynamesmenu[COformulation$idO]),
          hoverformat = ".2e",
          zeroline = FALSE
        ),
        title = "Optimization Results - Objective"
      )
    }else{
      # Trace 1: True values
      p <- plot_ly(
        x =  as.character(df1$x),
        y = df1$y,
        name = "True", 
        type = "scatter", 
        mode = "markers",
        marker = list(symbol = "circle")
      )
      # Trace 2: Pred values 
      p <- add_trace(p, 
        x = as.character(df2$x),
        y = df2$y,
        name = "Pred", 
        type = "scatter", 
        mode = "markers",
        marker = list(symbol = "circle-open"),
        inherit = FALSE
      )
      # No constraints, just plot with objective
      p <- layout(
        p,
        xaxis = list(
          title = "Iteration_Simulation", 
          categoryorder = "array",
          categoryarray = categoriesArray()
        ),
        yaxis = list(
          title = isolate(DOE$ynamesmenu[COformulation$idO]),
          hoverformat = ".2e",
          zeroline = FALSE
        ),
        title = "Optimization Results - Objective")
    }
    return(p)
  })
  
  plot.constr <- reactiveValues(generated=FALSE)
  
  output$plotConstr <- renderPlotly({
    req(isolate(DOE$Y), seq$ninit > 0, COformulation$idC, 
        COformulation$COsign, COformulation$COt, input$nitermax, input$nptsiter, cancelOutput = TRUE)
    r <- ifelse(
      respect.constraints(isolate(DOE$Y[1:seq$ninit, COformulation$idC, drop=FALSE]), COformulation$COsign, COformulation$COt),
      1,
      0
    )
    r <- matrix(as.numeric(r),ncol=seq$ninit, byrow=FALSE)
    tt = COformulation$thresholds
    texty <- paste0(colnames(tt), tt)
    textz <- matrix(rep("", length(COformulation$idC) * (seq$ninit + input$nitermax * input$nptsiter)), nrow = length(COformulation$idC))
    textz[r==0] <- "Non-Feasible"
    textz[r==1] <- "Feasible"
    plot.constr$generated <- TRUE

    # Hack to emulate a categorical color scale
    colors <- c("red", "green")
    if (all(r == 1)) {
      colors <- c("green", "green")
    }
    else if (all(r == 0)) {
      colors <- c("red", "red")
    }

    plot_ly(source = "heatplotConstr") %>%
      add_heatmap(
        z = r,
        x = categoriesArray(),
        y = 1:length(COformulation$idC),
        colors = colors,
        type = "heatmap",
        zauto = FALSE,
        zmin = 0,
        zmax = 1,
        showscale = FALSE,
        hoverinfo = 'x+y+text',
        hoverongaps = FALSE,
        text = textz
      )%>%
      layout(
        xaxis = list(
          title = "Iteration_Simulation",
          categoryorder = "array",
          categoryarray = categoriesArray(),
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Constraint Satisfaction",
          showgrid = FALSE,
          tickvals = 1:length(COformulation$idC),
          ticktext = texty
        ),
        showlegend = TRUE,
        title = "Optimization Results - Constraints"
      )
  })
  
  observe({
    req(plot.constr$generated)
    click_event <- event_data("plotly_click", source = "heatplotConstr")
    if (length(click_event)) {
      idy <- click_event$y
      tt <- COformulation$thresholds
      text <- paste0(colnames(tt)[idy],tt[idy])
      showModal(modalDialog(
        title = paste("Constraint Analysis for",text),
        plotlyOutput(ns("plotConstrOne")),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  
  output$plotConstrOne <- renderPlotly({
    req(isolate(DOE$Y), seq$ninit > 0, COformulation$idC, 
        COformulation$COsign, COformulation$COt, input$nitermax, input$nptsiter, cancelOutput = TRUE)
    c <- event_data("plotly_click", source = "heatplotConstr")
    ny <- seq$ninit+input$nitermax*input$nptsiter
    tt <- COformulation$thresholds
    idy <- which(DOE$ynamesmenu==colnames(tt)[c$y])
    text <- paste0(colnames(tt)[c$y],tt[c$y])
    y <- matrix(NA,1,ny)
    y[1:nrow(DOE$Y)] <- DOE$Y[,idy]
    x <- categoriesArray()
    t <- rep(COformulation$COt[c$y],ny)
    p <- plot_ly(
      x = x, 
      y = c(y), 
      type = "scatter",
      mode = "markers",
      name = "Constraint Value"
    )%>%
    add_trace(
      x = x,
      y = t,
      type = "scatter",
      mode = "lines",
      name = "Threshold",
      inherit = FALSE
    )%>%
    layout(
      title = text, 
      xaxis = list(
        title = "Iteration_Simulation",
        categoryorder = "array",
        categoryarray = categoriesArray()
      ),
      yaxis = list(title = DOE$ynamesvisu[idy]),
      showlegend = FALSE
    )
    return(p)
  })
  
  
  output$plotQ2 <- renderPlotly({
    req(isolate(DOE$Y), seq$ninit > 0,
        input$nitermax, isolate(seq$Q2), cancelOutput = TRUE)
    dfinit <- isolate(seq$Q2[seq$idmodels,1:seq$ninit,drop=FALSE])
    df <- melt(dfinit)
    colnames(df) <- c("Output","Iterations","Q2")
    df$Output <- DOE$ynamesmenu[seq$idmodels][df$Output]
    p <- layout(
      plot_ly(df,x = ~Iterations, y = ~Q2, color = ~Output, mode = "lines", 
              type = "scatter", colors = "Set3"),
      showlegend = FALSE,
      title = "Q2"
    )
    return(p)
  })
  
  output$Xaddcontents <- DT::renderDataTable({
    req(isolate(DOE$X),input$selected.iter)
    selectedIndex <- match(input$selected.iter, addedCategories())
    df <- isolate(DOE$X[seq$ninit+selectedIndex,,drop=FALSE])
    colnames(df) <- DOE$xnamesvisu
    rownames(df) <- input$selected.iter
    DT::datatable(
      df, escape = FALSE,
      options = list(
        dom = 't',
        scrollX = TRUE,scroller = TRUE
      ))
  })
  
  return(simulations)
}