source("modules/shared/dynamicSelectpicker.R", local = TRUE)
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

###############################
#  Define client user interface
###############################

parallelCoordPlot.ui <- function(id) {
  ns <- NS(id)

  ClusterModal <- bsModal(
    ns("modalcluster"), "Cluster Observations", NULL,
    fluidRow(
      column(6,
             numericInput(ns("nbclust"), "Nb of Clusters", 2, min = 1, max = 10)
      ),
      column(6,
             switchInput(ns("scaleclust"), value = T, label = "Scale Before Clustering",size = "mini")
      )
    ),
    dynamicSelectpicker.ui(ns("choosecluster")),
    br(),
    fluidRow(
      column(6,
             actionButton(ns("buildclusters"), "Launch Clustering", class = "btn-primary", width = '100%')),
      column(6,
             actionButton(ns("removeclusters"), "Remove Clustering", class = "btn-primary", width = '100%'))
    ),
    size="large"
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
                column(6,tags$h5("Center Plot")),
                column(6,tags$h5("Bounds"))
              ),
              fluidRow(
                column(6,switchInput(ns("centerref"), label = "Center Ref.", size="small", disabled=TRUE),align="center"),
                column(6,switchInput(ns("keepbounds"), label = "Keep bounds", size="small"),align="center")
              ),
              hr(),
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
            ),align="left"
      ),
      column(2,dynamicSelectpicker.ui(ns("chooseXParcoords"))),
      column(2,dynamicSelectpicker.ui(ns("chooseYParcoords"))),
      column(2,dynamicSelectpicker.ui(ns("chooseHistParcoords"))),
      column(2,dynamicSelectpicker.ui(ns("chooseBoundsParcoords"))),
      column(2,uiOutput(ns("dynui_groups"))),
      column(1,br(),actionButton(ns("cluster"),label="Cluster", class = "btn-primary"), align="center")
    ),
    fluidRow(
      column(12,uiOutput(ns("dynui_bounds")),align="center")
    ),
    tags$script(paste0('$( "#', ns('chooseXParcoords'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseXPCPclosed'),'", 1, {priority: "event"}); });')),
    tags$script(paste0('$( "#', ns('chooseYParcoords'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseYPCPclosed'),'", 1, {priority: "event"}); });')),
    tags$script(paste0('$( "#', ns('chooseHistParcoords'), '-select" ).on( "hidden.bs.select", function() { Shiny.onInputChange("',ns('chooseHistPCPclosed'),'", 1, {priority: "event"}); });')),
    div(id = ns("pcpspm"),
      parallelPlotOutput(ns("parcoords")),
      scatterPlotMatrixOutput(ns("scatterPlotMatrix"), height = "1000px")
    ),
    ClusterModal
  )
}

#####################
# Define server logic
#####################

parallelCoordPlot.server <- function(input, output, session, DOE, window.dimension) {
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
  
  Xcat <- reactive({
    req(DOE$Xinfos)
    cat <- which(sapply(DOE$Xinfos, function(var){var$type}) == 'categorical')
    return(DOE$xnamesmenu[cat])
  })
  
  Ycat <- reactive({
    req(DOE$Yinfos)
    cat <- which(DOE$Yinfos$type == 'categorical')
    return(DOE$ynamesmenu[cat])
  })
  
  Allcat <- reactive({
    req(DOE$Xinfos,DOE$Yinfos)
    categorical <- lapply(1:DOE$nX, function(i) {
      if (DOE$Xinfos[[i]]$type == "categorical") {
        return(as.character(DOE$Xinfos[[i]]$levels))
      }
      return(NULL)
    })
    categorical <- c(categorical, lapply(1:DOE$nY,function(i){
      if (DOE$Yinfos$type[i] == "categorical") {
        return(levels(DOE$Y[,i]))
      }
      return(NULL)
    }))
    names(categorical) <- c(DOE$xnamesmenu,DOE$ynamesmenu)
    return(categorical)
  })
  
  Allb <- reactive({
    req(DOE$Xinfos,DOE$Yinfos)
    bounds <- lapply(1:DOE$nX, function(i) {
      if (DOE$Xinfos[[i]]$type == "numeric") {
        return(DOE$Xinfos[[i]]$bounds)
      }
      return(NULL)
    })
    bounds <- c(bounds, lapply(1:DOE$nY,function(i){
      if (DOE$Yinfos$type[i] == "numeric") {
        y <- DOE$Y[,i]
        return(range(y[!is.na(y)]))
      }
      return(NULL)
    }))
    names(bounds) <- c(DOE$xnamesmenu,DOE$ynamesmenu)
    return(bounds)
  })
  
  # Detect if there are NAs
  # If there are some, detect groups of observations (DOE fusion)
  # ngroups: number of groups (in a given group, columns having 'NA' value are the same)
  # groups: each group is defined by 'idrows' (which rows belongs to this group), 'idX' (which 'DOE$X' columns), 'idY' (which 'DOE$Y' columns)
  rowgroups <- reactiveValues(ngroups=1,groups=NULL)
  observe({
    req(DOE$XY)
    if (anyNA(DOE$X)){
      n <- !is.na(DOE$XY)
      u <- unique(n)
      nu <- nrow(u)
      groups <- list()
      for (i in 1:nu){
        idrows <- which(apply(n == matrix(u[i,],nrow(DOE$XY),ncol(DOE$XY),byrow=TRUE),1,all))
        idX <- which(apply(!is.na(DOE$X[idrows,,drop=F]),2,all))
        idY <- which(apply(!is.na(DOE$Y[idrows,,drop=F]),2,all))
        groups[[i]] <- list(idrows=idrows,idX=idX,idY=idY)
      }
      rowgroups$ngroups <- nu
      rowgroups$groups <- groups
    }
  })

  # Dynamic UI for groups in parcoords if any
  output$dynui_groups <- renderUI({
    req(rowgroups$ngroups > 1)
    pickerInput(inputId = session$ns("choicegroups"),
                label = "Choose Rows Group",
                choices = paste("Group",1:rowgroups$ngroups),
                selected = "Group 1",
                options = list(`actions-box` = TRUE,style = "btn-primary"),
                multiple = FALSE)
  })
  
  choicesXParcoords <- reactive({
    req(DOE$xnamesmenu,rowgroups$ngroups)
    l <- list()
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idX <- rowgroups$groups[[idgroup]]$idX
    }else{
      idX <- 1:DOE$nX
    }
    # Use a 'section' titled 'Active' to display the X names
    if (length(idX)>0) l[["Active"]] <- as.list(DOE$xnamesmenu[idX])
    return(l)
  })
  selectedXParcoords <- reactive({
    req(DOE$xnamesmenu,rowgroups$ngroups)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idX <- rowgroups$groups[[idgroup]]$idX
    }else{
      idX <- 1:DOE$nX
    }
    sel <-  DOE$xnamesmenu[idX]
    return(sel)
  })
  
  xnameParcoords <- callModule(
    dynamicSelectpicker.server, "chooseXParcoords", label.title =  "Choose Input(s) to Visualize", choices = choicesXParcoords,
    selected = selectedXParcoords(), livesearch = TRUE
  )
  
  choicesYParcoords <- reactive({
    req(DOE$ynamesmenu,DOE$Yinfos,rowgroups$ngroups)
    l <- list()
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idY <- rowgroups$groups[[idgroup]]$idY
      int.intersect <- intersect(DOE$Yinfos$int.ids,idY)
      control.intersect <- intersect(DOE$Yinfos$control.ids,idY)
      const.intersect <- intersect(DOE$Yinfos$const.ids,idY)
      status.intersect <- intersect(DOE$Yinfos$status.ids,idY)
      if (length(int.intersect)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[int.intersect])
      if (length(control.intersect)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[control.intersect])
      if (length(const.intersect)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[const.intersect])
      if (length(status.intersect)>0) l[["Status"]] <- as.list(DOE$ynamesmenu[status.intersect])
    }else{
      if (length(DOE$Yinfos$int.ids)>0) l[["Interest"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$int.ids])
      if (length(DOE$Yinfos$control.ids)>0) l[["Control"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$control.ids])
      if (length(DOE$Yinfos$const.ids)>0) l[["Constant"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$const.ids])
      if (length(DOE$Yinfos$status.ids)>0) l[["Status"]] <- as.list(DOE$ynamesmenu[DOE$Yinfos$status.ids])
    }
    return(l)
  })
  selectedYParcoords <- reactive({
    req(DOE$ynamesmenu,DOE$Yinfos,rowgroups$ngroups)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idY <- rowgroups$groups[[idgroup]]$idY
      int.intersect <- intersect(DOE$Yinfos$int.ids,idY)
      sel <-  DOE$ynamesmenu[int.intersect]
    }else{
      sel <-  DOE$ynamesmenu[DOE$Yinfos$int.ids]
    }
    return(sel)
  })
  
  ynameParcoords <- callModule(
    dynamicSelectpicker.server, "chooseYParcoords", label.title =  "Choose Output(s) to Visualize",
    choices = choicesYParcoords, livesearch = TRUE, selected = selectedYParcoords
  )
  
  choicesHistParcoords <- reactive({
    req(choicesXParcoords(), choicesYParcoords())
    return(c(choicesXParcoords(), choicesYParcoords()))
  })
  
  histnameParcoords <- callModule(
    dynamicSelectpicker.server, "chooseHistParcoords", label.title =  "Visualize Histograms",
    choices = choicesHistParcoords, livesearch = TRUE, selected = NULL
  )
  
  choicesBoundsParcoords <- reactive({
    req(choicesXParcoords(),choicesYParcoords(),BoundsParcoords$names)
    return(BoundsParcoords$names)
  })
  
  BoundsParcoords <- reactiveValues(names="No cutoff")
  observe({
    req(menuson$on, DOE$nX, DOE$nY)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idXgroup <- rowgroups$groups[[idgroup]]$idX
      idYgroup <- rowgroups$groups[[idgroup]]$idY
    }else{
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
    }
    XYcat <- c(intersect(Xcat(), DOE$xnamesmenu[idXgroup]), intersect(Ycat(), DOE$ynamesmenu[idYgroup]))
    if (length(XYcat)){
      BoundsParcoords$names <- XYcat
    }else{
      BoundsParcoords$names <- "No cutoff"
    }
  })
  
  boundsnameParcoords <- callModule(
    dynamicSelectpicker.server, "chooseBoundsParcoords", label.title =  "Manual Bounds",
    choices = choicesBoundsParcoords, livesearch = TRUE, selected = "None", maxOptions = 1, abox = FALSE
  )
  
  menuson <- reactiveValues(on = FALSE)
  observe({
    req(xnameParcoords(),ynameParcoords())
    menuson$on <- TRUE
  })
  
  output$dynui_bounds <- renderUI({
    req(boundsnameParcoords(), !is.null(Xcat()), !is.null(Ycat()), Allcat(), Allb())
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idXgroup <- rowgroups$groups[[idgroup]]$idX
      idYgroup <- rowgroups$groups[[idgroup]]$idY
    }else{
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
    }
    xynames <- c(DOE$xnames[idXgroup], DOE$ynames[idYgroup])
    id <- which(boundsnameParcoords()==c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup]))
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
    req(menuson$on, DOE$nX, DOE$nY)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idrows <- rowgroups$groups[[idgroup]]$idrows
      idXgroup <- rowgroups$groups[[idgroup]]$idX
      idYgroup <- rowgroups$groups[[idgroup]]$idY
    }else{
      idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
      idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
    }
    xynames <- c(DOE$xnames[idXgroup], DOE$ynames[idYgroup])
    choices.widget <- Allcat()[[boundsnameParcoords()]]
    id <- which(boundsnameParcoords()==c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup]))
    selected.widget <- input[["manualcat"]]
    # Update storeCutoffs
    if (all(choices.widget %in% selected.widget)){
      storeCutoffs$cutoffs[id] <- list(NULL)
    }else{
      storeCutoffs$cutoffs[[id]] <- as.list(selected.widget)
    }
    # Update pcp and spm cutoffs
    if (!is.null(cluster$id) & !anyNA(cluster$id[idrows])){
      updatePcpSpmCutoffs(c(list(NULL),storeCutoffs$cutoffs))
    }else{
      updatePcpSpmCutoffs(storeCutoffs$cutoffs)
    }
  })
  
  observe({ # We assume a maximum of 5 cutoffs have been applied on a column
    lapply(1:5, function(i){
      observeEvent(input[[paste0('remove', i)]], {
        req(menuson$on, DOE$nX, DOE$nY)
        if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
          idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
          idrows <- rowgroups$groups[[idgroup]]$idrows
          idXgroup <- rowgroups$groups[[idgroup]]$idX
          idYgroup <- rowgroups$groups[[idgroup]]$idY
        }else{
          idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
          idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
          idXgroup <- 1:DOE$nX
          idYgroup <- 1:DOE$nY
        }
        xynamesmenu <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup])
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
        if (!is.null(cluster$id) & !anyNA(cluster$id[idrows])){
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
        req(menuson$on, DOE$nX, DOE$nY)
        if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
          idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
          idrows <- rowgroups$groups[[idgroup]]$idrows
          idXgroup <- rowgroups$groups[[idgroup]]$idX
          idYgroup <- rowgroups$groups[[idgroup]]$idY
        }else{
          idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
          idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
          idXgroup <- 1:DOE$nX
          idYgroup <- 1:DOE$nY
        }
        xynamesmenu <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup])
        selected.widget <- input[[paste0('manualbounds', i)]]
        # Update storeCutoffs
        id <- which(boundsnameParcoords()==xynamesmenu)
        storeCutoffs$cutoffs[[id]][[i]] <- as.list(selected.widget)
        # Update pcp and smp cutoffs
        if (!is.null(cluster$id) & !anyNA(cluster$id[idrows])){
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

  observeEvent(input$choicegroups, {
    storeCutoffs$cutoffs <- vector('list',DOE$nX+DOE$nY)
  })
  
  datavisu <- reactiveVal(NULL)

  dataPCP <- reactiveValues(data=NULL,categorical=NULL,columnLabels=NULL,
                            zAxisDim=NULL, refColumnDim=NULL, keptColumns=NULL, histoVisibility=NULL,
                            refRowIndex=NULL,cutoffs=NULL,init=FALSE)
  storeCutoffs <- reactiveValues(cutoffs=NULL)
  
  observe({
    req(menuson$on, DOE$XY, isolate(rowgroups$ngroups), isolate(ynameParcoords() %in% DOE$ynamesmenu))
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idrows <- rowgroups$groups[[idgroup]]$idrows
      idXgroup <- rowgroups$groups[[idgroup]]$idX 
      idYgroup <- rowgroups$groups[[idgroup]]$idY
    }else{
      idYselected <- as.numeric(sapply(isolate(ynameParcoords()), function(name){which(name == DOE$ynamesmenu)}))
      idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
    }
    xynames <- c(DOE$xnames[idXgroup], DOE$ynames[idYgroup])
    xynamesvisu <- c(DOE$xnamesvisu[idXgroup], DOE$ynamesvisu[idYgroup])
    datanorm <- DOE$XY[idrows, c(idXgroup, DOE$nX + idYgroup)]
    categorical <- lapply(idXgroup, function(i) {
      if (DOE$Xinfos[[i]]$type == "categorical") {
        return(as.character(DOE$Xinfos[[i]]$levels))
      }
      return(NULL)
    })
    categorical <- c(categorical, lapply(idYgroup,function(i){
      if (DOE$Yinfos$type[i] == "categorical") {
        return(as.character(unique(DOE$Y[,i])))
      }
      return(NULL)
    }))
    names(categorical) <- xynames
    
    idvisu <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup]) %in% c(selectedXParcoords(),selectedYParcoords())
    idhist <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup]) %in% isolate(histnameParcoords())

    clustering <- !is.null(cluster$id) & !anyNA(cluster$id[idrows])
    if (clustering){
      df <- cbind(data.frame(Cluster=cluster$id[idrows]),datanorm)
      colnames(df) <- c("Cluster",xynames)
      categorical <- c(list(Cluster=as.character(1:input$nbclust)),categorical)
      categorical.final <- categorical[c("Cluster",xynames)]
      xynamesvisu.final <- c("Cluster",xynamesvisu)
      refColumnDim <- "Cluster"
      zAxisDim <- "Cluster"
      idvisu <- c(TRUE,idvisu)
      idhist <- c(TRUE,idhist)
    }else{
      df <- datanorm
      categorical.final <- categorical
      xynamesvisu.final <- xynamesvisu
      refColumnDim <- NULL
      zAxisDim <- NULL
    }
    names(categorical.final) <- NULL
    names(xynamesvisu.final) <- NULL
    if (input$centerref){
      if (is.null(DOE$idref)){
        refRowIndex <- NULL
      }else{
        refRowIndex <- DOE$idref
      }
    }else{
      refRowIndex <- NULL
    }
    if (isolate(input$keepbounds)){
      cutoffs <- storeCutoffs$cutoffs
      if (clustering){
        cutoffs <- c(vector('list',1),cutoffs)
      }
    }else{
      cutoffs <- NULL
    }
    datavisu(df)
    dataPCP$data <- df
    dataPCP$categorical <- categorical.final
    dataPCP$columnLabels <- xynamesvisu.final
    dataPCP$refColumnDim <- refColumnDim
    dataPCP$zAxisDim <- zAxisDim
    dataPCP$keptColumns <- idvisu
    parallelPlot::setKeptColumns(ns("parcoords"), idvisu)
    scatterPlotMatrix::setKeptColumns(ns("scatterPlotMatrix"), idvisu)
    dataPCP$histoVisibility <- idhist
    dataPCP$refRowIndex <- refRowIndex
    dataPCP$cutoffs <- cutoffs
    dataPCP$init <- TRUE
  })
  
  # Filter NA for outputs when no groups are identified
  observeEvent(input$chooseYPCPclosed, {
    req(rowgroups$ngroups == 1, anyNA(DOE$Y), dataPCP$data)
    idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
    idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
    df <- DOE$XY[idrows, ]
    if (!is.null(cluster$id) & !anyNA(cluster$id[idrows])){
      df <- cbind(data.frame(Cluster=cluster$id[idrows]), df)
    }
    datavisu(df)
    dataPCP$data <- df
  })

  # Initialize parallel plot
  output$parcoords <- renderParallelPlot({
    req(dataPCP$init,dataPCP$data)
    dataPCP$refRowIndex
    isolate({
      parallelPlot(
        data = datavisu(),
        categorical = dataPCP$categorical,
        arrangeMethod = input$arrange.method,
        rotateTitle = DOE$adapt.visu,
        columnLabels = dataPCP$columnLabels,
        refColumnDim = dataPCP$refColumnDim,
        keptColumns = dataPCP$keptColumns,
        histoVisibility = dataPCP$histoVisibility,
        refRowIndex = dataPCP$refRowIndex,
        continuousCS = input$choose.palette.num,
        categoricalCS = input$choose.palette.cat,
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
    req(length(union(isolate(xnameParcoords()), isolate(ynameParcoords())))>1)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idXgroup <- rowgroups$groups[[idgroup]]$idX 
      idYgroup <- rowgroups$groups[[idgroup]]$idY
      idrows <- rowgroups$groups[[idgroup]]$idrows
    }else{
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
      idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
      idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
    }
    id <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup]) %in% c(xnameParcoords(), ynameParcoords())
    if (!is.null(cluster$id) & !anyNA(cluster$id[idrows])){
      id <- c(TRUE,id)
    }
    if (any(id!=dataPCP$keptColumns)){
      dataPCP$keptColumns <- id
      parallelPlot::setKeptColumns(ns("parcoords"), id)
      scatterPlotMatrix::setKeptColumns(ns("scatterPlotMatrix"), id)
    }
  })
  
  # If selected histograms have been changed ...
  observeEvent(input$chooseHistPCPclosed, {
    req(DOE$nX, DOE$nY, DOE$XY)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idXgroup <- rowgroups$groups[[idgroup]]$idX 
      idYgroup <- rowgroups$groups[[idgroup]]$idY
      idrows <- rowgroups$groups[[idgroup]]$idrows
    }else{
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
      idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
      idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
    }
    id <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup]) %in% isolate(histnameParcoords())
    if (!is.null(cluster$id) & !anyNA(cluster$id[idrows])){
      id <- c(TRUE,id)
    }
    if (any(id!=dataPCP$histoVisibility)){
      dataPCP$histoVisibility <- id
      parallelPlot::setHistoVisibility(ns("parcoords"), id)
    }
  })
  
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
  
  # Disable center button if no reference was given
  observe({
    if (is.null(DOE$idref)){
      updateSwitchInput(session = session, inputId="centerref",disabled = TRUE)
    }else{
      updateSwitchInput(session = session, inputId="centerref",disabled = FALSE)
    }
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
    req(DOE$XY)
    if (is.null(isolate(input$keepbounds)) || !input$keepbounds){
      ll <- vector('list',DOE$nX+DOE$nY)
      storeCutoffs$cutoffs <- ll
    }
  })

  observeEvent(input$pcpEvent,{
    if (!is.null(input$pcpEvent) && input$pcpEvent$type == "cutoffChange" && !input$pcpEvent$value$adjusting){
      updatedDim <- input$pcpEvent$value$updatedDim
      if (updatedDim != "Cluster"){
        req(menuson$on, DOE$nX, DOE$nY)
        if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
          idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
          idXgroup <- rowgroups$groups[[idgroup]]$idX
          idYgroup <- rowgroups$groups[[idgroup]]$idY
        }else{
          idXgroup <- 1:DOE$nX
          idYgroup <- 1:DOE$nY
        }
        xynamesmenu <- c(DOE$xnamesmenu[idXgroup], DOE$ynamesmenu[idYgroup])
        id <- which(updatedDim==c(DOE$xnames[idXgroup], DOE$ynames[idYgroup]))
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
  
  choicescluster <- reactive({
    req(DOE$xnamesmenu,DOE$ynamesmenu,DOE$Yinfos)
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idXgroup <- rowgroups$groups[[idgroup]]$idX 
      idYgroup <- rowgroups$groups[[idgroup]]$idY
    }else{
      idXgroup <- 1:DOE$nX
      idYgroup <- 1:DOE$nY
    }
    xnames <- DOE$xnamesmenu[idXgroup][sapply(idXgroup, function(i){DOE$Xinfos[[i]]$type == 'numeric'})]
    if (rowgroups$ngroups == 1 & anyNA(DOE$Y)){
      ynames <- ynameParcoords()
    }else{
      ynames <- DOE$ynamesmenu[intersect(idYgroup, setdiff(c(DOE$Yinfos$int.ids,DOE$Yinfos$control.ids,DOE$Yinfos$const.ids),
                                                which(DOE$Yinfos$type == 'categorical')))]
    }
    c(xnames, ynames)
  })
  namecluster <- callModule(
    dynamicSelectpicker.server, "choosecluster", label.title =  "Choose Input(s)/Output(s) to Cluster",
    choices = choicescluster, selected = DOE$ynamesmenu, livesearch = TRUE
  )
  
  observeEvent(input$cluster, {
    req(xnameParcoords(), ynameParcoords(), DOE$XY)
    toggleModal(session, "modalcluster", toggle = "open")
  })
  
  cluster <- reactiveValues(id = NULL)
  observeEvent(input$buildclusters,{
    req(input$nbclust,!is.na(input$nbclust),namecluster(),!is.null(input$scaleclust))
    xynames <- c(DOE$xnames, DOE$ynames)
    df <- DOE$XY[xynames[namecluster()]]
    if (input$scaleclust){
      df <- scale(df)
    }
    if (rowgroups$ngroups > 1 & !is.null(input$choicegroups)){
      idgroup <- as.numeric(strsplit(input$choicegroups,"Group ")[[1]][2])
      idrows <- rowgroups$groups[[idgroup]]$idrows
    }else{
      idYselected <- as.numeric(sapply(ynameParcoords(), function(name){which(name == DOE$ynamesmenu)}))
      idrows <- (1:nrow(DOE$XY))[!apply(DOE$Y[, idYselected, drop = FALSE], 1, anyNA)]
    }
    if (length(idrows) > 0){
      cluster$id <- rep(NA, nrow(df))
      cluster$id[idrows] <- kmeans(df[idrows,], centers = input$nbclust, iter.max = 50, nstart=20)$cluster
    }
    toggleModal(session, "modalcluster", toggle = "close")
  })
  observeEvent(input$removeclusters,{
    cluster$id <- NULL
    toggleModal(session, "modalcluster", toggle = "close")
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
          if (!is.null(dataPCP$categorical[[which(dimNames == dimName)]])) {
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

}
