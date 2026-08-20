#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# module uncertaintyDefinition

source("modules/menuExplore/UQModel/UQparamsChange.R", local = TRUE)
source("modules/menuExplore/UQModel/UQparamsDependence.R", local = TRUE)
source("modules/menuExplore/UQModel/distributionfitting.R", local = TRUE)

initialize.UQparams <- function(Xinfos){
  lapply(1:length(Xinfos), function(i, Xinfos){
    if (Xinfos[[i]]$type == 'constant'){
      typeDistr <- "unif"
      P1Distr <- NA
      P2Distr <- NA
      P3Distr <- NA
      P4Distr <- NA
      levels <- NA
      weights <- list(1)
    }
    if (Xinfos[[i]]$type == 'numeric'){
      typeDistr <- "unif"
      P1Distr <- Xinfos[[i]]$bounds[1]
      P2Distr <- Xinfos[[i]]$bounds[2]
      P3Distr <- NA
      P4Distr <- NA
      levels <- NA
      weights <- NA
    }
    if (Xinfos[[i]]$type == 'categorical'){
      typeDistr <- "Cat"
      P1Distr <- NA
      P2Distr <- NA
      P3Distr <- NA
      P4Distr <- NA
      levels <- Xinfos[[i]]$levels
      weights <- lapply(1:Xinfos[[i]]$nlevels,function(x) signif(1/Xinfos[[i]]$nlevels,3))
    }
    return(list(typeDistr = typeDistr, P1Distr = P1Distr, P2Distr = P2Distr, 
                P3Distr = P3Distr, P4Distr = P4Distr, levels = levels, weights = weights))
  }, Xinfos = Xinfos)
}

uncertaintyDefinition.ui <- function(id) {
  ns <- NS(id)
  
  infoMessage <-  "If you do not import a file (or the imported file is not valid),
  by default each input will follow a uniform distribution on the domain given by
  the imported bounds (for continuous ones) or the levels (for categorical ones)."
  estimationMessage <-  "If you do not know the distribution of some inputs but
  want to estimate it with available observations, you can change their type to 'Estimated'
  and fit their distribution in the following panel."
  copulaMessage <-  "You can specify groups of numeric inputs which are dependent. If you know the groups
  you can then either define all the group copulas or estimate them with a dataset. If you do not know
  the groups, you can also estimate them with available observations in the following panel."
  
  tabsetPanel(id = ns('tabs'), type = "tabs",
              tabPanel(h4("Definition & Summary"), value = ns("definition"),
                       tagList(
                         br(),
                         fluidRow(
                           column(
                             4,
                             wellPanel(
                               h4("Uncertainty Parameters"), br(),
                               UQparamsChange.ui(ns("UQparams")), br(), br(),
                               infoMessage,
                               br(),
                               estimationMessage,
                               br(),
                               hr(),
                               UQparamsDependence.ui(ns("listCopulas")), br(), br(),
                               copulaMessage
                             )
                           ),
                           column(8, UQparamsChange.ui.preview(ns("UQparams")))
                         )
                       )
              ),
              tabPanel(h4("Distribution Fitting"), value = ns('fitting'),
                       distributionfitting.ui(ns("fitdist"))
              )
  )
}

uncertaintyDefinition.server <- function(input, output, session, DOE, persistence) {
  
  ns <- session$ns

  initialUQparams <- reactiveValues(UQparams = NULL)
  initiallistCopulas <- reactiveValues(listCopulas = NULL)
  fitdistUQparams <- reactiveValues(UQparams = NULL, selection.marginals = "unconfirmed",
                                    listCopulas = NULL, selection.copulas = "unconfirmed")
  finalUQparams <- reactiveValues(UQparams = NULL, listCopulas = NULL)
  
  observeEvent(list(DOE$Xinfos), {
    req(DOE$nX, DOE$Xinfos)
    print("initializing UQparams and listCopulas with DOE")
    initialUQparams$UQparams <- initialize.UQparams(DOE$Xinfos)
    initiallistCopulas$listCopulas <- list(inputs=rep(FALSE,DOE$nX),groups=NULL,unique.groups=NULL,typeCopulas=NULL,Copulas=NULL)
  })
  
  UQparams <- callModule(UQparamsChange.server, "UQparams", initialUQparams, listCopulas, fitdistUQparams, DOE, persistence)
  listCopulas <- callModule(UQparamsDependence.server, "listCopulas", initiallistCopulas, fitdistUQparams, DOE, persistence)
  fitdist <- callModule(distributionfitting.server, "fitdist", UQparams, listCopulas, DOE)

  observeEvent(fitdist$selection.marginals,{
    fitdistUQparams$UQparams <- fitdist$UQparams
    fitdistUQparams$selection.marginals <- fitdist$selection.marginals
  })

  observeEvent(fitdist$selection.copulas,{
    fitdistUQparams$listCopulas <- fitdist$listCopulas
    fitdistUQparams$selection.copulas <- fitdist$selection.copulas
  })

  observeEvent(UQparams$UQparams,{
    finalUQparams$UQparams <- UQparams$UQparams
  })

  observeEvent(listCopulas$listCopulas,{
    finalUQparams$listCopulas<- listCopulas$listCopulas
  })
  
  return(finalUQparams)
}