#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

if (as.numeric(R.version$major) == 4){
  if (as.numeric(R.version$minor) < 4.1) stop("R version > 4.4.1 is required.")
}

# worskspace cleaning
rm(list = ls())

# set file size limit to 500 MB
options(shiny.maxRequestSize = 500*1024^2)

source("loadPackages.R")
source("logger.R")
source("utilityFunctions.R")
source("modules/navigation.R", local = TRUE)

detectclosejs <- HTML("
function goodbye(e) {
        if(!e) e = window.event;
           //e.cancelBubble is supported by IE - this will kill the bubbling process.
           e.cancelBubble = true;
           e.returnValue = 'You sure you want to leave?'; //This is displayed on the dialog
           
           //e.stopPropagation works in Firefox.
           if (e.stopPropagation) {
           e.stopPropagation();
           e.preventDefault();
           }
}

window.onbeforeunload=goodbye;
")

appCSS <- "
#loading-content {
position: absolute;
background: #FFFFFF;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

ui <-  fluidPage(
  theme = "bootstrap_spacelab.css",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tags$head(tags$script(detectclosejs)),tags$head(tags$script(HTML('
                             Shiny.addCustomMessageHandler("jsCode",
                                                                   function(message) {
                                                                   console.log(message)
                                                                   eval(message.code);
                                                                   }
  );
                                                                   '))),
  tags$head(tags$script('
                                var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  useShinyjs(),
  extendShinyjs(text ='
                      shinyjs.show_modal = function(who){
                        $("#"+who).css("display", "block");
                      }
                      shinyjs.hide_modal = function(who){
                        $("#"+who).css("display", "none");
                      }
                      ', functions = c('show_modal', 'hide_modal')),
  inlineCSS(appCSS),
  # Loading message
  div(
    id = "loading-content",
    h2("Loading Lagun...")
  ),
  hidden(
    div(
      id = "app-content",
      navigation.ui(id = "nav")
    )
  )
)

if (!isTRUE(getOption("shiny.testmode"))) {
  logger$setupSink()
}

server <- function(input, output, session) {
  logger$print("Connection to Lagun")
  
  # Register a function that will be called when an unhandled error occurs and try to print a relevent stacktrace
  onUnhandledError(function(err) {
    print(err)
    # Retrieve relevent stacktrace (more info at https://github.com/rstudio/shiny/wiki/Stack-traces-in-R#capturing)
    calls <- attr(err, "stack.trace", exact = TRUE)
    # https://renkun.me/2020/03/31/a-simple-way-to-show-stack-trace-on-error-in-r/
    if (length(calls) >= 2L) {
      cat("Backtrace:\n")
      calls <- rev(calls[-length(calls)])
      for (i in seq_along(calls)) {
        cat(i, ": ", deparse(calls[[i]], nlines = 1L), "\n", sep = "")
      }
    }
  })
  onSessionEnded(function() {
    logger$print("Disconnection from Lagun")
  })
  window.dimension <- reactiveValues(width=NULL, height = NULL)
  observeEvent(input$dimension,{
    window.dimension$width <- input$dimension[1]
    window.dimension$height <- input$dimension[2]
  })
  
  callModule(navigation.server, "nav", window.dimension)
  
  # Hide the loading message when the rest of the server function has executed
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade")   
  shinyjs::show("app-content")
}

onStart = function() {
  print("Doing application setup")

  onStop(function() {
    print("Doing application cleanup")
    if (!isTRUE(getOption("shiny.testmode"))) {
      logger$unsetSink()
    }
  })
}

# app
shinyApp(ui, server, onStart)