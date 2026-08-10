SimulatorsConnection <- R6Class("SimulatorsConnection",
  public = list(

    launcherServer = NULL,

    initialize = function(launcherServer = NULL) {
      self$launcherServer <- launcherServer
    },

    getLauncherConfig = function() {
      return(self$launcherServer$launcherConfig)
    },

    getRunSets = function() {
        runSetIds <- append(vector(), names(launcherServer$runsSets))

        # Add runSets which are detected scanning 'storingDir'
        tryCatch({
            if (file.exists(launcherServer$launcherConfig$storing_dir)) {
                runDirNames <- list.files(launcherServer$launcherConfig$storing_dir)
                for (runDirName in runDirNames) {
                    fullRunDirName <- file.path(launcherServer$launcherConfig$storing_dir, runDirName)
                    if (dir.exists(fullRunDirName)) {
                        runSetIds <- append(runSetIds, runDirName)
                    }
                }
            }
        },
        error = function(err) {
            logger$print("getRunSets failed")
            logger$print(err)
        })

        runSetIds <- unique(runSetIds)
        # Return 'runSetIds' as a sorted array
        return(runSetIds[order(runSetIds, decreasing = FALSE)])
    }
  )
)
