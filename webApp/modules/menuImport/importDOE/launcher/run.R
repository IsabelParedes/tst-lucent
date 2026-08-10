Run <- R6Class("Run",
  public = list(

    id = NULL,

    status = NULL,

    simulatorId = NULL,

    paramNames = NULL,

    paramValues = NULL,

    result = NULL,

    queueState = NA,

    actionId = NULL,

    initialize = function(id, status = NULL, simulatorId = NULL, paramNames = NULL, paramValues = NULL, result = NULL) {
      self$id <- id
      self$status <- status
      self$simulatorId <- simulatorId
      self$paramNames <- paramNames
      self$paramValues <- paramValues
      self$result <- result
      self$queueState <- NA
      self$actionId <- NULL
    }
  )
)
