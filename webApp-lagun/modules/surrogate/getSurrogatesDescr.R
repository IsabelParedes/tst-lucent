getSurrogateDescr <- function(LAGUN_SURROGATES_DIRECTORY, Xinfos) {
  #
  # Returns the list of optimizers that can be used for the input problem definition
  # by comparison of the problem characteristics (Pbdefinition$tags) and optimizer tags (read in optimizers/"solvers"/"method.R")
  #
  # with the fields: 
  #   - Filename (file to be sourced before running the optimizer)
  #   - name : the optimizer is called with "name.run(Pbdefinition, ParametersValues, AdvancedParametersValues)" 
  #   - the fields of the list returned by the function "name.description(Pbdefinition)"
  #
  if (!file.exists(LAGUN_SURROGATES_DIRECTORY)) {
    return(list())
  }
  if (file.exists(paste(LAGUN_SURROGATES_DIRECTORY,"/listSurrogateFolders.R",sep=""))) {
    source(paste(LAGUN_SURROGATES_DIRECTORY,"/listSurrogateFolders.R",sep=""));
    SurrogateFolders <- listSurrogateFolders()
  }
  else {SurrogateFolders <- list.dirs(path=LAGUN_SURROGATES_DIRECTORY,full.names = F, recursive = F)}
  
  k <- 0
  filteredSurrogates <- list()
  kAll <- 0
  allSurrogates <- list()
  for (i in seq_len(length(SurrogateFolders))) 
    # browse the sub-folders of "surrogatesmodels"
  {
    #cat(SurrogateFolders[[i]],"\n")
    if (file.exists(paste(LAGUN_SURROGATES_DIRECTORY,"/",SurrogateFolders[[i]],"/listSurrogates.R",sep=""))) 
    {
      source(paste(LAGUN_SURROGATES_DIRECTORY,"/",SurrogateFolders[[i]],"/listSurrogates.R",sep=""))
      SurrogateFiles <- lapply(listSurrogates(), function(x) paste(LAGUN_SURROGATES_DIRECTORY,"/",SurrogateFolders[[i]],"/",x,sep=""))
    }
    else {SurrogateFiles <- list.files(path=paste(LAGUN_SURROGATES_DIRECTORY,"/",SurrogateFolders[[i]],sep=""),pattern="*.R",full.names = T, recursive = F)}
    
    categorical <- which(sapply(Xinfos, function(var){var$type}) == 'categorical')

    for (j in seq_len(length(SurrogateFiles)))
      # retrieve the list of surrogates
    {
      #cat(SurrogateFiles[[j]],"\n")
      Filename = SurrogateFiles[[j]]
      source(SurrogateFiles[[j]])
      name = tools::file_path_sans_ext(basename(SurrogateFiles[[j]]))
      Surrogate <- paste(name,".description",sep="")
      if (exists(Surrogate, mode = "function")) {
        descr <- do.call(Surrogate,list())
        if ((length(categorical) > 0) && descr$tags$CategoricalInputs) {
          k <- k+1
          filteredSurrogates[[k]] <- descr
          filteredSurrogates[[k]]$Filename <- Filename
          filteredSurrogates[[k]]$name <- name
        }else if (length(categorical) == 0){
          k <- k+1
          filteredSurrogates[[k]] <- descr
          filteredSurrogates[[k]]$Filename <- Filename
          filteredSurrogates[[k]]$name <- name
        }
        kAll <- kAll+1
        allSurrogates[[kAll]] <- descr
        allSurrogates[[kAll]]$Filename <- Filename
        allSurrogates[[kAll]]$name <- name
      }
     }
  }
  return(list(filtered = filteredSurrogates, all = allSurrogates))
}
