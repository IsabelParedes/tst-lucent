getOptimDescr <- function(Pbdefinition) {
  #
  # Returns the list of optimizers that can be used for the input problem definition
  # by comparison of the problem characteristics (Pbdefinition$tags) and optimizer tags (read in optimizers/"solvers"/"method.R")
  #
  # with the fields: 
  #   - Filename (file to be sourced before running the optimizer)
  #   - name : the optimizer is called with "name.run(Pbdefinition, ParametersValues, AdvancedParametersValues)" 
  #   - the fields of the list returned by the function "name.description(Pbdefinition)"
  #
  if (file.exists("optimizers/listSolverFolders.R")) {
    source("optimizers/listSolverFolders.R");
    SolverFolders <- listSolverFolders()
  }
  else {SolverFolders <- list.dirs(path="./optimizers",full.names = F, recursive = F)}
  
  k <- 0
  Optimizers <- list()
  for (i in seq_len(length(SolverFolders))) 
    # browse the sub-folders of "optimizers"
  {
    #cat(SolverFolders[[i]],"\n")
    if (file.exists(paste("optimizers/",SolverFolders[[i]],"/listSolvers.R",sep=""))) 
    {
      source(paste("optimizers/",SolverFolders[[i]],"/listSolvers.R",sep=""))
      OptimFiles <- lapply(listSolvers(), function(x) paste("optimizers/",SolverFolders[[i]],"/",x,sep=""))
    }
    else {OptimFiles <- list.files(path=paste("optimizers/",SolverFolders[[i]],sep=""),pattern="*.R",full.names = T, recursive = F)}
    
    for (j in seq_len(length(OptimFiles)))
      # retrieve the list of optimizers compatible with the problem definition
    {
      #cat(OptimFiles[[j]],"\n")
      Filename = OptimFiles[[j]]
      source(OptimFiles[[j]])
      name = tools::file_path_sans_ext(basename(OptimFiles[[j]]))
      Optim <- paste(name,".description",sep="")
      if (exists(Optim, mode = "function")) {
        descr <- do.call(Optim,list())
        checkedPb <- unlist(lapply(names(Pbdefinition$tags[Pbdefinition$tags==T]),function(x) isTRUE(descr$tags[[x]])))
        if(isFALSE(Pbdefinition$tags$inversion) & isTRUE(descr$tags$inversion)) checkedPb <- FALSE
        if (length(checkedPb) != 0 && all(checkedPb)){
          k <- k+1
          Optimizers[[k]] <- descr
          Optimizers[[k]]$Filename <- Filename
          Optimizers[[k]]$name <- name
          #cat(k," ",Optimizers[[k]]$name," ",Optimizers[[k]]$dispname,"\n")
        }
      }
    }
  }
  return(Optimizers)
}
