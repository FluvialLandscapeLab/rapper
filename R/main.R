# execute function must rely on variables in the rapper env. Execute function cannot have parameters
rapper = function(drivingValues, init, execute) {
  # set parent environment to make the new rapper env a sibling of the global environment
  newRapper = new.env(parent = parent.env(globalenv()))

  # install the objects passed to the function
  newRapper$drivingValues = drivingValues
  newRapper$init = init
  newRapper$execute = execute

  # set the environment of the init and execute functions so that they run
  # within the rapper environment
  environment(newRapper$init) = newRapper
  environment(newRapper$execute) = newRapper

  # set the class
  class(newRapper) = "rapper"

  return(newRapper)
}
# perhaps initRapper should automatically install the ... list in rapper and then call the init function with no parameters. When you write init function you have to know to make changes to the parent.frame
initRapper = function(rapper, ...) {
  do.call(rapper$init, list(...), envir = rapper)
  # rapper$init(rapper, ...)
}

executeRapper = function(rapper) {
  lapply(0:nrow((rapper$drivingValues) - 1), updateAndExecute, rapper = rapper)
}

updateAndExecute = function(rapper, timeStep) {
  rapper$timestep = timeStep
  #do something here to set the driving variables
  currentDrivingValues = drivingValues[as.character(timeStep),]
  if(is.na(currentDrivingValues)) stop("The timestep '", timeStep, "' was not found as a row name in the driving values." )
  mapply(assign, x = names(drivingValues), value = currentDrivingValues, moreArgs = list(envir = rapper))
  rapper$execute()
}
