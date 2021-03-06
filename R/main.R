# execute function must rely on variables in the rapper env. Execute function
# must take only the model environment as an argument.

#' Create Rapper Object
#'
#' Creates an S3 object consisting of an environment with the \code{class}
#' attribute set to "rapper." This object is then passed to
#' \code{\link{executeRapper}} to execute a model in the rapper environment.
#' @param execute A function containing model code. This function will be
#'   executed iteratively, once for each timestep (see \code{drivingValues}
#'   argument). The function should accept one and only one argument
#'   representing the \code{environment} within which model parameters, driving
#'   values, and state variables exist, typically a \code{rapper} object.
#' @param drivingValues A \code{data.frame} with columns named for driving
#'   variables required by the function associated with the \code{execute} argument.
#'   Each row represents the values of the driving variables for one timestep.
#'   The \code{row.names} must be equal to \code{0:nTimeSteps} where nTimeSteps
#'   is the number of timesteps to execute the simulation.
#' @param ... Named arguments describing model parameters or inital conidtion
#'   for state variables where the name of the argument represents the variable
#'   name that will store the parameter value within the rapper environment.
#' @param initValues An alternative way to specify parameters and initial values
#'   of state variables using a named list. May be used in conjunction with
#'   \code{...} arguments.
#' @return Returns a rapper object (an \code{environment} with the \code{class}
#'   attribute set to "rapper"). The rapper object contains \code{names} bound
#'   to parameter values and initial state variable values passed in by the
#'   user. It also contains a sub-\code{environment} called ".config" containing
#'   the driving variables \code{data.frame} and the \code{execute} function
#'   passed in by the user.
#' @export

rapper = function(execute, drivingValues, ..., initValues = list()) {

  if(!is.data.frame(drivingValues)) stop("Argument 'drivingValues' must be (or inherit from) a data.frame.")
  if(is.null(drivingValues) | "" %in% names(drivingValues)) stop("Columns in 'drivingValues' must be named.  (Column names are used as variable names in the model.)")

  # set parent environment to make the new rapper env a sibling of the global environment
  newRapper = new.env(parent = parent.env(globalenv()))
  newRapper$.config = new.env(parent = newRapper)

  # install the objects passed to the function
  newRapper$.config$drivingValues = drivingValues
  #newRapper$.config$init = init
  newRapper$.config$execute = execute
  # set the environment of the init and execute functions so that they run
  # within the rapper environment
  #environment(newRapper$.config$init) = newRapper
  environment(newRapper$.config$execute) = newRapper

  # install the initial values passed by the user.
  if(class(initValues)!="list") stop("Argument 'initValues' must be a list!")
  initValues = c(list(...), initValues)
  initNames = names(initValues)

  if(length(initValues) != 0) {
    if("" %in% initNames) stop("Names of values passed in '...' are used as model variable names.  Therefore, values passed in '...' must be named.")
    mapply(assign, x = initNames, value = initValues, MoreArgs = list(envir = newRapper))
  }
  #lapply(assign, names(list(...))

  # set the class
  class(newRapper) = "rapper"

  return(newRapper)
}
# perhaps initRapper should automatically install the ... list in rapper and then call the init function with no parameters. When you write init function you have to know to make changes to the parent.frame
#initRapper = function(rapper, ...) {
#  do.call(rapper$init, list(...), envir = rapper)
  # rapper$init(rapper, ...)
#}

#' Execute A "Rapped" Model
#'
#' Iterative execution of a simulation model built as a \code{\link{rapper}}
#' object.
#' @param rapper A model built as a \code{\link{rapper}} object.
#' @return A list containing the return values from the model \code{execute}
#'   function (see \code{\link{rapper}}). Each item in the list represents model
#'   output for each timestep.
#' @export
executeRapper = function(rapper) {
  environment(updateAndExecute) = rapper
  lapply(0:(nrow(rapper$.config$drivingValues) - 1), updateAndExecute, rapper = rapper)
}

updateAndExecute = function(rapper, timeStep) {
  rapper$timestep = timeStep
  #do something here to set the driving variables
  if(!(as.character(timeStep) %in% row.names(.config$drivingValues))) stop("The timestep '", timeStep, "' was not found as a row name in the driving values." )
   currentDrivingValues = .config$drivingValues[as.character(timeStep),]
  mapply(assign, x = names(currentDrivingValues), value = currentDrivingValues, MoreArgs = list(envir = rapper))
  rapper$.config$execute(rapper)
}
