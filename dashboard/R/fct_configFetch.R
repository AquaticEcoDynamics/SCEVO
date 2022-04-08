#' configFetch 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

configFetch <- function(configName, valueNames){
  config <- configName
  values <- valueNames
  configValues <- NULL
  for(i in values){
    configValues[i] <-  get_golem_config(i, config = config)
  }
  return(as.list(configValues))
}