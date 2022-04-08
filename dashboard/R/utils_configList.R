#' configList 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

configList <- function(configVals){
  list  <- trimws(unlist(strsplit(configVals, ",")))
  return(list)
}
