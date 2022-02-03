#' activeSensorColours 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

activeSensorColours <- function(checkBoxInputs, sensorInfo){
  selectedSites <- checkBoxInputs
  filteredSensorInfo <- sensorInfo[,"sensorCode"] %in% selectedSites
  sensorColours <- sensorInfo[filteredSensorInfo, "colour"]
  return(sensorColours)
}