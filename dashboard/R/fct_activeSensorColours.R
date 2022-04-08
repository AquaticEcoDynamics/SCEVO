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


sensorColours <- function(allCodes, allColours, selectedCodes) {
  lookup <- data.frame(
    codes = allCodes,
    colours = allColours
  )
  lookup <- lookup[lookup$codes %in% selectedCodes,]
  selectedColours <- lookup$colours
  return(selectedColours)
}
