#' sensorMapMarkers 
#'
#' @description Based on the current tab panel, removes previous sensor markers from map and adds the current ones.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

sensorMapMarkers <- function(mapID, data, subGroup){

  subGroupData <- data[data[["subGroup"]]==subGroup,]
  leaflet::leafletProxy(
    mapId = mapID,
    data = subGroupData
  ) %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addCircleMarkers(
      lng = subGroupData[["lon"]], 
      lat = subGroupData[["lat"]], 
      color = "white", 
      radius = 7, 
      weight = 2, 
      fillColor = subGroupData[["colour"]], 
      opacity = 1, 
      fillOpacity = 1,
      popup = paste0("<b>Station Name: </b>",subGroupData[["label"]], "<br><b>Agency: </b>", subGroupData[["agency"]])
    )
}