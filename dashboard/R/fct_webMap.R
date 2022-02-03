#' webMap 
#'
#' @description Generates an empty leaflet web-map centered on Perth.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

webMap <- function(){
  leaflet::leaflet() %>%
    leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
    leaflet::setView(115.8613, -31.9523, 8) 
}