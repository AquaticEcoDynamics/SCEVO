#' leafletMap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_leafletMap_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(leafletMap, height = '700px')
  )
}
    
#' leafletMap Server Functions
#'
#' @noRd 
mod_leafletMap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$leafletMap <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
        leaflet::setView(115.8613, -31.9523, 8) 
    })
 
  })
}
    
## To be copied in the UI
# mod_leafletMap_ui("leafletMap_ui_1")
    
## To be copied in the server
# mod_leafletMap_server("leafletMap_ui_1")
