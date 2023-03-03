#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny leaflet 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),  
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "litera"),
      title = "SCEVO",
      id = 'navbar',
      tabPanel(
        "Hydrology",
        icon = icon("water"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("hydroMap", height = '700px')
          ),
          column(
            8,
            tabsetPanel(
              id = "hydroTabset",
              type = "tabs",
              tabPanel(
                title = "Flow",
                #style = "overflow-y:scroll; max-height: 700px",
                #tags$details(
                  tags$summary(HTML("Select sites and date range:")),
                  sensorCheckBoxUI(
                    namespace = "hydroFlow",
                    sensorInfo = sensorInfo, 
                    group = "hydro", 
                    subGroup = "Flow"
                  ),
                  dateRangeUI(namespace = "hydroFlow"),
                #),
                plotOutput("hydroFlowPlot", height = "400px"),
                dateSliderUI(namespace = "hydroFlow")
                #timeseriesUI()
              ),
              tabPanel(
                title = "Tide",
                style = "overflow-y:scroll; max-height: 700px",
                plotOutput("hydroTidePlot", height = "400px"),
                tags$details(
                  tags$summary(HTML("Select sites <i>(click to expand)</i>:")),
                  sensorCheckBoxUI(
                    namespace = "hydroTide",
                    sensorInfo = sensorInfo, 
                    group = "hydro", 
                    subGroup = "Tide"
                  )
                ),
                timeseriesUI(namespace = "hydroTide")
              )
            )
          )
        )
      ),
      tabPanel(
        "Weather",
        icon = icon("cloud-sun"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("weatherMap", height = '700px')
          ),
          column(
            8,
            tabsetPanel(
              id = "weatherTabset",
              type = "tabs"
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'secvo'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

