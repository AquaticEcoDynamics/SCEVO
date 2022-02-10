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
    # Your application UI logic 
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "litera"),
      title = "SCEVO",
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
              type = "tabs",
              tabPanel(
                title = "Temperature",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "weatherTemp",
                  sensorInfo = sensorInfo, 
                  group = "weather", 
                  subGroup = "Temp"
                ),
                dateRangeUI(namespace = "weatherTemp"),
                plotOutput("weatherTempPlot", height = "400px"),
                dateSliderUI(namespace = "weatherTemp")
              )
            )
          )
        )
      ),
      tabPanel(
        "Water Quality",
        icon = icon("vial"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("wqMap", height = '700px')
          ),
          column(
            8,
            tabsetPanel(
              id = "wqTabset",
              type = "tabs",
              tabPanel(
                title = "Conductivity",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "wqConductivity",
                  sensorInfo = sensorInfo, 
                  group = "wq", 
                  subGroup = "Conductivity"
                ),
                dateRangeUI(namespace = "wqConductivity"),
                plotOutput("wqConductivityPlot", height = "400px"),
                dateSliderUI(namespace = "wqConductivity")
              ),
              tabPanel(
                title = "Salinity",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "wqSalinity",
                  sensorInfo = sensorInfo, 
                  group = "wq", 
                  subGroup = "Salinity"
                ),
                dateRangeUI(namespace = "wqSalinity"),
                plotOutput("wqSalinityPlot", height = "400px"),
                dateSliderUI(namespace = "wqSalinity")
              ),
              tabPanel(
                title = "pH",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "wqpH",
                  sensorInfo = sensorInfo, 
                  group = "wq", 
                  subGroup = "pH"
                ),
                dateRangeUI(namespace = "wqpH"),
                plotOutput("wqpHPlot", height = "400px"),
                dateSliderUI(namespace = "wqpH")
              ),
              tabPanel(
                title = "Temperature",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "wqTemp",
                  sensorInfo = sensorInfo, 
                  group = "wq", 
                  subGroup = "Temp"
                ),
                dateRangeUI(namespace = "wqTemp"),
                plotOutput("wqTempPlot", height = "400px"),
                dateSliderUI(namespace = "wqTemp")
              ),
              tabPanel(
                title = "Dissolved Oxygen",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "wqDO",
                  sensorInfo = sensorInfo, 
                  group = "wq", 
                  subGroup = "DO"
                ),
                dateRangeUI(namespace = "wqDO"),
                plotOutput("wqDOPlot", height = "400px"),
                dateSliderUI(namespace = "wqDO")
              ),
              tabPanel(
                title = "Dissolved Oxygen - Saturation",
                tags$summary(HTML("Select sites and date range:")),
                sensorCheckBoxUI(
                  namespace = "wqDOSat",
                  sensorInfo = sensorInfo, 
                  group = "wq", 
                  subGroup = "DOSaturation"
                ),
                dateRangeUI(namespace = "wqDOSat"),
                plotOutput("wqDOSatPlot", height = "400px"),
                dateSliderUI(namespace = "wqDOSat")
              )
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

