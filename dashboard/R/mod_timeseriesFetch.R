#' timeseriesFetch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timeseriesFetch_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        6,
        dateInput(
          inputId = ns("dateFrom"),
          label = "From:"
        )
      ),
      column(
        6,
        dateInput(
          inputId = ns("dateTo"),
          label = "To:"
        )
      )
    ),
    fluidRow(
      column(
        9,
        uiOutput(
          outputId = ns("dateSlider")
        )
      ),
      column(
        3,
        tags$style(
          HTML(
            '.buttonWrapper {margin: 0;position: absolute;top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%);}'
          )
        ),
        div(
          class = "buttonWrapper",
          actionButton(
            inputId = ns("fetchData"),
            label = "Plot"
          )
        )
      )
    )
  )
}
    
#' timeseriesFetch Server Functions
#'
#' @noRd 
mod_timeseriesFetch_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$dateFrom,{
      print(paste0("From: ", input$dateFrom))
    })
    
    observeEvent(input$dateFrom,{
      print(paste0("To: ", input$dateFrom))
    })
    
    output$dateSlider <- renderUI({
      sliderInput(
        inputId = ns("dateSlider"),
        "Filter dates:",
        min = as.Date(input$dateFrom),
        max = as.Date(input$dateTo),
        value = c(as.Date(input$dateFrom),as.Date(input$dateTo)),
        timeFormat="%Y-%m-%d",
        width = '100%',
        animate = animationOptions(1000)
      )
    })
    
    #data <- reactiveValues()
     #data <- NULL
    observeEvent(input$fetchData,{
      print(weatherTempData$data)
      data <- databaseConnect(sensorCodes = c("sensor_repository_00234", "sensor_repository_00627"))
      #weatherTempData$data 
      #print(weatherTempData$data)
      
      
    })
    
    return(data)
    
    
    
    
    
 
  })
}
    
## To be copied in the UI
# mod_timeseriesFetch_ui("timeseriesFetch_ui_1")
    
## To be copied in the server
# mod_timeseriesFetch_server("timeseriesFetch_ui_1")
