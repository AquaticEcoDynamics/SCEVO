#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny RPostgreSQL DBI
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  #sensorInfo <- base::readRDS("inst/extdata/sensorInfo.csv")
  #sensorInfo <- base::load("inst/extdata/sensorInfo.rda")
  #### WEATHER ####
  
  sensorInfo <- scevo::sensorInfo
  
  #### HYDRO ####
  
  # Filter sensorInfo data to only hydro sensors
  hydroSensorInfo <- sensorInfo[sensorInfo[["group"]]=="hydro",]
  
  # Generates blank hydro tab web-map
  output$hydroMap <- leaflet::renderLeaflet({
    webMap()
  })
  
  # On panel change within the Hydro tabset, update map markers to reflect graphable sensors
  observeEvent(input$hydroTabset,{
    switch(
      input$hydroTabset,
      "Flow" = sensorMapMarkers(
        mapID = "hydroMap", 
        data = hydroSensorInfo, 
        subGroup = input$hydroTabset
        ),
      "Tide" = sensorMapMarkers(
        mapID = "hydroMap", 
        data = hydroSensorInfo, 
        subGroup = input$hydroTabset
      )
    )
  })
  
  ##### HYDRO - FLOW ####
  
  # Update slider from calendar date inputs
  output$hydroFlowDateSlider <- renderUI({
    plotSlider(
      inputID = "hydroFlowDateSlider",
      minDate = input$hydroFlowDateRange[1],
      maxDate = input$hydroFlowDateRange[2]
    )
  })
  
  shinyjs::hide("hydroFlowDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$hydroFlowFetchData,{ 
    shinyjs::show("hydroFlowDateSliderBox", anim = TRUE, animType = "fade")
    hydroFlowData <- databaseConnect(sensorCodes = input$hydroFlowSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    hydroFlowDataColours <- activeSensorColours(
      checkBoxInputs = input$hydroFlowSiteCheckBox,
      sensorInfo = hydroSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$hydroFlowPlot <- renderPlot({
      hydroFlowData <- dplyr::filter(
        hydroFlowData,
        datetime >= as.POSIXct(input$hydroFlowDateSlider[1]),
        datetime <= as.POSIXct(input$hydroFlowDateSlider[2])
      )
    
    # Plot the graph    
    plotLine(
      plotData = hydroFlowData,
      plotDataX = "datetime",
      plotDataY = "st_value_1",
      plotDataGroup = "st_sensor_code",
      plotLabelX = "Date",
      plotLabelY = "Flow (m3/s)",
      plotDataColours = hydroFlowDataColours
    )
    })
  })
  
  ##### HYDRO - TIDE ####
  
  # Update slider from calendar date inputs
  output$hydroTideDateSlider <- renderUI({
    plotSlider(
      inputID = "hydroTideDateSlider",
      minDate = input$hydroTideDateFrom,
      maxDate = input$hydroTideDateTo
    )
  })
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$hydroTideFetchData,{ 
    print(input$hydroTideSiteCheckBox)
    hydroTideData <- databaseConnect(sensorCodes = input$hydroTideSiteCheckBox) 
    print(head(hydroTideData))
    # Get line plot colours for selected sensors
    hydroTideDataColours <- activeSensorColours(
      checkBoxInputs = input$hydroTideSiteCheckBox,
      sensorInfo = hydroSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$hydroTidePlot <- renderPlot({
      hydroTideData <- dplyr::filter(
        hydroTideData,
        datetime >= as.POSIXct(input$hydroTideDateSlider[1]),
        datetime <= as.POSIXct(input$hydroTideDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = hydroTideData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Tide (mAHD)",
        plotDataColours = hydroTideDataColours
      )
    })
  })
  
  #### WEATHER #### 
  
  # Fetch weather station data from config to map
  weatherMapStations <- data.frame(
    name = configList(get_golem_config("name", config = "mod_weather_temp")),
    colour = configList(get_golem_config("colour", config = "mod_weather_temp")),
    source = configList(get_golem_config("source", config = "mod_weather_temp")),
    lat = configList(get_golem_config("lat", config = "mod_weather_temp")),
    lon = configList(get_golem_config("lon", config = "mod_weather_temp"))
  )
  # Generates blank weather tab web-map
  output$weatherMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 8) 
  })
  #outputOptions(output, "weatherMap", suspendWhenHidden = FALSE)
  
  observe({
    input$navbar
    input$weatherTabset
    if(input$navbar=="Weather" && input$weatherTabset=="Temperature"){
      leaflet::leafletProxy(
        mapId = 'weatherMap'
      ) %>% 
        leaflet::clearMarkers() %>% 
        leaflet::addCircleMarkers(
          lng = as.numeric(weatherMapStations[["lon"]]), 
          lat = as.numeric(weatherMapStations[["lat"]]), 
          color = "white", 
          radius = 7, 
          weight = 2, 
          fillColor = weatherMapStations[["colour"]], 
          opacity = 1, 
          fillOpacity = 1,
          popup = paste0("<b>Station Name: </b>",weatherMapStations[["name"]], "<br><b>Agency: </b>", weatherMapStations[["source"]])
        )
    }
  })
  
  
  
  ##### WEATHER - TEMPERATURE ####
  
  modWeatherEnable <- as.logical(
    get_golem_config("enable", config = "mod_weather")
    )
  
  modWeatherTempEnable <- as.logical(
    get_golem_config("enable", config = "mod_weather_temp")
  )
  modWeatherTempName <- configList(
    get_golem_config("name", config = "mod_weather_temp")
  )
  modWeatherTempCode <- configList(
    get_golem_config("sensor_code", config = "mod_weather_temp")
  )
  modWeatherTempColour <- configList(
    get_golem_config("colour", config = "mod_weather_temp")
  )
  
 
  if(isTRUE(modWeatherTempEnable))
    {

    print(paste0("modWeatherTempEnable: ", modWeatherTempEnable))
      
      insertTab(
        inputId = "weatherTabset",
        select = TRUE,
        tabPanel(
          title = "Temperature",
          tags$summary(HTML("Select sites and date range:")),
          checkboxGroupInput(
            inputId = "weatherTempSiteCheckBox",
            label = NULL,
            inline = TRUE,
            choiceNames = modWeatherTempName,
            choiceValues =  modWeatherTempCode
          ),
          fluidRow(
            column(
              8,
              dateRangeInput(
                inputId = "weatherTempDateRange",
                label = NULL,
                start = Sys.Date()-7,
                end = Sys.Date()
              )
            ),
            column(
              4,
              actionButton(
                inputId = "weatherTempFetchData",
                label = "Plot"
              )
            )
          ),
          plotOutput("weatherTempPlot", height = "400px"),
          uiOutput("weatherTempDateSliderUI")
        )
      )
      
  } else {
      print('no tab')
      return(NULL)
    }
  
  output$weatherTempDateSliderUI <- renderUI({
    sliderInput(
      inputId = "weatherTempDateSlider",
      "Filter dates:",
      min = as.Date(input$weatherTempDateRange[1]),
      max = as.Date(input$weatherTempDateRange[2]),
      value = c(
        as.Date(input$weatherTempDateRange[1]),
        as.Date(input$weatherTempDateRange[2])
      ),
      timeFormat="%Y-%m-%d",
      width = '95%',
      animate = animationOptions(1000)
    )
  })
  
  
  observeEvent(input$weatherTempFetchData,{ 
    #browser()
    weatherTempData <- databaseConnect(
      sensorCodes = input$weatherTempSiteCheckBox
      )
   
    # Get line plot colours for selected sensors
    weatherTempDataColours <- sensorColours(
      allCodes = modWeatherTempCode,
      allColours = modWeatherTempColour,
      selectedCodes = input$weatherTempSiteCheckBox
    )
    # Generate line graph from fetched data and display between slider dates
    output$weatherTempPlot <- renderPlot({
      weatherTempData <- dplyr::filter(
        weatherTempData,
        datetime >= as.POSIXct(input$weatherTempDateSlider[1]),
        datetime <= as.POSIXct(input$weatherTempDateSlider[2])
      )
      # Plot the graph    
      plotLine(
        plotData = weatherTempData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Temperature (°C)",
        plotDataColours = weatherTempDataColours
      )
    })
    
  })  
  
  ######################## EXPERIMENTAL ####################################
  
  # Experimenting with generating completely new tabs from the `mod_init`
  # config in golem-config.yml
  # e.g. a user can define a custom module other than predefined ones like
  # 'hydro' or 'weather'
  
  modInit <- data.frame(
    name = configList(get_golem_config("name", config = "mod_init")),
    id = configList(get_golem_config("id", config = "mod_init")),
    icon = configList(get_golem_config("icon", config = "mod_init"))
  )
  
  for(i in 1:NROW(modInit)){
    insertTab(
      inputId = 'navbar',
      tabPanel(
        title = modInit[i,"name"],
        icon = shiny::icon(modInit[i,"icon"]),
        fluidRow(
          column(
            4,
            
          )
        )
      )
    )
    
    mod_leafletMap_server(modInit[i,"name"])
  }
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  