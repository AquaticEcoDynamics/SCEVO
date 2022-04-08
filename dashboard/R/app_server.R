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
  
  
  
  # # Update slider from calendar date inputs
  # output$weatherTempDateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "weatherTempDateSlider",
  #     minDate = input$weatherTempDateRange[1],
  #     maxDate = input$weatherTempDateRange[2]
  #   )
  # })
  # 
  # shinyjs::hide("weatherTempDateSliderBox")
  
  # # On button click, fetch sensor data from SCEVO and graph
  # observeEvent(input$weatherTempFetchData,{ 
  #   shinyjs::show("weatherTempDateSliderBox", anim = TRUE, animType = "fade")
  #   weatherTempData <- databaseConnect(sensorCodes = input$weatherTempSiteCheckBox) 
  #   
  #   # Get line plot colours for selected sensors
  #   weatherTempDataColours <- activeSensorColours(
  #     checkBoxInputs = input$weatherTempSiteCheckBox,
  #     sensorInfo = weatherSensorInfo
  #   )
  #   
  #   # Generate line graph from fetched data and display between slider dates
  #   output$weatherTempPlot <- renderPlot({
  #     
  #     weatherTempData <- dplyr::filter(
  #       weatherTempData,
  #       datetime >= as.POSIXct(input$weatherTempDateSlider[1]),
  #       datetime <= as.POSIXct(input$weatherTempDateSlider[2])
  #     )
  #     
  #     # Plot the graph    
  #     plotLine(
  #       plotData = weatherTempData,
  #       plotDataX = "datetime",
  #       plotDataY = "st_value_1",
  #       plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "Temperature (°C)",
  #       plotDataColours = weatherTempDataColours
  #     )
  #   })
  # })
  
  
  
  
  
  
  
  
  
  
  #### WATER QUALITY ####
  
  # Filter sensorInfo data to only water quality sensors
  wqSensorInfo <- sensorInfo[sensorInfo[["group"]]=="wq",]
  wqsubGroupData <- wqSensorInfo[wqSensorInfo[["subGroup"]]=="DOSaturation",]
  wqsubGroupData <- wqsubGroupData[1:2,]
  # Generates blank waterquality tab web-map
  output$wqMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 8) %>% 
      leaflet::addCircleMarkers(
        lng = wqsubGroupData[["lon"]], 
        lat = wqsubGroupData[["lat"]], 
        color = "white", 
        radius = 7, 
        weight = 2, 
        fillColor = wqsubGroupData[["colour"]], 
        opacity = 1, 
        fillOpacity = 1,
        popup = paste0("<b>Station Name: </b>",wqsubGroupData[["label"]], "<br><b>Agency: </b>", wqsubGroupData[["agency"]])
      )
  })
  
  ##### WATER QUALITY - CONDUCTIVITY ####
  
  # Update slider from calendar date inputs
  output$wqConductivityDateSlider <- renderUI({
    plotSlider(
      inputID = "wqConductivityDateSlider",
      minDate = input$wqConductivityDateRange[1],
      maxDate = input$wqConductivityDateRange[2]
    )
  })
  
  shinyjs::hide("wqConductivityDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqConductivityFetchData,{ 
    shinyjs::show("wqConductivityDateSliderBox", anim = TRUE, animType = "fade")
    wqConductivityData <- databaseConnect(sensorCodes = input$wqConductivitySiteCheckBox) 
    
    # Get line plot colours for selected sensors
    wqConductivityDataColours <- activeSensorColours(
      checkBoxInputs = input$wqConductivitySiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqConductivityPlot <- renderPlot({

      wqConductivityData <- dplyr::filter(
        wqConductivityData,
        datetime >= as.POSIXct(input$wqConductivityDateSlider[1]),
        datetime <= as.POSIXct(input$wqConductivityDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqConductivityData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Conductivity (uS/cm)",
        plotDataColours = wqConductivityDataColours
      )
    })
  })
  
  ##### WATER QUALITY - Salinity ####
  
  # Update slider from calendar date inputs
  output$wqSalinityDateSlider <- renderUI({
    plotSlider(
      inputID = "wqSalinityDateSlider",
      minDate = input$wqSalinityDateRange[1],
      maxDate = input$wqSalinityDateRange[2]
    )
  })
  
  shinyjs::hide("wqSalinityDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqSalinityFetchData,{ 
    
    shinyjs::show("wqSalinityDateSliderBox", anim = TRUE, animType = "fade")
    wqSalinityData <- databaseConnect(sensorCodes = input$wqSalinitySiteCheckBox) 
    
    # Get line plot colours for selected sensors
    wqSalinityDataColours <- activeSensorColours(
      checkBoxInputs = input$wqSalinitySiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqSalinityPlot <- renderPlot({
      
      wqSalinityData <- dplyr::filter(
        wqSalinityData,
        datetime >= as.POSIXct(input$wqSalinityDateSlider[1]),
        datetime <= as.POSIXct(input$wqSalinityDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqSalinityData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Salinity (PSU)",
        plotDataColours = wqSalinityDataColours
      )
    })
  })
  
  ##### WATER QUALITY - pH ####
  
  # Update slider from calendar date inputs
  output$wqpHDateSlider <- renderUI({
    plotSlider(
      inputID = "wqpHDateSlider",
      minDate = input$wqpHDateRange[1],
      maxDate = input$wqpHDateRange[2]
    )
  })
  
  shinyjs::hide("wqpHDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqpHFetchData,{ 
    
    shinyjs::show("wqpHDateSliderBox", anim = TRUE, animType = "fade")
    wqpHData <- databaseConnect(sensorCodes = input$wqpHSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    wqpHDataColours <- activeSensorColours(
      checkBoxInputs = input$wqpHSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqpHPlot <- renderPlot({
      
      wqpHData <- dplyr::filter(
        wqpHData,
        datetime >= as.POSIXct(input$wqpHDateSlider[1]),
        datetime <= as.POSIXct(input$wqpHDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqpHData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "pH",
        plotDataColours = wqpHDataColours
      )
    })
  })
  
  ##### WATER QUALITY - Temperature ####
  
  # Update slider from calendar date inputs
  output$wqTempDateSlider <- renderUI({
    plotSlider(
      inputID = "wqTempDateSlider",
      minDate = input$wqTempDateRange[1],
      maxDate = input$wqTempDateRange[2]
    )
  })
  
  shinyjs::hide("wqTempDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqTempFetchData,{ 
    
    shinyjs::show("wqTempDateSliderBox", anim = TRUE, animType = "fade")
    wqTempData <- databaseConnect(sensorCodes = input$wqTempSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    wqTempDataColours <- activeSensorColours(
      checkBoxInputs = input$wqTempSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqTempPlot <- renderPlot({
      
      wqTempData <- dplyr::filter(
        wqTempData,
        datetime >= as.POSIXct(input$wqTempDateSlider[1]),
        datetime <= as.POSIXct(input$wqTempDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqTempData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Temperature (°C)",
        plotDataColours = wqTempDataColours
      )
    })
  })
  
  ##### WATER QUALITY - DO ####
  
  # Update slider from calendar date inputs
  output$wqDODateSlider <- renderUI({
    plotSlider(
      inputID = "wqDODateSlider",
      minDate = input$wqDODateRange[1],
      maxDate = input$wqDODateRange[2]
    )
  })
  
  shinyjs::hide("wqDODateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqDOFetchData,{ 
    
    shinyjs::show("wqDODateSliderBox", anim = TRUE, animType = "fade")
    wqDOData <- databaseConnect(sensorCodes = input$wqDOSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    wqDODataColours <- activeSensorColours(
      checkBoxInputs = input$wqDOSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqDOPlot <- renderPlot({
      
      wqDOData <- dplyr::filter(
        wqDOData,
        datetime >= as.POSIXct(input$wqDODateSlider[1]),
        datetime <= as.POSIXct(input$wqDODateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqDOData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Dissolved Oxygen (mg/L)",
        plotDataColours = wqDODataColours
      )
    })
  })
  
  ##### WATER QUALITY - DO SAT ####
  
  # Update slider from calendar date inputs
  output$wqDOSatDateSlider <- renderUI({
    plotSlider(
      inputID = "wqDOSatDateSlider",
      minDate = input$wqDOSatDateRange[1],
      maxDate = input$wqDOSatDateRange[2]
    )
  })
  
  shinyjs::hide("wqDOSatDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqDOSatFetchData,{ 
    
    shinyjs::show("wqDOSatDateSliderBox", anim = TRUE, animType = "fade")
    wqDOSatData <- databaseConnect(sensorCodes = input$wqDOSatSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    wqDOSatDataColours <- activeSensorColours(
      checkBoxInputs = input$wqDOSatSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqDOSatPlot <- renderPlot({
      
      wqDOSatData <- dplyr::filter(
        wqDOSatData,
        datetime >= as.POSIXct(input$wqDOSatDateSlider[1]),
        datetime <= as.POSIXct(input$wqDOSatDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqDOSatData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "DO Saturation (%)",
        plotDataColours = wqDOSatDataColours
      )
    })
  })
  
  #### MOORING ####
  
  # Generates blank mooring tab web-map
  output$moorMap <- leaflet::renderLeaflet({
    webMap()
  })
  
  moorConfig <-  configFetch(
    configName = "mod_mooring",
    valueNames = "enable"
  )

  moorSysBatConfig <-  configFetch(
    configName = "mod_mooring_sysBatt", 
    valueNames = c('enable', 'name', 'sensor_code', 'colour')
    )
  
  
  if(isTRUE(as.logical(moorConfig$enable))){
    if(isTRUE(as.logical(moorSysBatConfig$enable))){
      insertTab(
        inputId = "moorTabset",
        select = TRUE,
        tabPanel(
          title = "System Battery",
          tags$summary(HTML("Select sites and date range:")),
          checkboxGroupInput(
            inputId = "moorSysBatSiteCheckBox",
            label = NULL,
            inline = TRUE,
            choiceNames = moorSysBatConfig$name,
            choiceValues =  moorSysBatConfig$sensor_code
          ),
          fluidRow(
            column(
              8,
              dateRangeInput(
                inputId = "moorSysBatDateRange",
                label = NULL,
                start = Sys.Date()-7,
                end = Sys.Date()
              )
            ),
            column(
              4,
              actionButton(
                inputId = "moorSysBatFetchData",
                label = "Plot"
              )
            )
          ),
          plotOutput("moorSysBatPlot", height = "400px"),
          uiOutput("moorSysBatDateSliderUI")
        )
      )
    }
    
    output$moorSysBatDateSliderUI <- renderUI({
      sliderInput(
        inputId = "moorSysBatDateSlider",
        "Filter dates:",
        min = as.Date(input$moorSysBatDateRange[1]),
        max = as.Date(input$moorSysBatDateRange[2]),
        value = c(
          as.Date(input$moorSysBatDateRange[1]),
          as.Date(input$moorSysBatDateRange[2])
        ),
        timeFormat="%Y-%m-%d",
        width = '95%',
        animate = animationOptions(1000)
      )
    })
    
    observeEvent(input$moorSysBatFetchData,{ 
      moorSysBatData <- databaseConnect(
        sensorCodes = input$moorSysBatSiteCheckBox
      )
      
      # Get line plot colours for selected sensors
      moorSysBatDataColours <- sensorColours(
        allCodes = moorSysBatConfig$sensor_code,
        allColours = moorSysBatConfig$colour,
        selectedCodes = input$moorSysBatSiteCheckBox
      )
      # Generate line graph from fetched data and display between slider dates
      output$moorSysBatPlot <- renderPlot({
        moorSysBatData <- dplyr::filter(
          moorSysBatData,
          datetime >= as.POSIXct(input$moorSysBatDateSlider[1]),
          datetime <= as.POSIXct(input$moorSysBatDateSlider[2])
        )
        # Plot the graph    
        plotLine(
          plotData = moorSysBatData,
          plotDataX = "datetime",
          plotDataY = "st_value_1",
          plotDataGroup = "st_sensor_code",
          plotLabelX = "Date",
          plotLabelY = "Volts",
          plotDataColours = moorSysBatDataColours
        )
      })
    })
    
    
  }
 
   
}

