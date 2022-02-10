#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny 
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  #sensorInfo <- read.csv("inst/extdata/sensorInfo.csv")
  #### WEATHER ####
  
 
  
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
  
  # Filter sensorInfo data to only weather quality sensors
  weatherSensorInfo <- sensorInfo[sensorInfo[["group"]]=="weather",]
  weathersubGroupData <- weatherSensorInfo[weatherSensorInfo[["subGroup"]]=="Temp",]
  # Generates blank weather tab web-map
  output$weatherMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 8) %>% 
      leaflet::addCircleMarkers(
        lng = weathersubGroupData[["lon"]], 
        lat = weathersubGroupData[["lat"]], 
        color = "white", 
        radius = 7, 
        weight = 2, 
        fillColor = weathersubGroupData[["colour"]], 
        opacity = 1, 
        fillOpacity = 1,
        popup = paste0("<b>Station Name: </b>",weathersubGroupData[["label"]], "<br><b>Agency: </b>", weathersubGroupData[["agency"]])
      )
    
  })
  
  # sensorMapMarkers(
  #   mapID = "weatherMap", 
  #   data = weatherSensorInfo, 
  #   subGroup = "Temp"
  # )
  
  #On panel change within the Hydro tabset, update map markers to reflect graphable sensors
  # observeEvent(input$weatherTabset,{
  #   print(input$weatherTabset)
  #   # switch(
  #   #   input$weatherTabset,
  #   #   "Temperature" = sensorMapMarkers(
  #   #     mapID = "weatherMap",
  #   #     data = weatherSensorInfo,
  #   #     subGroup = "Temp"
  #   #   ),
  #   #   NULL
  #   # )
  #   
  # 
  #     sensorMapMarkers(
  #       mapID = "weatherMap",
  #       data = weatherSensorInfo,
  #       subGroup = "Temp"
  #     )
  #   
  # })
  
  # weathersubGroupData <- weatherSensorInfo[weatherSensorInfo[["subGroup"]]=="Temp",]
  # leaflet::leafletProxy(
  #   mapId =  "weatherMap",
  #   data = weathersubGroupData
  # ) %>% 
  #   leaflet::clearMarkers() %>% 
  #   leaflet::addCircleMarkers(
  #     lng = weathersubGroupData[["lon"]], 
  #     lat = weathersubGroupData[["lat"]], 
  #     color = "white", 
  #     radius = 7, 
  #     weight = 2, 
  #     fillColor = weathersubGroupData[["colour"]], 
  #     opacity = 1, 
  #     fillOpacity = 1,
  #     popup = paste0("<b>Station Name: </b>",weathersubGroupData[["label"]], "<br><b>Agency: </b>", weathersubGroupData[["agency"]])
  #   )

  
  
  ##### WEATHER - TEMPERATURE ####
  
  # Update slider from calendar date inputs
  output$weatherTempDateSlider <- renderUI({
    plotSlider(
      inputID = "weatherTempDateSlider",
      minDate = input$weatherTempDateRange[1],
      maxDate = input$weatherTempDateRange[2]
    )
  })
  
  shinyjs::hide("weatherTempDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$weatherTempFetchData,{ 
    shinyjs::show("weatherTempDateSliderBox", anim = TRUE, animType = "fade")
    weatherTempData <- databaseConnect(sensorCodes = input$weatherTempSiteCheckBox) 
    
    # Get line plot colours for selected sensors
    weatherTempDataColours <- activeSensorColours(
      checkBoxInputs = input$weatherTempSiteCheckBox,
      sensorInfo = weatherSensorInfo
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
   
}
