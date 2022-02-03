#' databaseConnect 
#'
#' @description Establishes connection to the scevo PostgreSQL database and fetches data for one or more sensor codes
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

databaseConnect <- function(sensorCodes){
  driver <- DBI::dbDriver("PostgreSQL")
  connection <- DBI::dbConnect(
    drv = driver,
    host = '130.95.204.63', 
    port='5433', 
    dbname='scevo',
    user='dpaw_write', 
    password='dpaw_write'
  )
  
  fetchedData  <- data.frame(
    "st_sensor_code" = integer(),
    "st_value_1" = double(),
    "datetime" = as.POSIXlt(character())
  )
  
  for(i in sensorCodes){
    sensorData <- dplyr::tbl(connection, i) %>% as.data.frame()
    sensorData  <- sensorData[,c("st_sensor_code","st_value_1","st_feed_date_jdn")]
    fetchedData <- rbind(fetchedData, sensorData)
  }
  
  fetchedData$datetime  <- as.POSIXlt(
    fetchedData$st_feed_date_jdn*86400,
    origin=structure(-210866760000,
                     class=c("POSIXct", "POSIXt"),
                     tzone="Australia/Perth"),
    tz="Australia/Perth"
    )
  
  DBI::dbDisconnect(connection)
  
  return(fetchedData)
}

