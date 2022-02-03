## code to prepare `sensorInfo` dataset goes here
sensorInfo <- read.csv("inst/extdata/sensorInfo.csv")
usethis::use_data(sensorInfo, overwrite = TRUE)
