#' sensorCheckBoxUI 
#'
#' @description Generates UI checkbox elements from a provided @param sensorInfo df.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

sensorCheckBoxUI <- function(namespace,sensorInfo, group, subGroup){
  
  groupData <- sensorInfo[sensorInfo[["group"]]==group,]
  subGroupData <- groupData[groupData[["subGroup"]]==subGroup,]
  
  choiceNames <- lapply(1:nrow(subGroupData), function(i) {
    span(
      icon(
        subGroupData[,"icon"][i], 
        style = paste0("color:",subGroupData[,"colour"][i],";")
        ), 
      paste0(" ",subGroupData[,"label"][i]))
  })
  
  return(
    tags$div(
      column(
        12,
        fluidRow(
          checkboxGroupInput(
            inputId = paste0(namespace,"SiteCheckBox"),
            label = NULL,#"Select sites:",
            inline = TRUE,
            choiceNames = choiceNames,
            choiceValues =  subGroupData[,"sensorCode"]
          )
        )
      )
    )
  )
  
  
  
}
