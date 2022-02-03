#' plotSlider 
#'
#' @description Updates slider range base on the currently selected min/max calendar date inputs.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotSlider <- function(inputID, minDate, maxDate){
  sliderInput(
    inputId = inputID,
    "Filter dates:",
    min = as.Date(minDate),
    max = as.Date(maxDate),
    value = c(
      as.Date(minDate),
      as.Date(maxDate)
    ),
    timeFormat="%Y-%m-%d",
    width = '95%',
    animate = animationOptions(1000)
  )
}

