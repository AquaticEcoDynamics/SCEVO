#' dateSliderUI 
#'
#' @description Generates the date range UI element. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

dateSliderUI <- function(namespace){
  fluidRow(
    column(
      12,
      shinydashboard::box(
        id = paste0(namespace,"DateSliderBox"),
        width = 12,
        uiOutput(
          outputId = paste0(namespace,"DateSlider")
        )
      )
    )
  )
}
      
    