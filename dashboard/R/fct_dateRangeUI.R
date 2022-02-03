#' dateRangeUI 
#'
#' @description Generates the date range UI elements. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

dateRangeUI <- function(namespace){
  fluidRow(
    # column(
    #   4,
    #   dateInput(
    #     inputId = paste0(namespace,"DateFrom"),
    #     label = "From:",
    #     value = Sys.Date()-7
    #   )
    # ),
    # column(
    #   4,
    #   dateInput(
    #     inputId = paste0(namespace,"DateTo"),
    #     label = "To:",
    #     value = Sys.Date()
    #   )
    # ),
    column(
      8,
      dateRangeInput(
        inputId = paste0(namespace,"DateRange"),
        label = NULL,
        start = Sys.Date()-7,
        end = Sys.Date()
      )
    ),
    column(
      4,
      tags$style(
        HTML(
          '.buttonWrapper {margin: 0;position: absolute;top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%);}'
        )
      ),
      div(
        class = "buttonWrapper",
        actionButton(
          inputId = paste0(namespace,"FetchData"),
          label = "Plot"
        )
      )
    )
  )
}