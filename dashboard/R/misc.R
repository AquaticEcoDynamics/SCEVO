# 
# 
# 
# foo(
#   tabInputId = "moorTabset",
#   tabPanelTitle = "System Battery",
#   checkboxGroupInputId = "moorSysBatSiteCheckBox",
#   checkboxGroupChoiceNames = moorSysBatConfig$name,
#   checkboxGroupChoiceValues = moorSysBatConfig$sensor_code,
#   dateRangeInputId = "moorSysBatDateRange",
#   actionButtonInputId = "moorSysBatFetchData",
#   plotOutputId = "moorSysBatPlot",
#   sliderOutputId = "moorSysBatDateSliderUI"
# )
# 
# fii(
#   sliderInputId = "moorSysBatDateSlider",
#   minDateInput = input$moorSysBatDateRange[1],
#   maxDateInput = input$moorSysBatDateRange[2]
# )
# 
# foo <- function(
#   tabPanelTitle,
#   checkboxGroupInputId,
#   checkboxGroupChoiceNames,
#   checkboxGroupChoiceValues,
#   dateRangeInputId,
#   actionButtonInputId,
#   plotOutputId,
#   sliderOutputId
#   ){
#   tagList(
#     tabPanel(
#       title = tabPanelTitle,
#       checkboxGroupInput(
#         inputId = checkboxGroupInputId,
#         label = NULL,
#         inline = TRUE,
#         choiceNames = checkboxGroupChoiceNames,
#         choiceValues = checkboxGroupChoiceValues
#       ),
#       fluidRow(
#         column(
#           8,
#           dateRangeInput(
#             inputId = dateRangeInputId,
#             label = NULL,
#             start = Sys.Date()-7,
#             end = Sys.Date()
#           )
#         ),
#         column(
#           4,
#           actionButton(
#             inputId = actionButtonInputId,
#             label = "Plot"
#           )
#         )
#       ),
#       plotOutput(
#         outputId = plotOutputId
#       ),
#       uiOutput(
#         outputId = sliderOutputId
#       )
#       
#     )
#   )
# }
# 
# fii <- function(
#   sliderInputId,
#   minDateInput,
#   maxDateInput
#   ){
#   tagList(
#     sliderInput(
#       inputId = sliderInputId,
#       label = "Filter dates:",
#       min = as.Date(minDateInput),
#       max = as.Date(maxDateInput),
#       value = c(as.Date(minDateInput),as.Date(maxDateInput)),
#       timeFormat="%Y-%m-%d",
#       width = '95%',
#       animate = animationOptions(1000)
#     )
#   )
# }
