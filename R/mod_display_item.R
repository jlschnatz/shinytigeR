#' display_item UI function
#'
#' @description The UI part of the display_item module that creates the stimulus and MC-questionnaire interface.
#'
#' @param id A character string of the id of the module
#'
#' @returns A UI definition
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_display_item_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(), br(),
    rep_br(4),
    uiOutput(ns("stimulus")),
    rep_br(4),
    uiOutput(ns("radio_item"))
  )
}

#' display_item Server Functions
#'
#' @description
#' The server part of the display_item module.
#'
#' @param id A character string of the id of the module
#' @param data_item A dataframe that contains the item data from the item database
#' @param index_display A vector of the indices of the filtered data (the output of mod_select_item_server)
#'
#' @returns A named list with the reactive vector cur_selection of length 1L that contains the current selected answeroption of the MC-item.
#'
#' @export
mod_display_item_server <- function(id, data_item, index_display) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to keep track of the current item index

    item_index <- reactive(index_display()[1])

    output$stimulus <- renderUI(
      if (!is.null(item_index())) {
        displayStimulus(
          .text = data_item$stimulus_text[item_index()],
          .img = data_item$stimulus_image[item_index()],
          .type_stimulus = data_item$type_stimulus[item_index()]
        )
      }
    )

    output$radio_item <- renderUI(
      if (!is.null(item_index())) {
        radioButtonsDynamic(
          inputId = "radio_item",
          choices = get_answeroptions(data_item, item_index()),
          type_answer = data_item$type_answer[item_index()],
          correct_id = data_item$answer_correct[item_index()]
        )
      }
    )

    return(list(cur_selection = reactive(input$radio_item)))
  })
}
