#' display_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_display_item_ui <- function(id) {
  ns <- NS(id)
  tagList(
    rep_br(2),
    # bslib::card(
    # bslib::card_header("Übung"),
    uiOutput(ns("stimulus")),
    rep_br(2),
    uiOutput(ns("radio_item"))
    # purrr::map(c(ns("stimulus"), ns("radio_item")), uiOutput)
    # )
  )
}

#' display_item Server Functions
#'
#' @noRd
mod_display_item_server <- function(id, data_item, index) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to keep track of the current item index

    item_index <- reactive(index()$id_display[1])

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

    return(reactive(input$radio_item))

  })
}

## To be copied in the UI
# mod_display_item_ui("display_item_1")

## To be copied in the server
# mod_display_item_server("display_item_1")
