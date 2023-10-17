#' display_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_display_item_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    bslib::card(
      bslib::card_header("Übung"),
      purrr::map(c("stimulus", "radio_item"), uiOutput)
    )
  )
}

#' display_item Server Functions
#'
#' @noRd
mod_display_item_server <- function(id, data_item, index){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$stimulus <- renderUI(
      displayStimulus(
        .text = data_item$stimulus_text[index()],
        .img = data_item$stimulus_image[index()],
        .type_stimulus = data_item$type_stimulus[index()]
      )
    )

    output$radio_item <- renderUI(
      radioButtonsDynamic(
        inputId = "radio_item",
        choices = get_answeroptions(data_item, index()),
        type_answer = data_item$type_answer[index()],
        correct_id = data_item$answer_correct[index()]
      )
    )

  })
}

## To be copied in the UI
# mod_display_item_ui("display_item_1")

## To be copied in the server
# mod_display_item_server("display_item_1")
