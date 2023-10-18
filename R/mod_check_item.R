#' check_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_check_item_ui <- function(id){
  ns <- NS(id)
  tagList(
    # generate two action buttons
    purrr::pmap(
      .l = tibble::tibble(
        inputId = c(ns("check_button"), ns("next_button")),
        label = c("Antwort überprüfen", "Nächste Frage"),
        icon = list(icon("check", icon("forward-step")))
      ),
      .f = actionButton
    ),
    uiOutput(ns("feedback"))
  )
}

#' check_item Server Functions
#'
#' @noRd
mod_check_item_server <- function(id, data_item, cur_item_id, cur_answer_txt, cur_answer_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    feedback_message <- reactiveVal(NULL)

    observeEvent(input$check_button, {
      if (!is.null(cur_selection())) {

      }
    })



  })
}
