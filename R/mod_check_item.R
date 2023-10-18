#' check_item UI Function
#'
#' @description The UI part of the check_item module that provides a interface with a two action buttons to check the item, receive feedback and proceed to the next item.
#'
#' @param id A character string of the id of the module
#'
#' @returns A UI definition
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_check_item_ui <- function(id) {
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
#' @description The server part of the check_item module that provides a interface with a two action buttons to check the item, receive feedback and proceed to the next item.
#'
#' @param id A character string of the id of the module
#' @param data_item A dataframe that contains the item data from the item database
#' @param cur_item_id test
#' @param cur_answer_txt test
#' @param cur_answer_id test
#'
#' @returns Test
#'
#' @export
mod_check_item_server <- function(id, data_item, cur_item_id, cur_answer_txt, cur_answer_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    feedback_message <- reactiveVal(NULL)

    observeEvent(input$check_button, {
      if (!is.null(cur_answer_txt())) {
        is_correct <- cur_answer_id == data_item$answer_correct[cur_item_id()]

        # Common logic for both correct and incorrect cases
        if (df_item$type_answer[cur_item_id()] == "image") {
          class_mapping <- ifelse(is_correct, "correct_answer_img", "incorrect_answer_img")
        } else if (df_item$type_answer[cur_item_id()] == "text") {
          class_mapping <- ifelse(is_correct, "correct_answer_txt", "incorrect_answer_txt")
        }

        shinyjs::addClass(selector = paste0("#label_radio_item", selected_index), class = class_mapping)

        if (!is_correct && df_item$type_answer[cur_item_id()] == "image") {
          shinyjs::addClass(
            selector = paste0("#label_radio_item", df_item$answer_correct[cur_item_id()]),
            class = "correct_answer_img"
            )
        } else if (!is_correct && df_item$type_answer[cur_item_id()] == "text") {
          shinyjs::addClass(
            selector = paste0("#label_radio_item", df_item$answer_correct[cur_item_id()]),
            class = "correct_answer_txt"
            )
        }

        feedback_message(
          HTML(
            case_when(
              selected_index == 1 ~ df_item$if_answeroption_01[cur_item_id()],
              selected_index == 2 ~ df_item$if_answeroption_02[cur_item_id()],
              selected_index == 3 ~ df_item$if_answeroption_03[cur_item_id()],
              selected_index == 4 ~ df_item$if_answeroption_04[cur_item_id()],
              selected_index == 5 ~ df_item$if_answeroption_05[cur_item_id()]
            )
          )
        )
      }
    })

    output$feedback <- renderUI(feedback_message())
  })
}
