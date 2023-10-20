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
    shinyjs::hidden(
      purrr::pmap(
      .l = tibble::tibble(
        inputId = c(ns("check_button"), ns("next_button")),
        label = c("Antwort überprüfen", "Nächste Frage"),
        icon = list(icon("check", icon("forward-step")))
      ),
      .f = actionButton
      )
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
mod_check_item_server <- function(
    id, data_item, index_display, cur_item_id, cur_answer_txt,
    cur_answer_id, submit_btn_value, credentials
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe if submit button is clicked, only then show both check and next buttons
    observeEvent(submit_btn_value(), {
      shinyjs::show("check_button")
    })

    # initialize empty feedback message
    feedback_message <- reactiveVal(NULL)

    # initialize empty reactive dataframe of the tracked user data
    response_data_df <- reactiveVal(
      tibble::tibble(
        id_item = integer(), #change from item_index to id_item
        selected_option = integer(),
        answer_correct = integer(),
        bool_correct = logical()
      )
    )

    observeEvent(input$check_button, {
      if (!is.null(cur_answer_txt())) {
        req(cur_answer_txt)
        is_correct <- cur_answer_id() == data_item$answer_correct[cur_item_id()]

        # Common logic for both correct and incorrect cases
        if (data_item$type_answer[cur_item_id()] == "image") {
          class_mapping <- ifelse(is_correct, "correct_answer_img", "incorrect_answer_img")
        } else if (data_item$type_answer[cur_item_id()] == "text") {
          class_mapping <- ifelse(is_correct, "correct_answer_txt", "incorrect_answer_txt")
        }

        shinyjs::addClass(selector = paste0("#label_radio_item", cur_answer_id()), class = class_mapping)

        if (!is_correct && data_item$type_answer[cur_item_id()] == "image") {
          shinyjs::addClass(
            selector = paste0("#label_radio_item", data_item$answer_correct[cur_item_id()]),
            class = "correct_answer_img"
            )
        } else if (!is_correct && data_item$type_answer[cur_item_id()] == "text") {
          shinyjs::addClass(
            selector = paste0("#label_radio_item", data_item$answer_correct[cur_item_id()]),
            class = "correct_answer_txt"
            )
        }

        feedback_message(
          HTML(
            dplyr::case_when(
              cur_answer_id() == 1 ~ data_item$if_answeroption_01[cur_item_id()],
              cur_answer_id() == 2 ~ data_item$if_answeroption_02[cur_item_id()],
              cur_answer_id() == 3 ~ data_item$if_answeroption_03[cur_item_id()],
              cur_answer_id() == 4 ~ data_item$if_answeroption_04[cur_item_id()],
              cur_answer_id() == 5 ~ data_item$if_answeroption_05[cur_item_id()]
            )
          )
        )

        shinyjs::disable("radio_item")
        shinyjs::show("next_button")

      }
    })

    output$feedback <- renderUI(feedback_message())

    observeEvent(input$next_button, {
      if (!is.null(cur_answer_txt())) {
        response_data_df(
          dplyr::bind_rows(
            response_data_df(),
            tibble::tibble(
              id_item = cur_item_id(),
              selected_option = cur_answer_id(),
              answer_correct = data_item$answer_correct[cur_item_id()],
              bool_correct = data_item$answer_correct[cur_item_id()] == cur_answer_id()
            )
          )
        )
      }

      # Find next index in random sequence that has not beed completed
      next_index <- dplyr::first(which(!index_display() %in% response_data_df()$id_item))

      # If there is no next index, it means all items have been answered, show the completion message
      if (is.na(next_index)) {
        shinyalert::shinyalert(
          title = "Hurra!",
          text = paste0("Alle Items abgeschlossen!  ", icon("thumbs-up", class = "duotone")),
          type = "success",
          html = TRUE
        )
      } else {
        # Set the next index as the current index
        cur_item_id(index_display()[next_index])
      }

      feedback_message(NULL)

    })

    out <- list(
      check_button_value = reactive(input$check_button)
    )
    return(out)

  })
}
