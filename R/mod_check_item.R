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
    uiOutput(ns("feedback")),
    tableOutput(ns("test"))
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

    #observe if submit button is clicked, only then show both check and next buttons
    observeEvent(submit_btn_value(), {
      shinyjs::show("check_button")
    })

    # initialize empty feedback message
    feedback_message <- reactiveVal(NULL)

    shinyjs::hide("next_button")


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

        response_data_df <- tibble::tibble(
          id_user = as.character(credentials()$info$user_name),
          id_session = as.character(session$token),
          id_date = as.integer(Sys.Date()), # unfortunately class integer -> change later to POSIXct
          id_datetime = as.integer(Sys.time()), # unfortunately class integer -> change later to POSIXct
          id_item = as.integer(cur_item_id()),
          learning_area = as.character(data_item$learning_area[cur_item_id()]),
          selected_option = as.integer(cur_answer_id()),
          answer_correct = as.integer(data_item$answer_correct[cur_item_id()]),
          bool_correct = as.logical(data_item$answer_correct[cur_item_id()] == cur_answer_id())
        )

        write_userdata_db(
          id_user = as.character(credentials()$info$user_name),
          .response_data_df = response_data_df
        )

      }

    })

    output$feedback <- renderUI(feedback_message())
   #output$test <- renderTable(response_data_df())

    observeEvent(input$next_button, {

      # Find next index in random sequence that has not beed completed
      next_index <- if (!(which(index_display() == cur_item_id()) == length(index_display()))) {
        which(index_display() == cur_item_id()) + 1
      } else NA

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
      shinyjs::hide("next_button")
      shinyjs::enable("radio_item")

    })

    out <- list(
      check_button_value = reactive(input$check_button)
    )
    return(out)
  })
}
