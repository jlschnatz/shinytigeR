#' check_item UI Function
#'
#' @description The UI part of the check_item module that provides a interface with a two action buttons to check the item, receive feedback and proceed to the next item.
#'
#' @param id A character string of the id of the module
#'
#' @returns A UI definition
#'
#'
#'
#' @importFrom shiny NS tagList
mod_check_item_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    uiOutput(ns("check"))
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
#'
mod_check_item_server <- function(
    id, data_item, index_display, cur_item_id, cur_answer_txt,
    cur_answer_id, submit_btn_value, credentials) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observe if submit button is clicked, only then show both check and next buttons
    shiny::observeEvent(submit_btn_value(), {
      shinyjs::show("check_button")
      shinyjs::show("next_button")
    })

    # initialize empty feedback message
    feedback_message <- reactiveVal(NULL)

    shiny::observeEvent(cur_answer_txt(), {
      shinyjs::enable("check_button")
    })

    shiny::observeEvent(input$check_button, {
      if (!is.null(cur_answer_txt())) {
        req(cur_answer_txt)
        req(credentials()$user_auth)

        eval_logic <- dplyr::case_when(
          cur_answer_id() == data_item[data_item$id_item == cur_item_id(), "answer_correct"] ~ "correct",
          (cur_answer_id() != data_item[data_item$id_item == cur_item_id(), "answer_correct"])
           & (cur_answer_id() != length(get_answeroptions(data_item, data_item$id_item == cur_item_id()))) ~ "incorrect",
          cur_answer_id() == length(get_answeroptions(data_item, data_item$id_item == cur_item_id())) ~ "skip",
        )

        if (data_item$type_answer[data_item$id_item == cur_item_id()] == "image") {
          class_mapping <- dplyr::case_match(
            eval_logic,
            "correct" ~ "correct_answer_img",
            "incorrect" ~ "incorrect_answer_img",
            "skip" ~ "skip_answer_img"
          )
        } else if (data_item$type_answer[data_item$id_item == cur_item_id()] == "text") {
          class_mapping <- dplyr::case_match(
            eval_logic,
            "correct" ~ "correct_answer_txt",
            "incorrect" ~ "incorrect_answer_txt",
            "skip" ~ "skip_answer_txt"
          )
        }

        shinyjs::addClass(selector = paste0("#label_display_item_1-radio_item", cur_answer_id()), class = class_mapping)

        feedback_message(
          bslib::card(
            bslib::card_header(
              tags$b(dplyr::case_match(
                eval_logic,
                "correct" ~ "Richtige Antwort!",
                "incorrect" ~ "Leider falsch!",
                "skip" ~ "Überspringen!"
              )),
              class = dplyr::case_match(
                eval_logic,
                "correct" ~ "bg-success text-white",
                "incorrect" ~ "bg-danger text-white",
                "skip" ~ "bg-warning text-white"
              )
            ),
            bslib::card_body(
              shiny::HTML(
                markdown::markdownToHTML(text = get_feedbackoptions(data_item, data_item$id_item == cur_item_id())[cur_answer_id()], fragment.only = TRUE)
              )
            )
          )
        )

        shinyjs::disable("radio_item")
        shinyjs::enable("next_button")
        shinyjs::disable("check_button")

        response_data_df <- tibble::tibble(
          id_user = as.character(credentials()$info$user_name),
          id_session = as.character(session$token),
          id_date = as.integer(Sys.Date()),
          id_datetime = as.integer(Sys.time()),
          id_item = as.integer(cur_item_id()),
          learning_area = as.character(data_item$learning_area[data_item$id_item == cur_item_id()]),
          selected_option = as.integer(cur_answer_id()),
          answer_correct = as.integer(data_item$answer_correct[data_item$id_item == cur_item_id()]),
          bool_correct = as.logical(data_item$answer_correct[data_item$id_item == cur_item_id()] == cur_answer_id())
        )

        # Only write to DB if NOT the last answeroption (skip button) was selected
        if (!cur_answer_id() == length(get_answeroptions(data_item, data_item$id_item == cur_item_id()))) {
          write_userdata_db(
            id_user = as.character(credentials()$info$user_name),
            .response_data_df = response_data_df
          )
        }
      }
    })


    output$check <- renderUI({
      req(credentials()$user_auth)
      tagList(
        # generate two action buttons
        shinyjs::disabled(actionButton(
          ns("check_button"),
          tags$p(bsicons::bs_icon("ui-checks"), HTML("&nbsp"), "Antwort überprüfen"),
          class = "btn btn-primary"
        )),
        shinyjs::disabled(actionButton(
          ns("next_button"),
          tags$p(bsicons::bs_icon("arrow-right"), HTML("&nbsp"), "Nächste Frage", ),
          class = "btn btn-primary"
        )),
        shiny::actionButton(
          ns("back_button"),
          shiny::tags$p(bsicons::bs_icon("back"), HTML("&nbsp"), "Zurück zur Auswahl"),
          class = "btn btn-primary"
        ),
        rep_br(3),
        uiOutput(ns("feedback"))
      )
    })

    output$feedback <- renderUI(feedback_message())

    observeEvent(input$next_button, {
      # Find next index in random sequence that has not beed completed
      next_index <- if (!(which(index_display() == cur_item_id()) == length(index_display()))) {
        which(index_display() == cur_item_id()) + 1
      } else {
        NA
      }

      # If there is no next index, it means all items have been answered, show the completion message
      if (is.na(next_index)) {
        shinyalert::shinyalert(
          title = "Geschafft!",
          text = paste0("Du bist alle ausgewählten Fragen einmal durchgegangen. ", icon("thumbs-up", class = "solid")),
          type = "success",
          html = TRUE
        )
      } else {
        # Set the next index as the current index
        cur_item_id(index_display()[next_index])
      }

      feedback_message(NULL)
      shinyjs::enable("radio_item")
      shinyjs::disable("check_button")
      shinyjs::disable("next_button")
    })

    out <- list(
      check_button_value = reactive(input$check_button),
      back_button_value = reactive(input$back_button)
    )
    return(out)
  })
}
