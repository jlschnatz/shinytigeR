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
  tagList(uiOutput(ns("display")))
}

#' display_item Server Functions
#'
#' @description
#' The server part of the display_item module.
#'
#' @param id A character string of the id of the module
#' @param data_item A dataframe that contains the item data from the item database
#' @param index_display A (reactive) vector of the indices of the filtered data (the output of mod_select_item_server)
#'
#' @returns
#' A named list with the following elements:
#'
#' \itemize{
#'   \item A reactive vector `cur_answer_txt` of length 1L that contains the the text string of
#'    the current selected answeroption of the MC-item
#'   \item A reactive vector `cur_item_id` of length 1L that contains the current selected
#'    index of the answeroption
#'   \item A reactive vector `cur_answer_id` of length 1L that contains the current selected
#'    index of the selected answeroption (1-5)
#' }
#'
#' @export
#'
mod_display_item_server <- function(id, data_item, index_display, check_button_value, credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to keep track of the current item index
    #cur_item_id <- reactive(index_display()[1])
    #cur_item_id <- reactiveVal(index_display()[1])

    # this fixes the error!!
    cur_item_id <- reactiveVal()
    observe({
      cur_item_id(index_display()[1])
    })


    output$stimulus <- renderUI({
      req(credentials()$user_auth) # require authentification before eval
      if (!is.null(cur_item_id())) {
        displayStimulus(
          .text = data_item$stimulus_text[cur_item_id()],
          .img = data_item$stimulus_image[cur_item_id()],
          .type_stimulus = data_item$type_stimulus[cur_item_id()]
        )
      }
    }
    )

    output$radio_item <- renderUI({
      req(credentials()$user_auth) # require authentification before eval
      if (!is.null(cur_item_id())) {
        radioButtonsDynamic(
          inputId = ns("radio_item"),
          choices = get_answeroptions(data_item, cur_item_id()),
          type_answer = data_item$type_answer[cur_item_id()],
          correct_id = data_item$answer_correct[cur_item_id()]
        )
      }
    }
    )

    observe({
      if (!is.null(check_button_value())) {
        shinyjs::disable("radio_item")
      } else {
        shinyjs::enable("radio_item")
      }
    })


    cur_answer_txt <- reactive(input$radio_item)
    cur_answer_id <- reactive(which(get_answeroptions(data_item, cur_item_id()) == cur_answer_txt()))


    output$display <- renderUI({
      req(credentials()$user_auth)
      tagList(
        rep_br(1),
        uiOutput(ns("stimulus")),
        rep_br(1),
        uiOutput(ns("radio_item")),
      )
    })

    out <- list(
      cur_item_id = cur_item_id,
      cur_answer_txt = cur_answer_txt,
      cur_answer_id = cur_answer_id
      )
    return(out)
  })
}
