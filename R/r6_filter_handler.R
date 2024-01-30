#' R6 Class Item Filtering Handler
#' @description This class handles the filtering of items based on the user's selection.
#' @importFrom R6 R6Class
#' @importFrom shiny reactive
#' @importFrom shiny observeEvent
#' @importFrom shiny req
#' @export
FilterHandler <- R6::R6Class(
  classname = "FilterHandler",
  public = list(
    #' @field data item data
    data = NULL,
    #' @field filter_data filtered item data
    filter_data = NULL,
    #' @field learning_area selected learning areas (from picker)
    learning_area = NULL,
    #' @field unsolved whether to filter for unsolved items
    filter_incorrect = FALSE,
    #' @field msg message to be displayed
    msg = NULL,
    #' @field index randomized order of items
    index = NULL,

    #' @description initialize the class
    #' @param data item data
    #' @param filter_data filtered item data
    #' @param learning_area selected learning areas (from picker)
    #' @param filter_incorrect whether to filter for unsolved items
    #' @param msg message to be displayed
    #' @param index randomized order of items
    initialize = function(data, filter_data, learning_area, filter_incorrect, msg, index) {
      self$data <- data
      self$filter_data <- reactive(filter_data)
      self$learning_area <- reactive(learning_area)
      self$filter_incorrect <- reactive(filter_incorrect)
      self$msg <- msg
      self$index <- reactive(index)
    },

    #' @description observe the filter inputs
    #' @param picker picker input from UI (input$picker)
    #' @param filter_incorrect whether to filter for unsolved items from UI (input$filter_incorrect)
    #' @param credentials credentials (user auth from shinyauthr)
    observeFilters = function(picker, filter_incorrect, credentials) {
      react_comb_input <- reactive(list(picker, filter_incorrect))
      observeEvent(react_comb_input(), {
        req(credentials()$user_auth)
        self$learning_area <- picker
        self$filter_incorrect <- filter_incorrect
        if (!is.null(picker)) {
          self$filter_data <- dplyr::filter(self$data, learning_area %in% self$learning_area)
          self$msg <- "pass"
          if (isTRUE(self$unsolved)) {
            user_data <- db_get_userdata(as.character(credentials()$info$user_name)) # path of db must be updated
            unsolved_item <- user_data %>%
              dplyr::group_by(id_item) %>%
              dplyr::summarise(unsolved = all(bool_correct == 0)) %>% # check for each item if all answers are wrong
              dplyr::filter(unsolved) %>%
              dplyr::pull(id_item)
            new_filter <- dplyr::filter(self$filter_data, id_item %in% unsolved_item)
            if (nrow(new_filter) == 0) {
              self$msg <- "error_unsolved"
              self$filter_data <- NULL
            } else {
              self$filter_data <- new_filter
              self$msg <- "pass"
            }
          }
        } else {
          self$msg <- "error_topics"
        }
        print(self$filter_data)
        print(self$msg)
        print(self$learning_area)
      })
    },

    #' @description observe the submit button
    #' @param submit_btn action submit button from UI (input$submit_btn)
    #' @param credentials credentials (user auth from shinyauthr)
    #' @param session shiny session
    observeSubmit = function(submit_btn, credentials, session) {
      observeEvent(submit_btn, {
        req(credentials()$user_auth)
        if (!is.null(self$learning_area)) {
          shinyjs::enable(selector = '.nav-item a[data-value="item"]')
          shiny::updateTabsetPanel(session = session, "navset_train", "item")
        }
        if (self$msg == "pass") {
          self$index <- sample_vec(self$filter_data$id_item)
          print(self$index)
        } else if (self$msg == "error_topics") {
          shinyalert::shinyalert(
            title = "Achtung!",
            text = "Bitte wählen Sie mindestens eine Kategorie aus.",
            type = "warning"
          )
        } else if (self$msg == "error_unsolved") {
          shinyalert::shinyalert(
            title = "Achtung!",
            text = "Sie haben alle Fragen zu den ausgewählten Kategorien bereits richtig beantwortet.",
            type = "warning"
          )
        }
      })
    }
  )
)
