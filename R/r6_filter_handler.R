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
    data = NULL,
    filter = NULL,
    topics = NULL,
    unsolved = FALSE,
    msg = NULL,
    indices = NULL,
    current_index = NULL,
    initialize = function(data, filter, topics, unsolved, msg, indices, current_index) {
      self$data <- data
      self$filter <- reactive(filter)
      self$topics <- reactive(topics)
      self$unsolved <- reactive(unsolved)
      self$msg <- msg
      self$indices <- reactive(indices)
      self$current_index <- reactive(current_index)
    },
    observeFilters = function(picker, filter_unsolved, credentials) {
      react_comb_input <- reactive(list(picker(), filter_unsolved()))
      observeEvent(react_comb_input(), {
        req(credentials()$user_auth)
        self$topics <- picker()
        self$unsolved <- filter_unsolved()
        if (!is.null(picker())) {
          self$filter <- dplyr::filter(self$data, learning_area %in% self$topics)
          self$msg <- "pass"
          if (isTRUE(self$unsolved)) {
            user_data <- db_get_userdata(as.character(credentials()$info$user_name)) # path of db must be updated
            unsolved_item <- user_data %>%
              dplyr::group_by(id_item) %>%
              dplyr::summarise(unsolved = all(bool_correct == 0)) %>% # check for each item if all answers are wrong
              dplyr::filter(unsolved) %>%
              dplyr::pull(id_item)
            new_filter <- dplyr::filter(self$filter, id_item %in% unsolved_item)
            if (nrow(new_filter) == 0) {
              self$msg <- "error_unsolved"
              self$filter <- NULL
            } else {
              self$filter <- new_filter
              self$msg <- "pass"
            }
          }
        } else {
          self$msg <- "error_topics"
        }
        print(self$filter)
        print(self$msg)
        print(self$topics)
      })
    },
    observeSubmit = function(submit_btn, credentials, session) {
      observeEvent(submit_btn(), {
        req(credentials()$user_auth)
        if (!is.null(self$topics)) {
          shinyjs::enable(selector = '.nav-item a[data-value="item"]')
          shiny::updateTabsetPanel(session = session, "navset_train", "item")
        }
        if (self$msg == "pass") {
          self$indices <- sample_vec(self$filter$id_item)
          self$current_index <- self$indices[1]
          print(self$indices)
          print(self$current_index)
        } else if (self$msg == "error_topics") {
          shinyalert::shinyalert(
            title = "Achtung!",
            text = "Bitte wählen Sie mindestens eine Kategorie aus.",
            type = "warning"
          )
          self$indices <- NULL
          self$current_index <- NULL
        } else if (self$msg == "error_unsolved") {
          shinyalert::shinyalert(
            title = "Achtung!",
            text = "Sie haben alle Fragen zu den ausgewählten Kategorien bereits richtig beantwortet.",
            type = "warning"
          )
          self$indices <- NULL
          self$current_index <- NULL
        }
      })
    }
  )
)
