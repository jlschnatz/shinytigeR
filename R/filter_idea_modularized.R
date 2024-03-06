
library(dplyr)
library(shiny)
library(gargoyle)
library(bslib)

sample_vec <- function(x, ...) x[sample(length(x), ...)]

FilterHandler <- R6::R6Class(
  classname = "FilterHandler",
  public = list(

    # initialize objects
    data = NULL,
    filter = NULL,
    topics = NULL,
    unsolved = FALSE,
    msg = NULL,
    indices = NULL,
    current_index = NULL,

    # initialize method
    initialize = function(data, filter, topics, unsolved, msg, indices, current_index) {
      self$data <- data
      self$filter <- reactive(filter)
      self$topics <- reactive(topics)
      self$unsolved <- reactive(unsolved)
      self$msg <- msg
      self$indices <- reactive(indices)
      self$current_index = reactive(current_index)
    },

    # observe Filter
    observeFilters = function(picker, filter_unsolved, user_data) {
      react_comb_input <- reactive(list(picker(), filter_unsolved()))
      observeEvent(react_comb_input(), {
        self$topics <- picker()
        self$unsolved <- filter_unsolved()
        if (!is.null(picker())) {
          self$filter <- dplyr::filter(self$data, learning_area %in% self$topics)
          self$msg <- "pass"
          if (isTRUE(self$unsolved)) {
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
        gargoyle::trigger("test")
      })
    },

    observeSubmit = function(submit_btn) {
      observeEvent(submit_btn(), {
        if (self$msg == "pass") {
          self$indices <- sample_vec(self$filter$id_item)
          self$current_index <- self$indices[1]
          print(self$indices)
        } else if (self$msg == "error_topics") {
          shinyalert::shinyalert(
            title = "Achtung!",
            text = "Bitte wählen Sie mindestens eine Kategorie aus.",
            type = "warning"
          )
          self$indices <- NULL
        } else if (self$msg == "error_unsolved") {
          shinyalert::shinyalert(
            title = "Achtung!",
            text = "Sie haben alle Fragen zu den ausgewählten Kategorien bereits richtig beantwortet.",
            type = "warning"
          )
          self$indices <- NULL
        }
        gargoyle::trigger("test")
      })
    }
  )
)


mod1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("picker"), "Picker", LETTERS[1:3], multiple = TRUE),
    checkboxInput(ns("filter_unsolved"), "Unsolved", FALSE),
    actionButton(ns("start"), "Start")
  )
}

mod1_server <- function(id, fh, user_data) {
  moduleServer(
    id, function(input, output, session) {
      fh$observeFilters(reactive(input$picker), reactive(input$filter_unsolved), user_data)
      fh$observeSubmit(reactive(input$start))
    }
  )
}


mod2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput("text")
  )
}

mod2_server <- function(id, fh) {
  moduleServer(
    id, function(input, output, session) {
      observeEvent(gargoyle::watch("test"), {
        output$text <- renderText({
          fh$indices()
        })
      })
    }
  )
}

ui <- fluidPage(
  mod1_ui("mod1"),
  mod2_ui("mod2")
)

server <- function(input, output, session) {
  user_data <- data.frame(
    user = "Luca",
    id_item = c(1, 2, 4),
    answer = c(1, 2, 4),
    answer_correct = c(1, 1, 1),
    bool_correct = c(TRUE, FALSE, FALSE)
  )
  data_item <- data.frame(
    learning_area = rep(LETTERS[1:3], 2),
    id_item = 1:6,
    itemtext = paste0("Item ", 1:6),
    answer_correct = rep(1, 6)
  )

  fh <- FilterHandler$new(data_item, NULL, NULL, FALSE, NULL, NULL, NULL)
  gargoyle::init("test")

  mod1_server("mod1", fh, user_data)
  mod2_server("mod2", fh)
}

shinyApp(ui, server)




