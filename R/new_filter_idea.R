
library(dplyr)
library(shiny)

sample_vec <- function(x, ...) x[sample(length(x), ...)]


ui <- bslib::page_navbar(
  bslib::nav_panel(
    title = "Home",
    fluidRow(
      column(2, shinyWidgets::pickerInput(
        inputId = "picker",
        choices = LETTERS[1:3], # here reali_item topic names as input
        selected = NULL,
        multiple = TRUE,
        inline = TRUE,
        width = "200px",
        autocomplete = TRUE
        )
        ),
      column(2, shinyWidgets::materialSwitch(
        "filter_unsolved", label = "Unsolved",
        value = FALSE, status = "primary", inline = TRUE, right = TRUE, width = "200px")),
      column(4, actionButton("start", "Start"))
    ),
    fluidRow(uiOutput("test"))
  ),
  bslib::nav_panel(title = "Train")
)

server <- function(input, output, session) {
  # Item Data
  data_item <- data.frame(
    learning_area = rep(LETTERS[1:3], 2),
    id_item = 1:6,
    itemtext = paste0("Item ", 1:6),
    answer_correct = rep(1, 6)
  )

  user_data <- data.frame(
    user = "Luca",
    id_item = c(1, 2, 4),
    answer = c(1, 2, 4),
    answer_correct = c(1, 1, 1),
    bool_correct = c(TRUE, FALSE, FALSE)
  )

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
      observeFilters = function() {
        react_comb_input <- reactive(list(input$picker, input$filter_unsolved))
        observeEvent(react_comb_input(), {
          self$topics <- input$picker
          self$unsolved <- input$filter_unsolved
          if (!is.null(input$picker)) {
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
        })
      },

      observeSubmit = function() {
        observeEvent(input$start, {
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
        })
      }
    )
  )

  fh <- FilterHandler$new(data_item, NULL, NULL, FALSE, NULL, NULL, NULL)
  fh$observeFilters()
  fh$observeSubmit()

  observeEvent(input$start, {

    output$test <- renderUI({
      tagList(
        renderPrint(fh$indices),
        renderPrint(fh$current_index)
      )
    })


  })
}

shinyApp(ui, server)
