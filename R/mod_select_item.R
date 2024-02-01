#' select_item UI Function
#'
#' @description
#' The UI part of the select_item module to filter learning areas from the item data via a picker and submit button
#'
#' @param id A character string of the id of the module
#' @param item_data A dataframe that contains the item data from the item database
#'
#' @returns A UI definition
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#'
mod_select_item_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("select")),
    uiOutput(ns("test"))
  )
}

#' select_item Server Functions
#'
#' @description
#' The server part of the select_item module that filters the item data by learning area.
#'
#' @param id A character string of the if of the module
#' @param data_item A dataframe that contains the item data from the item database
#'
#' @returns
#' A named list with the reactive vector index_display which contains the randomized order in which the items of the filtered learning areas should be passed on to the display_item module
#'
#'
#'
mod_select_item_server <- function(id, data_item, credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # build observe events into r6 class
    # FilterHandler <- R6::R6Class(
    #   classname = "FilterHandler",
    #   public = list(
    #     data = NULL,
    #     filter = NULL,
    #     topics = NULL,
    #     unsolved = FALSE,
    #     msg = NULL,
    #     indices = NULL,
    #     current_index = NULL,
    #     initialize = function(data, filter, topics, unsolved, msg, indices, current_index) {
    #       self$data <- data
    #       self$filter <- reactive(filter)
    #       self$topics <- reactive(topics)
    #       self$unsolved <- reactive(unsolved)
    #       self$msg <- msg
    #       self$indices <- reactive(indices)
    #       self$current_index <- reactive(current_index)
    #     },
    #     observeFilters = function(picker, filter_unsolved) {
    #       react_comb_input <- reactive(list(picker(), filter_unsolved()))
    #       observeEvent(react_comb_input(), {
    #         req(credentials()$user_auth)
    #         self$topics <- picker()
    #         self$unsolved <- filter_unsolved()
    #         if (!is.null(picker())) {
    #           self$filter <- dplyr::filter(self$data, learning_area %in% self$topics)
    #           self$msg <- "pass"
    #           if (isTRUE(self$unsolved)) {
    #             user_data <- db_get_userdata(as.character(credentials()$info$user_name)) # path of db must be updated
    #             unsolved_item <- user_data %>%
    #               dplyr::group_by(id_item) %>%
    #               dplyr::summarise(unsolved = all(bool_correct == 0)) %>% # check for each item if all answers are wrong
    #               dplyr::filter(unsolved) %>%
    #               dplyr::pull(id_item)
    #             new_filter <- dplyr::filter(self$filter, id_item %in% unsolved_item)
    #             if (nrow(new_filter) == 0) {
    #               self$msg <- "error_unsolved"
    #               self$filter <- NULL
    #             } else {
    #               self$filter <- new_filter
    #               self$msg <- "pass"
    #             }
    #           }
    #         } else {
    #           self$msg <- "error_topics"
    #         }
    #         print(self$filter)
    #         print(self$msg)
    #         print(self$topics)
    #       })
    #     },
    #     observeSubmit = function(submit_btn) {
    #       observeEvent(submit_btn(), {
    #         req(credentials()$user_auth)
    #         if (!is.null(self$topics)) {
    #           shinyjs::enable(selector = '.nav-item a[data-value="item"]')
    #           shiny::updateTabsetPanel(session = session, "navset_train", "item")
    #         }
    #         if (self$msg == "pass") {
    #           self$indices <- sample_vec(self$filter$id_item)
    #           self$current_index <- self$indices[1]
    #           print(self$indices)
    #           print(self$current_index)
    #         } else if (self$msg == "error_topics") {
    #           shinyalert::shinyalert(
    #             title = "Achtung!",
    #             text = "Bitte wählen Sie mindestens eine Kategorie aus.",
    #             type = "warning"
    #           )
    #           self$indices <- NULL
    #           self$current_index <- NULL
    #         } else if (self$msg == "error_unsolved") {
    #           shinyalert::shinyalert(
    #             title = "Achtung!",
    #             text = "Sie haben alle Fragen zu den ausgewählten Kategorien bereits richtig beantwortet.",
    #             type = "warning"
    #           )
    #           self$indices <- NULL
    #           self$current_index <- NULL
    #         }
    #       })
    #     }
    #   )
    # )

    fh <- FilterHandler$new(data = data_item, filter = NULL, topics = NULL,
                            unsolved = FALSE, msg = NULL, indices = NULL, current_index = NULL)
    fh$observeFilters(reactive(input$picker), reactive(input$filter_unsolved), credentials)
    fh$observeSubmit(reactive(input$submit_btn), credentials, session)

    output$select <- renderUI({
      req(credentials()$user_auth) # only show after user authentification
      tagList(
        fluidRow(
          col_1(),
          col_10(
            br(),
            bslib::card(
              bslib::card_header(tags$h6(tags$b("Auswahl der Übungsinhalte"))),
              bslib::card_body(
                fillable = TRUE,
                tags$p("Du hast die Möglichkeit, aus dem Fragenpool die Themen auszuwählen, die du filtern und speziell für deine Übungen verwenden möchtest. Hierbei kannst du gezielt jene Themen oder Kategorien selektieren, die deinem individuellen Lernbedarf entsprechen."),
                tags$p("Zusätzlich dazu, ist es empfehlenswert, die App in regelmäßigen Abständen zu besuchen, da alle zwei Wochen neue Themen und Lerninhalte hinzugefügt werden. Dies bedeutet, dass der Fragenpool kontinuierlich erweitert wird, um dir eine immer breitere und aktuelle Auswahl an Übungsmaterialien zur Verfügung zu stellen."),
                col_10(
                  shinyWidgets::pickerInput(
                    inputId = ns("picker"),
                    choices = unique(data_item$learning_area), # here reali_item topic names as input
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `deselect-all-text` = "Auswahl löschen",
                      `select-all-text` = "Alle auswählen",
                      `none-selected-text` = "Bitte wählen Sie mindestens eine Kategorie aus.",
                      iconBase = "fas",
                      `multiple-separator` = " | "
                    ),
                    choicesOpt = list(
                      disabled = c(rep(FALSE, 3), TRUE, rep(FALSE, 3), TRUE),
                      subtext = paste0("Themengebiet ", 1:8)
                    ),
                    width = "auto"
                  )
                ),
                col_4(
                  checkboxInput(
                    inputId = ns("filter_unsolved"),
                    label = "Nur ungelöste Aufgaben",
                    value = FALSE
                  )
                ),
                col_4(
                  actionButton(
                    ns("submit_btn"),
                    bslib::tooltip(
                      span(
                        HTML("Weiter zum Übungsbereich"),
                        bsicons::bs_icon("shuffle")
                      ),
                      HTML("<b>Doppelklick</b>, um direkt auf den Übungsbereich weitergeleitet zu werden"),
                      placement = "bottom"
                    ),
                    class = "btn-primary"
                  )
                )
              ),
              height = 640,
              full_screen = FALSE,
            )
          ),
          col_1()
        )
      )
    })

    observeEvent(list(input$submit_btn), {
      output$test <- renderUI({
        tagList(
          renderPrint(fh$indices),
          renderPrint(fh$current_index)
        )
      })
    })

    out <- list(
      index_display = fh$indices,
      submit_btn_value = reactive(input$submit_btn),
      selected_topics = fh$topics
    )
    return(out)
  })
}
