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
