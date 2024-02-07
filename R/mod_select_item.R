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
#'
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
#'
mod_select_item_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("select"))
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

    selected_topics <- reactiveVal(NULL)

    # Filter the data based on selected topics
    filtered_data <- reactive({
      if (!is.null(selected_topics())) {
        dplyr::filter(data_item, learning_area %in% selected_topics())
      }
    })

    # Observe the submit button click
    observeEvent(input$submit_btn, {
      # Update selected_topics to whats chosen in the pickerInput
      selected_topics(input$picker)

      # If nothing selected: warning pop-up
      if (is.null(selected_topics())) {
        shinyalert::shinyalert(
          title = "Achtung!",
          text = "Bitte wählen Sie mindestens eine Kategorie aus.",
          type = "warning",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE,
          html = TRUE,
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#25607D"
        )
      }
    })

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
                    disabled = rep(FALSE, 9),
                    subtext = paste0("Themengebiet ", 1:9)
                    ),
                  width = "auto"
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
            full_screen = FALSE
          )
        ),
        col_1()
      ))
    })

    index_display <- reactive(sample(filtered_data()$id_item))

    observeEvent(input$submit_btn, {

      if (!is.null(selected_topics())) {
        shinyjs::enable(selector = '.nav-item a[data-value="item"]')
        shiny::updateTabsetPanel(session = session, "navset_train", "item")
      }
    })



    # return a named list with reactive indices of the filtered data
    out <- list(
      index_display = index_display,
      submit_btn_value = reactive(input$submit_btn),
      selected_topics = selected_topics
    )
    return(out)
  })
}
