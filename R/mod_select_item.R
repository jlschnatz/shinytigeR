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
#' @export
#'
#' @importFrom shiny NS tagList
#'
mod_select_item_ui <- function(id, data_item) {
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$h4(tags$b("Auswahl der Übungsinhalte")),
      tags$text("Wähle einen Pool an Items aus, die du üben möchtest. Diese kannst du inhaltlich auswählen."),
      rep_br(2),
      shinyWidgets::pickerInput(
        inputId = ns("picker"),
        choices = unique(data_item$learning_area), # here reali_item topic names as input
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `deselect-all-text` = "Auswahl löschen",
          `select-all-text` = "Alle auswählen",
          `none-selected-text` = "Bitte wählen Sie mindestens eine Kategorie aus."
        )
      ),
      actionButton(ns("submit_btn"), "Start")
    )
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
#' @export
#'
mod_select_item_server <- function(id, data_item) {
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

    index_display <- reactive(sample(filtered_data()$id_item))

    # return a named list with reactive indices of the filtered data
    out <- list(
      index_display = index_display,
      submit_btn_value = reactive(input$submit_btn)
      )
    return(out)
  })
}
