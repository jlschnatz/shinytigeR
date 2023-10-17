#' select_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_item_ui <- function(id, picker_choices) {
  ns <- NS(id)
  tagList(
    fluidRow(
      wellPanel(
        tags$h4(tags$b("Auswahl der Übungsfragen")),
        tags$text("Wähle einen Pool an Items aus, die du üben möchtest. Diese kannst du inhaltlich auswählen."),
        rep_br(2),
        shinyWidgets::pickerInput(
          inputId = ns("picker"),
          choices = picker_choices, # here reali_item topic names as input
          selected = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `deselect-all-text` = "Auswahl löschen",
            `select-all-text` = "Alle auswählen",
            `none-selected-text` = "Bitte wählen Sie mindestens eine Kategorie aus."
          )
        ),
        actionButton(ns("submit_btn"), "Start"),
        uiOutput(ns("if_empty")),
        tableOutput(ns("table_test")),
        full_screen = FALSE
      )
    )
  )
}

#' select_item Server Functions
#'
#' @noRd
mod_select_item_server <- function(id, data_item) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_topics <- reactiveVal(NULL)

    # Filter the data based on selected topics
    filtered_data <- reactive({
      if (!is.null(selected_topics())) {
        data_item %>%
          dplyr::filter(learning_area %in% selected_topics())
      } else {
        data_item  # Return the original data if no topics are selected
      }
    })

    # Observe the submit button click
    observeEvent(input$submit_btn, {
      selected_topics(input$picker)
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

    # Display the selected variables
    output$if_empty <- renderUI({
      if (is.null(selected_topics())) {
        HTML("Bitte wählen Sie mindestens eine Kategorie aus.")
      } else {
        table_output <- renderTable(filtered_data())
        if (nrow(filtered_data()) == 0) {
          HTML("Keine Daten gefunden.")
        } else {
          table_output
        }
      }
    })
  })

}



## To be copied in the UI
# mod_select_item_ui("select_item_1")

## To be copied in the server
# mod_select_item_server("select_item_1")
