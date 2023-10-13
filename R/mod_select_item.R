#' select_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_item_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h2(tags$b("Auswahl")),
    tags$h3("Wähle einen Pool an Items aus, die du Üben möchtest"),
    shinyWidgets::pickerInput(
      inputId = NS(id, "picker"),
      label = "Auswahl",
      choices = list("A", "B", "C", "D", "E"),
      selected = NULL,
      multiple = TRUE
    ),
    actionButton(NS(id, "submit_btn"), "Start"),
    textOutput(NS(id, "if_empty"))
  )
}

#' select_item Server Functions
#'
#' @noRd
mod_select_item_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_select_item_ui("select_item_1")

## To be copied in the server
# mod_select_item_server("select_item_1")
