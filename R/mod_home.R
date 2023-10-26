#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("home"))
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, credentials){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$home <- renderUI({

      req(credentials()$user_auth)
      fluidRow(
        col_6(
          tags$h5("Willkommen bei tigeR – deinem Selbstlerntools für Statistik"),
          tags$p("Wir freuen uns, dich auf tigeR begrüßen zu dürfen!"),
          tags$p("Hier findest du Übungsaufgaben begleitend zu den Inhalten des Moduls PsyBSc2."),
          tags$p("Unter dem Reiter Üben geht es direkt zu den Aufgaben und unter Fortschritt erhältst du einen Überblick über deine bisherigen Aktivitäten in tigeR."),
          tags$p("Bei Fragen, Problemen oder Anmerkungen kannst du dich jederzeit an Julia Beitner unter beitner@psych.uni-frankfurt.de wenden."),
          tags$p("Viel Spaß :)")
        ),
        col_6(
          # You can add an image or any other content in the right column
          tags$img(src = "www/img_logo/tigeR_Logo_transparent.png", width = "30%")
        )
      )
    })

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")


