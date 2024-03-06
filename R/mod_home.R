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
      tagList(
          fluidRow(
            col_1(),
            col_10(
              bslib::card(
                tags$img(src = "www/img_logo/tigeR_Logo_transparent.png", width = "10%"),
                tags$h5("Willkommen bei tigeR – deiner Übungsplattform für Statistik", class = "d-flex justify-content-between"),
                tags$p("Wir freuen uns, dich auf tigeR begrüßen zu dürfen! Hier findest du Übungsaufgaben begleitend zu den Inhalten des Moduls PsyBSc2"),
                tags$ul(tags$li("Unter dem Reiter", code("Üben") ,"geht es direkt zu den Aufgaben und unter Fortschritt erhältst du einen Überblick über deine bisherigen Aktivitäten in tigeR."),
                tags$li("Unter dem Reiter", code("Datenset"), "findest du alle wichtige Informationen zum tigeR Datenset. Dieses wird für die Bearbeitung von R-Aufgaben benötigt."),
                tags$li("Unter dem Reiter", code("Fortschritt"), "erhältst du einen Überblick über deine bisherigen Aktivitäten in tigeR sowie Feedback zu deinen Kompetenzen."),
                ),
                tags$p("Möchtest du die Grundlagen von R unabhängig von Statistik nochmal vertiefend üben, dann schau doch mal bei ", HTML("<a href='https://meikesteinhilber.github.io/otter/'><code>otter</code></a>"), "vorbei!"),
                tags$p("Bei Fragen, Problemen oder Anmerkungen kannst du dich jederzeit an Julia Beitner unter", a("beitner@psych.uni-frankfurt.de", href = "mailto:beitner@psych.uni-frankfurt.de"), "wenden."),
                tags$p("Viel Spaß :)")
              )
            ),
            col_1()
        )
      )
    })

  })
}
