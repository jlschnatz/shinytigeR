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
                tags$p("Wir freuen uns, dich auf tigeR begrüßen zu dürfen! Hier findest du Übungsaufgaben begleitend zu den Inhalten des Moduls PsyBSc2."),
                htmltools::withTags(
                  ul(
                    li("Unter ", b("Üben"), "geht es direkt zu den Aufgaben"),
                    li("Unter ", b("Fortschritt"), "erhältst du einen Überblick über deine bisherigen Aktivitäten sowie Feedback zu deinen Kompetenzen"),
                    li("Unter ", b("Datenset"), "findest du alle wichtigen Informationen zum tigeR Datenset. Dieses wird für die Bearbeitung von R-Aufgaben benötigt.")
                  )
                ),
                tags$p("Möchtest du die Grundlagen von R unabhängig von Statistik nochmal vertiefend üben, dann schau doch mal bei ", HTML("<a href='https://meikesteinhilber.github.io/otter/'><code>otter</code></a>"), "vorbei!"),
                tags$p("Bist du an Materialien aus der R-bezogenen Statistik Lehre im Psychologie-Studium interessiert, schau bei", HTML("<a href='https://pandar.netlify.app/'><code>pandaR</code></a>"), "vorbei!"),
                tags$p("Bei Fragen, Problemen oder Anmerkungen kannst du dich jederzeit an Martin Schultze unter", a("schultze@psych.uni-frankfurt.de", href = "mailto:schultze@psych.uni-frankfurt.de"), "wenden."),
                tags$p("Viel Spaß", bsicons::bs_icon("emoji-smile"))
              )
            ),
            col_1()
        )
      )
    })

  })
}
