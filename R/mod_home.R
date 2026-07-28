mod_home_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "main-content",
    bslib::card(
      bslib::card_body(
        class = "p-4",
        # ── Greeting ───────────────────────────────────────────────────────────
        uiOutput(ns("greeting")),
        tags$hr(class = "my-4"),
        # ── How it works ───────────────────────────────────────────────────────
        tags$p(
          class = "fw-semibold text-muted text-uppercase small mb-2",
          "So funktioniert tigeR"
        ),
        div(
          class = "home-steps",
          div(
            class = "home-step",
            div(class = "home-step-num", "1"),
            div(
              tags$b("Üben"),
              tags$p(
                class = "text-muted mb-0",
                "Wähle Themenbereiche und Aufgabentypen aus, beantworte Fragen und erhalte direktes, aufgabenbezogenes Feedback."
              )
            )
          ),
          div(
            class = "home-step",
            div(class = "home-step-num", "2"),
            div(
              tags$b("Fortschritt"),
              tags$p(
                class = "text-muted mb-0",
                "Verfolge im Dashboard, wie sich deine Kompetenz in den einzelnen Themenbereichen entwickelt."
              )
            )
          ),
          div(
            class = "home-step",
            div(class = "home-step-num", "3"),
            div(
              tags$b("FAQ"),
              tags$p(
                class = "text-muted mb-0",
                "Finde Antworten auf häufige Fragen sowie weiterführende Links zu Aufgabenpool und Dokumentation."
              )
            )
          )
        )
      )
    ),
    # ── Start button ───────────────────────────────────────────────────────────
    div(
      class = "d-grid mt-3",
      actionButton(
        ns("start"),
        div(bsicons::bs_icon("play-fill"), tags$b("Jetzt üben")),
        class = "btn btn-primary btn-lg"
      )
    )
  )
}

mod_home_server <- function(id, data_item, credentials, go_train) {
  moduleServer(id, function(input, output, session) {
    output$greeting <- renderUI({
      user <- credentials()$info$user_name
      div(
        tags$h4(class = "fw-bold mb-1", sprintf("Hallo, %s!", user)),
        tags$p(
          class = "text-muted mb-0",
          "Willkommen bei tigeR — deiner Übungsplattform für Statistik und R mit individuellem Feedback."
        )
      )
    })

    observeEvent(input$start, go_train())
  })
}
