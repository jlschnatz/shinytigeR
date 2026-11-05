mod_home_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "main-content",
    # ── Greeting: the page title, above the card like the FAQ's ──────────────
    uiOutput(ns("greeting")),
    # ── How it works ─────────────────────────────────────────────────────────
    # A card with a header like every other card (it used to be one card with
    # the greeting, a divider and a small caps label, with its own, larger
    # padding)
    bslib::card(
      bslib::card_header(tags$b("So funktioniert tigeR")),
      bslib::card_body(
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
                "Finde Antworten auf häufige Fragen, zum Beispiel zu deiner Einschätzung, und Links zu weiteren Materialien."
              )
            )
          )
        )
      )
    ),
    # ── Start buttons ──────────────────────────────────────────────────────────
    # Practising is the main action (filled); the progress button is the
    # second way on (outlined), with the Fortschritt tab's icon. Side by
    # side from 576 px, stacked on phones (.home-actions in app.css)
    div(
      class = "home-actions",
      actionButton(
        ns("start"),
        div(bsicons::bs_icon("play-fill"), tags$b("Jetzt üben")),
        class = "btn btn-primary btn-lg"
      ),
      actionButton(
        ns("progress"),
        div(bsicons::bs_icon("bar-chart-fill"), tags$b("Fortschritt ansehen")),
        class = "btn btn-outline-primary btn-lg"
      )
    )
  )
}

# `go_train()` / `go_progress()`: callbacks from server.R that switch the app's
# tabs (they need the app-level session, see CLAUDE.md)
mod_home_server <- function(id, data_item, credentials, go_train, go_progress = NULL) {
  moduleServer(id, function(input, output, session) {
    output$greeting <- renderUI({
      user <- credentials()$info$user_name
      div(
        class = "page-intro",
        tags$h4(class = "page-title mb-1", sprintf("Hallo, %s!", user)),
        tags$p(
          class = "text-muted mb-0",
          "Willkommen bei tigeR — deiner Übungsplattform für Statistik und R mit individuellem Feedback."
        )
      )
    })

    observeEvent(input$start, go_train())
    observeEvent(input$progress, if (!is.null(go_progress)) go_progress())
  })
}
