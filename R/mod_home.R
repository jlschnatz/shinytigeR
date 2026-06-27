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
              tags$b("Aufgaben auswählen"),
              tags$p(
                class = "text-muted mb-0",
                "Wähle Themenbereiche und Aufgabentypen aus."
              )
            )
          ),
          div(
            class = "home-step",
            div(class = "home-step-num", "2"),
            div(
              tags$b("Üben & Feedback erhalten"),
              tags$p(
                class = "text-muted mb-0",
                "Beantworte Fragen und erhalte direktes, individuelles Feedback."
              )
            )
          ),
          div(
            class = "home-step",
            div(class = "home-step-num", "3"),
            div(
              tags$b("Fortschritt verfolgen"),
              tags$p(
                class = "text-muted mb-0",
                "Sieh im Dashboard, wie sich deine Kompetenz entwickelt."
              )
            )
          )
        ),
        tags$hr(class = "my-4"),
        # ── Pool info + contact ────────────────────────────────────────────────
        div(
          class = "d-flex flex-column gap-1 mt-3",
          uiOutput(ns("pool_info")),
          tags$p(
            class = "text-muted mb-0",
            bsicons::bs_icon("book"),
            " Dokumentation der R-Datensätze: ",
            tags$a(
              href = "https://pandar.netlify.app/daten/datensaetze/",
              target = "_blank",
              "https://pandar.netlify.app/daten/datensaetze/"
            )
          ),
          tags$p(
            class = "text-muted mb-0",
            bsicons::bs_icon("terminal"),
            " R-Grundlagen üben: ",
            tags$a(
              href = "https://meikesteinhilber.github.io/otter/",
              target = "_blank",
              "otter"
            )
          ),
          tags$p(
            class = "text-muted mb-0",
            bsicons::bs_icon("envelope"),
            " Fragen oder Probleme? ",
            tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL)
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

    output$pool_info <- renderUI({
      n_total <- nrow(data_item)
      n_areas <- length(unique(data_item$learning_area))
      tags$p(
        class = "text-muted mb-0",
        bsicons::bs_icon("collection"),
        sprintf(
          " Der aktuelle Aufgabenpool umfasst %d Aufgaben aus %d Themenbereichen.",
          n_total,
          n_areas
        )
      )
    })

    observeEvent(input$start, go_train())
  })
}
