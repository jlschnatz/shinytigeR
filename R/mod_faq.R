mod_faq_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "faq-wrap",
    tags$h3(class = "fw-bold mb-3", "Häufig gestellte Fragen"),
    uiOutput(ns("faq_content")),
    tags$hr(class = "my-4"),
    div(
      class = "d-flex flex-column gap-1",
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
}

mod_faq_server <- function(id, data_item) {
  moduleServer(id, function(input, output, session) {
    output$faq_content <- renderUI({
      tags$p(class = "text-muted", "Inhalte folgen in Kürze.")
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
  })
}
