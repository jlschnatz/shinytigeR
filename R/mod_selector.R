mod_selector_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "selector-wrap",
    bslib::card(
      bslib::card_header(
        div(class = "d-flex align-items-center gap-2",
          bsicons::bs_icon("funnel-fill"),
          tags$b("Aufgaben auswählen")
        )
      ),
      bslib::card_body(
        # ── Themenbereich ──────────────────────────────────────────────────────
        tags$p(class = "fw-semibold mb-1 text-muted text-uppercase small",
               "Themenbereich"),
        div(class = "chip-group mb-3",
          checkboxGroupInput(ns("areas"), label = NULL,
            choices  = LEARNING_AREA_LABELS,
            selected = LEARNING_AREA_LABELS,
            inline   = TRUE
          )
        ),
        # ── Aufgabentyp ────────────────────────────────────────────────────────
        tags$p(class = "fw-semibold mb-1 text-muted text-uppercase small",
               "Aufgabentyp"),
        div(class = "chip-group mb-3",
          checkboxGroupInput(ns("types"), label = NULL,
            choices  = ITEM_TYPE_LABELS,
            selected = ITEM_TYPE_LABELS,
            inline   = TRUE
          )
        ),
        # ── Options row ────────────────────────────────────────────────────────
        div(class = "d-flex align-items-start gap-4 flex-wrap mb-2",
          div(
            tags$p(class = "fw-semibold mb-1 text-muted text-uppercase small",
                   "Anzahl"),
            div(class = "d-flex align-items-center gap-2",
              numericInput(ns("n_items"), label = NULL,
                           value = 10L, min = 1L, max = 50L, step = 1L,
                           width = "90px")
            )
          ),
          div(class = "ms-auto pt-3",
            div(class = "form-check form-switch",
              tags$input(type = "checkbox", class = "form-check-input",
                         id = ns("only_new"), role = "switch"),
              tags$label(class = "form-check-label", `for` = ns("only_new"),
                         "Nur neue Aufgaben")
            )
          )
        ),
        uiOutput(ns("avail_info")),
        div(class = "d-grid mt-3",
          actionButton(ns("submit"),
            div(bsicons::bs_icon("play-fill"), tags$b("Üben starten")),
            class = "btn btn-primary btn-lg"
          )
        )
      )
    )
  )
}

mod_selector_server <- function(id, data_item, practice_ids, credentials) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    answered_ids <- reactive({
      credentials()$user_auth  # invalidate on login
      uid <- credentials()$info$user_name
      ud  <- db_get_userdata(uid)
      if (nrow(ud) == 0L) return(integer(0))
      unique(as.integer(ud$id_item))
    })

    filtered_items <- reactive({
      req(input$areas, input$types)
      df <- data_item[
        data_item$learning_area %in% input$areas &
        data_item$type_item     %in% input$types, ,
        drop = FALSE
      ]
      if (isTRUE(input$only_new)) {
        df <- df[!df$id_item %in% answered_ids(), , drop = FALSE]
      }
      df
    })

    output$avail_info <- renderUI({
      n_avail <- nrow(filtered_items())
      n_want  <- as.integer(input$n_items)
      if (is.na(n_want) || n_want < 1L) {
        return(div(class = "text-danger small",
          bsicons::bs_icon("exclamation-circle"), " Bitte gib eine gültige Anzahl ein."))
      }
      if (n_avail == 0L) {
        return(div(class = "text-warning small",
          bsicons::bs_icon("exclamation-triangle"),
          " Keine Aufgaben für diese Auswahl."))
      }
      n_sel <- min(n_want, n_avail)
      div(class = "text-muted small",
        sprintf("%d Aufgabe%s verfügbar — %d werden geladen.",
                n_avail, if (n_avail == 1L) "" else "n", n_sel)
      )
    })

    observeEvent(input$submit, {
      req(input$submit)
      fi     <- filtered_items()
      n_want <- as.integer(input$n_items)
      if (nrow(fi) == 0L || is.na(n_want) || n_want < 1L) {
        showNotification("Keine Aufgaben für diese Auswahl.", type = "warning")
        return()
      }
      ids <- safe_sample(as.integer(fi$id_item), size = n_want)
      practice_ids(ids)
    })
  })
}
