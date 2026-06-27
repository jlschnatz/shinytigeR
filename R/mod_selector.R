
mod_selector_ui <- function(id) {
  ns <- NS(id)

  area_keys  <- names(LEARNING_AREA_LABELS)   # short display labels
  area_vals  <- unname(LEARNING_AREA_LABELS)  # DB values
  type_keys  <- names(ITEM_TYPE_LABELS)       # "Inhaltlich", "R-Code"
  n_areas    <- length(area_keys)
  n_types    <- length(type_keys)

  # Raw checkbox helper — Shiny binds any <input type="checkbox"> by id
  cb <- function(input_id, checked = TRUE) {
    tags$input(type = "checkbox", id = input_id,
               class = "sel-cb", checked = if (checked) NA else NULL)
  }

  # Header row: "select-all" corner + one col header per learning area
  header_cells <- tagList(
    tags$th(class = "sel-corner", cb(ns("all"))),
    lapply(seq_len(n_areas), function(j)
      tags$th(
        class = "sel-col-header",
        tags$label(`for` = ns(paste0("col_", j)), class = "sel-col-label",
                   area_keys[j]),
        cb(ns(paste0("col_", j)))
      )
    )
  )

  # Data rows: one per item type
  data_rows <- lapply(seq_len(n_types), function(i) {
    tags$tr(
      tags$th(
        class = "sel-row-header",
        cb(ns(paste0("row_", i))),
        tags$label(`for` = ns(paste0("row_", i)), class = "sel-row-label",
                   type_keys[i])
      ),
      lapply(seq_len(n_areas), function(j)
        tags$td(class = "sel-cell",
                cb(ns(paste0("cell_", i, "_", j))))
      )
    )
  })

  div(
    class = "selector-wrap",
    bslib::card(
      bslib::card_header(
        div(class = "d-flex align-items-center gap-2",
          bsicons::bs_icon("funnel-fill"),
          tags$b("Aufgaben auswählen")
        )
      ),
      bslib::card_body(class = "p-4",
        # ── Checkbox matrix ────────────────────────────────────────────────────
        tags$p(class = "text-muted mb-3",
          "Wähle Themenbereiche und Aufgabentypen aus — du bekommst dann eine zufällige Auswahl zum Üben."
        ),
        div(class = "sel-matrix-wrap mb-2",
          tags$table(
            class = "sel-matrix",
            tags$thead(tags$tr(header_cells)),
            tags$tbody(data_rows)
          )
        ),
        # ── Options row ────────────────────────────────────────────────────────
        div(class = "d-flex align-items-center gap-3 flex-wrap mb-1 sel-options-row",
          numericInput(ns("n_items"), label = NULL,
                       value = 10L, min = 1L, max = 50L, step = 1L,
                       width = "75px"),
          uiOutput(ns("avail_info"), class = "sel-avail-info"),
          div(class = "ms-auto",
            div(class = "form-check form-switch mb-0",
              tags$input(type = "checkbox", class = "form-check-input",
                         id = ns("only_new"), role = "switch"),
              tags$label(class = "form-check-label", `for` = ns("only_new"),
                         "Nur neue Aufgaben")
            )
          )
        ),
        div(class = "d-grid mt-2",
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
    area_vals <- unname(LEARNING_AREA_LABELS)
    type_vals <- unname(ITEM_TYPE_LABELS)
    n_areas   <- length(area_vals)
    n_types   <- length(type_vals)

    # ── Select-all / row / col header observers ──────────────────────────────

    # "Select all" drives all row and col headers (cells follow via those)
    observeEvent(input$all, {
      for (i in seq_len(n_types))  updateCheckboxInput(session, paste0("row_", i), value = input$all)
      for (j in seq_len(n_areas))  updateCheckboxInput(session, paste0("col_", j), value = input$all)
    }, ignoreInit = TRUE)

    # Row header drives its cells
    lapply(seq_len(n_types), function(i) {
      observeEvent(input[[paste0("row_", i)]], {
        for (j in seq_len(n_areas))
          updateCheckboxInput(session, paste0("cell_", i, "_", j), value = input[[paste0("row_", i)]])
      }, ignoreInit = TRUE)
    })

    # Col header drives its cells
    lapply(seq_len(n_areas), function(j) {
      observeEvent(input[[paste0("col_", j)]], {
        for (i in seq_len(n_types))
          updateCheckboxInput(session, paste0("cell_", i, "_", j), value = input[[paste0("col_", j)]])
      }, ignoreInit = TRUE)
    })

    # ── Derived state ─────────────────────────────────────────────────────────

    answered_ids <- reactive({
      credentials()$user_auth
      uid <- credentials()$info$user_name
      ud  <- db_get_userdata(uid)
      if (nrow(ud) == 0L) return(integer(0))
      unique(as.integer(ud$id_item))
    })

    # Collect selected (area, type) combinations from the cell checkboxes
    selected_combos <- reactive({
      combos <- list()
      for (i in seq_len(n_types)) {
        for (j in seq_len(n_areas)) {
          if (isTRUE(input[[paste0("cell_", i, "_", j)]])) {
            combos <- c(combos, list(c(type = type_vals[i], area = area_vals[j])))
          }
        }
      }
      combos
    })

    filtered_items <- reactive({
      combos <- selected_combos()
      if (length(combos) == 0L) return(data_item[0L, ])
      keep <- Reduce(`|`, lapply(combos, function(co)
        data_item$type_item == co[["type"]] & data_item$learning_area == co[["area"]]
      ))
      df <- data_item[keep, , drop = FALSE]
      if (isTRUE(input$only_new))
        df <- df[!df$id_item %in% answered_ids(), , drop = FALSE]
      df
    })

    output$avail_info <- renderUI({
      n_avail <- nrow(filtered_items())
      n_want  <- as.integer(input$n_items)
      if (is.na(n_want) || n_want < 1L) {
        return(tags$span(class = "text-danger",
          bsicons::bs_icon("exclamation-circle"), " Ungültige Anzahl."))
      }
      if (n_avail == 0L) {
        return(tags$span(class = "text-warning",
          bsicons::bs_icon("exclamation-triangle"), " Keine Aufgaben verfügbar."))
      }
      n_sel <- min(n_want, n_avail)
      tags$span(class = "text-muted",
        sprintf("von %d verfügbar, %d werden geladen", n_avail, n_sel)
      )
    })

    observeEvent(input$submit, {
      fi     <- filtered_items()
      n_want <- as.integer(input$n_items)
      if (nrow(fi) == 0L || is.na(n_want) || n_want < 1L) {
        showNotification("Keine Aufgaben für diese Auswahl.", type = "warning")
        return()
      }
      practice_ids(safe_sample(as.integer(fi$id_item), size = n_want))
    })
  })
}
