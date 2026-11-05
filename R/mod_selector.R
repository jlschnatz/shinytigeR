# A selector card title with its step number (styled like the home page's
# numbered steps, .sel-step-num)
sel_step_title <- function(n, title) {
  div(
    class = "d-flex align-items-center gap-2",
    tags$span(class = "sel-step-num", `aria-hidden` = "true", n),
    tags$b(title)
  )
}

mod_selector_ui <- function(id, data_item) {
  ns <- NS(id)

  area_keys <- names(LEARNING_AREA_LABELS)
  area_vals <- unname(LEARNING_AREA_LABELS)
  type_keys <- names(ITEM_TYPE_LABELS)
  type_vals <- unname(ITEM_TYPE_LABELS)
  n_areas <- length(area_keys)
  n_types <- length(type_keys)

  # Total item counts per cell — static, used only to permanently disable empty cells
  total_counts <- outer(
    seq_len(n_types),
    seq_len(n_areas),
    Vectorize(function(i, j) {
      sum(data_item$type_item == type_vals[i] & data_item$learning_area == area_vals[j])
    })
  )

  # Each cell is a hidden checkbox + a `<label>` styled as a pill via CSS
  # (`.sel-chip` / `:checked` in app.css) — same pattern as the old checkbox
  # matrix, just restyled. Keeps state in the DOM so shinyjs::disable() /
  # updateCheckboxInput() can drive it without losing checked state on re-render.

  # ── Column headers — select-all per item type ───────────────────────────────
  type_header_row <- div(
    class = "sel-area-row sel-type-header",
    div(class = "sel-area-label"),
    div(
      class = "sel-chip-group",
      lapply(seq_len(n_types), function(i) {
        div(
          class = "sel-chip-wrap",
          tags$input(
            type = "checkbox",
            id = ns(paste0("type_all_", i)),
            class = "sel-chip-input"
          ),
          tags$label(
            `for` = ns(paste0("type_all_", i)),
            class = "sel-chip sel-chip-header",
            type_keys[i]
          )
        )
      })
    )
  )

  # ── One row per learning area, one chip per item type ───────────────────────
  area_rows <- lapply(seq_len(n_areas), function(j) {
    div(
      class = "sel-area-row",
      div(class = "sel-area-label", area_vals[j]),
      div(
        class = "sel-chip-group",
        lapply(seq_len(n_types), function(i) {
          empty <- total_counts[i, j] == 0L
          cell_id <- ns(paste0("cell_", i, "_", j))
          div(
            class = "sel-chip-wrap",
            tags$input(
              type = "checkbox",
              id = cell_id,
              class = "sel-chip-input",
              disabled = if (empty) NA else NULL
            ),
            tags$label(
              `for` = cell_id,
              class = "sel-chip",
              type_keys[i],
              uiOutput(ns(paste0("count_", i, "_", j)), inline = TRUE, container = tags$span),
              uiOutput(ns(paste0("badge_", i, "_", j)), inline = TRUE, container = tags$span)
            )
          )
        })
      )
    )
  })

  # Layout: from 1200 px two columns (.sel-layout in app.css), the area list
  # left (step 1) and, on the right and as tall as it, what to do with it (step 2:
  # only-new switch, count, start) plus the direct lookup. Before, all of it
  # was one full-width card whose rows had ~700 px of nothing between the
  # area names and their chips on a laptop. Narrower screens stack the three
  # cards in this order.
  div(
    class = "selector-wrap sel-layout",
    # Numbered like the steps on the home page, so the order is clear:
    # 1 = which topics, 2 = how many (and whether only new ones) + start.
    # The direct lookup is the alternative to both, so it has no number.
    bslib::card(
      class = "sel-main",
      bslib::card_header(sel_step_title(1L, "Themenbereiche wählen")),
      bslib::card_body(
        tags$p(
          class = "text-muted mb-0",
          "Wähle, aus welchen Themenbereichen und Aufgabentypen geübt wird. Daraus bekommst du eine zufällige Auswahl."
        ),
        div(
          class = "sel-area-list mb-1",
          type_header_row,
          area_rows
        )
      )
    ),
    div(
      class = "sel-side",
      bslib::card(
        class = "sel-choice",
        bslib::card_header(sel_step_title(2L, "Anzahl festlegen und starten")),
        # gap: bslib's default of 1rem between children made the card airy
        bslib::card_body(
          gap = "0.9rem",
          # ── Only-new toggle ────────────────────────────────────────────────────
          div(
            class = "form-check form-switch mb-0",
            tags$input(
              type = "checkbox",
              class = "form-check-input",
              id = ns("only_new"),
              role = "switch"
            ),
            tags$label(
              class = "form-check-label",
              `for` = ns("only_new"),
              "Nur neue Aufgaben"
            )
          ),
          # ── Item count: stepper + presets, one row ─────────────────────────────
          div(
            tags$div(class = "text-muted mb-1", "Anzahl Aufgaben"),
            # two groups, so a narrow screen wraps between them, not inside
            div(
              class = "sel-count-row",
              div(
                class = "sel-count-group",
                actionButton(ns("n_minus"), "−", class = "btn btn-outline-secondary sel-stepper-btn"),
                div(class = "sel-count-value", uiOutput(ns("n_value"), inline = TRUE)),
                actionButton(ns("n_plus"), "+", class = "btn btn-outline-secondary sel-stepper-btn")
              ),
              div(
                class = "sel-count-group sel-preset-group",
                actionButton(ns("preset_5"), "5", class = "btn btn-outline-secondary sel-preset-btn"),
                actionButton(ns("preset_10"), "10", class = "btn btn-outline-secondary sel-preset-btn"),
                actionButton(ns("preset_20"), "20", class = "btn btn-outline-secondary sel-preset-btn"),
                actionButton(ns("preset_all"), uiOutput(ns("preset_all_label"), inline = TRUE), class = "btn btn-outline-secondary sel-preset-btn")
              )
            )
          ),
          # ── Submit ─────────────────────────────────────────────────────────────
          div(
            class = "d-grid sel-footer",
            actionButton(
              ns("submit"),
              div(bsicons::bs_icon("play-fill"), tags$b("Starten")),
              class = "btn btn-primary btn-lg"
            )
          )
        )
      ),
      # ── Direct item lookup ──────────────────────────────────────────────────
      bslib::card(
        class = "sel-direct",
        bslib::card_header(
          div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("hash"),
            tags$b("Spezifische Aufgabe auswählen")
          )
        ),
        bslib::card_body(
          gap = "0.5rem",
          tags$p(
            class = "text-muted mb-0",
            "Direkt zu einer Aufgabe springen."
          ),
          div(
            class = "input-group sel-direct-row",
            tags$input(
              id = ns("direct_id"),
              type = "number",
              class = "shiny-input-number form-control",
              min = 1L,
              step = 1L,
              placeholder = "ID",
              # grows/shrinks with the column, so field and button stay on one
              # line in the narrow side column too
              style = "flex: 1 1 5rem; min-width: 0;"
            ),
            actionButton(
              ns("direct_submit"),
              # shorter label on phones, so the ID field keeps room
              div(
                bsicons::bs_icon("eye"),
                tags$span(class = "d-none d-sm-inline", "Aufgabe ansehen"),
                tags$span(class = "d-sm-none", "Ansehen")
              ),
              class = "btn btn-outline-primary"
            )
          )
        )
      )
    )
  )
}

mod_selector_server <- function(
  id,
  data_item,
  practice_ids,
  inspect_id,
  credentials,
  write_trigger
) {
  moduleServer(id, function(input, output, session) {
    area_vals <- unname(LEARNING_AREA_LABELS)
    type_vals <- unname(ITEM_TYPE_LABELS)
    n_areas <- length(area_vals)
    n_types <- length(type_vals)

    total_counts <- outer(
      seq_len(n_types),
      seq_len(n_areas),
      Vectorize(function(i, j) {
        sum(data_item$type_item == type_vals[i] & data_item$learning_area == area_vals[j])
      })
    )

    # ── Column header ("select all of this type") drives its cells ─────────────
    lapply(seq_len(n_types), function(i) {
      observeEvent(
        input[[paste0("type_all_", i)]],
        {
          for (j in seq_len(n_areas)) {
            updateCheckboxInput(
              session,
              paste0("cell_", i, "_", j),
              value = input[[paste0("type_all_", i)]]
            )
          }
        },
        ignoreInit = TRUE
      )
    })

    # ── Derived state ─────────────────────────────────────────────────────────

    # Items already ANSWERED (right or wrong), with the dashboard's definition
    # (answered_rows()): an item that was only skipped stays "neu", so "Nur
    # neue Aufgaben" can still draw it, matching the coverage card's "offen"
    answered_ids <- reactive({
      write_trigger()
      uid <- credentials()$info$user_name
      ud <- db_get_userdata(uid)
      if (nrow(ud) == 0L) {
        return(integer(0))
      }
      unique(as.integer(answered_rows(ud)$id_item))
    })

    new_counts <- reactive({
      ids <- answered_ids()
      outer(
        seq_len(n_types),
        seq_len(n_areas),
        Vectorize(function(i, j) {
          sum(
            !data_item$id_item %in% ids &
              data_item$type_item == type_vals[i] &
              data_item$learning_area == area_vals[j]
          )
        })
      )
    })

    # ── Per-cell count text + "N neu" badge ─────────────────────────────────────
    for (i in seq_len(n_types)) {
      for (j in seq_len(n_areas)) {
        local({
          ii <- i
          jj <- j
          output[[paste0("count_", ii, "_", jj)]] <- renderUI({
            only_new_on <- isTRUE(input$only_new)
            n <- if (only_new_on) new_counts()[ii, jj] else total_counts[ii, jj]
            tags$span(class = "sel-chip-count", n)
          })
          output[[paste0("badge_", ii, "_", jj)]] <- renderUI({
            n_new <- new_counts()[ii, jj]
            if (isTRUE(input$only_new) || n_new == 0L) {
              return(NULL)
            }
            tags$span(class = "sel-chip-badge", sprintf("%d neu", n_new))
          })
        })
      }
    }

    # ── Disable + auto-deselect cells with no new items when only_new is on ────
    observe({
      only_new_on <- isTRUE(input$only_new)
      nc <- new_counts()
      for (i in seq_len(n_types)) {
        for (j in seq_len(n_areas)) {
          cell_id <- paste0("cell_", i, "_", j)
          permanently_empty <- total_counts[i, j] == 0L
          should_disable <- permanently_empty || (only_new_on && nc[i, j] == 0L)
          if (should_disable) {
            if (isTRUE(input[[cell_id]])) {
              updateCheckboxInput(session, cell_id, value = FALSE)
            }
            shinyjs::disable(cell_id)
          } else {
            shinyjs::enable(cell_id)
          }
        }
      }
    })

    # Collect selected (area, type) combinations from the cell checkboxes
    selected_combos <- reactive({
      combos <- list()
      for (i in seq_len(n_types)) {
        for (j in seq_len(n_areas)) {
          if (isTRUE(input[[paste0("cell_", i, "_", j)]])) {
            combos <- c(
              combos,
              list(c(type = type_vals[i], area = area_vals[j]))
            )
          }
        }
      }
      combos
    })

    filtered_items <- reactive({
      combos <- selected_combos()
      if (length(combos) == 0L) {
        return(data_item[0L, ])
      }
      keep <- Reduce(
        `|`,
        lapply(combos, function(co) {
          data_item$type_item == co[["type"]] &
            data_item$learning_area == co[["area"]]
        })
      )
      df <- data_item[keep, , drop = FALSE]
      if (isTRUE(input$only_new)) {
        df <- df[!df$id_item %in% answered_ids(), , drop = FALSE]
      }
      df
    })

    # ── Item count: stepper + presets ────────────────────────────────────────
    n_items <- reactiveVal(10L)
    want_all <- reactiveVal(FALSE)

    effective_n <- reactive({
      if (want_all()) max(nrow(filtered_items()), 1L) else n_items()
    })

    observeEvent(input$n_minus, {
      want_all(FALSE)
      n_items(max(1L, effective_n() - 1L))
    })
    observeEvent(input$n_plus, {
      want_all(FALSE)
      n_items(min(50L, effective_n() + 1L))
    })
    observeEvent(input$preset_5, {
      want_all(FALSE)
      n_items(5L)
    })
    observeEvent(input$preset_10, {
      want_all(FALSE)
      n_items(10L)
    })
    observeEvent(input$preset_20, {
      want_all(FALSE)
      n_items(20L)
    })
    observeEvent(input$preset_all, want_all(TRUE))

    output$n_value <- renderUI(effective_n())

    output$preset_all_label <- renderUI(
      sprintf("Alle (%d)", nrow(filtered_items()))
    )

    observeEvent(input$submit, {
      fi <- filtered_items()
      n_want <- effective_n()
      if (nrow(fi) == 0L || is.na(n_want) || n_want < 1L) {
        showNotification("Keine Aufgaben für diese Auswahl.", type = "warning")
        return()
      }
      n_avail <- nrow(fi)
      if (n_want > n_avail) {
        showNotification(
          sprintf("Nur %d Aufgaben verfügbar — %d werden geladen.", n_avail, n_avail),
          type = "message",
          duration = 4
        )
        n_want <- n_avail
      }
      practice_ids(safe_sample(as.integer(fi$id_item), size = n_want))
    })

    # ── Direct item lookup by ID ──────────────────────────────────────────────
    observeEvent(input$direct_submit, {
      target_id <- as.integer(input$direct_id)
      if (is.na(target_id) || !target_id %in% data_item$id_item) {
        showNotification(
          "Keine Aufgabe mit dieser ID gefunden.",
          type = "warning"
        )
        return()
      }
      inspect_id(target_id)
    })
  })
}
