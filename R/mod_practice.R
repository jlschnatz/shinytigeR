mod_practice_ui <- function(id, data_item, practice_ids) {
  ns <- NS(id)

  # Which of mc_wrap/num_wrap should be visible for the FIRST item of this
  # queue, baked directly into the initial HTML. This can't be left to the
  # server's observe() (below) alone: that observe() and this UI function are
  # two separate reactive contexts that both react to practice_ids() but have
  # no dependency edge between them, so their relative order within the same
  # flush is unspecified. If the observe()'s shinyjs::show()/hide() messages
  # for mc_wrap/num_wrap happen to be processed client-side before this UI's
  # HTML has actually been inserted into the DOM, jQuery finds no matching
  # element and the message is silently dropped — leaving whichever default
  # was baked into the HTML (both visible, if neither is pre-hidden here).
  # Item-to-item transitions later (via "Weiter") don't have this problem —
  # the DOM already exists by then — so only the *first* item needs this.
  ids <- practice_ids()
  first_is_num <- FALSE
  if (length(ids) > 0L) {
    row <- data_item[data_item$id_item == ids[1], , drop = FALSE]
    if (nrow(row) == 1L) first_is_num <- identical(as.character(row$answer_mode[1]), "num")
  }
  mc_ui <- div(id = ns("mc_wrap"), mod_mc_answer_ui(ns("answer_mc")))
  num_ui <- div(id = ns("num_wrap"), mod_numeric_answer_ui(ns("answer_num")))
  if (first_is_num) {
    mc_ui <- shinyjs::hidden(mc_ui)
  } else {
    num_ui <- shinyjs::hidden(num_ui)
  }

  div(
    class = "practice-wrap",
    # ── Progress bar ──────────────────────────────────────────────────────────
    uiOutput(ns("progress_bar")),
    # ── Item card ─────────────────────────────────────────────────────────────
    bslib::card(
      bslib::card_body(
        # Stimulus — only re-renders when item changes, NOT on check
        shiny::withMathJax(uiOutput(ns("item_stimulus"))),
        # Answers + feedback — both answer-mode child modules are mounted
        # statically (never remounted per item); only their visibility is
        # toggled, and only the numeric module's post-check panel re-renders
        # via its own uiOutput. See mod_numeric_answer.R for why the numeric
        # module's input/skip button specifically must stay a stable DOM node.
        div(
          class = "practice-answers-section",
          shiny::withMathJax(
            div(mc_ui, num_ui)
          )
        )
      )
    ),
    # ── Action buttons ────────────────────────────────────────────────────────
    div(
      class = "d-flex gap-2 justify-content-between mt-3",
      actionButton(
        ns("back"),
        div(bsicons::bs_icon("arrow-left"), "Neue Auswahl"),
        class = "btn btn-outline-secondary"
      ),
      div(
        class = "d-flex gap-2",
        shinyjs::disabled(
          actionButton(
            ns("check"),
            div(bsicons::bs_icon("check2-circle"), "Antwort prüfen"),
            class = "btn btn-primary"
          )
        ),
        shinyjs::hidden(
          actionButton(
            ns("next_item"),
            div(tags$b("Weiter"), bsicons::bs_icon("arrow-right")),
            class = "btn btn-success"
          )
        )
      )
    )
  )
}

mod_practice_server <- function(
  id,
  data_item,
  practice_ids,
  credentials,
  write_trigger
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Internal state — position in the queue + check state.
    # `result` (set once state$checked is TRUE) is a list with `category`
    # ("correct"/"incorrect"/"skip"/"unmatched" — the last only for numeric
    # items), `answer_idx` (matched distractor index, or NA), and `typed_value`
    # (numeric input only, NA otherwise) — this is what the answer child
    # modules read to render their post-check view.
    state <- reactiveValues(
      pos = 1L,
      checked = FALSE,
      result = NULL
    )

    # Reset to position 1 whenever a new queue is set
    observeEvent(
      practice_ids(),
      {
        state$pos <- 1L
        state$checked <- FALSE
        state$result <- NULL
      },
      ignoreNULL = TRUE
    )

    current_item <- reactive({
      ids <- practice_ids()
      req(length(ids) > 0L, state$pos >= 1L, state$pos <= length(ids))
      data_item[data_item$id_item == ids[state$pos], , drop = FALSE]
    })

    # Both answer-mode child modules are registered once and stay active for
    # the module's lifetime (same "register once, toggle UI" pattern as the
    # top-level modules in server.R) — the observe() below picks which one's
    # UI is actually visible, based on the current item's answer_mode.
    mc_answer <- mod_mc_answer_server(
      "answer_mc",
      item = current_item,
      checked = reactive(state$checked),
      result = reactive(state$result)
    )
    num_answer <- mod_numeric_answer_server(
      "answer_num",
      item = current_item,
      checked = reactive(state$checked),
      result = reactive(state$result)
    )

    is_numeric_item <- reactive({
      item <- current_item()
      req(nrow(item) == 1L)
      identical(item$answer_mode[1], "num")
    })

    # ── Progress bar ──────────────────────────────────────────────────────────
    output$progress_bar <- renderUI({
      ids <- practice_ids()
      req(length(ids) > 0L)
      pct <- round(100 * (state$pos - 1L) / length(ids))
      div(
        div(
          class = "d-flex justify-content-between text-muted small mb-1",
          span(
            class = "d-inline-flex align-items-center",
            sprintf("Aufgabe %d von %d", state$pos, length(ids)),
            item_id_badge(ids[state$pos], ns("copy_item_id"), class = "ms-2")
          ),
          span(sprintf("%d%%", pct))
        ),
        div(
          class = "progress mb-3",
          style = "height: 6px;",
          div(
            class = "progress-bar",
            role = "progressbar",
            style = sprintf("width: %d%%;", pct),
            `aria-valuenow` = pct,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100
          )
        )
      )
    })

    # ── Stimulus — only invalidates when the item changes, never on check ────────
    output$item_stimulus <- renderUI({
      item <- current_item()
      req(nrow(item) == 1L)
      item <- as.list(item[1, ])
      session$onFlushed(
        function() session$sendCustomMessage("mathjax_typeset", TRUE),
        once = TRUE
      )
      div(
        class = "stimulus mb-2",
        if (
          !is.na(item$stimulus_text) && nzchar(trimws(item$stimulus_text))
        ) {
          div(
            class = "stimulus-text",
            shiny::HTML(render_md(item$stimulus_text))
          )
        },
        if (!is.na(item$stimulus_image) && nzchar(item$stimulus_image)) {
          div(
            class = "text-center my-3",
            tags$img(
              src = item$stimulus_image,
              class = "img-fluid stimulus-img"
            )
          )
        }
      )
    })

    # ── Toggle which answer-mode child is visible — no remounting ────────────
    observe({
      item <- current_item()
      req(nrow(item) == 1L)
      if (identical(item$answer_mode[1], "num")) {
        shinyjs::hide("mc_wrap")
        shinyjs::show("num_wrap")
      } else {
        shinyjs::show("mc_wrap")
        shinyjs::hide("num_wrap")
      }
    })

    # ── Check-button gating — mirrors whichever child module is active ───────
    observe({
      ready <- if (is_numeric_item()) {
        isTRUE(num_answer$ready())
      } else {
        isTRUE(mc_answer$ready())
      }
      if (ready) shinyjs::enable("check") else shinyjs::disable("check")
    })

    # ── Reset button state when moving to a new item ──────────────────────────
    observeEvent(
      state$pos,
      {
        state$checked <- FALSE
        state$result <- NULL
        shinyjs::disable("check")
        shinyjs::hide("next_item")
        shinyjs::show("check")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      practice_ids(),
      {
        state$checked <- FALSE
        state$result <- NULL
        shinyjs::disable("check")
        shinyjs::hide("next_item")
        shinyjs::show("check")
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # ── Shared check-flow: store the result, write the response row ──────────
    finish_check <- function(category, answer_idx = NA_integer_, typed_value = NA_real_) {
      state$result <- list(
        category = category,
        answer_idx = answer_idx,
        typed_value = typed_value
      )
      state$checked <- TRUE

      shinyjs::hide("check")
      shinyjs::show("next_item")

      item <- as.list(current_item()[1, ])
      uid <- credentials()$info$user_name
      token <- session$token
      row <- build_response_row(
        item,
        answer_idx,
        uid,
        token,
        typed_value = typed_value,
        skipped = category == "skip"
      )
      tryCatch(
        {
          db_write_response(uid, row)
          write_trigger(write_trigger() + 1L)
        },
        error = function(e) {
          showNotification(
            paste(
              "Antwort konnte nicht gespeichert werden:",
              conditionMessage(e)
            ),
            type = "error",
            duration = 8
          )
        }
      )
    }

    # ── Check answer ──────────────────────────────────────────────────────────
    observeEvent(input$check, {
      req(input$check)
      item <- current_item()
      req(nrow(item) == 1L)
      item_list <- as.list(item[1, ])

      if (is_numeric_item()) {
        typed_value <- num_answer$raw_answer()
        req(!is.na(typed_value))
        ev <- evaluate_numeric_answer(item_list, typed_value)
        finish_check(ev$result, answer_idx = ev$matched_idx, typed_value = typed_value)
      } else {
        ans_idx <- mc_answer$raw_answer()
        req(!is.na(ans_idx))
        category <- evaluate_answer(item_list, ans_idx)
        finish_check(category, answer_idx = ans_idx)
      }
    })

    # ── Explicit skip (numeric items only) ────────────────────────────────────
    observeEvent(
      num_answer$skip_requested(),
      {
        req(is_numeric_item(), !isTRUE(state$checked))
        finish_check("skip")
      },
      ignoreInit = TRUE
    )

    # ── Next item ─────────────────────────────────────────────────────────────
    observeEvent(input$next_item, {
      req(input$next_item)
      ids <- practice_ids()
      if (state$pos < length(ids)) {
        state$pos <- state$pos + 1L
      } else {
        # Finished all items — return to selector
        practice_ids(NULL)
        showNotification(
          paste0("Du hast alle ", length(ids), " Aufgaben bearbeitet!"),
          type = "message",
          duration = 5
        )
      }
    })

    # ── Back to selector ──────────────────────────────────────────────────────
    observeEvent(input$back, {
      req(input$back)
      practice_ids(NULL)
    })
  })
}
