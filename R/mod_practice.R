mod_practice_ui <- function(id) {
  ns <- NS(id)

  # This UI is mounted once at login and never remounted (see train_view in
  # server.R), so the server's observers always find mc_wrap/num_wrap in the
  # DOM and set the right visibility per item. The hidden() default here is
  # only the pre-first-item state.
  mc_ui <- div(id = ns("mc_wrap"), mod_mc_answer_ui(ns("answer_mc")))
  num_ui <- shinyjs::hidden(
    div(id = ns("num_wrap"), mod_numeric_answer_ui(ns("answer_num")))
  )


  div(
    class = "practice-wrap",
    # ── Progress bar ──────────────────────────────────────────────────────────
    uiOutput(ns("progress_bar")),
    # ── Item card ─────────────────────────────────────────────────────────────
    # From 992 px two columns, stimulus left (top-aligned), answer options
    # right (vertically centred). Both are sticky, so the shorter one stays in
    # view while the page scrolls through the longer one. Narrower screens
    # stack both, as before.
    bslib::card(
      class = "practice-card",
      bslib::card_body(
        fillable = FALSE,
        div(
          class = "practice-layout",
          # Stimulus — only re-renders when item changes, NOT on check
          div(
            class = "practice-stimulus-col",
            shiny::withMathJax(uiOutput(ns("item_stimulus")))
          ),
          # Answers — both answer-mode child modules are mounted statically
          # (never remounted per item); only their visibility is toggled. See
          # mod_numeric_answer.R for why the numeric module's input field
          # specifically must stay a stable DOM node.
          div(
            class = "practice-answers-section",
            # The sticky part; the section around it keeps the grey
            # background down to the end of the card
            div(
              class = "practice-answers-inner",
              shiny::withMathJax(
                div(mc_ui, num_ui)
              )
            )
          )
        )
      )
    ),
    # ── Feedback after the check, full width below the card ───────────────────
    # (MC and numeric alike). Below the card rather than in a column: it reads
    # in fewer lines and sits in the same place for every item.
    div(
      class = "practice-feedback",
      shiny::withMathJax(uiOutput(ns("feedback")))
    ),
    # ── Buttons: leave the queue left, the item's actions right ───────────────
    # Outside the card; from 992 px it sticks to the bottom of the screen, so
    # the buttons are reachable however long the stimulus or the options are.
    div(
      class = "practice-actions-bar",
      actionButton(
        ns("back"),
        div(bsicons::bs_icon("arrow-left"), tags$span(id = ns("back_label"), "Neue Auswahl")),
        class = "btn btn-outline-secondary"
      ),
      div(
        class = "practice-actions",
        # Skipping is an app action for every item, not an answer option
        # stored in the item DB. Shown/hidden together with "check".
        actionButton(
          ns("skip"),
          div(bsicons::bs_icon("skip-forward"), "Aufgabe überspringen"),
          class = "btn btn-outline-secondary"
        ),
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
            class = "btn btn-next"
          )
        )
      )
    )
  )
}

# The feedback card after the check, for MC and numeric items: `item` is the
# current single-row item, `res` the check result (state$result). NULL if
# there is nothing to show (an MC option without feedback text). Lives here,
# not in the answer modules, so the layout can place it on its own (full width
# below the item card).
practice_feedback_card <- function(item, res) {
  if (is.null(res) || nrow(item) != 1L) {
    return(NULL)
  }
  it <- as.list(item[1, ])
  cat <- res$category
  idx <- res$answer_idx
  numeric <- identical(it$answer_mode, "num")
  body <- if (cat == "skip") {
    SKIP_FEEDBACK
  } else if (cat == "unmatched") {
    "Wir können anhand deiner Eingabe leider nicht nachvollziehen, wie du auf diese Zahl gekommen bist. Schau dir die Aufgabe noch einmal in Ruhe an oder sprich sie ggf. mit einer Betreuungsperson durch."
  } else if (numeric) {
    # answer_idx is the option's column position (see get_numeric_options())
    opts <- get_numeric_options(it)
    fb <- if (!is.na(idx) && idx %in% opts$idx) opts$feedback[opts$idx == idx] else NA
    if (is.na(fb)) "" else fb
  } else {
    feedbacks <- get_feedbackoptions(it)
    if (!is.na(idx) && idx <= length(feedbacks)) feedbacks[[idx]] else ""
  }
  # MC: no card without text (as before); numeric: always the header
  if (!numeric && !nzchar(body)) {
    return(NULL)
  }
  color_var <- switch(
    cat,
    correct = "var(--tiger-correct)",
    incorrect = "var(--tiger-incorrect)",
    "var(--tiger-skip)"
  )
  icon_name <- switch(
    cat,
    correct = "check-circle-fill",
    incorrect = "x-circle-fill",
    unmatched = "question-circle-fill",
    "skip-forward-fill"
  )
  title_str <- switch(
    cat,
    correct = "Richtig!",
    incorrect = "Leider falsch",
    unmatched = "Antwort nicht erkannt",
    "Übersprungen"
  )
  div(
    class = "feedback-card",
    style = sprintf("border-left: 4px solid %s;", color_var),
    div(
      class = "feedback-header",
      style = sprintf("color: %s;", color_var),
      bsicons::bs_icon(icon_name),
      tags$b(title_str)
    ),
    if (nzchar(body)) {
      div(class = "feedback-body mt-1", shiny::HTML(render_md(body)))
    }
  )
}

mod_practice_server <- function(
  id,
  data_item,
  practice_ids,
  credentials,
  write_trigger,
  # TRUE while the queue is a retry from the dashboard ("Offene Fehler"):
  # answers are stored with practice_mode = "retry", and leaving the queue
  # leads back to the dashboard (server.R), so the back label says so
  retry_active = reactive(FALSE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      shinyjs::html("back_label", if (isTRUE(retry_active())) "Zurück zum Fortschritt" else "Neue Auswahl")
    })

    # Internal state — position in the queue + check state.
    # `result` (set once state$checked is TRUE) is a list with `category`
    # ("correct"/"incorrect"/"skip"/"unmatched" — the last only for numeric
    # items), `answer_idx` (matched distractor index, or NA), and `typed_value`
    # (numeric input only, NA otherwise) — this is what the answer child
    # modules read to color their post-check view, and what the feedback card
    # (practice_feedback_card()) is built from.
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

    # ── Feedback after the check (placed by the layout, see the UI) ──────────
    output$feedback <- renderUI({
      req(isTRUE(state$checked))
      card <- practice_feedback_card(current_item(), state$result)
      req(card)
      session$onFlushed(
        function() session$sendCustomMessage("mathjax_typeset", TRUE),
        once = TRUE
      )
      card
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
        shinyjs::show("skip")
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
        shinyjs::show("skip")
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
      shinyjs::hide("skip")
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
        skipped = category == "skip",
        practice_mode = if (isTRUE(retry_active())) "retry" else "standard"
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

    # ── Skip (MC and numeric items) ───────────────────────────────────────────
    # No ignoreInit needed: the button is never remounted (see train_view in
    # server.R), and its initial value 0 is already ignored by ignoreNULL.
    observeEvent(input$skip, {
      req(nrow(current_item()) == 1L, !isTRUE(state$checked))
      finish_check("skip")
    })

    # ── Next item ─────────────────────────────────────────────────────────────
    observeEvent(input$next_item, {
      req(input$next_item)
      ids <- practice_ids()
      if (state$pos < length(ids)) {
        state$pos <- state$pos + 1L
      } else {
        # Finished all items — return to selector (or to the dashboard, if
        # the queue was a retry from there: see retry_active in server.R)
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
