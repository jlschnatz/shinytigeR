mod_practice_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "practice-wrap",
    # ── Progress bar ──────────────────────────────────────────────────────────
    uiOutput(ns("progress_bar")),
    # ── Item card ─────────────────────────────────────────────────────────────
    bslib::card(
      bslib::card_body(
        # Stimulus — only re-renders when item changes, NOT on check
        shiny::withMathJax(uiOutput(ns("item_stimulus"))),
        # Answers + feedback — re-renders on check; MathJax re-typesets only here
        div(
          class = "practice-answers-section",
          shiny::withMathJax(uiOutput(ns("item_answers")))
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

    # Internal state — position in the queue + check state
    state <- reactiveValues(
      pos = 1L,
      checked = FALSE,
      answer_id = NULL
    )

    # Reset to position 1 whenever a new queue is set
    observeEvent(
      practice_ids(),
      {
        state$pos <- 1L
        state$checked <- FALSE
        state$answer_id <- NULL
      },
      ignoreNULL = TRUE
    )

    current_item <- reactive({
      ids <- practice_ids()
      req(length(ids) > 0L, state$pos >= 1L, state$pos <= length(ids))
      data_item[data_item$id_item == ids[state$pos], , drop = FALSE]
    })

    # ── Progress bar ──────────────────────────────────────────────────────────
    output$progress_bar <- renderUI({
      ids <- practice_ids()
      req(length(ids) > 0L)
      pct <- round(100 * (state$pos - 1L) / length(ids))
      div(
        div(
          class = "d-flex justify-content-between text-muted small mb-1",
          span(sprintf("Aufgabe %d von %d", state$pos, length(ids))),
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
      session$sendCustomMessage("mathjax_typeset", TRUE)
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

    # ── Answers + feedback — re-renders on check; stimulus is untouched ───────
    output$item_answers <- renderUI({
      item <- current_item()
      req(nrow(item) == 1L)
      item <- as.list(item[1, ])
      choices <- get_answeroptions(item)

      answers_ui <- if (!state$checked) {
        radio_opts <- lapply(seq_along(choices), function(i) {
          id_i <- paste0(ns("answer"), "_", i)
          if (item$type_answer == "image" && is_img_path(choices[i])) {
            div(
              class = "d-flex align-items-center mb-3",
              tags$input(
                type = "radio",
                class = "form-check-input me-2",
                name = ns("answer"),
                id = id_i,
                value = i,
                style = "width:1.2em;height:1.2em;cursor:pointer;"
              ),
              tags$label(
                `for` = id_i,
                class = "label-answer",
                tags$img(src = choices[i], class = "img-fluid answer-img")
              )
            )
          } else {
            div(
              class = "answer-option mb-2",
              tags$input(
                type = "radio",
                class = "form-check-input",
                name = ns("answer"),
                id = id_i,
                value = i,
                style = "width:1.1em;height:1.1em;cursor:pointer;"
              ),
              tags$label(
                `for` = id_i,
                class = "ms-2 answer-label",
                style = "cursor:pointer;",
                shiny::HTML(render_md(choices[i]))
              )
            )
          }
        })
        tagAppendChildren(
          div(id = ns("answer"), class = "shiny-input-radiogroup"),
          .list = radio_opts
        )
      } else {
        result <- evaluate_answer(item, state$answer_id)
        feedbacks <- get_feedbackoptions(item)
        fb_text <- if (state$answer_id <= length(feedbacks)) {
          feedbacks[[state$answer_id]]
        } else {
          ""
        }
        suffix <- if (item$type_answer == "image") "img" else "txt"
        hi_class <- paste0(result, "_answer_", suffix)
        color_var <- switch(
          result,
          correct = "var(--tiger-correct)",
          incorrect = "var(--tiger-incorrect)",
          skip = "var(--tiger-skip)"
        )
        icon_name <- switch(
          result,
          correct = "check-circle-fill",
          incorrect = "x-circle-fill",
          skip = "skip-forward-fill"
        )
        title_str <- switch(
          result,
          correct = "Richtig!",
          incorrect = "Leider falsch",
          skip = "Übersprungen"
        )

        tagList(
          lapply(seq_along(choices), function(i) {
            is_sel <- i == state$answer_id
            id_i <- paste0(ns("answer"), "_", i)
            radio_cls <- paste(
              "form-check-input",
              if (is_sel) paste0("radio-result-", result) else ""
            )
            if (item$type_answer == "image" && is_img_path(choices[i])) {
              div(
                class = paste(
                  "d-flex align-items-center mb-3",
                  if (is_sel) hi_class else ""
                ),
                tags$input(
                  type = "radio",
                  class = paste(radio_cls, "me-2"),
                  name = paste0(ns("answer"), "_done"),
                  id = id_i,
                  disabled = NA,
                  checked = if (is_sel) NA else NULL,
                  style = "width:1.2em;height:1.2em;"
                ),
                tags$label(
                  `for` = id_i,
                  class = "label-answer",
                  tags$img(
                    src = choices[i],
                    class = "img-fluid answer-img",
                    style = if (!is_sel) "opacity:0.65;" else NULL
                  )
                )
              )
            } else {
              div(
                class = paste(
                  "answer-option mb-2",
                  if (is_sel) hi_class else ""
                ),
                tags$input(
                  type = "radio",
                  class = radio_cls,
                  name = paste0(ns("answer"), "_done"),
                  id = id_i,
                  disabled = NA,
                  checked = if (is_sel) NA else NULL,
                  style = "width:1.1em;height:1.1em;"
                ),
                tags$label(
                  `for` = id_i,
                  class = "ms-2 answer-label",
                  style = if (!is_sel) "opacity:0.65;" else NULL,
                  shiny::HTML(render_md(choices[i]))
                )
              )
            }
          }),
          if (nzchar(fb_text)) {
            div(
              class = "feedback-card mt-4",
              style = sprintf("border-left: 4px solid %s;", color_var),
              div(
                class = "feedback-header",
                style = sprintf("color: %s;", color_var),
                bsicons::bs_icon(icon_name),
                tags$b(title_str)
              ),
              div(class = "feedback-body mt-1", shiny::HTML(render_md(fb_text)))
            )
          }
        )
      }

      session$sendCustomMessage("mathjax_typeset", TRUE)
      div(class = "answer-options", answers_ui)
    })

    # ── Answer selection — enable check button ─────────────────────────────────
    observeEvent(
      input$answer,
      {
        shinyjs::enable("check")
      },
      ignoreNULL = TRUE
    )

    # ── Reset button state when moving to a new item ──────────────────────────
    observeEvent(
      state$pos,
      {
        state$checked <- FALSE
        state$answer_id <- NULL
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
        state$answer_id <- NULL
        shinyjs::disable("check")
        shinyjs::hide("next_item")
        shinyjs::show("check")
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # ── Check answer ──────────────────────────────────────────────────────────
    observeEvent(input$check, {
      req(input$check, input$answer)
      ans_idx <- as.integer(input$answer)
      state$answer_id <- ans_idx
      state$checked <- TRUE

      shinyjs::hide("check")
      shinyjs::show("next_item")

      # Write to DB
      item <- as.list(current_item()[1, ])
      uid <- credentials()$info$user_name
      token <- session$token
      tryCatch(
        {
          db_write_response(uid, build_response_row(item, ans_idx, uid, token))
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
    })

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
