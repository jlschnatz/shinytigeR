mod_inspect_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "practice-wrap",
    div(
      class = "text-muted small mb-1",
      uiOutput(ns("id_badge"))
    ),
    bslib::card(
      bslib::card_body(
        shiny::withMathJax(uiOutput(ns("item_stimulus"))),
        div(
          class = "practice-answers-section",
          shiny::withMathJax(uiOutput(ns("item_answers")))
        )
      )
    ),
    div(
      class = "d-flex gap-2 justify-content-between mt-3",
      actionButton(
        ns("back"),
        div(bsicons::bs_icon("arrow-left"), "Zurück zur Auswahl"),
        class = "btn btn-outline-secondary"
      ),
      uiOutput(ns("reveal_btn"))
    )
  )
}

mod_inspect_server <- function(id, data_item, inspect_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    revealed <- reactiveVal(FALSE)

    observeEvent(
      inspect_id(),
      revealed(FALSE),
      ignoreNULL = TRUE
    )

    current_item <- reactive({
      id_val <- inspect_id()
      req(id_val)
      data_item[data_item$id_item == id_val, , drop = FALSE]
    })

    output$id_badge <- renderUI({
      item <- current_item()
      req(nrow(item) == 1L)
      sprintf("Aufgabenübersicht — ID %d", item$id_item[1])
    })

    # ── Stimulus — only invalidates when the item changes ──────────────────────
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

    output$reveal_btn <- renderUI({
      if (revealed()) {
        return(NULL)
      }
      actionButton(
        ns("reveal"),
        div(bsicons::bs_icon("eye"), "Antwort & Feedback anzeigen"),
        class = "btn btn-primary"
      )
    })

    observeEvent(input$reveal, revealed(TRUE))

    # ── Answer options — every option shown; correct one + all feedback ────────
    # revealed on request. Nothing here is ever written to db_user.sqlite: this
    # is inspection, not an attempt.
    output$item_answers <- renderUI({
      item <- current_item()
      req(nrow(item) == 1L)
      item <- as.list(item[1, ])
      choices <- get_answeroptions(item)
      feedbacks <- get_feedbackoptions(item)
      correct_idx <- as.integer(item$answer_correct)
      show_answers <- revealed()

      rows <- lapply(seq_along(choices), function(i) {
        is_correct <- show_answers && i == correct_idx
        suffix <- if (item$type_answer == "image") "img" else "txt"
        hi_class <- if (is_correct) paste0("correct_answer_", suffix) else ""
        opt <- if (item$type_answer == "image" && is_img_path(choices[i])) {
          div(
            class = paste(
              "d-flex align-items-center mb-2 answer-option is-static",
              hi_class
            ),
            tags$img(src = choices[i], class = "img-fluid answer-img")
          )
        } else {
          div(
            class = paste("answer-option is-static mb-2", hi_class),
            div(
              class = "answer-label",
              shiny::HTML(render_md(choices[i]))
            )
          )
        }
        fb_text <- if (show_answers && i <= length(feedbacks)) {
          feedbacks[[i]]
        } else {
          ""
        }
        tagList(
          opt,
          if (show_answers && nzchar(fb_text)) {
            div(
              class = "feedback-card mt-1 mb-3",
              style = sprintf(
                "border-left: 4px solid %s;",
                if (is_correct) "var(--tiger-correct)" else "var(--tiger-skip)"
              ),
              div(class = "feedback-body", shiny::HTML(render_md(fb_text)))
            )
          }
        )
      })

      session$sendCustomMessage("mathjax_typeset", TRUE)
      div(class = "answer-options", rows)
    })

    observeEvent(input$back, inspect_id(NULL))
  })
}
