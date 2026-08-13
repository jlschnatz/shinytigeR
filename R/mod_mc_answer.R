# Multiple-choice answer widget — used by mod_practice.R (and, in future,
# anywhere else an MC item needs to be answered). Pure UI/input component: it
# renders the radio options and reports the raw selection back to the caller;
# it does not evaluate the answer or write to the database itself.
#
# Contract (see also mod_numeric_answer.R, which implements the same shape):
#   mod_mc_answer_server(id, item, checked, result) returns a list with
#     - raw_answer:     reactive() -> integer index of the selected radio, or
#                        NA_integer_ if nothing is selected yet
#     - ready:          reactive() -> TRUE once an option is selected (gates
#                        the caller's "check" button)
#     - skip_requested: reactive() -> a click-counter reactive for a dedicated
#                        skip action. MC has no such action (skipping is just
#                        selecting the last radio option), so this is a
#                        reactive that never changes.
#
#   `item` is a reactive single-row item data.frame, `checked` a reactive
#   logical (TRUE after the caller has evaluated the answer), `result` a
#   reactive list with `category` ("correct"/"incorrect"/"skip") and
#   `answer_idx` (the evaluated index) — populated by the caller once checked.
mod_mc_answer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("answer_ui"))
}

mod_mc_answer_server <- function(id, item, checked, result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$answer_ui <- renderUI({
      it <- item()
      req(nrow(it) == 1L, identical(it$answer_mode[1], "mc"))
      it <- as.list(it[1, ])
      choices <- get_answeroptions(it)

      if (!isTRUE(checked())) {
        radio_opts <- lapply(seq_along(choices), function(i) {
          id_i <- paste0(ns("answer"), "_", i)
          if (it$type_answer == "image" && is_img_path(choices[i])) {
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
        div(
          class = "answer-options",
          tagAppendChildren(
            div(id = ns("answer"), class = "shiny-input-radiogroup"),
            .list = radio_opts
          )
        )
      } else {
        res <- result()
        req(!is.null(res))
        feedbacks <- get_feedbackoptions(it)
        sel <- res$answer_idx
        fb_text <- if (!is.na(sel) && sel <= length(feedbacks)) feedbacks[[sel]] else ""
        suffix <- if (it$type_answer == "image") "img" else "txt"
        hi_class <- paste0(res$category, "_answer_", suffix)
        color_var <- switch(
          res$category,
          correct = "var(--tiger-correct)",
          incorrect = "var(--tiger-incorrect)",
          skip = "var(--tiger-skip)"
        )
        icon_name <- switch(
          res$category,
          correct = "check-circle-fill",
          incorrect = "x-circle-fill",
          skip = "skip-forward-fill"
        )
        title_str <- switch(
          res$category,
          correct = "Richtig!",
          incorrect = "Leider falsch",
          skip = "Übersprungen"
        )

        div(
          class = "answer-options",
          tagList(
            lapply(seq_along(choices), function(i) {
              is_sel <- !is.na(sel) && i == sel
              id_i <- paste0(ns("answer"), "_", i)
              radio_cls <- paste(
                "form-check-input",
                if (is_sel) paste0("radio-result-", res$category) else ""
              )
              if (it$type_answer == "image" && is_img_path(choices[i])) {
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
        )
      }
    })

    list(
      raw_answer = reactive({
        idx <- suppressWarnings(as.integer(input$answer))
        if (length(idx) == 0L || is.na(idx)) NA_integer_ else idx
      }),
      ready = reactive(!is.null(input$answer)),
      skip_requested = reactive(0L)
    )
  })
}
