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
#
#   `item` is a reactive single-row item data.frame, `checked` a reactive
#   logical (TRUE after the caller has evaluated the answer), `result` a
#   reactive list with `category` ("correct"/"incorrect"/"skip") and
#   `answer_idx` (the evaluated index; NA for "skip") — populated by the caller
#   once checked. Skipping is the caller's skip button (mod_practice.R), not an
#   answer option, so after a skip no option is highlighted.
#
#   The options render once per item, never on the check: the result is
#   marked on them in the browser ("mc_mark_result", www/js/practice.js:
#   colour the selected option, disable all, dim the others), so they don't
#   flicker on every check and skip.
#
#   After the check this module only colors the selected option; the feedback
#   card is the caller's (practice_feedback_card() in mod_practice.R), so it can
#   sit in its own place in the layout (full width below the item card).
mod_mc_answer_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("answer_ui"), class = "reveal-output")
}

mod_mc_answer_server <- function(id, item, checked, result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The options, once per item (depends on the item only, not on the check)
    output$answer_ui <- renderUI({
      it <- item()
      req(nrow(it) == 1L, identical(it$answer_mode[1], "mc"))
      it <- as.list(it[1, ])
      choices <- get_answeroptions(it)

      radio_opts <- lapply(seq_along(choices), function(i) {
        id_i <- paste0(ns("answer"), "_", i)
        if (it$type_answer == "image" && is_img_path(choices[i])) {
          div(
            class = "answer-img-row d-flex align-items-center mb-3",
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
              answer_img(choices[i])
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
      # revealed in the browser (www/js/practice.js, .reveal-output); the
      # key tells it a new item arrived, so it fades in
      div(
        id = ns("options"),
        class = "answer-options",
        `data-reveal-key` = it$id_item,
        tagAppendChildren(
          div(id = ns("answer"), class = "shiny-input-radiogroup"),
          .list = radio_opts
        )
      )
    })

    # After the check: mark the result on the options already shown. Sent
    # after the flush, so it reaches the browser after any re-render.
    observeEvent(list(checked(), result()), {
      req(isTRUE(checked()))
      it <- item()
      res <- result()
      req(nrow(it) == 1L, identical(it$answer_mode[1], "mc"), !is.null(res))
      suffix <- if (identical(it$type_answer[1], "image")) "img" else "txt"
      msg <- list(
        id = ns("options"),
        sel = if (is.na(res$answer_idx)) NULL else as.integer(res$answer_idx),
        row_class = paste0(res$category, "_answer_", suffix),
        radio_class = paste0("radio-result-", res$category)
      )
      session$onFlushed(
        function() session$sendCustomMessage("mc_mark_result", msg),
        once = TRUE
      )
    })

    list(
      raw_answer = reactive({
        idx <- suppressWarnings(as.integer(input$answer))
        if (length(idx) == 0L || is.na(idx)) NA_integer_ else idx
      }),
      ready = reactive(!is.null(input$answer))
    )
  })
}
