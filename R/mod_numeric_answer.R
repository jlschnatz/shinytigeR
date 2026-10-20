# Numeric answer widget — same contract as mod_mc_answer.R (see there for the
# full contract description). A student types a number instead of picking a
# radio option; matching against the item's options (per-option value ranges,
# see evaluate_numeric_answer() in utils.R) happens in the caller, not here.
#
# Differences from mod_mc_answer.R:
#   - The input is a plain text field (not type="number") so both "3.5" and
#     "3,5" are typeable; parsing/locale normalization is parse_numeric_input().
#   - `result()$category` can additionally be "unmatched" — the typed number
#     didn't land in any option's range — which renders a fixed
#     fallback message rather than any if_answeroption_0X feedback text.
#   - Unlike mod_mc_answer's radio group, the input field is STATIC UI (not
#     inside a renderUI keyed on the current item): it is cleared, re-enabled
#     and un-colored per item instead of remounted, so the typed value can stay
#     visible (disabled) after check. Only the post-check feedback panel
#     re-renders via output$feedback_ui.
#   - Skipping is handled by the parent's skip button (mod_practice.R), as for
#     MC items; this module only renders the "skip" result.
mod_numeric_answer_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "numeric-answer-wrap",
    div(
      id = ns("input_row"),
      class = "numeric-answer-card",
      div(class = "numeric-answer-label", "Deine Antwort"),
      tags$input(
        type = "text",
        inputmode = "decimal",
        class = "form-control numeric-answer-input",
        id = ns("num_value"),
        placeholder = "",
        autocomplete = "off"
      ),
      div(class = "numeric-answer-hint", "Punkt oder Komma als Dezimaltrennzeichen")
    ),
    uiOutput(ns("feedback_ui"))
  )
}

mod_numeric_answer_server <- function(id, item, checked, result) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    result_classes <- paste(
      "numeric-result-correct",
      "numeric-result-incorrect",
      "numeric-result-skip"
    )

    # Fresh (unchecked) numeric item — clear the field and reset it to its
    # editable, uncolored state. Runs whenever the active item changes,
    # including the first time a numeric item becomes current.
    observeEvent(item(), {
      it <- item()
      req(nrow(it) == 1L, identical(it$answer_mode[1], "num"))
      shiny::updateTextInput(session, "num_value", value = "")
      shinyjs::enable("num_value")
      shinyjs::removeClass("num_value", result_classes)
    })

    # Post-check, the input row stays visible (like MC's radios) with the
    # typed value locked in and its border colored by result; the feedback
    # card renders below it via output$feedback_ui.
    observeEvent(checked(), {
      it <- item()
      req(nrow(it) == 1L, identical(it$answer_mode[1], "num"))
      shinyjs::removeClass("num_value", result_classes)
      if (isTRUE(checked())) {
        shinyjs::disable("num_value")
        res <- result()
        if (!is.null(res)) {
          suffix <- if (res$category %in% c("unmatched", "skip")) "skip" else res$category
          shinyjs::addClass("num_value", paste0("numeric-result-", suffix))
        }
      } else {
        shinyjs::enable("num_value")
      }
    })

    output$feedback_ui <- renderUI({
      it <- item()
      req(nrow(it) == 1L, identical(it$answer_mode[1], "num"), isTRUE(checked()))
      res <- result()
      req(!is.null(res))
      it <- as.list(it[1, ])
      # answer_idx is the option's column position (see get_numeric_options())
      opts <- get_numeric_options(it)
      idx <- res$answer_idx
      fb_text <- if (
        res$category %in% c("correct", "incorrect") && !is.na(idx) && idx %in% opts$idx
      ) {
        fb <- opts$feedback[opts$idx == idx]
        if (is.na(fb)) "" else fb
      } else {
        ""
      }
      color_var <- switch(
        res$category,
        correct = "var(--tiger-correct)",
        incorrect = "var(--tiger-incorrect)",
        unmatched = "var(--tiger-skip)",
        skip = "var(--tiger-skip)"
      )
      icon_name <- switch(
        res$category,
        correct = "check-circle-fill",
        incorrect = "x-circle-fill",
        unmatched = "question-circle-fill",
        skip = "skip-forward-fill"
      )
      title_str <- switch(
        res$category,
        correct = "Richtig!",
        incorrect = "Leider falsch",
        unmatched = "Antwort nicht erkannt",
        skip = "Übersprungen"
      )
      body_text <- if (res$category == "unmatched") {
        "Wir können anhand deiner Eingabe leider nicht nachvollziehen, wie du auf diese Zahl gekommen bist. Schau dir die Aufgabe noch einmal in Ruhe an oder sprich sie ggf. mit einer Betreuungsperson durch."
      } else if (res$category == "skip") {
        SKIP_FEEDBACK
      } else {
        fb_text
      }

      session$onFlushed(
        function() session$sendCustomMessage("mathjax_typeset", TRUE),
        once = TRUE
      )

      div(
        class = "feedback-card mt-4",
        style = sprintf("border-left: 4px solid %s;", color_var),
        div(
          class = "feedback-header",
          style = sprintf("color: %s;", color_var),
          bsicons::bs_icon(icon_name),
          tags$b(title_str)
        ),
        if (nzchar(body_text)) {
          div(class = "feedback-body mt-1", shiny::HTML(render_md(body_text)))
        }
      )
    })

    list(
      raw_answer = reactive(parse_numeric_input(input$num_value)),
      ready = reactive(!is.na(parse_numeric_input(input$num_value)))
    )
  })
}
