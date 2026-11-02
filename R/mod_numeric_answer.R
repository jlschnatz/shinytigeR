# Numeric answer widget — same contract as mod_mc_answer.R (see there for the
# full contract description). A student types a number instead of picking a
# radio option; matching against the item's options (per-option value ranges,
# see evaluate_numeric_answer() in utils.R) happens in the caller, not here.
#
# Differences from mod_mc_answer.R:
#   - The input is a plain text field (not type="number") so both "3.5" and
#     "3,5" are typeable; parsing/locale normalization is parse_numeric_input().
#   - `result()$category` can additionally be "unmatched" — the typed number
#     didn't land in any option's range (the caller's feedback card shows a
#     fixed fallback message for it).
#   - Unlike mod_mc_answer's radio group, the input field is STATIC UI (not
#     inside a renderUI keyed on the current item): it is cleared, re-enabled
#     and un-colored per item instead of remounted, so the typed value can stay
#     visible (disabled) after check. Nothing here re-renders; the feedback
#     card is the caller's (practice_feedback_card() in mod_practice.R).
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
    )
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
    # typed value locked in and its border colored by result.
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

    list(
      raw_answer = reactive(parse_numeric_input(input$num_value)),
      ready = reactive(!is.na(parse_numeric_input(input$num_value)))
    )
  })
}
