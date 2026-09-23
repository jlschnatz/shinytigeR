# Numeric answer widget — same contract as mod_mc_answer.R (see there for the
# full contract description). A student types a number instead of picking a
# radio option; matching against the item's options (per-option value ranges,
# see evaluate_numeric_answer() in utils.R) happens in the caller, not here.
#
# Differences from mod_mc_answer.R:
#   - The input is a plain text field (not type="number") so both "3.5" and
#     "3,5" are typeable; parsing/locale normalization is parse_numeric_input().
#   - There is a dedicated "Überspringen" button, reported via skip_requested,
#     since there's no last-radio-option slot to select instead. Its UI is a
#     separate function, mod_numeric_answer_skip_ui(), which the parent places
#     in its action-button row next to "Antwort prüfen".
#   - `result()$category` can additionally be "unmatched" — the typed number
#     didn't land in any option's range — which renders a fixed
#     fallback message rather than any if_answeroption_0X feedback text.
#   - Unlike mod_mc_answer's radio group, the input field and skip button are
#     STATIC UI (not inside a renderUI keyed on the current item): a freshly
#     recreated actionButton resends its client-side reset value (0) on the
#     next real render, which Shiny then treats as a genuine click if the
#     server's last remembered value was nonzero — the same quirk documented
#     on item_id_badge() in utils.R. Keeping the button's DOM node stable
#     across item changes (only toggling visibility, and clearing the text
#     field's value) avoids that. Only the post-check feedback panel — which
#     has no actionButton in it — re-renders via output$feedback_ui.
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

# The skip button lives in mod_practice.R's action-button row (left of
# "Antwort prüfen"), not inside the answer card — so it's a separate UI
# function the parent places itself, called with the same namespaced id as
# mod_numeric_answer_ui(). Still owned by this module: input$skip is read
# here, and its visibility is toggled by mod_numeric_answer_server(). Same
# static-DOM-node rule as the input field applies (see comment above).
mod_numeric_answer_skip_ui <- function(id) {
  ns <- NS(id)
  actionButton(
    ns("skip"),
    div(bsicons::bs_icon("skip-forward"), "Aufgabe überspringen"),
    class = "btn btn-outline-secondary"
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

    # The skip button sits outside num_wrap (in the parent's action row), so
    # it isn't hidden along with it for MC items — toggle it explicitly.
    # Deliberately no req() on answer_mode here, unlike the observers above:
    # the num -> MC transition must hide it too.
    observe({
      it <- item()
      shinyjs::toggle(
        "skip",
        condition = nrow(it) == 1L &&
          identical(it$answer_mode[1], "num") &&
          !isTRUE(checked())
      )
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
        "Aufgabe übersprungen."
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
      ready = reactive(!is.na(parse_numeric_input(input$num_value))),
      skip_requested = reactive(input$skip)
    )
  })
}
