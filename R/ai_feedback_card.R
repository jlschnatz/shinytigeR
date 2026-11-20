# The "Persönliches Feedback" card on the dashboard (top right). A pure
# function of a `view` (built in mod_dashboard_server()), so it can be tested
# without a session:
#
#   state     ai_feedback_status()$state: "ready", or why a new one isn't possible
#   notice    ai_feedback_status()$notice (German, NULL when ready)
#   running   TRUE while a feedback is being created
#   feedback  ai_latest_feedback() or NULL
#   cfg       ai_config() (for the model's label)
#   flash     list(type = "warning"/"info", text) from the last click, or NULL
#
# The model's text is shown as plain text (escaped), never as Markdown/HTML.
# The card says it's AI-generated, and by which model (legally required, and
# students should know when a fallback model wrote it).

ai_feedback_card <- function(view, ns = identity) {
  off <- identical(view$state, "off")
  fb <- if (off) NULL else view$feedback
  running <- isTRUE(view$running)

  body <- if (off) {
    tags$p(class = "text-muted mb-0 ai-fb-note", view$notice)
  } else if (is.null(fb) && running) {
    tagList(
      tags$p(class = "text-muted ai-fb-note", "Dein Feedback wird erstellt. Das dauert meist nur einige Sekunden."),
      ai_feedback_skeleton()
    )
  } else if (is.null(fb)) {
    tagList(
      tags$p(
        class = "ai-fb-note",
        "Lass dir eine kurze Rückmeldung zu deinem Lernstand schreiben: was schon ",
        "gut läuft, wo du ansetzen kannst, und zu jedem deiner schwächeren Themen ",
        "ein passendes Lernmaterial."
      ),
      tags$p(
        class = "text-muted ai-fb-note",
        "Geschrieben wird sie von einer KI. Sie bekommt dafür nur deine Einstufung ",
        "aus „Dein Lernstand“, ohne deinen Namen und ohne deine Antworten."
      )
    )
  } else {
    ai_feedback_body(fb, ns)
  }

  bslib::card(
    height = "100%",
    class = if (running) "ai-feedback-card is-running" else "ai-feedback-card",
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center gap-2",
      div(
        class = "d-flex align-items-center gap-2",
        bsicons::bs_icon("stars"),
        tags$b("Persönliches Feedback")
      ),
      tags$span(class = "ai-feedback-badge", "KI-generiert")
    ),
    bslib::card_body(
      body,
      if (!is.null(view$flash)) {
        div(
          class = paste0("ai-fb-flash is-", view$flash$type),
          role = "status",
          view$flash$text
        )
      }
    ),
    if (!off) {
      bslib::card_footer(
        class = "ai-fb-footer",
        if (!is.null(fb)) ai_feedback_meta(fb, view$cfg),
        ai_feedback_action(view, has_feedback = !is.null(fb), ns = ns)
      )
    }
  )
}

ai_feedback_skeleton <- function() {
  div(
    class = "ai-feedback-skeleton",
    `aria-hidden` = "true",
    lapply(c(92, 78, 85, 60), function(w) {
      div(class = "ai-skel-line", style = sprintf("width:%d%%;", w))
    })
  )
}

# Summary and the recommendations (topic, linked title, the model's tip).
# A click on a title is logged (link_click) via input$ai_link.
ai_feedback_body <- function(fb, ns = identity) {
  rec <- fb$recommendations
  div(
    class = "ai-fb",
    tags$p(class = "ai-fb-summary", fb$summary),
    if (nrow(rec) > 0L) {
      tags$ol(
        class = "ai-fb-recs",
        lapply(seq_len(nrow(rec)), function(i) {
          r <- rec[i, ]
          title <- if (is.na(r$title) || is.na(r$link)) {
            tags$span(class = "ai-fb-gone", "Dieses Material ist nicht mehr verfügbar.")
          } else {
            tags$a(
              class = "ai-fb-title", href = r$link, target = "_blank", rel = "noopener",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {id: %d, pos: %d, t: Date.now()}, {priority: 'event'})",
                ns("ai_link"), as.integer(fb$id_feedback), as.integer(r$position)
              ),
              HTML(paste0(
                htmltools::htmlEscape(r$title),
                as.character(bsicons::bs_icon("box-arrow-up-right", class = "ai-fb-ext"))
              ))
            )
          }
          tags$li(
            class = "ai-fb-rec",
            div(class = "ai-fb-topic", r$topic),
            title,
            tags$p(class = "ai-fb-text", r$text)
          )
        })
      )
    }
  )
}

# "Von einer KI erstellt mit Gemma 4 (31B), 20. Nov 2026, 14:03". Says so
# when a fallback model wrote it.
ai_feedback_meta <- function(fb, cfg) {
  t <- as.POSIXct(fb$created_at, origin = "1970-01-01")
  when <- paste0(format_date_de(local_date(t)), ", ", format(t, "%H:%M"))
  model <- if (is.null(cfg)) fb$model else ai_model_label(fb$model, cfg)
  tags$p(
    class = "ai-fb-meta",
    sprintf("Von einer KI erstellt mit %s, %s.", model, when),
    if (isTRUE(as.logical(fb$used_fallback))) {
      " Das Standardmodell war nicht erreichbar, deshalb hat ein Ersatzmodell geschrieben."
    },
    " KI kann Fehler machen."
  )
}

# The button, or why there's no new feedback yet
ai_feedback_action <- function(view, has_feedback, ns = identity) {
  if (isTRUE(view$running)) {
    return(tags$button(
      type = "button", class = "btn btn-sm btn-primary ai-fb-btn", disabled = NA,
      tags$span(class = "spinner-border spinner-border-sm", `aria-hidden` = "true"),
      "Wird erstellt …"
    ))
  }
  if (!identical(view$state, "ready")) {
    return(tags$p(class = "ai-fb-wait", view$notice))
  }
  tags$button(
    type = "button", class = "btn btn-sm btn-primary ai-fb-btn",
    onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority: 'event'})", ns("ai_create")),
    bsicons::bs_icon("stars"),
    if (has_feedback) "Neues Feedback erstellen" else "Feedback erstellen"
  )
}
