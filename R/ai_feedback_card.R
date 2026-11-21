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
#   rating    ai_get_rating() for the shown feedback, or NULL
#   rate_open TRUE while the rating form is open (after a thumb or "Ändern")
#   rate_locked TRUE if the rating was given in an earlier session: it's final
#             then (the chosen thumb shown, nothing clickable)
#   reported  positions of the recommendations reported (ai_reported_positions())
#
# The rating's buttons carry data-ai-action (ai_rate, ai_rate_edit,
# ai_report); its form saves itself (www/js/dashboard.js): each tick at once
# (ai_rate_reasons), the comment shortly after typing stops and on leaving
# the field (ai_rate_comment). Those saves don't re-render the card.
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
    tagList(
      ai_feedback_body(fb, ns, reported = if (running) NULL else view$reported, report = !running),
      if (!running) ai_rating_block(fb$id_feedback, view$rating, isTRUE(view$rate_open), ns, locked = isTRUE(view$rate_locked))
    )
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

# Summary and the recommendations (topic, linked title, the model's tip,
# "Problem melden"). A click on a title is logged (link_click) via
# input$ai_link.
ai_feedback_body <- function(fb, ns = identity, reported = NULL, report = TRUE) {
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
            tags$p(class = "ai-fb-text", r$text),
            if (report) ai_report_form(fb$id_feedback, r$position, r$position %in% reported, ns)
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
# "Problem melden" under one recommendation: a native <details>, so opening
# it needs no round trip. Once reported, a short thank-you instead.
ai_report_form <- function(id_feedback, position, reported, ns = identity) {
  if (reported) {
    return(tags$p(class = "ai-fb-reported", bsicons::bs_icon("check2"), "Gemeldet, danke!"))
  }
  name <- sprintf("%s-%d-%d", ns("ai_rep"), as.integer(id_feedback), as.integer(position))
  tags$details(
    class = "ai-fb-report",
    tags$summary("Problem melden"),
    div(
      class = "ai-fb-form",
      lapply(seq_along(AI_REPORT_REASONS), function(k) {
        id <- paste0(name, "-", k)
        div(
          class = "form-check",
          tags$input(class = "form-check-input", type = "radio", name = name, id = id, value = names(AI_REPORT_REASONS)[k]),
          tags$label(class = "form-check-label", `for` = id, AI_REPORT_REASONS[[k]])
        )
      }),
      ai_comment_field(paste0(name, "-c"), NA, "Was genau? (optional)"),
      tags$button(
        type = "button", class = "btn btn-sm btn-outline-primary",
        `data-ai-action` = "report-send", `data-input` = ns("ai_report"),
        `data-id` = as.integer(id_feedback), `data-pos` = as.integer(position),
        "Melden"
      )
    )
  )
}

# The comment field shared by the rating form and the reports. Says that the
# comment is stored.
ai_comment_field <- function(id, value, label) {
  tagList(
    tags$label(class = "ai-fb-label", `for` = id, label),
    tags$textarea(
      class = "form-control form-control-sm", id = id, rows = "2",
      maxlength = AI_COMMENT_MAX_CHARS,
      if (!is.na(value)) value
    ),
    tags$p(
      class = "ai-fb-hint",
      sprintf("Höchstens %d Zeichen. Dein Kommentar wird gespeichert und hilft uns, das Feedback zu verbessern.", AI_COMMENT_MAX_CHARS)
    )
  )
}

# "War dieses Feedback hilfreich?" The thumb is saved on its click
# (input$ai_rate); then the form opens with the reasons for that thumb and
# the comment, both saved as they change, so nobody can forget to send them.
# "Einklappen" (input$ai_rate_close) folds it to a thank-you with "Ändern"
# (input$ai_rate_edit), which opens it again. Only within the session it was
# given in: coming back later, the rating is final (`locked`), a thank-you
# with the chosen thumb, nothing to click.
ai_rating_block <- function(id_feedback, rating, open, ns = identity, locked = FALSE) {
  id_feedback <- as.integer(id_feedback)
  helpful <- if (is.null(rating)) NA_integer_ else rating$helpful
  locked <- locked && !is.null(rating)
  thumb <- function(value, icon, label) {
    on <- identical(helpful, value)
    tags$button(
      type = "button",
      class = paste("btn btn-sm ai-fb-thumb", if (on) "is-on"),
      `aria-pressed` = if (on) "true" else "false",
      `aria-label` = label,
      disabled = if (locked) NA,
      `data-ai-action` = if (!locked) "rate", `data-input` = if (!locked) ns("ai_rate"),
      `data-id` = id_feedback, `data-helpful` = value,
      bsicons::bs_icon(if (on) paste0(icon, "-fill") else icon),
      label
    )
  }
  question <- div(
    class = "ai-fb-rate-row",
    tags$span(class = "ai-fb-rate-q", "War dieses Feedback hilfreich?"),
    div(
      class = "ai-fb-thumbs",
      thumb(1L, "hand-thumbs-up", "Ja"),
      thumb(0L, "hand-thumbs-down", "Nein")
    )
  )
  if (is.null(rating)) {
    return(div(class = "ai-fb-rate", question))
  }
  if (locked) {
    return(div(
      class = "ai-fb-rate is-locked",
      question,
      tags$p(class = "ai-fb-thanks", "Danke für deine Rückmeldung!")
    ))
  }
  if (!open) {
    return(div(
      class = "ai-fb-rate",
      question,
      tags$p(
        class = "ai-fb-thanks",
        "Danke für deine Rückmeldung! ",
        tags$button(
          type = "button", class = "btn btn-link btn-sm p-0 align-baseline ai-fb-edit",
          `data-ai-action` = "rate-edit", `data-input` = ns("ai_rate_edit"), `data-id` = id_feedback,
          "Ändern"
        )
      )
    ))
  }
  reasons <- if (identical(helpful, 1L)) AI_REASONS_POSITIVE else AI_REASONS_NEGATIVE
  prefix <- sprintf("%s-%d", ns("ai_rate_r"), id_feedback)
  div(
    class = "ai-fb-rate",
    question,
    div(
      class = "ai-fb-form ai-fb-rate-form",
      `data-id` = id_feedback,
      `data-reasons-input` = ns("ai_rate_reasons"),
      `data-comment-input` = ns("ai_rate_comment"),
      tags$p(
        class = "ai-fb-label",
        if (identical(helpful, 1L)) "Was war hilfreich? (optional)" else "Was hat nicht gepasst? (optional)"
      ),
      lapply(seq_along(reasons), function(k) {
        id <- paste0(prefix, "-", k)
        div(
          class = "form-check",
          tags$input(
            class = "form-check-input", type = "checkbox", id = id, value = names(reasons)[k],
            checked = if (names(reasons)[k] %in% rating$reasons) NA
          ),
          tags$label(class = "form-check-label", `for` = id, reasons[[k]])
        )
      }),
      ai_comment_field(paste0(prefix, "-c"), rating$comment, "Kommentar (optional)"),
      div(
        class = "ai-fb-form-end",
        tags$p(
          class = "ai-fb-autosave",
          "Deine Auswahl und dein Kommentar werden automatisch gespeichert.",
          tags$span(class = "ai-fb-saved", `aria-live` = "polite")
        ),
        tags$button(
          type = "button", class = "btn btn-link btn-sm p-0 ai-fb-fold",
          `data-ai-action` = "rate-close", `data-input` = ns("ai_rate_close"), `data-id` = id_feedback,
          bsicons::bs_icon("chevron-up"),
          "Einklappen"
        )
      )
    )
  )
}

