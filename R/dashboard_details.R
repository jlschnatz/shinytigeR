# ── Dashboard: detail sections below the trajectory ──────────────────────────
# "Dein Lernstand im Detail" (the current estimate with its uncertainty, and
# how it is computed) and "Dein Lernverhalten" (when, where and how the
# student practiced). Both are accordion panels, closed by default. Every card
# answers one question in the student's words; the stats are pure functions of
# the user's responses, so they're testable without a session.

# ── Lernstand im Detail ───────────────────────────────────────────────────────

# Position of a theta value on a THETA_RANGE scale strip, in % of its width
theta_pct <- function(theta) {
  x <- pmin(pmax(theta, THETA_RANGE[1]), THETA_RANGE[2])
  100 * (x - THETA_RANGE[1]) / diff(THETA_RANGE)
}

# Central ESTIMATE_INTERVAL_LEVEL interval of the normal approximation
# theta_true ~ N(theta, se^2) of the EAP posterior (R/irt.R), clipped to
# THETA_RANGE
estimate_interval <- function(theta, se, level = ESTIMATE_INTERVAL_LEVEL) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  pmin(pmax(c(theta - z * se, theta + z * se), THETA_RANGE[1]), THETA_RANGE[2])
}

scale_label_at <- function(theta) {
  COMPETENCY_SCALE$label[findInterval(theta, COMPETENCY_SCALE$lower)]
}

# The competency scale as a strip (the same tinted bands as the trajectory
# panels), with the interval as a bar and the estimate as a dot on top
interval_strip <- function(theta, se) {
  bands <- lapply(seq_len(nrow(COMPETENCY_SCALE)), function(k) {
    lo <- theta_pct(max(COMPETENCY_SCALE$lower[k], THETA_RANGE[1]))
    hi <- theta_pct(min(COMPETENCY_SCALE$upper[k], THETA_RANGE[2]))
    tags$span(
      class = "ivl-band",
      style = sprintf("left:%.2f%%;width:%.2f%%;--seg:%s;", lo, hi - lo, COMPETENCY_SCALE$color_hex[k])
    )
  })
  marks <- if (!is.na(theta)) {
    tagList(
      if (!is.na(se) && is.finite(se)) {
        ivl <- theta_pct(estimate_interval(theta, se))
        tags$span(class = "ivl-bar", style = sprintf("left:%.2f%%;width:%.2f%%;", ivl[1], ivl[2] - ivl[1]))
      },
      tags$span(class = "ivl-dot", style = sprintf("left:%.2f%%;", theta_pct(theta)))
    )
  }
  div(class = "ivl-strip", `aria-hidden` = "true", tags$span(class = "ivl-bands", bands), marks)
}

# Shared axes for all strips of the card: the level names above (angled, as
# the narrowest levels are only a few percent wide) and theta below. Each sits
# in the strip column of an .ivl-row grid, so it lines up with the strips.
interval_axis <- function(type = c("names", "numbers")) {
  type <- match.arg(type)
  ticks <- if (type == "names") {
    lo <- pmax(COMPETENCY_SCALE$lower, THETA_RANGE[1])
    hi <- pmin(COMPETENCY_SCALE$upper, THETA_RANGE[2])
    lapply(seq_len(nrow(COMPETENCY_SCALE)), function(k) {
      tags$span(
        class = "ivl-axis-tick",
        style = sprintf("left:%.2f%%;", theta_pct((lo[k] + hi[k]) / 2)),
        tags$span(class = "ivl-axis-name", COMPETENCY_SCALE$label[k])
      )
    })
  } else {
    lapply(seq(THETA_RANGE[1], THETA_RANGE[2]), function(v) {
      tags$span(
        class = "ivl-axis-tick",
        style = sprintf("left:%.2f%%;", theta_pct(v)),
        if (v < 0) paste0("−", abs(v)) else v
      )
    })
  }
  div(
    class = "ivl-row ivl-axis-row",
    `aria-hidden` = "true",
    div(class = "ivl-area"),
    div(class = paste0("ivl-axis ivl-axis-", type), ticks)
  )
}

# One sentence per area, in words rather than numbers
interval_sentence <- function(theta, se, n_items) {
  cl <- competency_label(theta, n_items)
  aufgaben <- sprintf("%d Aufgabe%s", n_items, if (n_items == 1L) "" else "n")
  if (cl$level == 0L) {
    return("Noch keine Daten – bearbeite Aufgaben aus diesem Bereich.")
  }
  if (is.na(se) || !is.finite(se)) {
    return(sprintf(
      "Stufe „%s“ (%s). Die Unsicherheit erscheint nach der nächsten Aktualisierung.",
      cl$label, aufgaben
    ))
  }
  ivl <- estimate_interval(theta, se)
  from <- scale_label_at(ivl[1])
  to <- scale_label_at(ivl[2])
  range_txt <- if (from == to) sprintf("„%s“", from) else sprintf("zwischen „%s“ und „%s“", from, to)
  sprintf("Dein Stand liegt wahrscheinlich %s (%s).", range_txt, aufgaben)
}

# "Wie sicher ist die Einschätzung?" — `comp` is the competency() snapshot
# (NULL if there is none yet)
interval_card <- function(comp) {
  rows <- lapply(LEARNING_AREA_LEVELS, function(area) {
    r <- if (!is.null(comp)) comp[comp$learning_area == area, ]
    theta <- if (!is.null(r)) r$theta else NA_real_
    se <- if (!is.null(r)) r$se else NA_real_
    n <- if (!is.null(r)) r$n_items else 0L
    div(
      class = "ivl-row",
      div(class = "ivl-area", area),
      interval_strip(theta, se),
      div(class = "ivl-text", interval_sentence(theta, se, n))
    )
  })
  div(
    class = "la-card ivl-card",
    div(class = "la-card-title", "Wie sicher ist die Einschätzung?"),
    div(
      class = "ivl-legend",
      tags$span(class = "ivl-legend-item", tags$span(class = "ivl-dot is-legend"), "Schätzung"),
      tags$span(
        class = "ivl-legend-item",
        tags$span(class = "ivl-bar is-legend"),
        sprintf("Bereich, in dem dein Stand mit %d %% Wahrscheinlichkeit liegt", round(100 * ESTIMATE_INTERVAL_LEVEL))
      )
    ),
    interval_axis("names"),
    rows,
    interval_axis("numbers")
  )
}

explainer_card <- function() {
  div(
    class = "la-card",
    div(class = "la-card-title", "Wie wird dein Lernstand berechnet?"),
    tags$ul(
      class = "la-explain",
      tags$li("Jede Aufgabe hat eine Schwierigkeit. Wer schwierige Aufgaben löst, wird höher eingeschätzt als jemand, der nur leichte löst."),
      tags$li("Pro Aufgabe zählt deine letzte Antwort: Löst du eine Aufgabe nach einem Fehler richtig, zählt die richtige Lösung."),
      tags$li("Übersprungene Aufgaben und nicht erkannte Zahleneingaben zählen nicht."),
      tags$li("Je mehr Aufgaben du in einem Bereich bearbeitest, desto kürzer wird die Linie – die Einschätzung wird sicherer."),
      tags$li("Bei wenigen Aufgaben wird deine Einschätzung vorsichtig zur Mitte gezogen. So springt sie nicht nach ein, zwei Zufallstreffern hin und her."),
      tags$li("Neu berechnet wird höchstens einmal pro Sitzung: beim Anmelden oder über „Fähigkeitsverlauf aktualisieren“."),
      tags$li("Die Einschätzung ist eine Orientierung für dein Üben, keine Note.")
    )
  )
}

detail_section <- function(comp) {
  bslib::accordion_panel(
    title = "Dein Lernstand im Detail",
    value = "detail",
    icon = bsicons::bs_icon("bullseye"),
    div(class = "la-grid la-grid-detail", interval_card(comp), explainer_card())
  )
}

# ── Lernverhalten ─────────────────────────────────────────────────────────────

WEEKDAYS_DE <- c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")

# Monday of the week containing each date
week_monday <- function(date) date - (as.integer(format(date, "%u")) - 1L)

# Calendar grid: one column per week (at most `max_weeks`, ending with the
# current one), one cell per day, shaded by the number of answers that day
activity_calendar_data <- function(dates, today = Sys.Date(), max_weeks = 16L) {
  first <- week_monday(if (length(dates) > 0L) min(dates) else today)
  first <- max(first, week_monday(today) - 7L * (max_weeks - 1L))
  days <- seq(first, week_monday(today) + 6L, by = "day")
  n <- as.integer(table(factor(as.character(dates), levels = as.character(days))))
  data.frame(
    date = days,
    n = n,
    level = findInterval(n, c(1L, 5L, 10L, 20L)), # 0 | 1-4 | 5-9 | 10-19 | 20+
    future = days > today
  )
}

calendar_card <- function(ud, today = Sys.Date()) {
  cal <- activity_calendar_data(ud$date, today)
  n_days <- length(unique(ud$date))
  n_week <- length(unique(ud$date[ud$date >= week_monday(today)]))
  cells <- lapply(seq_len(nrow(cal)), function(i) {
    d <- cal[i, ]
    tip <- sprintf(
      "%s, %s: %s",
      WEEKDAYS_DE[as.integer(format(d$date, "%u"))], format(d$date, "%d.%m."),
      if (d$n == 0L) "nicht geübt" else sprintf("%d Antwort%s", d$n, if (d$n == 1L) "" else "en")
    )
    tags$span(
      class = paste0("cal-cell cal-l", d$level, if (d$future) " is-future"),
      title = if (!d$future) tip
    )
  })
  div(
    class = "la-card",
    div(class = "la-card-title", "Wann hast du geübt?"),
    div(
      class = "la-summary",
      sprintf("An %d Tag%s geübt, davon %d diese Woche", n_days, if (n_days == 1L) "" else "en", n_week)
    ),
    div(
      class = "cal-wrap",
      div(class = "cal-weekdays", `aria-hidden` = "true", lapply(WEEKDAYS_DE, function(w) tags$span(if (w %in% c("Mo", "Mi", "Fr")) w))),
      div(class = "cal-grid", role = "img", `aria-label` = sprintf("Übungskalender: an %d Tagen geübt", n_days), cells)
    ),
    div(
      class = "cal-legend",
      "weniger",
      lapply(0:4, function(l) tags$span(class = paste0("cal-cell cal-l", l))),
      "mehr"
    ),
    tags$p(
      class = "la-note",
      "Jedes Kästchen ist ein Tag. Über mehrere Tage verteiltes Üben bleibt besser hängen als viel Üben an einem Tag."
    )
  )
}

# Items seen per learning area, with the same definition as the selector's
# "neu" badges: any answer (a skip too) means the item is no longer new
coverage_by_area <- function(ud, data_item) {
  seen <- unique(as.integer(ud$id_item))
  data.frame(
    learning_area = LEARNING_AREA_LEVELS,
    n_total = vapply(LEARNING_AREA_LEVELS, function(a) sum(data_item$learning_area == a), integer(1)),
    n_seen = vapply(LEARNING_AREA_LEVELS, function(a) {
      sum(data_item$learning_area == a & data_item$id_item %in% seen)
    }, integer(1)),
    row.names = NULL
  )
}

coverage_card <- function(cov) {
  div(
    class = "la-card",
    div(class = "la-card-title", "Wo hast du schon geübt?"),
    lapply(seq_len(nrow(cov)), function(i) {
      r <- cov[i, ]
      pct <- if (r$n_total > 0L) 100 * r$n_seen / r$n_total else 0
      n_new <- r$n_total - r$n_seen
      div(
        class = "cov-row",
        div(
          class = "cov-head",
          tags$span(class = "cov-area", r$learning_area),
          tags$span(class = "cov-count", sprintf("%d von %d", r$n_seen, r$n_total))
        ),
        div(class = "cov-bar", tags$span(style = sprintf("width:%.1f%%;", pct))),
        if (n_new > 0L) tags$span(class = "cov-new", sprintf("noch %d neu", n_new))
      )
    }),
    tags$p(class = "la-note", "Wie viele Aufgaben jedes Bereichs du schon gesehen hast. Neue Aufgaben findest du unter „Üben“.")
  )
}

# New items vs. repeats, and what happened to items answered wrong at first.
# Skips are left out: they're not an attempt at the item.
retry_stats <- function(ud) {
  a <- ud[!(ud$skipped %in% c(TRUE, 1L)), , drop = FALSE]
  a <- a[order(a$id_item, a$id_datetime), , drop = FALSE]
  first <- !duplicated(a$id_item)
  per_item <- lapply(split(a, a$id_item), function(d) {
    list(
      first_wrong = isFALSE(as.logical(d$bool_correct[1])),
      retried = nrow(d) > 1L,
      solved = isTRUE(as.logical(d$bool_correct[nrow(d)]))
    )
  })
  wrong <- Filter(function(x) x$first_wrong, per_item)
  retried <- Filter(function(x) x$retried, wrong)
  list(
    n_new = sum(first),
    n_repeat = sum(!first),
    n_first_wrong = length(wrong),
    n_retried = length(retried),
    n_solved = sum(vapply(retried, function(x) x$solved, logical(1)))
  )
}

retry_card <- function(st) {
  n_ans <- st$n_new + st$n_repeat
  pct_new <- if (n_ans > 0L) 100 * st$n_new / n_ans else 0
  step <- function(n, label, cls = NULL) {
    div(class = paste("rt-step", cls), div(class = "rt-num", n), div(class = "rt-lbl", label))
  }
  div(
    class = "la-card",
    div(class = "la-card-title", "Neue Aufgaben oder Wiederholungen?"),
    div(
      class = "la-summary",
      sprintf("%d Antworten: %d zu neuen Aufgaben, %d Wiederholungen", n_ans, st$n_new, st$n_repeat)
    ),
    div(
      class = "rt-split", `aria-hidden` = "true",
      tags$span(class = "rt-split-new", style = sprintf("width:%.1f%%;", pct_new)),
      tags$span(class = "rt-split-rep")
    ),
    div(
      class = "rt-split-legend",
      tags$span(tags$span(class = "rt-swatch rt-split-new"), "neu"),
      tags$span(tags$span(class = "rt-swatch rt-split-rep"), "wiederholt")
    ),
    div(class = "la-subtitle", "Lernst du aus Fehlern?"),
    if (st$n_first_wrong == 0L) {
      tags$p(class = "la-note mb-0", "Bisher hast du keine Aufgabe beim ersten Versuch falsch beantwortet.")
    } else {
      tagList(
        div(
          class = "rt-steps",
          step(st$n_first_wrong, "beim ersten Versuch falsch"),
          tags$span(class = "rt-arrow", `aria-hidden` = "true", bsicons::bs_icon("arrow-right")),
          step(st$n_retried, "erneut versucht"),
          tags$span(class = "rt-arrow", `aria-hidden` = "true", bsicons::bs_icon("arrow-right")),
          step(st$n_solved, "inzwischen gelöst", "is-good")
        ),
        tags$p(
          class = "la-note",
          "Falsch beantwortete Aufgaben später noch einmal zu lösen, ist einer der wirksamsten Lernschritte."
        )
      )
    }
  )
}

behaviour_section <- function(ud, data_item, today = Sys.Date()) {
  bslib::accordion_panel(
    title = "Dein Lernverhalten",
    value = "behaviour",
    icon = bsicons::bs_icon("calendar3"),
    div(
      class = "la-grid la-grid-behaviour",
      calendar_card(ud, today),
      coverage_card(coverage_by_area(ud, data_item)),
      retry_card(retry_stats(ud))
    )
  )
}
