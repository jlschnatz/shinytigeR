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
# theta_true ~ N(theta, se^2), clipped to THETA_RANGE
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
      style = sprintf(
        "left:%.2f%%;width:%.2f%%;--seg:%s;",
        lo,
        hi - lo,
        COMPETENCY_SCALE$color_hex[k]
      )
    )
  })
  marks <- if (!is.na(theta)) {
    tagList(
      if (!is.na(se) && is.finite(se)) {
        ivl <- theta_pct(estimate_interval(theta, se))
        tags$span(
          class = "ivl-bar",
          style = sprintf("left:%.2f%%;width:%.2f%%;", ivl[1], ivl[2] - ivl[1])
        )
      },
      tags$span(
        class = "ivl-dot",
        style = sprintf("left:%.2f%%;", theta_pct(theta))
      )
    )
  }
  div(
    class = "ivl-strip",
    `aria-hidden` = "true",
    tags$span(class = "ivl-bands", bands),
    marks
  )
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
      cl$label,
      aufgaben
    ))
  }
  ivl <- estimate_interval(theta, se)
  from <- scale_label_at(ivl[1])
  to <- scale_label_at(ivl[2])
  range_txt <- if (from == to) {
    sprintf("„%s“", from)
  } else {
    sprintf("zwischen „%s“ und „%s“", from, to)
  }
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
      tags$span(
        class = "ivl-legend-item",
        tags$span(class = "ivl-dot is-legend"),
        "Schätzung"
      ),
      tags$span(
        class = "ivl-legend-item",
        tags$span(class = "ivl-bar is-legend"),
        sprintf(
          "Unsicherheitsbereich (%d\u2011%%\u2011Konfidenzintervall)",
          round(100 * ESTIMATE_INTERVAL_LEVEL)
        )
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
      tags$li(
        "Jede Aufgabe hat eine Schwierigkeit. Wer schwierige Aufgaben löst, wird höher eingeschätzt als jemand, der nur leichte löst."
      ),
      tags$li(
        "Pro Aufgabe zählt deine letzte Antwort: Löst du eine Aufgabe nach einem Fehler richtig, zählt die richtige Lösung."
      ),
      tags$li(
        "Übersprungene Aufgaben und nicht erkannte Zahleneingaben zählen nicht."
      ),
      tags$li(
        "Je mehr Aufgaben du in einem Bereich bearbeitest, desto schmaler wird der Balken – die Einschätzung wird sicherer."
      ),
      tags$li(
        "Neu berechnet wird höchstens einmal pro Sitzung: beim Anmelden oder über „Fähigkeitsverlauf aktualisieren“."
      ),
      tags$li(
        "Die Einschätzung ist eine Orientierung für dein Üben, keine Note."
      )
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

# A small (i) button next to a card title that shows `text` as a tooltip —
# the explanations that would otherwise take a line of their own. Tooltips
# are any element with `data-tip`, shown by one shared script in ui.R (mouse
# hover, keyboard focus, and a tap on touch screens).
info_tip <- function(text) {
  tags$button(
    type = "button",
    class = "la-info",
    `data-tip` = text,
    `aria-label` = text,
    bsicons::bs_icon("info-circle")
  )
}

# One definition of "worked on" for the whole section: an item counts once it
# has been ANSWERED (right or wrong). Skips and unrecognized numeric answers
# (bool_correct NA) don't count — the same rule as the ability estimate. With
# `data_item`, answers to items no longer in the pool are dropped too. Adds a
# `date` column if the caller hasn't.
answered_rows <- function(ud, data_item = NULL) {
  if (nrow(ud) == 0L || is.null(ud$bool_correct)) {
    return(data.frame(
      id_item = integer(0),
      bool_correct = logical(0),
      id_datetime = numeric(0),
      date = as.Date(character(0)),
      learning_area = character(0)
    ))
  }
  keep <- !is.na(ud$bool_correct) & !(ud$skipped %in% c(TRUE, 1L))
  if (!is.null(data_item)) {
    keep <- keep & ud$id_item %in% data_item$id_item
  }
  a <- ud[keep, , drop = FALSE]
  a$bool_correct <- as.logical(a$bool_correct)
  if (is.null(a$date)) {
    a$date <- as.Date(as.POSIXct(a$id_datetime, origin = "1970-01-01"))
  }
  a[order(a$id_item, a$id_datetime), , drop = FALSE]
}

# The section's key numbers: cumulative effort (never goes down, unlike the
# estimate) and where the student left off. NULL if nothing was answered yet.
behaviour_stats <- function(ud, data_item, today = Sys.Date()) {
  a <- answered_rows(ud, data_item)
  if (nrow(a) == 0L) {
    return(NULL)
  }
  last <- a[a$date == max(a$date), , drop = FALSE]
  list(
    n_items = length(unique(a$id_item)),
    n_total = nrow(data_item),
    n_days = length(unique(a$date)),
    n_week = length(unique(a$date[a$date >= week_monday(today)])),
    last_date = max(a$date),
    last_n = nrow(last),
    last_correct = sum(last$bool_correct)
  )
}

MONTHS_DE <- c(
  "Jan", "Feb", "Mär", "Apr", "Mai", "Jun",
  "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"
)

# "27. Okt", with the year only if it isn't the current one
format_date_short <- function(date, today = Sys.Date()) {
  s <- sprintf(
    "%d. %s",
    as.integer(format(date, "%d")),
    MONTHS_DE[as.integer(format(date, "%m"))]
  )
  if (format(date, "%Y") != format(today, "%Y")) {
    paste(s, format(date, "%Y"))
  } else {
    s
  }
}

# Three small tiles at the top of the section, side by side on every screen
behaviour_tiles <- function(st, today = Sys.Date()) {
  if (is.null(st)) {
    return(tags$p(
      class = "la-summary la-behaviour-empty",
      "Du hast noch keine Aufgaben beantwortet. Sobald du übst, siehst du hier, wann, wo und wie."
    ))
  }
  tile <- function(value, label, sub) {
    div(
      class = "la-tile",
      div(class = "la-tile-val", value),
      div(class = "la-tile-lbl", label),
      div(class = "la-tile-sub", sub)
    )
  }
  plural <- function(n, one, many) if (n == 1L) one else many
  div(
    class = "la-tiles",
    tile(
      st$n_items,
      plural(st$n_items, "Aufgabe beantwortet", "Aufgaben beantwortet"),
      sprintf("von %d", st$n_total)
    ),
    tile(
      st$n_days,
      plural(st$n_days, "Tag geübt", "Tage geübt"),
      sprintf("%d diese Woche", st$n_week)
    ),
    tile(
      format_date_short(st$last_date, today),
      "zuletzt geübt",
      sprintf("%d von %d richtig", st$last_correct, st$last_n)
    )
  )
}

WEEKDAYS_DE <- c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")

# Monday of the week containing each date
week_monday <- function(date) date - (as.integer(format(date, "%u")) - 1L)

# Calendar grid: one column per week (at most `max_weeks`, ending with the
# current one), one cell per day, shaded by the number of answers that day
activity_calendar_data <- function(dates, today = Sys.Date(), max_weeks = 16L) {
  first <- week_monday(if (length(dates) > 0L) min(dates) else today)
  first <- max(first, week_monday(today) - 7L * (max_weeks - 1L))
  days <- seq(first, week_monday(today) + 6L, by = "day")
  n <- as.integer(table(factor(
    as.character(dates),
    levels = as.character(days)
  )))
  data.frame(
    date = days,
    n = n,
    level = findInterval(n, c(1L, 5L, 10L, 20L)), # 0 | 1-4 | 5-9 | 10-19 | 20+
    future = days > today
  )
}

# Month label per calendar row (week): the month whose 1st falls in that week
# (past days only), "" otherwise; the top row also names its month unless the
# next row does, so the oldest weeks are never unlabelled.
CALENDAR_PHONE_WEEKS <- 8L # keep in sync with .cal-week:nth-last-child() in app.css

calendar_month_labels <- function(days, today = Sys.Date()) {
  mondays <- days[seq(1L, length(days), by = 7L)]
  lab <- vapply(
    mondays,
    function(m) {
      wk <- m + 0:6
      first <- wk[format(wk, "%d") == "01" & wk <= today]
      if (length(first)) MONTHS_DE[as.integer(format(first, "%m"))] else ""
    },
    character(1)
  )
  if (lab[1] == "" && (length(lab) < 2L || lab[2] == "")) {
    lab[1] <- MONTHS_DE[as.integer(format(mondays[1], "%m"))]
  }
  lab
}

calendar_card <- function(ud, today = Sys.Date()) {
  ud <- answered_rows(ud)
  cal <- activity_calendar_data(ud$date, today)
  n_days <- length(unique(ud$date))
  cell <- function(d) {
    tip <- sprintf(
      "%s, %s: %s",
      WEEKDAYS_DE[as.integer(format(d$date, "%u"))],
      format(d$date, "%d.%m."),
      if (d$n == 0L) {
        "nicht geübt"
      } else {
        sprintf("%d Antwort%s", d$n, if (d$n == 1L) "" else "en")
      }
    )
    # tabindex -1: focusable by a tap (so the tooltip shows on touch screens)
    # but not a stop when tabbing through the page
    tags$span(
      class = paste0("cal-cell cal-l", d$level, if (d$future) " is-future"),
      `data-tip` = if (!d$future) tip,
      tabindex = if (!d$future) "-1"
    )
  }
  # One row per week (oldest on top, like a wall calendar), so a long history
  # grows downwards into the side column's free height instead of needing a
  # horizontal scrollbar. On phones CSS keeps only the last 8 weeks.
  week_idx <- split(seq_len(nrow(cal)), (seq_len(nrow(cal)) - 1L) %/% 7L)
  months <- calendar_month_labels(cal$date, today)
  # The top row on phones (the 8th-last week) names its month too, if it
  # isn't labelled already: shown via CSS only there (data-alt)
  k0 <- length(week_idx) - CALENDAR_PHONE_WEEKS + 1L
  alt <- if (k0 > 1L && months[k0] == "" && (k0 == length(months) || months[k0 + 1L] == "")) {
    MONTHS_DE[as.integer(format(cal$date[week_idx[[k0]][1]], "%m"))]
  }
  weeks <- lapply(seq_along(week_idx), function(k) {
    div(
      class = "cal-week",
      tags$span(
        class = "cal-month",
        `aria-hidden` = "true",
        `data-alt` = if (k == k0) alt,
        months[k]
      ),
      lapply(week_idx[[k]], function(i) cell(cal[i, ]))
    )
  })
  div(
    class = "la-card cal-card",
    div(
      class = "la-card-title",
      "Deine Übungsaktivität",
      info_tip(
        "Jedes Kästchen ist ein Tag, je dunkler, desto mehr Antworten. Wir empfehlen dir, über mehrere Tage verteilt zu lernen, statt an einem Tag sehr viele Aufgaben zu bearbeiten."
      )
    ),
    div(
      class = "cal-wrap",
      div(
        class = "cal-head",
        `aria-hidden` = "true",
        tags$span(),
        lapply(WEEKDAYS_DE, function(w) tags$span(substr(w, 1, 1)))
      ),
      div(
        class = "cal-weeks",
        role = "img",
        `aria-label` = sprintf("Übungskalender: an %d Tagen geübt", n_days),
        weeks
      )
    ),
    div(
      class = "cal-legend",
      "weniger",
      lapply(0:4, function(l) tags$span(class = paste0("cal-cell cal-l", l))),
      "mehr"
    )
  )
}

# Items per learning area x item type (the selector's layout), each split by
# its latest answer: richtig / falsch / noch offen (never answered)
coverage_matrix <- function(ud, data_item) {
  a <- answered_rows(ud, data_item)
  latest <- a[!duplicated(a$id_item, fromLast = TRUE), , drop = FALSE]
  status <- latest$bool_correct[match(data_item$id_item, latest$id_item)]
  grid <- expand.grid(
    type = unname(ITEM_TYPE_LABELS),
    learning_area = LEARNING_AREA_LEVELS,
    stringsAsFactors = FALSE
  )
  counts <- t(mapply(
    function(area, type) {
      in_cell <- data_item$learning_area == area & data_item$type_item == type
      c(
        n_total = sum(in_cell),
        n_correct = sum(status[in_cell] %in% TRUE),
        n_wrong = sum(status[in_cell] %in% FALSE)
      )
    },
    grid$learning_area,
    grid$type
  ))
  cbind(
    grid[, c("learning_area", "type")],
    as.data.frame(counts, row.names = NULL)
  )
}

coverage_cell <- function(r) {
  if (r$n_total == 0L) {
    return(div(
      class = "cov-cell is-empty",
      tags$span(class = "cov-count", "keine Aufgaben")
    ))
  }
  n_open <- r$n_total - r$n_correct - r$n_wrong
  pct <- function(n) sprintf("width:%.1f%%;", 100 * n / r$n_total)
  div(
    class = "cov-cell",
    `data-tip` = sprintf(
      "Zuletzt %d richtig, %d falsch, %d noch offen",
      r$n_correct,
      r$n_wrong,
      n_open
    ),
    tabindex = "-1",
    # "offen" on the count line, not below the bar, so all bars line up
    div(
      class = "cov-count",
      sprintf("%d von %d", r$n_correct + r$n_wrong, r$n_total),
      if (n_open > 0L) {
        tags$span(class = "cov-new", sprintf(" · %d offen", n_open))
      }
    ),
    div(
      class = "cov-bar",
      `aria-hidden` = "true",
      tags$span(class = "is-correct", style = pct(r$n_correct)),
      tags$span(class = "is-wrong", style = pct(r$n_wrong))
    )
  )
}

coverage_card <- function(cov) {
  types <- names(ITEM_TYPE_LABELS)
  div(
    class = "la-card cov-card",
    div(
      class = "la-card-title",
      "Deine Themenabdeckung",
      info_tip(
        "Aufgeteilt wie die Auswahl unter „Üben“: Dort findest du die offenen Aufgaben jedes Bereichs. Ein Balken zeigt, ob deine letzte Antwort richtig oder falsch war."
      )
    ),
    div(
      class = "cov-grid",
      div(
        class = "cov-head-row",
        `aria-hidden` = "true",
        tags$span(),
        lapply(types, tags$span)
      ),
      lapply(LEARNING_AREA_LEVELS, function(area) {
        div(
          class = "cov-row",
          div(class = "cov-area", area),
          lapply(ITEM_TYPE_LABELS, function(type) {
            coverage_cell(cov[cov$learning_area == area & cov$type == type, ])
          })
        )
      })
    ),
    div(
      class = "lt-legend cov-legend",
      tags$span(tags$span(class = "lt-swatch is-correct"), "zuletzt richtig"),
      tags$span(tags$span(class = "lt-swatch is-wrong"), "zuletzt falsch"),
      tags$span(tags$span(class = "lt-swatch is-none"), "noch offen")
    )
  )
}

# ── Answer tree: what became of all items ─────────────────────────────────────

# Counts for the answer tree, one level per attempt. Root = every item in the
# pool; a node's children split its items by the next attempt: richtig /
# falsch / none (no such attempt: "nicht bearbeitet" at the 1st column,
# "nicht wiederholt" / "erster Versuch" after that). Goes
# ANSWER_TREE_DEPTH attempts deep. `direction = "forward"` reads each item's
# answers from the first one on, `"backward"` from the latest one back in
# time (the card's toggle shows both). Skips and unrecognized numeric answers
# aren't answers here (same as for the estimate), so an item that was only
# skipped counts as not worked on yet. Returns a nested
# list(n, children = list(correct, wrong, none)); nodes at the last level and
# "none" nodes have no children. Plus `direction` and the numbers for the
# takeaway sentence: `$mistakes` (forward) or `$latest` (backward).
ANSWER_TREE_DEPTH <- 3L

answer_tree_stats <- function(
  ud,
  data_item,
  depth = ANSWER_TREE_DEPTH,
  direction = c("forward", "backward")
) {
  direction <- match.arg(direction)
  a <- answered_rows(ud, data_item)
  answered <- split(a$bool_correct, a$id_item)
  dates <- split(a$date, a$id_item)
  seqs <- lapply(as.character(data_item$id_item), function(id) {
    x <- if (is.null(answered[[id]])) logical(0) else answered[[id]]
    if (direction == "backward") rev(x) else x
  })
  build <- function(s, attempt) {
    node <- list(n = length(s))
    if (attempt <= depth) {
      at <- vapply(
        s,
        function(x) if (length(x) >= attempt) x[attempt] else NA,
        logical(1)
      )
      node$children <- list(
        correct = build(s[at %in% TRUE], attempt + 1L),
        wrong = build(s[at %in% FALSE], attempt + 1L),
        none = list(n = sum(is.na(at)))
      )
    }
    node
  }
  tree <- build(seqs, 1L)
  tree$direction <- direction
  if (direction == "backward") {
    # For the takeaway: the latest answers, and how the items got there
    last <- vapply(answered, function(x) x[length(x)], logical(1))
    before_wrong <- vapply(answered, function(x) any(!x[-length(x)]), logical(1))
    before_right <- vapply(answered, function(x) any(x[-length(x)]), logical(1))
    tree$latest <- list(
      n_correct = sum(last),
      n_wrong = sum(!last),
      n_learned = sum(last & before_wrong),
      n_regressed = sum(!last & before_right)
    )
    return(tree)
  }
  # For the takeaway sentence: what became of the items that were wrong at
  # the first attempt, over ALL later attempts (not cut off at `depth`)
  wrong_first <- names(Filter(function(x) !x[1], answered))
  retried <- wrong_first[lengths(answered[wrong_first]) > 1L]
  solved <- vapply(answered[retried], function(x) any(x[-1]), logical(1))
  # Retried on a later day than the first attempt (spaced, not right away)
  later_day <- vapply(dates[retried], function(d) any(d[-1] > d[1]), logical(1))
  tree$mistakes <- list(
    n_wrong = length(wrong_first),
    n_retried = length(retried),
    n_solved = sum(solved),
    n_open = length(wrong_first) - sum(solved),
    n_later_day = sum(later_day)
  )
  tree
}

# The tree's message in one sentence, so students who only skim get it too.
# `tree` is answer_tree_stats()'s result.
answer_tree_takeaway <- function(tree) {
  if (identical(tree$direction, "backward")) {
    return(answer_tree_takeaway_latest(tree))
  }
  m <- tree$mistakes
  n_answered <- tree$n - tree$children$none$n
  aufgaben <- function(n) sprintf("%d Aufgabe%s", n, if (n == 1L) "" else "n")
  if (n_answered == 0L) {
    return(
      "Sobald du Aufgaben bearbeitest, siehst du hier, wie es nach dem ersten Versuch weiterging."
    )
  }
  if (m$n_wrong == 0L) {
    return(
      "Du hast bisher keine Aufgabe beim ersten Versuch falsch beantwortet."
    )
  }
  start <- sprintf(
    "Von %s, die du beim ersten Versuch falsch hattest, ",
    aufgaben(m$n_wrong)
  )
  if (m$n_retried == 0L) {
    return(paste0(start, "hast du noch keine erneut versucht."))
  }
  solved <- if (m$n_solved == m$n_retried) {
    if (m$n_retried == 1L) {
      "und dann gelöst"
    } else {
      sprintf("und alle %d dann gelöst", m$n_solved)
    }
  } else if (m$n_solved == 0L) {
    "aber noch keine davon gelöst"
  } else {
    sprintf("und %d davon gelöst", m$n_solved)
  }
  open <- if (m$n_open > 0L) {
    sprintf(
      " %d %s noch auf dich.",
      m$n_open,
      if (m$n_open == 1L) "wartet" else "warten"
    )
  } else {
    ""
  }
  sep <- if (m$n_solved == 0L) ", " else " "
  timing <- if (m$n_later_day == m$n_retried) {
    if (m$n_retried == 1L) {
      " (an einem späteren Tag)"
    } else {
      " (alle an einem späteren Tag)"
    }
  } else if (m$n_later_day == 0L) {
    " (am selben Tag)"
  } else {
    sprintf(" (%d davon an einem späteren Tag)", m$n_later_day)
  }
  paste0(
    start,
    sprintf(
      "hast du %d noch einmal versucht%s%s%s.",
      m$n_retried,
      timing,
      sep,
      solved
    ),
    open
  )
}

# The backward view's sentence: the latest answers, and how many of them
# came after an earlier mistake / after the item was once right
answer_tree_takeaway_latest <- function(tree) {
  l <- tree$latest
  if (l$n_correct + l$n_wrong == 0L) {
    return(
      "Sobald du Aufgaben bearbeitest, siehst du hier, wie es zu deinen letzten Antworten kam."
    )
  }
  right <- if (l$n_correct == 0L) {
    "bei keiner Aufgabe richtig"
  } else {
    sprintf(
      "bei %d Aufgabe%s richtig%s",
      l$n_correct,
      if (l$n_correct == 1L) "" else "n",
      if (l$n_learned > 0L) {
        sprintf(" (%d davon nach einem früheren Fehler)", l$n_learned)
      } else {
        ""
      }
    )
  }
  wrong <- if (l$n_wrong == 0L) {
    "bei keiner falsch"
  } else {
    sprintf(
      "bei %d falsch%s",
      l$n_wrong,
      if (l$n_regressed > 0L) {
        sprintf(" (%d davon hattest du früher schon einmal richtig)", l$n_regressed)
      } else {
        ""
      }
    )
  }
  sprintf("Deine letzte Antwort war %s und %s.", right, wrong)
}

answer_tree_label <- function(cat, attempt, direction = "forward") {
  switch(
    cat,
    correct = "richtig",
    wrong = "falsch",
    none = if (attempt == 1L) {
      "nicht bearbeitet"
    } else if (direction == "backward") {
      "erster Versuch"
    } else {
      "nicht wiederholt"
    }
  )
}

# A ring showing how a node splits into its children (correct/wrong/none)
answer_tree_donut <- function(node) {
  counts <- vapply(node$children, `[[`, integer(1), "n")
  pct <- if (sum(counts) > 0L) {
    100 * cumsum(counts) / sum(counts)
  } else {
    c(0, 0, 0)
  }
  tags$span(
    class = "lt-donut",
    `aria-hidden` = "true",
    style = sprintf("--a:%.1f%%;--b:%.1f%%;", pct[1], pct[2])
  )
}

# One node and (recursively) its branch, as a <li>. Nodes with children are
# buttons; the client-side toggle in ui.R flips the <li>'s .is-open, which
# shows or hides its child list — so any number of branches can be open.
# `attempt` is the attempt this node stands for (0 = root).
answer_tree_item <- function(
  node,
  cat,
  attempt,
  parent_n,
  open_attempts,
  direction = "forward"
) {
  expandable <- !is.null(node$children) && node$n > 0L
  is_open <- expandable && attempt %in% open_attempts
  share <- if (parent_n > 0L) {
    sprintf("%d %%", round(100 * node$n / parent_n))
  } else {
    NULL
  }
  # Donut left of the text, label and share on one line: keeps nodes flat
  body <- tagList(
    if (expandable) answer_tree_donut(node),
    tags$span(
      class = "lt-text",
      tags$span(class = "lt-num", node$n),
      tags$span(
        class = "lt-lbl",
        if (attempt == 0L) {
          "Aufgaben"
        } else {
          answer_tree_label(cat, attempt, direction)
        },
        if (!is.null(share)) {
          tags$span(
            class = "lt-share",
            tags$span(class = "lt-dot", "\u00b7\u00a0"),
            share
          )
        }
      )
    )
  )
  cls <- paste("lt-node", paste0("is-", cat), if (node$n == 0L) "is-empty")
  el <- if (expandable && attempt > 0L) {
    tags$button(
      type = "button",
      class = paste(cls, "is-toggle"),
      `data-node` = "",
      `aria-expanded` = if (is_open) "true" else "false",
      body,
      tags$span(
        class = "lt-chevron",
        `aria-hidden` = "true",
        bsicons::bs_icon("chevron-right")
      )
    )
  } else {
    div(class = cls, body)
  }
  tags$li(
    class = if (is_open || attempt == 0L) "is-open",
    el,
    if (expandable) {
      # Empty nodes are left out: at deeper attempts most would be 0
      kids <- Filter(
        function(k) node$children[[k]]$n > 0L,
        names(node$children)
      )
      tags$ul(
        class = "lt-children",
        lapply(kids, function(k) {
          answer_tree_item(
            node$children[[k]],
            k,
            attempt + 1L,
            node$n,
            open_attempts,
            direction
          )
        })
      )
    }
  )
}

# Items whose latest answer is wrong — including ones answered right before
# and wrong later — most recently answered first. The same set as the
# coverage card's "zuletzt falsch" and as what the estimate counts as wrong
# (latest attempt). One row per item: id, area, type, date of the latest answer.
open_items <- function(ud, data_item) {
  a <- answered_rows(ud, data_item)
  # answered_rows() sorts by time, so the last row is the latest answer
  # (same rule as coverage_matrix(), also for answers in the same second)
  open <- Filter(function(d) !d$bool_correct[nrow(d)], split(a, a$id_item))
  if (length(open) == 0L) {
    return(data.frame(
      id_item = integer(0),
      learning_area = character(0),
      type_item = character(0),
      last_date = as.Date(character(0))
    ))
  }
  ids <- as.integer(names(open))
  last_time <- vapply(open, function(d) max(d$id_datetime), numeric(1))
  idx <- match(ids, data_item$id_item)
  out <- data.frame(
    id_item = ids,
    learning_area = data_item$learning_area[idx],
    type_item = data_item$type_item[idx],
    last_date = as.Date(as.POSIXct(last_time, origin = "1970-01-01")),
    stringsAsFactors = FALSE
  )
  out[order(-last_time, out$id_item), , drop = FALSE]
}

# Open mistakes that may be retried today: the latest answer is at least
# RETRY_MIN_DAYS calendar days old (the time-out against redoing the same
# items over and over)
retry_eligible <- function(oi, today = Sys.Date()) {
  oi[as.integer(today - oi$last_date) >= RETRY_MIN_DAYS, , drop = FALSE]
}

# The panel next to the tree, closing the loop back to practice: a button
# that starts a practice queue with the retryable open mistakes
# (`input$retry_open`; the dashboard server picks at most RETRY_MAX_ITEMS).
# Deliberately no per-item list or solution lookup: looking up the answer
# right before retrying it would defeat the retry. A plain button sending a
# Shiny.setInputValue() event, not an actionButton(), since this panel
# re-renders after every answer (see item_id_badge() for that quirk). `ns` is
# the dashboard's.
open_items_panel <- function(oi, ns, today = Sys.Date()) {
  n_open <- nrow(oi)
  n_ready <- nrow(retry_eligible(oi, today))
  n_wait <- n_open - n_ready
  div(
    class = "la-card oi-card",
    div(
      class = "la-card-title",
      "Offene Fehler",
      if (n_open > 0L) tags$span(class = "oi-count", n_open),
      info_tip(
        "Aufgaben, die du zuletzt falsch beantwortet hast (auch wenn du sie früher schon einmal richtig hattest). Du kannst diese Aufgaben frühestens 24 Stunden nach deiner letzten Antwort nochmal versuchen."
      )
    ),
    if (n_open == 0L) {
      tags$p(
        class = "la-note mt-0",
        "Keine! Deine letzte Antwort war überall richtig."
      )
    } else {
      tagList(
        if (n_ready > 0L) {
          tags$button(
            type = "button",
            class = "btn btn-sm btn-primary oi-retry",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
              ns("retry_open")
            ),
            bsicons::bs_icon("arrow-repeat"),
            if (n_ready > RETRY_MAX_ITEMS) {
              sprintf("%d davon nochmal üben", RETRY_MAX_ITEMS)
            } else {
              sprintf("Alle %d nochmal üben", n_ready)
            }
          )
        },
        if (n_wait > 0L) {
          tags$p(
            class = "la-note oi-wait",
            if (n_ready == 0L) {
              "Alle von heute, nochmal üben kannst du sie ab morgen."
            } else {
              sprintf("%d von heute erst ab morgen", n_wait)
            }
          )
        }
      )
    }
  )
}

# One reading direction of the tree: takeaway, hint, the tree and its legend
answer_tree_view <- function(tree, depth = ANSWER_TREE_DEPTH) {
  backward <- identical(tree$direction, "backward")
  heads <- if (backward) {
    c("Letzter", "Vorletzter", "Drittletzter", sprintf("%d. letzter", 4:9))[seq_len(depth)]
  } else {
    sprintf("%d. Versuch", seq_len(depth))
  }
  div(
    class = "lt-view",
    `data-view` = tree$direction,
    div(class = "la-summary lt-takeaway", answer_tree_takeaway(tree)),
    div(
      class = "la-note lt-hint",
      sprintf(
        "Tippe auf einen Kasten mit Pfeil, um %s zu sehen.",
        if (backward) "den Versuch davor" else "den nächsten Versuch"
      )
    ),
    div(
      class = "lt-tree",
      lang = "de",
      div(
        class = "lt-inner",
        # Narrow cards drop the root column (CSS) and show the total here
        div(class = "lt-total", sprintf("%d Aufgaben insgesamt", tree$n)),
        div(
          class = "lt-head",
          `aria-hidden` = "true",
          tags$span("Alle"),
          lapply(heads, tags$span)
        ),
        # The 1st column's branches are open by default, deeper ones on click
        tags$ul(
          class = "lt-top",
          answer_tree_item(
            tree,
            "root",
            0L,
            0L,
            open_attempts = 1L,
            direction = tree$direction
          )
        )
      )
    ),
    div(
      class = "lt-legend",
      tags$span(tags$span(class = "lt-swatch is-correct"), "richtig"),
      tags$span(tags$span(class = "lt-swatch is-wrong"), "falsch"),
      tags$span(
        tags$span(class = "lt-swatch is-none"),
        if (backward) {
          "nicht bearbeitet / erster Versuch"
        } else {
          "nicht bearbeitet / wiederholt"
        }
      )
    )
  )
}

# The tree card. With `backward` (answer_tree_stats(direction = "backward"))
# it gets a toggle between both reading directions: from the first attempt
# on, or from the latest answer back. Both views are rendered; the switch is
# client-side (ui.R), so it needs no round trip. The card's data-view says
# which one is shown (forward by default).
answer_tree_card <- function(tree, backward = NULL, depth = ANSWER_TREE_DEPTH) {
  switch_btn <- function(view, label, pressed) {
    tags$button(
      type = "button",
      class = "lt-switch-btn",
      `data-lt-view` = view,
      `aria-pressed` = if (pressed) "true" else "false",
      label
    )
  }
  div(
    class = "la-card lt-card",
    `data-view` = "forward",
    div(
      class = "la-card-title",
      "Deine Antwortverläufe",
      info_tip(paste(
        "Jede Spalte ist ein Versuch, die Zeit läuft von links nach rechts.",
        "„Ab 1. Versuch“ beginnt links bei allen Aufgaben und zeigt deinen 1., 2. und 3. Versuch.",
        "„Ab letztem Versuch“ beginnt rechts bei deiner letzten Antwort und geht zurück zu den Versuchen davor.",
        "Der Ring in einem Kasten zeigt, wie er sich in der nächsten Spalte aufteilt.",
        "Übersprungene Aufgaben zählen nicht."
      ))
    ),
    if (!is.null(backward)) {
      div(
        class = "lt-switch",
        role = "group",
        `aria-label` = "Ansicht des Baums",
        switch_btn("forward", "Ab 1. Versuch", TRUE),
        switch_btn("backward", "Ab letztem Versuch", FALSE)
      )
    },
    answer_tree_view(tree, depth),
    if (!is.null(backward)) answer_tree_view(backward, depth)
  )
}

behaviour_section <- function(
  ud,
  data_item,
  ns = shiny::NS(NULL),
  today = Sys.Date()
) {
  bslib::accordion_panel(
    title = "Dein Lernverhalten",
    value = "behaviour",
    icon = bsicons::bs_icon("calendar3"),
    div(
      class = "la-grid la-grid-behaviour",
      # Narrow left column on wide screens: key numbers, calendar and the
      # retry card; coverage and tree share the rest side by side (CSS)
      div(
        class = "la-side",
        behaviour_tiles(behaviour_stats(ud, data_item, today), today),
        calendar_card(ud, today),
        open_items_panel(open_items(ud, data_item), ns, today = today)
      ),
      coverage_card(coverage_matrix(ud, data_item)),
      answer_tree_card(
        answer_tree_stats(ud, data_item),
        backward = answer_tree_stats(ud, data_item, direction = "backward")
      )
    )
  )
}
