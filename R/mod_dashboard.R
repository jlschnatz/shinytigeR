# ── Helpers ───────────────────────────────────────────────────────────────────

competency_label <- function(theta, n_unique) {
  if (is.na(n_unique) || n_unique == 0L || is.na(theta)) {
    return(list(label = "Keine Daten", color = NA_character_, level = 0L))
  }
  i <- findInterval(theta, COMPETENCY_SCALE$lower)
  list(
    label = COMPETENCY_SCALE$label[i],
    color = COMPETENCY_SCALE$color_hex[i],
    level = i
  )
}

# Label plus a segmented strip of the whole COMPETENCY_SCALE: segments up to
# the student's level are filled in their scale color, the current one is
# raised, the rest stay gray — so the label reads as a position on the scale,
# not an isolated verdict.
competency_scale_bar <- function(cl) {
  n <- nrow(COMPETENCY_SCALE)
  desc <- if (cl$level == 0L) {
    "Keine Daten"
  } else {
    sprintf("Kompetenz: %s (Stufe %d von %d)", cl$label, cl$level, n)
  }
  div(
    class = if (cl$level == 0L) "comp-level is-empty" else "comp-level",
    # the app's tooltip (www/js/tooltips.js), not a native `title`; tabindex
    # -1 so a tap shows it on touch screens without adding a Tab stop
    `data-tip` = desc,
    tabindex = "-1",
    tags$span(class = "comp-level-label", cl$label),
    tags$span(
      class = "comp-scale",
      role = "img",
      `aria-label` = desc,
      lapply(seq_len(n), function(k) {
        tags$span(
          class = paste(
            "comp-seg",
            if (k <= cl$level) "is-filled",
            if (k == cl$level) "is-current"
          ),
          style = if (k <= cl$level) sprintf("--seg:%s;", COMPETENCY_SCALE$color_hex[k])
        )
      })
    )
  )
}

# Certainty of the competency label: P(true theta lies in the label's
# interval), from prob_in_interval() (R/irt.R), simplified to three dots and
# a plain-language hover text. Snapshots saved before the standard error was
# stored (se = NA) fall back to the item count via evidence_label().
label_certainty <- function(theta, se, cl) {
  if (cl$level == 0L || is.na(se) || !is.finite(se)) {
    return(NA_real_)
  }
  # Outer labels are clipped to THETA_RANGE: otherwise an all-correct or
  # all-wrong start (theta stuck at +/-3, huge SE) would put most of its
  # probability mass in the open-ended Aufbau/Souverän interval and show
  # "wahrscheinlich" after only a handful of items.
  prob_in_interval(
    theta,
    se,
    max(COMPETENCY_SCALE$lower[cl$level], THETA_RANGE[1]),
    min(COMPETENCY_SCALE$upper[cl$level], THETA_RANGE[2])
  )
}

certainty_label <- function(p) {
  if (p >= CERTAINTY_HIGH) {
    list(label = "Wahrscheinlich", dots = 3L)
  } else if (p >= CERTAINTY_MED) {
    list(label = "Etwa so wahrscheinlich wie nicht", dots = 2L)
  } else {
    list(label = "Unsicher", dots = 1L)
  }
}

# Probability as a rounded, student-facing phrase ("etwa 70 %")
format_prob_de <- function(p) {
  pct <- 5L * round(100 * p / 5)
  if (pct < 5L) "unter 5 %" else if (pct > 95L) "über 95 %" else sprintf("etwa %d %%", pct)
}

evidence_label <- function(n_unique) {
  if (is.na(n_unique) || n_unique < 1L) {
    list(label = "Keine", dots = 0L)
  } else if (n_unique >= EVIDENCE_HIGH) {
    list(label = "Hoch", dots = 3L)
  } else if (n_unique >= EVIDENCE_MED) {
    list(label = "Mittel", dots = 2L)
  } else {
    list(label = "Niedrig", dots = 1L)
  }
}

certainty_dots <- function(theta, se, n_items, cl) {
  n <- if (is.na(n_items)) 0L else as.integer(n_items)
  aufgaben <- sprintf("%d Aufgabe%s", n, if (n == 1L) "" else "n")
  p <- label_certainty(theta, se, cl)
  if (!is.na(p)) {
    ct <- certainty_label(p)
    dots <- ct$dots
    desc <- sprintf(
      "%s: Mit %s Wahrscheinlichkeit liegt dein Stand im Bereich \u201e%s\u201c (%s).",
      ct$label, format_prob_de(p), cl$label, aufgaben
    )
  } else {
    ev <- evidence_label(n)
    dots <- ev$dots
    desc <- if (n == 0L) "Noch keine Daten" else sprintf("Evidenz: %s (%s)", ev$label, aufgaben)
  }
  tags$span(
    class = "ev-dots",
    `data-tip` = desc, # app tooltip, see competency_scale_bar()
    tabindex = "-1",
    role = "img",
    `aria-label` = desc,
    lapply(seq_len(3L), function(k) {
      tags$span(class = if (k <= dots) "ev-dot is-filled" else "ev-dot")
    })
  )
}

# Locale-independent "24. Aug 2026" — format(..., "%b") depends on the
# server's locale, which is not German in the Docker image.
format_date_de <- function(date) {
  months <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
  sprintf(
    "%d. %s %s",
    as.integer(format(date, "%d")),
    months[as.integer(format(date, "%m"))],
    format(date, "%Y")
  )
}

# Right-hand card next to "Dein Lernstand". `feedback` is the generated text
# (Markdown, rendered via render_md()); NULL shows the placeholder. This is
# the hook for the personalized LLM feedback planned in the "kiwi" project —
# the card chrome stays, only the source of `feedback` needs wiring up.
ai_feedback_card <- function(feedback = NULL) {
  bslib::card(
    height = "100%",
    class = "ai-feedback-card",
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      div(
        class = "d-flex align-items-center gap-2",
        bsicons::bs_icon("stars"),
        tags$b("Persönliches Feedback")
      ),
      if (is.null(feedback)) tags$span(class = "ai-feedback-badge", "Bald verfügbar")
    ),
    bslib::card_body(
      if (is.null(feedback)) {
        tagList(
          tags$p(
            class = "text-muted mb-4",
            "Hier bekommst du bald eine KI-gestützte Rückmeldung zu deinem ",
            "Lernstand: was dir schon gut gelingt, wo es noch hakt und welche ",
            "Aufgaben dich als Nächstes weiterbringen."
          ),
          div(
            class = "ai-feedback-skeleton",
            `aria-hidden` = "true",
            lapply(c(92, 78, 85, 60), function(w) {
              div(class = "ai-skel-line", style = sprintf("width:%d%%;", w))
            })
          )
        )
      } else {
        div(class = "ai-feedback-text", HTML(render_md(feedback)))
      }
    )
  )
}

# Reshapes the raw db_ability.sqlite history (one row per learning area per
# computed_at batch) into a plot-ready long data.frame: drops batches with no
# usable theta for that area and adds POSIXct `time` and Date `date` columns.
# The x axis uses `time`, not `date`: the login catch-up and the refresh button
# often save two batches on the same day.
ability_trajectory_data <- function(ability_raw) {
  ab <- ability_raw[!is.na(ability_raw$theta), ]
  if (nrow(ab) == 0L) {
    return(ab)
  }
  ab$time <- as.POSIXct(ab$computed_at, origin = "1970-01-01")
  ab$date <- local_date(ab$time)
  ab[order(ab$learning_area, ab$computed_at), ]
}

# y range of the trajectory panels: THETA_RANGE plus a little room, so markers
# at the bounds (an all-correct or all-wrong start) aren't cut in half
TRAJECTORY_Y_RANGE <- THETA_RANGE + c(-0.25, 0.25)

# Background of every trajectory panel: one band per COMPETENCY_SCALE level
# (the outer ones run to the panel edge) in its scale color, so a point reads
# as "in which competency zone", the same scale as the strips in "Dein
# Lernstand".
competency_band_shapes <- function(opacity = 0.2) {
  lapply(seq_len(nrow(COMPETENCY_SCALE)), function(k) {
    list(
      type = "rect", layer = "below", xref = "paper", x0 = 0, x1 = 1, yref = "y",
      y0 = max(COMPETENCY_SCALE$lower[k], TRAJECTORY_Y_RANGE[1]),
      y1 = min(COMPETENCY_SCALE$upper[k], TRAJECTORY_Y_RANGE[2]),
      fillcolor = COMPETENCY_SCALE$color_hex[k], opacity = opacity,
      line = list(width = 0)
    )
  })
}

# Shared time axis for all trajectory panels (so they line up), padded so the
# first and last points aren't cut off at the panel edge. With `days` (one of
# TRAJECTORY_WINDOWS) it's the last `days` days up to now instead, starting
# exactly at the window's start: a line from an earlier estimate then enters
# from the left edge, so the panel still shows where the student came from.
trajectory_x_range <- function(hist, days = NA, now = Sys.time()) {
  if (!is.na(days)) {
    return(c(now - days * 86400, now + days * 86400 * 0.03))
  }
  r <- range(hist$time)
  pad <- max(as.numeric(diff(r), units = "secs") * 0.06, 12 * 3600)
  r + c(-pad, pad)
}

# x-axis labels of the trajectory panels: `n` dates spread evenly over the
# visible range (inset from the edges so the labels aren't clipped), each on
# a full local day. Set explicitly because plotly's own "nice" ticks (weekly,
# every two months, ...) often left only 2 labels on a panel. A short range
# can yield fewer (two ticks on the same day collapse into one).
# Returns list(vals = "YYYY-MM-DD", text = "dd.mm.").
trajectory_ticks <- function(x_range, n = 4L) {
  span <- as.numeric(difftime(x_range[2], x_range[1], units = "secs"))
  at <- x_range[1] + span * seq(0.08, 0.92, length.out = n)
  days <- unique(local_date(at))
  midnight <- as.POSIXct(format(days), format = "%Y-%m-%d") # explicit format: guessing one was slow
  days <- days[midnight >= x_range[1] & midnight <= x_range[2]]
  list(vals = format(days, "%Y-%m-%d"), text = format(days, "%d.%m."))
}

# The latest saved estimate per learning area, in the learning_area/theta/
# se/n_items shape estimate_competency() returns. Per area, not the latest
# batch: compute_and_save_ability() only writes rows for the areas practised
# since the last estimate, so an area's current value can be older than
# another's. `computed_at` is the most recent update of any area (the date in
# the card header). Areas without a row read as NA / 0 items ("Keine Daten").
latest_ability <- function(ab) {
  if (nrow(ab) == 0L) {
    return(NULL)
  }
  ab <- ab[order(ab$computed_at), , drop = FALSE]
  ab <- ab[!duplicated(ab$learning_area, fromLast = TRUE), , drop = FALSE]
  idx <- match(LEARNING_AREA_LEVELS, ab$learning_area)
  data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = ab$theta[idx],
    # NA for estimates saved before the standard error was stored
    se = if ("se" %in% names(ab)) as.numeric(ab$se[idx]) else NA_real_,
    n_items = ifelse(is.na(ab$n_items[idx]), 0L, as.integer(ab$n_items[idx])),
    computed_at = max(ab$computed_at, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Times for plotly.js: local wall-clock strings, no time zone (plotly.js
# shows them as given, the same day the rest of the dashboard uses)
trajectory_time <- function(t) format(t, "%Y-%m-%d %H:%M:%S")

# The x axis of a time window (one of TRAJECTORY_WINDOWS): range and labels.
# The same for every panel, so trajectory_section() computes it once per
# window (the date/time-zone conversions are the slow part).
trajectory_window_axis <- function(x_range) {
  ticks <- trajectory_ticks(x_range)
  list(
    x_range = x_range,
    range = trajectory_time(x_range),
    tickvals = as.list(ticks$vals),
    ticktext = as.list(ticks$text)
  )
}

# The parts of a panel that depend on the time window: the window's axis
# (trajectory_window_axis()) plus the note for an area without an estimate
# in the window. The client-side script swaps them in.
trajectory_window_layout <- function(d, axis) {
  empty <- !any(d$time >= axis$x_range[1] & d$time <= axis$x_range[2])
  list(
    range = axis$range,
    tickvals = axis$tickvals,
    ticktext = axis$ticktext,
    annotations = if (empty) {
      list(list(
        text = "In diesem Zeitraum nicht geübt", showarrow = FALSE,
        xref = "paper", yref = "paper", x = 0.5, y = 0.5,
        font = list(size = 12, color = "#5b6477"),
        bgcolor = "rgba(255,255,255,0.85)", borderpad = 4
      ))
    } else {
      list()
    }
  )
}

# One small-multiples panel: the trajectory of a single learning area (`d`,
# rows of ability_trajectory_data() for that area) on the full theta range,
# as a plain plotly.js figure spec (data, layout, config), plus `windows`: the
# window-dependent layout parts for each window in `axes` (a named list of
# trajectory_window_axis(), or a single x range; the first is the default). Markers show the saved estimates (hover: date +
# competency label), joined by a plain line. Not smoothed: a lowess line
# visibly missed some of the points, which read as an error; damping the
# jumps is the job of the displayed estimate itself, not of the plot. No
# uncertainty is drawn here on purpose; intervals belong to the detailed
# explanation of the current estimate.
#
# Not built with plotly's R package (plot_ly() + renderPlotly()): its R
# pipeline (plotly_build, event registration, JSON) took ~50 ms per panel,
# ~0.35 s for opening "Fähigkeitsverlauf", while the spec itself is tiny.
# trajectory_section() embeds the specs, www/js/dashboard.js draws them with
# plotly.js (Plotly.react) and switches the time window without a round trip.
trajectory_panel_spec <- function(d, axes) {
  d <- d[order(d$time), ]
  if (inherits(axes, "POSIXct")) axes <- list(all = trajectory_window_axis(axes))
  lbl <- COMPETENCY_SCALE$label[findInterval(d$theta, COMPETENCY_SCALE$lower)]
  n <- as.integer(d$n_items)
  basis <- ifelse(
    is.na(n), "",
    sprintf("<br>Basierend auf %d %s", n, ifelse(n == 1L, "Aufgabe", "Aufgaben"))
  )
  hover <- sprintf(
    "%s<br><b>%s</b> (\u03b8 = %s)%s",
    format_date_de(d$date), lbl,
    formatC(d$theta, format = "f", digits = 2, decimal.mark = "."),
    basis
  )
  # I()/as.list(): arrays in the JSON even for a single estimate (a plain
  # string for `text` made the hover template show placeholders)
  x <- I(trajectory_time(d$time))
  y <- I(d$theta)
  # automargin: room for the date labels below the 4 px margin (plotly's R
  # package turned it on by default, plotly.js doesn't)
  axis <- list(
    title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE,
    showline = TRUE, linecolor = "#c8ccd1", mirror = TRUE, automargin = TRUE
  )
  windows <- lapply(axes, function(a) trajectory_window_layout(d, a))
  default <- windows[[1]]
  list(
    data = list(
      list(
        type = "scatter", mode = "lines", x = x, y = y, hoverinfo = "skip",
        line = list(color = TRAJECTORY_COLOR, width = 3)
      ),
      list(
        type = "scatter", mode = "markers", x = x, y = y,
        text = as.list(hover), hovertemplate = "%{text}<extra></extra>",
        marker = list(color = TRAJECTORY_COLOR, size = 9, line = list(color = "white", width = 1.5))
      )
    ),
    layout = list(
      shapes = competency_band_shapes(),
      annotations = default$annotations,
      showlegend = FALSE,
      # plotly doesn't inherit the page font; keep in sync with base_font in ui.R
      font = list(family = "'Fira Sans', sans-serif"),
      hoverlabel = list(font = list(family = "'Fira Sans', sans-serif")),
      xaxis = c(axis, list(
        type = "date", range = default$range, tickmode = "array",
        tickvals = default$tickvals, ticktext = default$ticktext
      )),
      yaxis = c(axis, list(range = TRAJECTORY_Y_RANGE, showticklabels = FALSE, ticks = "")),
      height = 170,
      margin = list(l = 4, r = 4, t = 4, b = 4),
      dragmode = FALSE,
      plot_bgcolor = "white",
      paper_bgcolor = "rgba(0,0,0,0)"
    ),
    config = list(displayModeBar = FALSE, scrollZoom = FALSE, responsive = TRUE),
    windows = windows
  )
}

# A panel's spec as JSON for a <script type="application/json"> ("</" is
# escaped so the hover text's </b> can't end the script element)
trajectory_spec_json <- function(spec) {
  json <- jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  gsub("</", "<\\/", json, fixed = TRUE)
}

# plotly.js itself, from the plotly package (no htmlwidget around it)
plotly_js_dependency <- function() {
  htmltools::htmlDependency(
    "plotly-main", as.character(utils::packageVersion("plotly")),
    src = "htmlwidgets/lib/plotlyjs", package = "plotly",
    script = "plotly-latest.min.js", all_files = FALSE
  )
}

# Legend of the competency zones (the panels' background bands), highest
# level first so it lines up with the bands. `class` picks the variant: a row
# above the grid ("traj-legend-top", only on screens too narrow for 2 columns)
# or the tile that fills the free 8th grid cell ("traj-legend-panel").
trajectory_legend <- function(class) {
  ks <- rev(seq_len(nrow(COMPETENCY_SCALE)))
  div(
    class = paste("traj-legend", class),
    `aria-hidden` = "true",
    if (class == "traj-legend-panel") div(class = "traj-panel-title", "Kompetenzstufen"),
    div(
      class = "traj-legend-items",
      lapply(ks, function(k) {
        tags$span(
          class = "traj-legend-item",
          tags$span(class = "traj-swatch", style = sprintf("--seg:%s;", COMPETENCY_SCALE$color_hex[k])),
          COMPETENCY_SCALE$label[k]
        )
      })
    )
  )
}

# Time-window switch above the trajectory grid (TRAJECTORY_WINDOWS), styled
# like the answer tree's toggle. Plain buttons: www/js/dashboard.js redraws the
# panels with the window's layout from their specs (no round trip) and keeps
# the choice across dashboard re-renders; "all" stands for NA.
trajectory_window_key <- function(days) if (is.na(days)) "all" else as.character(days)

trajectory_window_switch <- function() {
  div(
    class = "lt-switch traj-switch",
    role = "group",
    `aria-label` = "Zeitraum des Verlaufs",
    lapply(seq_along(TRAJECTORY_WINDOWS), function(k) {
      days <- TRAJECTORY_WINDOWS[[k]]
      tags$button(
        type = "button",
        class = "lt-switch-btn",
        `data-traj-window` = trajectory_window_key(days),
        `aria-pressed` = if (k == 1L) "true" else "false",
        names(TRAJECTORY_WINDOWS)[k]
      )
    })
  )
}

# The "Fähigkeitsverlauf" accordion panel: one small chart per learning area
# in a wrapping grid of at most 4 columns (see .traj-grid in app.css: 4 on a
# laptop, 2 on a tablet, 1 on a phone).
# The legend is the grid's 8th tile, so with 7 areas it fills the otherwise
# empty last cell at 4 and 2 columns; when the grid is down to 1 column it
# would end up below all charts, so a CSS container query swaps it for a row
# above the grid instead. `hist` is ability_trajectory_data().
trajectory_section <- function(hist, ns = shiny::NS(NULL), now = Sys.time()) {
  n_batches <- length(unique(hist$computed_at))
  body <- if (n_batches < 2L) {
    tags$p(
      class = "text-muted mb-0",
      "Dein Verlauf erscheint hier, sobald es mindestens zwei Schätzungen deines ",
      "Lernstands gibt (höchstens eine neue pro Sitzung)."
    )
  } else {
    # one shared x axis per time window, so the panels line up
    axes <- lapply(TRAJECTORY_WINDOWS, function(days) {
      trajectory_window_axis(trajectory_x_range(hist, days, now))
    })
    names(axes) <- vapply(TRAJECTORY_WINDOWS, trajectory_window_key, character(1))
    panels <- lapply(seq_along(LEARNING_AREA_LEVELS), function(k) {
      area <- LEARNING_AREA_LEVELS[k]
      short <- names(LEARNING_AREA_LABELS)[match(area, LEARNING_AREA_LABELS)]
      d <- hist[hist$learning_area == area, , drop = FALSE]
      div(
        class = "traj-panel",
        div(class = "traj-panel-title", short),
        if (nrow(d) > 0L) {
          tagList(
            div(id = ns(paste0("traj_", k)), class = "traj-plot"),
            tags$script(
              type = "application/json", class = "traj-spec",
              HTML(trajectory_spec_json(trajectory_panel_spec(d, axes)))
            )
          )
        } else {
          div(class = "traj-empty", "Noch keine Schätzung")
        }
      )
    })
    div(
      class = "traj-wrap",
      plotly_js_dependency(),
      # the time switch with an (i) explaining how to read the panels
      div(
        class = "traj-controls",
        trajectory_window_switch(),
        info_tip(paste(
          "Jeder Punkt ist eine gespeicherte Schätzung deines Lernstands in diesem Themenbereich.",
          "Eine neue entsteht höchstens einmal pro Sitzung (beim Login oder über „Fähigkeitsverlauf aktualisieren“),",
          "und nur für Bereiche, die du seitdem geübt hast. Die farbigen Bänder sind die Kompetenzstufen."
        ))
      ),
      trajectory_legend("traj-legend-top"),
      div(class = "traj-grid", panels, trajectory_legend("traj-legend-panel"))
    )
  }
  bslib::accordion_panel(
    title = "Fähigkeitsverlauf",
    value = "trajectory",
    icon = bsicons::bs_icon("graph-up-arrow"),
    body
  )
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  div(class = "dashboard-wrap mt-4", uiOutput(ns("dash_content")))
}

# ── Server ────────────────────────────────────────────────────────────────────

# `retry_items(ids)` (optional callback, wired in server.R): the "nochmal
# üben" button of the "Offene Fehler" panel (dashboard_details.R). Starts a
# practice queue with up to RETRY_MAX_ITEMS of the open mistakes that are past
# their time-out (retry_eligible()), in random order. These answers count like
# any other, also for the ability estimate; they're stored with
# practice_mode = "retry".
mod_dashboard_server <- function(id, data_item, credentials, write_trigger, ability_computed_this_session,
                                 retry_items = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$retry_open, {
      ids <- retry_eligible(open_items(user_data(), data_item))$id_item
      if (!is.null(retry_items) && length(ids) > 0L) retry_items(safe_sample(ids, RETRY_MAX_ITEMS))
    })

    user_exists <- reactive({
      write_trigger()
      db_user_exists(credentials()$info$user_name)
    })

    user_data <- reactive({
      write_trigger()
      ud <- db_get_userdata(credentials()$info$user_name)
      if (nrow(ud) == 0L) {
        return(ud)
      }
      ud$bool_correct <- as.logical(ud$bool_correct)
      ud$date <- local_date(ud$id_datetime)
      ud
    })

    # Bumped after a successful compute_and_save_ability() call to force a
    # re-fetch of the persisted estimate below.
    ability_version <- reactiveVal(0L)

    needs_ability_update <- reactive({
      write_trigger()
      isTRUE(ability_needs_update(credentials()$info$user_name))
    })

    can_refresh_ability <- reactive({
      needs_ability_update() && !isTRUE(ability_computed_this_session())
    })

    # Full ability history (every saved row, not just the latest), feeds the
    # "Fähigkeitsverlauf" trajectory plot below.
    ability_history <- reactive({
      ability_version()
      db_get_ability(credentials()$info$user_name)
    })

    # The latest saved estimate per learning area (latest_ability()).
    competency <- reactive(latest_ability(ability_history()))

    observeEvent(input$refresh_ability, {
      req(can_refresh_ability())
      compute_and_save_ability(
        credentials()$info$user_name,
        session$token,
        data_item
      )
      ability_computed_this_session(TRUE)
      ability_version(ability_version() + 1L)
    })

    output$dash_content <- renderUI({
      if (!isTRUE(user_exists())) {
        return(div(
          class = "text-center py-5 text-muted",
          bsicons::bs_icon("bar-chart", size = "3em"),
          tags$p(
            class = "mt-3 fs-5",
            "Noch keine Antworten vorhanden. Starte im Bereich ",
            tags$b("Üben"),
            "."
          )
        ))
      }

      ud <- user_data()
      comp <- competency()
      can_refresh <- can_refresh_ability()

      tagList(
        # ── Top row: competency map + AI feedback ─────────────────────────────
        div(
          class = "row g-3 mb-3",

          # Competency map ("Dein Lernstand")
          div(
            class = "col-lg-7",
            bslib::card(
              height = "100%",
              bslib::card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$b("Dein Lernstand"),
                if (!is.null(comp)) {
                  tags$span(
                    class = "text-muted small",
                    format_date_de(local_date(comp$computed_at[1]))
                  )
                }
              ),
              bslib::card_body(
                class = "p-0",
                tags$table(
                  class = "table mb-0 dashboard-comp-table",
                  # explicit roles: on phones the rows are laid out as a grid
                  # (app.css), which makes some browsers drop the table
                  # semantics for screen readers
                  role = "table",
                  tags$thead(
                    role = "rowgroup",
                    tags$tr(
                      role = "row",
                      tags$th(role = "columnheader", tags$span(class = "visually-hidden", "Themenbereich")),
                      tags$th(
                        role = "columnheader",
                        `data-tip` = "Wie sicher die Einstufung ist (●●● = wahrscheinlich zutreffend)",
                        tabindex = "-1",
                        "Evidenz"
                      ),
                      tags$th(role = "columnheader", "Kompetenz")
                    )
                  ),
                  tags$tbody(
                    role = "rowgroup",
                    lapply(LEARNING_AREA_LEVELS, function(area) {
                      # Both columns come from the saved snapshot, so the
                      # certainty always describes the label shown next to it
                      # (not practice done since the last estimate).
                      row <- if (!is.null(comp)) comp[comp$learning_area == area, ]
                      theta <- if (!is.null(row)) row$theta else NA_real_
                      n_a <- if (!is.null(row)) row$n_items else 0L
                      se <- if (!is.null(row)) row$se else NA_real_
                      cl <- competency_label(theta, n_a)

                      tags$tr(
                        role = "row",
                        tags$td(role = "cell", class = "comp-area", area),
                        tags$td(role = "cell", certainty_dots(theta, se, n_a, cl)),
                        tags$td(role = "cell", competency_scale_bar(cl))
                      )
                    })
                  )
                )
              ),
              bslib::card_footer(
                class = "d-flex justify-content-between align-items-center gap-2 comp-footer",
                tags$span(
                  class = "text-muted small",
                  if (is.null(comp)) {
                    "Noch keine Fähigkeitsschätzung vorhanden."
                  } else if (can_refresh) {
                    NULL
                  } else if (isTRUE(ability_computed_this_session())) {
                    "In dieser Sitzung bereits aktualisiert."
                  } else {
                    "Keine neuen Daten seit der letzten Schätzung."
                  }
                ),
                actionButton(
                  session$ns("refresh_ability"),
                  "Fähigkeitsverlauf aktualisieren",
                  icon = shiny::icon("rotate"),
                  class = "btn-sm btn-outline-primary",
                  disabled = if (!can_refresh) "disabled" else NULL
                )
              )
            )
          ),

          # AI feedback (placeholder until the "kiwi" LLM feedback lands)
          div(
            class = "col-lg-5",
            ai_feedback_card(feedback = NULL)
          )
        ),

        # ── Expandable sections, all closed by default: trajectory, the
        # detailed estimate and the learning behaviour (R/dashboard_details.R)
        div(
          class = "mb-4",
          bslib::accordion(
            id = session$ns("dash_sections"),
            open = FALSE,
            multiple = TRUE,
            trajectory_ui(),
            detail_section(comp),
            behaviour_section(ud, data_item, session$ns)
          )
        )
      )
    })

    # ── Ability trajectory: one small chart per learning area ────────────────
    # Small multiples instead of one chart with 7 crossing lines: each panel
    # stays readable without a color per area, and the panels wrap with the
    # screen width (see trajectory_section()). Still plotly.js rather than
    # renderPlot: a server-rendered image doesn't reflow on a phone, it just
    # gets scaled down. The panels are drawn client-side from specs embedded
    # in dash_content (trajectory_panel_spec()), no output per panel.
    trajectory_hist <- reactive(ability_trajectory_data(ability_history()))
    # Cached: the trajectory only changes with a new estimate, while
    # dash_content re-renders after every answer (building the 7 specs takes
    # ~0.1 s)
    trajectory_ui <- reactive(trajectory_section(trajectory_hist(), session$ns))
  })
}
