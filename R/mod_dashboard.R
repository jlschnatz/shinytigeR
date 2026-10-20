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
    title = desc,
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
    title = desc,
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
  ab$date <- as.Date(ab$time)
  ab[order(ab$learning_area, ab$computed_at), ]
}

# y range of the trajectory panels: THETA_RANGE plus a little room, so markers
# at the bounds (an all-correct or all-wrong start) aren't cut in half
TRAJECTORY_Y_RANGE <- THETA_RANGE + c(-0.25, 0.25)

# Background of every trajectory panel: one band per COMPETENCY_SCALE level
# (the outer ones run to the panel edge) in its scale color, so a point reads
# as "in which competency zone", the same scale as the strips in "Dein
# Lernstand".
competency_band_shapes <- function(opacity = 0.25) {
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
# first and last points aren't cut off at the panel edge.
trajectory_x_range <- function(hist) {
  r <- range(hist$time)
  pad <- max(as.numeric(diff(r), units = "secs") * 0.06, 12 * 3600)
  r + c(-pad, pad)
}

# One small-multiples panel: the trajectory of a single learning area (`d`,
# rows of ability_trajectory_data() for that area) on the full theta range.
# Markers show the saved estimates (hover: date + competency label), joined by
# a plain line. Not smoothed: a lowess line visibly missed some of the points,
# which read as an error; damping the jumps is the job of the displayed
# estimate itself, not of the plot. No uncertainty is drawn here on purpose;
# intervals belong to the detailed explanation of the current estimate.
trajectory_panel_fig <- function(d, x_range) {
  d <- d[order(d$time), ]
  lbl <- COMPETENCY_SCALE$label[findInterval(d$theta, COMPETENCY_SCALE$lower)]
  hover <- sprintf(
    "%s<br><b>%s</b> (\u03b8 = %s)",
    format_date_de(d$date), lbl,
    formatC(d$theta, format = "f", digits = 2, decimal.mark = ",")
  )
  axis <- list(
    title = "", fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE,
    showline = TRUE, linecolor = "#c8ccd1", mirror = TRUE
  )
  plotly::plot_ly() |>
    plotly::add_lines(
      x = d$time, y = d$theta, hoverinfo = "skip",
      line = list(color = TRAJECTORY_COLOR, width = 2)
    ) |>
    plotly::add_markers(
      x = d$time, y = d$theta, text = hover, hovertemplate = "%{text}<extra></extra>",
      marker = list(color = TRAJECTORY_COLOR, size = 7, line = list(color = "white", width = 1))
    ) |>
    plotly::layout(
      shapes = competency_band_shapes(),
      showlegend = FALSE,
      xaxis = c(axis, list(type = "date", range = x_range, tickformat = "%d.%m.", nticks = 4)),
      yaxis = c(axis, list(range = TRAJECTORY_Y_RANGE, showticklabels = FALSE, ticks = "")),
      margin = list(l = 4, r = 4, t = 4, b = 4),
      dragmode = FALSE,
      plot_bgcolor = "white",
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE, scrollZoom = FALSE)
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

# The "Fähigkeitsverlauf" accordion panel: one small chart per learning area
# in a wrapping grid of at most 4 columns (see .traj-grid in app.css: 4 on a
# laptop, 2 on a tablet, 1 on a phone).
# The legend is the grid's 8th tile, so with 7 areas it fills the otherwise
# empty last cell at 4 and 2 columns; when the grid is down to 1 column it
# would end up below all charts, so a CSS container query swaps it for a row
# above the grid instead. `hist` is ability_trajectory_data().
trajectory_section <- function(hist, ns) {
  n_batches <- length(unique(hist$computed_at))
  body <- if (n_batches < 2L) {
    tags$p(
      class = "text-muted mb-0",
      "Dein Verlauf erscheint hier, sobald es mindestens zwei Schätzungen deines ",
      "Lernstands gibt (höchstens eine neue pro Sitzung)."
    )
  } else {
    panels <- lapply(seq_along(LEARNING_AREA_LEVELS), function(k) {
      area <- LEARNING_AREA_LEVELS[k]
      short <- names(LEARNING_AREA_LABELS)[match(area, LEARNING_AREA_LABELS)]
      has_data <- any(hist$learning_area == area)
      div(
        class = "traj-panel",
        div(class = "traj-panel-title", title = area, short),
        if (has_data) {
          plotly::plotlyOutput(ns(paste0("traj_", k)), height = "170px")
        } else {
          div(class = "traj-empty", "Noch keine Schätzung")
        }
      )
    })
    div(
      class = "traj-wrap",
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

mod_dashboard_server <- function(id, data_item, credentials, write_trigger, ability_computed_this_session) {
  moduleServer(id, function(input, output, session) {
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
      ud$date <- as.Date(as.POSIXct(ud$id_datetime, origin = "1970-01-01"))
      ud
    })

    # First attempts per item feed the descriptive ("Erstversuche") stats below
    # — this is unrelated to the ability estimate, which uses the *latest*
    # attempt per item instead (see compute_and_save_ability() in R/irt.R).
    first_attempts <- reactive({
      ud <- user_data()
      if (nrow(ud) == 0L) {
        return(ud)
      }
      ud[!duplicated(ud[, c("id_user", "id_item")]), ]
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

    # The persisted ability snapshot (most recent computed_at batch), reshaped
    # to the same learning_area/theta/n_items shape estimate_competency()
    # returns, so nothing downstream needs to change.
    competency <- reactive({
      ability_version()
      ab <- db_get_ability(credentials()$info$user_name)
      if (nrow(ab) == 0L) {
        return(NULL)
      }
      latest_batch <- max(ab$computed_at, na.rm = TRUE)
      ab <- ab[ab$computed_at == latest_batch, ]
      idx <- match(LEARNING_AREA_LEVELS, ab$learning_area)
      data.frame(
        learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
        theta = ab$theta[idx],
        # NA for snapshots saved before the standard error was stored
        se = if ("se" %in% names(ab)) as.numeric(ab$se[idx]) else NA_real_,
        n_items = ifelse(is.na(ab$n_items[idx]), 0L, as.integer(ab$n_items[idx])),
        computed_at = latest_batch,
        stringsAsFactors = FALSE
      )
    })

    # Full ability history (every saved batch, not just the latest) — feeds
    # the "Fähigkeitsverlauf" trajectory plot below.
    ability_history <- reactive({
      ability_version()
      db_get_ability(credentials()$info$user_name)
    })

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
      fa <- first_attempts()
      comp <- competency()
      can_refresh <- can_refresh_ability()

      # ── Aggregate stats ──────────────────────────────────────────────────────
      n_total <- nrow(ud)
      n_unique <- nrow(fa)
      n_correct <- sum(fa$bool_correct == TRUE, na.rm = TRUE)
      n_skipped <- sum(ud$skipped == TRUE, na.rm = TRUE)
      pct_correct <- if (n_unique > 0L) {
        round(100 * n_correct / n_unique)
      } else {
        0L
      }

      all_dates <- unique(ud$date)
      n_days <- length(all_dates)
      week_start <- Sys.Date() - (as.integer(format(Sys.Date(), "%u")) - 1L)
      n_this_week <- length(unique(ud$date[ud$date >= week_start]))
      n_repeated <- n_total - n_unique

      # Bloom accuracy
      bloom_lvls <- c("knowledge", "comprehension", "application")
      bloom_acc <- setNames(
        vapply(
          bloom_lvls,
          function(b) {
            rows <- merge(
              fa[!is.na(fa$bool_correct), ],
              data_item[, c("id_item", "bloom_taxonomy")],
              by = "id_item",
              all.x = TRUE
            )
            sub <- rows[
              !is.na(rows$bloom_taxonomy) & rows$bloom_taxonomy == b,
            ]
            if (nrow(sub) == 0L) {
              NA_real_
            } else {
              mean(sub$bool_correct, na.rm = TRUE)
            }
          },
          numeric(1)
        ),
        bloom_lvls
      )

      tagList(
        # ── Value boxes ──────────────────────────────────────────────────────────
        div(
          class = "row g-3 mb-4",
          div(
            class = "col-sm-4",
            bslib::value_box(
              title = "Geübt an",
              value = sprintf(
                "%d Tag%s",
                n_days,
                if (n_days == 1) "" else "en"
              ),
              showcase = bsicons::bs_icon("calendar-check"),
              theme = "primary"
            )
          ),
          div(
            class = "col-sm-4",
            bslib::value_box(
              title = "Einmalige Aufgaben",
              value = n_unique,
              showcase = bsicons::bs_icon("list-check"),
              theme = bslib::value_box_theme(bg = "#285f8a", fg = "white")
            )
          ),
          div(
            class = "col-sm-4",
            bslib::value_box(
              title = "Korrekt (Erstversuche)",
              value = sprintf("%d%%", pct_correct),
              showcase = bsicons::bs_icon("check-circle-fill"),
              theme = bslib::value_box_theme(bg = "#00618f", fg = "white")
            )
          )
        ),

        # ── Two-column: competency map + recommendations ──────────────────────
        div(
          class = "row g-3 mb-4",

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
                    format_date_de(as.Date(as.POSIXct(comp$computed_at[1], origin = "1970-01-01")))
                  )
                }
              ),
              bslib::card_body(
                class = "p-0",
                tags$table(
                  class = "table mb-0 dashboard-comp-table",
                  tags$thead(
                    tags$tr(
                      tags$th(tags$span(class = "visually-hidden", "Themenbereich")),
                      tags$th(
                        title = "Wie sicher die Einstufung ist (●●● = wahrscheinlich zutreffend)",
                        "Evidenz"
                      ),
                      tags$th("Kompetenz")
                    )
                  ),
                  tags$tbody(
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
                        tags$td(class = "comp-area", area),
                        tags$td(certainty_dots(theta, se, n_a, cl)),
                        tags$td(competency_scale_bar(cl))
                      )
                    })
                  )
                )
              ),
              bslib::card_footer(
                class = "d-flex justify-content-between align-items-center gap-2",
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

        # ── Ability trajectory (expandable, open by default) ──────────────────
        div(
          class = "mb-4",
          bslib::accordion(
            id = session$ns("dash_sections"),
            open = "trajectory",
            multiple = TRUE,
            trajectory_section(trajectory_hist(), session$ns)
          )
        ),

        # ── Bloom breakdown + practice behaviour ─────────────────────────────
        div(
          class = "row g-3 mb-4",

          # Bloom breakdown
          div(
            class = "col-md-5",
            bslib::card(
              height = "100%",
              bslib::card_header(
                div(
                  class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("layers"),
                  tags$b("Aufgabentyp (Bloom)")
                )
              ),
              bslib::card_body(
                tagList(lapply(bloom_lvls, function(b) {
                  lbl <- switch(
                    b,
                    knowledge = "Wissen",
                    comprehension = "Verständnis",
                    application = "Anwendung"
                  )
                  acc <- bloom_acc[[b]]
                  div(
                    class = "mb-3",
                    div(
                      class = "d-flex justify-content-between mb-1",
                      tags$span(class = "small fw-semibold", lbl),
                      tags$span(
                        class = "small text-muted",
                        if (is.na(acc)) {
                          "Keine Daten"
                        } else {
                          sprintf("%d%%", round(acc * 100))
                        }
                      )
                    ),
                    if (!is.na(acc)) {
                      div(
                        class = "progress",
                        style = "height:8px;",
                        div(
                          class = "progress-bar",
                          style = sprintf(
                            "width:%d%%;background:%s;",
                            round(acc * 100),
                            PRIMARY_COLOR
                          )
                        )
                      )
                    } else {
                      div(
                        class = "progress",
                        style = "height:8px;",
                        div(
                          class = "progress-bar",
                          style = "width:0%;background:#dee2e6;"
                        )
                      )
                    }
                  )
                }))
              )
            )
          ),

          # Practice behaviour
          div(
            class = "col-md-7",
            bslib::card(
              height = "100%",
              bslib::card_header(
                div(
                  class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("calendar3"),
                  tags$b("Übungsverhalten")
                )
              ),
              bslib::card_body(
                div(
                  class = "row g-2 mb-3",
                  div(
                    class = "col-6",
                    div(
                      class = "practice-stat",
                      div(class = "practice-stat-val", n_days),
                      div(
                        class = "practice-stat-lbl",
                        sprintf(
                          "Aktive Tag%s gesamt",
                          if (n_days == 1) "" else "e"
                        )
                      )
                    )
                  ),
                  div(
                    class = "col-6",
                    div(
                      class = "practice-stat",
                      div(class = "practice-stat-val", n_this_week),
                      div(
                        class = "practice-stat-lbl",
                        sprintf(
                          "Tag%s diese Woche",
                          if (n_this_week == 1) "" else "e"
                        )
                      )
                    )
                  ),
                  div(
                    class = "col-6",
                    div(
                      class = "practice-stat",
                      div(class = "practice-stat-val", n_unique),
                      div(class = "practice-stat-lbl", "Einmalige Aufgaben")
                    )
                  ),
                  div(
                    class = "col-6",
                    div(
                      class = "practice-stat",
                      div(class = "practice-stat-val", n_repeated),
                      div(class = "practice-stat-lbl", "Wiederholungen")
                    )
                  )
                ),
                if (n_repeated > 0L) {
                  pct_new <- round(100 * n_unique / n_total)
                  div(
                    class = "mt-2",
                    div(
                      class = "d-flex justify-content-between small text-muted mb-1",
                      span("Neue Aufgaben"),
                      span(sprintf("%d%%", pct_new))
                    ),
                    div(
                      class = "progress",
                      style = "height:8px;",
                      div(
                        class = "progress-bar",
                        style = sprintf(
                          "width:%d%%;background:#285f8a;",
                          pct_new
                        )
                      ),
                      div(
                        class = "progress-bar",
                        style = sprintf(
                          "width:%d%%;background:#b0c9dc;",
                          100 - pct_new
                        )
                      )
                    ),
                    div(
                      class = "small text-muted mt-1",
                      sprintf("%d%% Wiederholungen — ", 100 - pct_new),
                      if (n_repeated > n_unique * 0.5) {
                        "Versuche mehr neue Aufgaben für stärkere Lerneffekte."
                      } else {
                        "Gute Mischung aus neuen und wiederholten Aufgaben."
                      }
                    )
                  )
                }
              )
            )
          )
        )
      )
    })

    # ── Ability trajectory: one small chart per learning area ────────────────
    # Small multiples instead of one chart with 7 crossing lines: each panel
    # stays readable without a color per area, and the panels wrap with the
    # screen width (see trajectory_section()). Still plotly rather than
    # renderPlot: a server-rendered image doesn't reflow on a phone, it just
    # gets scaled down.
    trajectory_hist <- reactive(ability_trajectory_data(ability_history()))

    lapply(seq_along(LEARNING_AREA_LEVELS), function(k) {
      output[[paste0("traj_", k)]] <- plotly::renderPlotly({
        hist <- trajectory_hist()
        d <- hist[hist$learning_area == LEARNING_AREA_LEVELS[k], ]
        req(nrow(d) > 0L)
        trajectory_panel_fig(d, trajectory_x_range(hist))
      })
    })
  })
}
