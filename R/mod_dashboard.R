# ── Helpers ───────────────────────────────────────────────────────────────────

competency_label <- function(theta, n_unique) {
  if (n_unique == 0L || is.na(theta)) {
    return(list(
      label = "Keine Daten",
      bg = "#e9ecef",
      fg = "#6c757d",
      level = 0L
    ))
  }
  if (theta > IRT_THETA_HIGH) {
    list(label = "Stark", bg = "#d1ecf1", fg = "#00618f", level = 4L)
  } else if (theta > IRT_THETA_MED) {
    list(label = "Gut entwickelt", bg = "#d4edda", fg = "#1a6b3c", level = 3L)
  } else if (theta > IRT_THETA_LOW) {
    list(label = "Entwickelt sich", bg = "#fff3cd", fg = "#856404", level = 2L)
  } else {
    list(label = "Übungsbedarf", bg = "#fce4ec", fg = "#D81B60", level = 1L)
  }
}

evidence_label <- function(n_unique) {
  if (n_unique >= EVIDENCE_HIGH) {
    list(label = "Hoch", dots = 3L, color = "#198754")
  } else if (n_unique >= EVIDENCE_MED) {
    list(label = "Mittel", dots = 2L, color = "#fd7e14")
  } else if (n_unique >= 1L) {
    list(label = "Niedrig", dots = 1L, color = "#dc3545")
  } else {
    list(label = "–", dots = 0L, color = "#adb5bd")
  }
}

evidence_dots <- function(n_unique) {
  ev <- evidence_label(n_unique)
  filled <- strrep("●", ev$dots)
  empty <- strrep("○", 3L - ev$dots)
  tags$span(
    style = sprintf("color:%s; font-size:0.8em; letter-spacing:2px;", ev$color),
    paste0(filled, empty)
  )
}

pct_bar <- function(pct, color = PRIMARY_COLOR) {
  if (is.na(pct)) {
    return(tags$span(class = "text-muted small", "–"))
  }
  tags$div(
    class = "d-flex align-items-center gap-2",
    tags$div(
      class = "progress flex-grow-1",
      style = "height:6px;",
      tags$div(
        class = "progress-bar",
        style = sprintf("width:%d%%;background:%s;", round(pct * 100), color)
      )
    ),
    tags$span(class = "small text-muted", sprintf("%d%%", round(pct * 100)))
  )
}

rolling_mean_k <- function(x, k = 10L) {
  n <- length(x)
  vapply(
    seq_len(n),
    function(i) {
      mean(x[max(1L, i - k + 1L):i], na.rm = TRUE)
    },
    numeric(1)
  )
}

recommend_next <- function(comp, n_unique_vec) {
  recs <- lapply(LEARNING_AREA_LEVELS, function(area) {
    n <- n_unique_vec[[area]]
    theta <- comp$theta[comp$learning_area == area]
    if (is.na(n) || n == 0L) {
      list(
        area = area,
        priority = 10L,
        reason = "Noch keine Aufgaben bearbeitet"
      )
    } else if (n < EVIDENCE_MED) {
      list(
        area = area,
        priority = 20L,
        reason = sprintf(
          "Zu wenig Daten (%d Aufgabe%s) — mehr bearbeiten",
          n,
          if (n == 1L) "" else "n"
        )
      )
    } else if (!is.na(theta) && theta < IRT_THETA_LOW) {
      list(
        area = area,
        priority = as.integer(30L + round(theta * -10L)),
        reason = "Gezielte Praxis empfohlen"
      )
    } else {
      NULL
    }
  })
  recs <- Filter(Negate(is.null), recs)
  recs[order(vapply(recs, `[[`, integer(1), "priority"))]
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
        n_items = ifelse(is.na(ab$n_items[idx]), 0L, as.integer(ab$n_items[idx])),
        stringsAsFactors = FALSE
      )
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

      # Unique items per learning area (for evidence strength + recommendations)
      n_unique_area <- setNames(
        vapply(
          LEARNING_AREA_LEVELS,
          function(a) {
            sum(fa$learning_area == a, na.rm = TRUE)
          },
          integer(1)
        ),
        LEARNING_AREA_LEVELS
      )

      # Per-area accuracy split by item type
      area_type_acc <- lapply(LEARNING_AREA_LEVELS, function(a) {
        rows <- merge(
          fa[
            !is.na(fa$learning_area) &
              fa$learning_area == a &
              !is.na(fa$bool_correct),
          ],
          data_item[, c("id_item", "type_item")],
          by = "id_item",
          all.x = TRUE
        )
        content <- rows[!is.na(rows$type_item) & rows$type_item == "content", ]
        coding <- rows[!is.na(rows$type_item) & rows$type_item == "coding", ]
        list(
          content_acc = if (nrow(content) > 0L) {
            mean(content$bool_correct, na.rm = TRUE)
          } else {
            NA_real_
          },
          coding_acc = if (nrow(coding) > 0L) {
            mean(coding$bool_correct, na.rm = TRUE)
          } else {
            NA_real_
          },
          n_content = nrow(content),
          n_coding = nrow(coding)
        )
      })
      names(area_type_acc) <- LEARNING_AREA_LEVELS

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

      # ── Recommendations ───────────────────────────────────────────────────────
      recs <- if (!is.null(comp)) {
        recommend_next(comp, n_unique_area)
      } else {
        list()
      }

      # ── Rolling accuracy data ─────────────────────────────────────────────────
      ud_ord <- ud[order(ud$id_datetime), ]
      ud_ord <- ud_ord[!is.na(ud_ord$bool_correct), ]

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

          # Competency map
          div(
            class = "col-lg-7",
            bslib::card(
              height = "100%",
              bslib::card_header(
                div(
                  class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("map"),
                  tags$b("Kompetenzprofil"),
                  tags$span(
                    class = "text-muted small ms-1",
                    "(●●● = hohe Evidenz)"
                  )
                )
              ),
              bslib::card_body(
                class = "p-0",
                div(
                  class = "d-flex justify-content-between align-items-center p-2 border-bottom gap-2",
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
                ),
                tags$table(
                  class = "table table-sm table-hover mb-0 dashboard-comp-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Themenbereich"),
                      tags$th("Kompetenz"),
                      tags$th("Evidenz"),
                      tags$th("Inhaltlich"),
                      tags$th("R-Code")
                    )
                  ),
                  tags$tbody(
                    lapply(LEARNING_AREA_LEVELS, function(area) {
                      n_a <- n_unique_area[[area]]
                      theta <- if (!is.null(comp)) {
                        comp$theta[comp$learning_area == area]
                      } else {
                        NA_real_
                      }
                      cl <- competency_label(theta, n_a)
                      at <- area_type_acc[[area]]

                      short_area <- names(LEARNING_AREA_LABELS)[
                        LEARNING_AREA_LABELS == area
                      ]
                      if (length(short_area) == 0L) {
                        short_area <- area
                      }

                      tags$tr(
                        tags$td(class = "fw-semibold", short_area),
                        tags$td(
                          tags$span(
                            class = "badge rounded-pill",
                            style = sprintf(
                              "background:%s;color:%s;font-weight:500;",
                              cl$bg,
                              cl$fg
                            ),
                            cl$label
                          )
                        ),
                        tags$td(evidence_dots(n_a)),
                        tags$td(pct_bar(at$content_acc, "#285f8a")),
                        tags$td(pct_bar(at$coding_acc, "#5a3e8a"))
                      )
                    })
                  )
                )
              )
            )
          ),

          # Recommendations
          div(
            class = "col-lg-5",
            bslib::card(
              height = "100%",
              bslib::card_header(
                div(
                  class = "d-flex align-items-center gap-2",
                  bsicons::bs_icon("arrow-right-circle-fill"),
                  tags$b("Empfohlene nächste Schritte")
                )
              ),
              bslib::card_body(
                if (length(recs) == 0L) {
                  div(
                    class = "text-muted",
                    bsicons::bs_icon("stars"),
                    " Alle Bereiche gut abgedeckt — weiter so!"
                  )
                } else {
                  tagList(lapply(seq_along(recs), function(i) {
                    r <- recs[[i]]
                    short <- names(LEARNING_AREA_LABELS)[
                      LEARNING_AREA_LABELS == r$area
                    ]
                    if (length(short) == 0L) {
                      short <- r$area
                    }
                    div(
                      class = "rec-item d-flex gap-3 mb-3",
                      div(class = "rec-num", i),
                      div(
                        div(class = "fw-semibold", short),
                        div(class = "text-muted small", r$reason)
                      )
                    )
                  }))
                }
              )
            )
          )
        ),

        # ── Learning curve ────────────────────────────────────────────────────
        if (nrow(ud_ord) >= 3L) {
          bslib::card(
            class = "mb-4",
            bslib::card_header(
              div(
                class = "d-flex align-items-center gap-2",
                bsicons::bs_icon("graph-up-arrow"),
                tags$b("Lernkurve"),
                tags$span(
                  class = "text-muted small ms-1",
                  "(gleitender Durchschnitt, Fenstergröße 10)"
                )
              )
            ),
            bslib::card_body(
              plotOutput(session$ns("learning_curve_plot"), height = "220px")
            )
          )
        },

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

    # ── Learning curve plot ───────────────────────────────────────────────────
    output$learning_curve_plot <- renderPlot(
      {
        ud <- user_data()
        ud <- ud[order(ud$id_datetime), ]
        ud <- ud[!is.na(ud$bool_correct), ]
        req(nrow(ud) >= 3L)

        ud$item_num <- seq_len(nrow(ud))
        ud$roll_acc <- rolling_mean_k(as.numeric(ud$bool_correct), k = 10L)
        ud$area_short <- ifelse(
          ud$learning_area %in% LEARNING_AREA_LEVELS,
          names(LEARNING_AREA_LABELS)[match(
            ud$learning_area,
            LEARNING_AREA_LABELS
          )],
          ud$learning_area
        )

        ggplot2::ggplot(ud, ggplot2::aes(x = item_num, y = roll_acc)) +
          ggplot2::geom_line(colour = PRIMARY_COLOR, linewidth = 1) +
          ggplot2::geom_point(
            ggplot2::aes(colour = area_short),
            size = 2,
            alpha = 0.6,
            show.legend = TRUE
          ) +
          ggplot2::geom_hline(
            yintercept = 0.5,
            linetype = "dashed",
            colour = "grey60",
            linewidth = 0.4
          ) +
          ggplot2::scale_y_continuous(
            labels = \(x) paste(x * 100, "%", sep = ""),
            limits = c(0, 1),
            expand = c(0.02, 0)
          ) +
          ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL) +
          ggplot2::labs(x = "Aufgabe Nr.", y = "Korrektrate (gleitend)") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            legend.position = "bottom",
            legend.text = ggplot2::element_text(size = 9),
            panel.grid.minor = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(4, 8, 4, 4)
          )
      },
      res = 96
    )
  })
}
