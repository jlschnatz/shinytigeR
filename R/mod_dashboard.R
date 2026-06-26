mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "dashboard-wrap mt-4",
    uiOutput(ns("dash_content"))
  )
}

mod_dashboard_server <- function(id, data_item, credentials, write_trigger) {
  moduleServer(id, function(input, output, session) {

    user_exists <- reactive({
      write_trigger()
      uid <- credentials()$info$user_name
      db_user_exists(uid)
    })

    user_data <- reactive({
      write_trigger()
      uid <- credentials()$info$user_name
      db_get_userdata(uid)
    })

    competency <- reactive({
      ud <- user_data()
      if (nrow(ud) == 0L) return(NULL)
      ud$learning_area <- factor(ud$learning_area, levels = LEARNING_AREA_LEVELS)
      estimate_competency(ud, data_item)
    })

    output$dash_content <- renderUI({
      if (!isTRUE(user_exists())) {
        return(div(
          class = "text-center py-5 text-muted",
          bsicons::bs_icon("bar-chart", size = "3em"),
          tags$p(class = "mt-3 fs-5",
            "Noch keine Antworten vorhanden. Bearbeite zuerst einige Aufgaben im Bereich ",
            tags$b("Üben"), "."
          )
        ))
      }

      comp <- competency()
      ud   <- user_data()
      if (is.null(comp)) return(NULL)

      n_total    <- nrow(ud)
      n_correct  <- sum(ud$bool_correct == TRUE, na.rm = TRUE)
      n_skipped  <- sum(ud$skipped == TRUE, na.rm = TRUE)
      pct_correct <- if (n_total > 0) round(100 * n_correct / n_total) else 0L

      tagList(
        # ── Summary value boxes ───────────────────────────────────────────────
        div(class = "row g-3 mb-4",
          div(class = "col-sm-4",
            bslib::value_box(
              title    = "Bearbeitete Aufgaben",
              value    = n_total,
              showcase = bsicons::bs_icon("list-check"),
              theme    = "primary"
            )
          ),
          div(class = "col-sm-4",
            bslib::value_box(
              title    = "Richtig beantwortet",
              value    = sprintf("%d%%", pct_correct),
              showcase = bsicons::bs_icon("check-circle-fill"),
              theme    = bslib::value_box_theme(bg = "#00618f", fg = "white")
            )
          ),
          div(class = "col-sm-4",
            bslib::value_box(
              title    = "Übersprungen",
              value    = n_skipped,
              showcase = bsicons::bs_icon("skip-forward-fill"),
              theme    = bslib::value_box_theme(bg = "#FFA000", fg = "white")
            )
          )
        ),
        # ── Competency plot ───────────────────────────────────────────────────
        bslib::card(
          bslib::card_header(
            div(class = "d-flex align-items-center gap-2",
              bsicons::bs_icon("graph-up"),
              tags$b("Kompetenz nach Themenbereich"),
              tags$span(class = "text-muted small ms-2",
                "(2PL-IRT-Schätzung; θ > 0 bedeutet über Durchschnitt)")
            )
          ),
          bslib::card_body(
            plotOutput(session$ns("competency_plot"), height = "320px")
          )
        ),
        # ── Per-area table ────────────────────────────────────────────────────
        bslib::card(
          class = "mt-3",
          bslib::card_header(tags$b("Aufgaben nach Themenbereich")),
          bslib::card_body(
            tableOutput(session$ns("area_table"))
          )
        )
      )
    })

    output$competency_plot <- renderPlot({
      comp <- competency()
      req(!is.null(comp), any(!is.na(comp$theta)))

      comp$label <- sub("Grundlagen der ", "", as.character(comp$learning_area))
      comp$has_data <- !is.na(comp$theta)

      ggplot2::ggplot(comp, ggplot2::aes(
        x    = factor(label, levels = rev(label)),
        y    = ifelse(has_data, theta, 0),
        fill = has_data
      )) +
        ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = ifelse(has_data,
              sprintf("n=%d  θ=%.2f", n_items, theta),
              sprintf("n=%d", n_items)
            ),
            hjust = ifelse(has_data & theta >= 0, -0.1, 1.1)
          ),
          size = 3.5, colour = "grey30"
        ) +
        ggplot2::scale_fill_manual(values = c(`TRUE` = PRIMARY_COLOR, `FALSE` = "grey80")) +
        ggplot2::scale_y_continuous(limits = c(-3.5, 3.5), expand = c(0, 0)) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Fähigkeitsschätzung θ") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          axis.text.y        = ggplot2::element_text(colour = "grey20"),
          plot.margin        = ggplot2::margin(8, 16, 8, 8)
        )
    }, res = 96)

    output$area_table <- renderTable({
      ud <- user_data()
      req(nrow(ud) > 0L)
      areas <- LEARNING_AREA_LEVELS
      do.call(rbind, lapply(areas, function(a) {
        rows <- ud[!is.na(ud$learning_area) & ud$learning_area == a, ]
        data.frame(
          "Themenbereich"    = a,
          "Gesamt"           = nrow(rows),
          "Richtig"          = sum(rows$bool_correct == TRUE, na.rm = TRUE),
          "Falsch"           = sum(rows$bool_correct == FALSE, na.rm = TRUE),
          "Übersprungen"     = sum(rows$skipped == TRUE, na.rm = TRUE),
          check.names        = FALSE,
          stringsAsFactors   = FALSE
        )
      }))
    }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s",
       width = "100%")
  })
}
