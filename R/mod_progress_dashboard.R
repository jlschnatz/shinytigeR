#' progress_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import ggplot2 cowplot

mod_progress_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("Fortschritt"),
      tags$a("Hier erfährst du mehr über deinen bisherigen Fortschritt in tigeR."),
      tags$a("Im Laufe der Zeit wird das Feedback immer ausführlicher werden."),
      tags$a("Es lohnt sich also, regelmäßig vorbeizuschauen! :)"),
      div(style = "padding-top: 8px; padding-bottom: 30px;"),
      uiOutput(ns("response_analysis")),
      uiOutput(ns("feedback_plot"))
    )
  )
}

#' progress_dashboard Server Functions
#'
#' @noRd
mod_progress_dashboard_server <- function(id, feedback_data, bearbeitet, all_data, credentials, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ##### Response Analysis ----

    # Analyze today's practice
    output$todays_practice <- renderText({
      sum(user_data()$id_date == Sys.Date())
    })

    # Analyze overall total practice
    output$total_practice <- renderText({
      length(user_data()$id_user)
    })

    # Analyze number of sessions
    output$session_practice <- renderText({
      length(unique(user_data()$id_session))
    })

    # Christmas countdown
    output$xmas_countdown <- renderText({
      if (format(Sys.Date(), "%Y") == 2023) {
        xmas <- as.Date("2023-12-24")
      } else {
        xmas <- as.Date("2024-12-24")
      }

      today <- Sys.Date()
      length(seq(from = today, to = xmas, by = "day")) - 1
    })

    output$response_analysis <- renderUI({
      # check if user_name exists in db_user database
      if (isFALSE(db_check_userexists(as.character(credentials()$info$user_name)))) {
        fluidRow(
          col_1(),
          col_10(
            bslib::card(
              bslib::card_header(tags$h5(tags$b("Achtung!")), class = "bg-danger text-white"),
              bslib::card_body(
                HTML("Bitte schau nochmal vorbei, wenn du die ersten Aufgaben bearbeitet hast. Vorher kann kein Dashboard angezeigt werden")
              )
            )
          ),
          col_1()
        )
      } else {
        bslib::layout_columns(
          bslib::value_box(
            title = "Heute bearbeitete Aufgaben", value = textOutput(ns("todays_practice")),
            shiny::markdown("Super, weiter so!"),
            # theme = bslib::value_box_theme(bg = "#860047", fg = "#FFFFFF"),
            style = "background-color: #860047!important; padding-left: 10px;",
            showcase = bsicons::bs_icon("emoji-smile", style = "font-size: 45px; color: white"),
            # showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = "170px"
          ),
          bslib::value_box(
            title = "Insgesamt bearbeitete Aufgaben", value = textOutput(ns("total_practice")),
            # theme = value_box_theme(bg = "#FFFFFF", fg = "#C96215"),
            style = "background-color: #C96215!important;",
            showcase = bsicons::bs_icon("bar-chart-fill", style = "font-size: 45px; color: white"), # showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = "170px"
          ),
          bslib::value_box(
            title = "Schon", value = textOutput(ns("session_practice")), # tags$span(textOutput(ns("session_practice"))," mal"),
            shiny::markdown("mal warst du auf tigeR aktiv"),
            #  theme = value_box_theme(bg = "#737C45", fg = "#000000"),
            style = "background-color: #737C45!important; padding-right: 10px;",
            showcase = bsicons::bs_icon("calendar4-week", style = "font-size: 45px; color: white"), # showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = "170px"
          ),
          bslib::value_box(
            title = "Noch", value = textOutput(ns("xmas_countdown")),
            shiny::markdown("Tage bis Weihnachten"),
            # theme = value_box_theme(bg = "#FFFFFF", fg = "#B3062C"),
            style = "background-color: #B3062C!important;",
            showcase = fontawesome::fa_i("candy-cane", style = "font-size: 45px; color: white"), # showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = "170px"
          )
        )
      }
    })

    output$plot1 <- renderPlot({
      p1 <- ggplot(
        data = all_data,
        aes(x = Lerneinheit, y = theta, color = Lerneinheit)
      ) +
        geom_abline(intercept = 0, slope = 0) +
        geom_segment(aes(x = Lerneinheit, xend = Lerneinheit, y = 0, yend = theta), color = "darkgrey") +
        geom_point(size = 8) +
        # geom_point(aes(y=theta_sample, color = Lerneinheit), size=8, alpha=ifelse(is.na(all_data$theta), 0, 0.25)) +
        scale_x_discrete(limits = levels(feedback_data$Lerneinheit)) +
        scale_y_continuous(limits = c(-3, +3), n.breaks = 3, labels = c(":(", "Durchschnitt", ":)")) +
        scale_color_goethe() +
        theme_classic() +
        theme(
          panel.grid.minor.y = element_line(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 20),
          axis.text.x = suppressWarnings(element_text(color = bearbeitet, angle = 45, hjust = 1)),
          axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.ticks.y = element_blank(),
          legend.position = "none" # No legend for this plot
        ) +
        ylab("") +
        xlab("")
      suppressWarnings(print(p1))
    })


    # Render feedback lollipop plot
    output$feedback_plot <- renderUI({
      if (!isFALSE(db_check_userexists(as.character(credentials()$info$user_name)))) {
        # Lolliplot alone - thematisch sortiert
        # p1 <- ggplot(
        #   data = all_data,
        #   aes(x = Lerneinheit, y = theta, color = Lerneinheit)
        # ) +
        #   geom_abline(intercept = 0, slope = 0) +
        #   geom_segment(aes(x = Lerneinheit, xend = Lerneinheit, y = 0, yend = theta), color = "darkgrey") +
        #   geom_point(size = 8) +
        #   # geom_point(aes(y=theta_sample, color = Lerneinheit), size=8, alpha=ifelse(is.na(all_data$theta), 0, 0.25)) +
        #   scale_x_discrete(limits = levels(feedback_data$Lerneinheit)) +
        #   scale_y_continuous(limits = c(-3, +3), n.breaks = 3, labels = c(":(", "Durchschnitt", ":)")) +
        #   scale_color_goethe() +
        #   theme_classic() +
        #   theme(
        #     panel.grid.minor.y = element_line(),
        #     panel.grid.major.y = element_line(),
        #     panel.grid.minor.x = element_blank(),
        #     panel.grid.major.x = element_blank(),
        #     axis.text = element_text(size = 20),
        #     axis.text.x = suppressWarnings(element_text(color = bearbeitet, angle = 45, hjust = 1)),
        #     axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        #     axis.ticks.y = element_blank(),
        #     legend.position = "none" # No legend for this plot
        #   ) +
        #   ylab("") +
        #   xlab("")

        # p2 <- ggplot(data.frame(x=1, y=1, col="Du"), aes(x, y, color=col)) +
        #   geom_point(size = 7) +
        #   scale_color_manual(values = c("Du" = "darkgrey"), name="", labels = "Du") +
        #   theme_void()+
        #   theme(legend.text = element_text(size=14),
        #         legend.position = c(0.25, 0.75))
        #
        # legend <- get_legend(p2)

        # Dummy Plot for Legend
        # p2 <- ggplot() +
        #   theme_void() +
        #   # "Du" point
        #   annotate("point", x = 1, y = 1.85, size=8, color="darkgrey") +
        #   annotate("text", x = 1.15, y = 1.85, label="Du", hjust=0, size=5) +
        #   # "Kurs-Durchschnitt" point
        #
        #   coord_cartesian(xlim = c(0.75,2), ylim = c(-3,3)) +
        #   theme(legend.text = element_text(size=15),
        #         legend.position = c(.25, .75))
        #
        # (plot_grid(p2, p1, rel_widths = c(0.25, 1)))

        bslib::layout_columns(
          bslib::card(
            height = 700,
            full_screen = TRUE,
            bslib::card_header(tags$b("Individuelle Fähigkeitsschätzungen in 8 Lernbereichen")),
            bslib::card_body(
              plotOutput(ns("plot1"), width = "80%"), class = "align-items-center"
            ),
            bslib::card_body(
              #fill = TRUE, gap = 0,
              bslib::card_title("Erklärung zur Abbildung"),
              p(class = "text-muted", "In dieser Abbildung siehst du deine geschätzten Fähigkeitsskills aufgeteilt nach den 8
                verschiedenen Lernbereichen. Die Schätzung basiert auf einem psychometrischen Modell, das anhand einer Stichprobe
                von mehreren hundert Teilnehmenden erstellt wurde. Der Durchschnitt stellt dabei den Durchschnitt dieser Stichprobe
                dar. In die Schätzung der Fähigkeit geht dabei auch mit ein, wie schwierig eine Aufgabe ist."),
            )
          )
        )
      }
    })

    # Render comparison lollipop plot
    output$comparison_plot <- renderPlot({
      # Lollipop compared - thematisch
      # p1 <- ggplot(all_data, aes(x=Lerneinheit, y=theta, color=Lerneinheit)) +
      #   geom_abline(intercept = 0, slope = 0) +
      #   geom_segment(aes(x=Lerneinheit,xend=Lerneinheit, y=theta_sample, yend=theta), color="grey") +
      #   geom_point(size=7) +
      #   geom_point(aes(y=theta_sample, color = Lerneinheit), size=8, alpha=ifelse(is.na(all_data$theta), 0, 0.25)) +
      #   scale_x_discrete(limits = levels(all_data$Lerneinheit)) +
      #   scale_y_continuous(limits = c(-3,+3), n.breaks = 3, labels = c(":(", " ", ":)")) +
      #   scale_color_goethe() +
      #   theme_classic() +
      #   theme(
      #     panel.grid.minor.y = element_line(),
      #     panel.grid.major.y = element_line(),
      #     axis.text = element_text(size=16),
      #     axis.text.x = element_text(color=bearbeitet, angle = 45, hjust = 1),
      #     axis.ticks.y = element_blank(),
      #     legend.position = "none"  # No legend for this plot
      #   ) +
      #   ylab("") +
      #   xlab("")

      # Dummy Plot for Legend
      # p2 <- ggplot() +
      #   theme_void() +
      #   # "Du" point
      #   annotate("point", x = 1, y = 1.85, size=8, color="darkgrey") +
      #   annotate("text", x = 1.15, y = 1.85, label="Du", hjust=0, size=5) +
      #   # "Kurs-Durchschnitt" point
      #   annotate("point", x = 1, y = 1.5, size=8, color="darkgrey", alpha=0.25) +
      #   annotate("text", x = 1.15, y = 1.5, label="Kurs", hjust=0, size=5) +
      #
      #   coord_cartesian(xlim = c(0.75,2), ylim = c(-3,3)) +
      #   theme(legend.text = element_text(size=15),
      #         legend.position = c(.25, .75))


      # Combine Main Plot with Extracted Legend
      #     (combined_plot_new <- plot_grid(p2, p1, rel_widths = c(.25, 1)))
    })
  })
}

## To be copied in the UI
# mod_progress_dashboard_ui("progress_dashboard_1")

## To be copied in the server
# mod_progress_dashboard_server("progress_dashboard_1")
