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
      div(style = "padding-top: 8px; padding-bottom: 30px;")
    ),
    uiOutput(ns("combine"))
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

      current_year <- format(Sys.Date(), "%Y")
      this_xmas <- as.Date(paste0(current_year, "-12-24"))
      today <- Sys.Date()
      length(seq(from = today, to = this_xmas, by = "day")) - 1

    })

    value_box_list <- list(
      bslib::value_box(
        title = "Heute bearbeitete Aufgaben",
        value = textOutput(ns("todays_practice")), 
        shiny::markdown("Super, weiter so!"),
        theme = bslib::value_box_theme(bg = "#860047", fg = "#FFFFFF"),
        showcase = bsicons::bs_icon("emoji-smile", style = "font-size: 40px; color: white"),
        #showcase_layout = "left center",
        full_screen = FALSE, fill = F, height = "170px"
      ),
      bslib::value_box(
        title = "Insgesamt bearbeitete Aufgaben", value = textOutput(ns("total_practice")),
        theme = bslib::value_box_theme(bg = "#C96215", fg = "#FFFFFF"),
        showcase = bsicons::bs_icon("bar-chart-fill", style = "font-size: 40px; color: white"),  
        #showcase_layout = "left center",
        full_screen = FALSE, fill = F, height = "170px"
      ),
      bslib::value_box(
        title = "Schon", value = textOutput(ns("session_practice")),
        shiny::markdown("mal warst du auf tigeR aktiv"),
        theme = bslib::value_box_theme(bg = "#737C45", fg = "#FFFFFF"),
        showcase = bsicons::bs_icon("calendar4-week", style = "font-size: 40px; color: white"),  
        #showcase_layout = "left center",
        full_screen = FALSE, fill = F, height = "170px"
      ),
      bslib::value_box(
        title = "Noch", value = textOutput(ns("xmas_countdown")),
        shiny::markdown("Tage bis Weihnachten"),
        theme = bslib::value_box_theme(bg = "#B3062C", fg = "#FFFFFF"),
        showcase = fontawesome::fa_i("candy-cane", style = "font-size: 40px; color: white"),  
        #showcase_layout = "left center",
        full_screen = FALSE, fill = F, height = "170px"
      )
    )

    output$combine <- renderUI({
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
            bslib::layout_column_wrap(
          !!!value_box_list,
          width = 1,
          fill = FALSE, 
        ),
          bslib::card(
            height = 700,
            full_screen = TRUE,
            bslib::card_header(tags$b("Individuelle Fähigkeitsschätzungen in 8 Lernbereichen")),
            bslib::card_body(
              plotOutput(ns("plot1"), width = "80%"), class = "align-items-center"
            ),
            bslib::card_body(
              bslib::card_title("Erklärung zur Abbildung"),
              p(class = "text-muted", "In dieser Abbildung siehst du deine geschätzten Fähigkeitsskills aufgeteilt nach den 8
                verschiedenen Lernbereichen. Die Schätzung basiert auf einem psychometrischen Modell, das anhand einer Stichprobe
                von mehreren hundert Teilnehmenden erstellt wurde. Der Durchschnitt stellt dabei den Durchschnitt dieser Stichprobe
                dar. In die Schätzung der Fähigkeit geht dabei auch mit ein, wie schwierig eine Aufgabe ist."),
            )
          ),
          col_widths = c(3, 9)
        )
      }
    })

    output$plot1 <- renderPlot({
      sysfonts::font_add_google("Karla", "Karla")
      showtext::showtext_auto()
      p1 <- all_data |>
        dplyr::mutate(Lerneinheit = factor(Lerneinheit, levels = c("Deskriptivstatistik", "Wahrscheinlichkeit", "Grundlagen der Inferenzstatistik", "Gruppentests", "Poweranalyse", "Zusammenhangsmaße", "Regression"))) |>
        ggplot(
        data = _,
        aes(x = Lerneinheit, y = theta, color = Lerneinheit)
      ) +
        geom_abline(intercept = 0, slope = 0) +
        geom_segment(aes(x = Lerneinheit, xend = Lerneinheit, y = 0, yend = theta), color = "darkgrey") +
        geom_point(size = 8) +
        scale_x_discrete(limits = levels(feedback_data$Lerneinheit)) +
        scale_y_continuous(limits = c(-3, +3), n.breaks = 3, labels = c(":(", "Durchschnitt", ":)")) +
        scale_color_goethe() +
        theme_minimal() +
        theme(
          panel.grid.minor.y = element_line(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 18, family = "Karla"),
          axis.text.x = suppressWarnings(element_text(
            #color = bearbeitet, 
            angle = 45,
             hjust = 1
             )),
          axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.ticks.y = element_blank(),
          text = element_text(family = "Karla"),
          legend.position = "none" # No legend for this plot
        ) +
            theme(
       axis.line.x      = element_line(colour = "grey80"),
       axis.line.y      = element_line(colour = "grey80"),
       axis.text        = element_text(colour = "grey50"),
       axis.title       = element_text(colour = "grey30"),
       strip.background = element_rect(colour = "grey70", fill = "grey90"),
       strip.text       = element_text(colour = "grey30"),
       legend.title     = element_text(colour = "grey30"),
       legend.text      = element_text(colour = "grey30")
     ) +
        ylab("") +
        xlab("")
      suppressWarnings(print(p1))
    })

  })
}