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
mod_progress_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    #plotOutput(ns("feedback_plot")),
    div(style = "padding-bottom: 100px; padding-left: 30px; padding-right: 0px;", plotOutput(ns("feedback_plot"))),
    plotOutput(ns("comparison_plot"))
  )
}

#' progress_dashboard Server Functions
#'
#' @noRd
mod_progress_dashboard_server <- function(id, feedback_data, bearbeitet, all_data) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Render feedback lollipop plot
    output$feedback_plot <- renderPlot({

      # Lolliplot alone - thematisch sortiert
      p1 <- ggplot(feedback_data, aes(x=Lerneinheit, y=theta, color=Lerneinheit)) +
        geom_abline(intercept = 0, slope = 0) +
        geom_segment(aes(x=Lerneinheit ,xend=Lerneinheit, y=0, yend=theta), color="darkgrey") +
        geom_point(size=8) +
        scale_x_discrete(limits = levels(feedback_data$Lerneinheit)) +
        scale_y_continuous(limits = c(-3,+3), n.breaks = 3, labels = c(":(", "Durchschnitt", ":)")) +
        scale_color_goethe() +
        theme_classic() +
        theme(
          panel.grid.minor.y = element_line(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size=20),
          axis.text.x = element_text(color=bearbeitet, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.ticks.y = element_blank(),
          legend.position = "none"  # No legend for this plot
        ) +
        ylab("") +
        xlab("")

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
      #(plot_grid(p2, p1, rel_widths = c(0.25, 1)))
      p1
    })



    # Render comparison lollipop plot
   output$comparison_plot <- renderPlot({

     # Lollipop compared - thematisch
     p1 <- ggplot(all_data, aes(x=Lerneinheit, y=theta, color=Lerneinheit)) +
       geom_abline(intercept = 0, slope = 0) +
       geom_segment(aes(x=Lerneinheit,xend=Lerneinheit, y=theta_sample, yend=theta), color="grey") +
       geom_point(size=7) +
       geom_point(aes(y=theta_sample, color = Lerneinheit), size=8, alpha=ifelse(is.na(all_data$theta), 0, 0.25)) +
       scale_x_discrete(limits = levels(all_data$Lerneinheit)) +
       scale_y_continuous(limits = c(-3,+3), n.breaks = 3, labels = c(":(", " ", ":)")) +
       scale_color_goethe() +
       theme_classic() +
       theme(
         panel.grid.minor.y = element_line(),
         panel.grid.major.y = element_line(),
         axis.text = element_text(size=16),
         axis.text.x = element_text(color=bearbeitet, angle = 45, hjust = 1),
         axis.ticks.y = element_blank(),
         legend.position = "none"  # No legend for this plot
       ) +
       ylab("") +
       xlab("")

     # Dummy Plot for Legend
     p2 <- ggplot() +
       theme_void() +
       # "Du" point
       annotate("point", x = 1, y = 1.85, size=8, color="darkgrey") +
       annotate("text", x = 1.15, y = 1.85, label="Du", hjust=0, size=5) +
       # "Kurs-Durchschnitt" point
       annotate("point", x = 1, y = 1.5, size=8, color="darkgrey", alpha=0.25) +
       annotate("text", x = 1.15, y = 1.5, label="Kurs", hjust=0, size=5) +

       coord_cartesian(xlim = c(0.75,2), ylim = c(-3,3)) +
       theme(legend.text = element_text(size=15),
             legend.position = c(.25, .75))


     # Combine Main Plot with Extracted Legend
#     (combined_plot_new <- plot_grid(p2, p1, rel_widths = c(.25, 1)))

   })


  })
}

## To be copied in the UI
# mod_progress_dashboard_ui("progress_dashboard_1")

## To be copied in the server
# mod_progress_dashboard_server("progress_dashboard_1")
