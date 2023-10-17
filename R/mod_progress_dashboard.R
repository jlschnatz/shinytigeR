#' progress_dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_progress_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("feedback_plot")),
    plotOutput(ns("comparison_plot"))
  )
}

#' progress_dashboard Server Functions
#'
#' @noRd
mod_progress_dashboard_server <- function(id, input, output, session, feedback_data, bearbeitet, all_data) {
  moduleServer(id, function(input, output, session, feedback_data, bearbeitet, all_data){
    ns <- session$ns

    # Test
    output$feedback_plot <- renderPlot({
      plot(1:10, 1:10, main = "Test Plot")
    })

#    # Render feedback lollipop plot
#    output$feedback_plot <- renderPlot({
#
#      # Lolliplot alone - thematisch sortiert
#      ggplot(feedback_data, aes(x=Lerneinheit, y=korrekt)) +
#        geom_segment(aes(x=Lerneinheit ,xend=Lerneinheit, y=0, yend=korrekt), color="grey") +
#        geom_point(size=5, color="#69b3a2") +
#        scale_x_discrete(limits = rev(levels(feedback_data$Lerneinheit))) +
#        scale_y_continuous(limits = c(0,1), n.breaks = 3, labels = c("Leider alles \nfalsch :(", " ", "Juhu, alles \nrichtig! :)")) +
#        coord_flip() +
#        theme_minimal() +
#        theme(
#          panel.grid.minor.y = element_blank(),
#          panel.grid.major.y = element_blank(),
#          axis.text = element_text(size=14),
#          axis.text.y = element_text(color=rev(bearbeitet)),
#          title = element_text(size=17),
#          legend.position="none"
#        ) +
#        ggtitle("Dein Feedback") +
#        ylab("") +
#        xlab("")
#
#    })

    # Render comparison lollipop plot
    output$comparison_plot <- renderPlot({

      # Lollipop compared - thematisch
      ggplot(all_data, aes(x=Lerneinheit, y=korrekt, color="Du")) +
        geom_segment(aes(x=Lerneinheit ,xend=Lerneinheit, y=korrekt_sample, yend=korrekt), color="grey") +
        geom_point(size=5) +
        geom_point(aes(y=korrekt_sample, color = "Kurs-Durchschnitt"), size=5, alpha=0.25) +
        scale_x_discrete(limits = rev(levels(all_data$Lerneinheit))) +
        scale_y_continuous(limits = c(0,1), n.breaks = 3, labels = c("Leider alles \nfalsch :(", " ", "Juhu, alles \nrichtig! :)")) +
        scale_color_manual(values = c("#69b3a2", "#69b3a2")) +
        guides(color = guide_legend(override.aes = list(alpha=c(1,0.25)))) +
        coord_flip() +
        theme_minimal() +
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text = element_text(size=14),
          axis.text.y = element_text(color=rev(bearbeitet)),
          legend.title = element_blank(),
          legend.position = "right",
          legend.text = element_text(size=12)
        ) +
        ylab("") +
        xlab("")

    })


  })
}

## To be copied in the UI
# mod_progress_dashboard_ui("progress_dashboard_1")

## To be copied in the server
# mod_progress_dashboard_server("progress_dashboard_1")
