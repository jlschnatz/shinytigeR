#' response_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_response_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' response_analysis Server Functions
#'
#' @noRd
mod_response_analysis_server <- function(id, input, output, session, data_item, user_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Fetch user's data from the database
    #user_data <- db_get_userdata(user_id)
    user_data <- data.frame("id_user" = as.factor(rep("user1",8)),
                            "id_session" = as.factor(c(rep("sess1",3), rep("sess2",2), rep("sess3",3))),
                            "id_item" = as.factor(c(1, 2, 3, 4, 5, 6, 7, 8)),
                            "learning_area" = factor(as.factor(rep(c("Datenrepräsentationen", "Maße zentraler Tendenz"), each = 4)),
                                                     levels = unique(data_item$learning_area), labels = unique(data_item$learning_area)),
                            "selected_option" = c(3, 3, 4, 4, 3, 1, 4, 2),
                            "answer_correct" = c(1, 4, 4, 4, 3, 1, 4, 2),
                            "bool_correct" = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
                            "data" = rep(Sys.Date(), 8))


    # Analyze today's practice
    todays_date <- Sys.Date()
    todays_practice <- sum(user_data$date == todays_date)
    overall_practice <- length(user_data$id_user)


    # feedback generation
    feedback_data_reactive <- reactive({

      feedback_data <- with(user_data,
                            by(user_data, learning_area,
                               function(x) estimate_theta(x$bool_correct, data_item$irt_discr[x$id_item], data_item$irt_diff[x$id_item])))

      feedback_data <- as.data.frame(feedback_data)
      colnames(feedback_data) <- "theta"
      feedback_data$Lerneinheit <- factor(rownames(feedback_data), levels = unique(data_item$learning_area), labels = unique(data_item$learning_area))
      rownames(feedback_data) <- 1:8
      feedback_data$theta <- as.numeric(feedback_data$theta)

      # Return the feedback data
      return(feedback_data)
    })


    #  # IRT analysis
    #  irt_score_reactive <- reactive({
    #
    #   estimated_theta <- estimate_theta(user_data$bool_correct, data_item$irt_discr[user_data$id_item], data_item$irt_diff[user_data$id_item])
    #
    #    # Return the IRT score
    #    return(estimated_theta)
    #  })


    # Combine user data and course data
    all_data_reactive <- reactive({

      sample_data <- with(data_item,
                          by(data_item, learning_area,
                             function(x) estimate_theta(x$ia_diff, x$irt_discr, x$irt_diff)))

      sample_data <- as.data.frame(sample_data)
      colnames(sample_data) <- "theta_sample"
      sample_data$Lerneinheit <- rownames(sample_data)
      sample_data$Lerneinheit <- factor(rownames(sample_data), levels = unique(data_item$learning_area), labels = unique(data_item$learning_area))
      rownames(sample_data) <- 1:8
      sample_data$theta_sample <- as.numeric(sample_data$theta_sample)
      str(sample_data)

      all_data <- merge(feedback_data_reactive(), sample_data, by = "Lerneinheit")

      # Return all data
      return(all_data)
    })


    bearbeitet_reactive <- reactive({
      bearbeitet <- ifelse(feedback_data_reactive()$Lerneinheit %in% subset(feedback_data_reactive(), !is.na(theta))$Lerneinheit, "black", "grey")

      # Return bearbeitet
      return(bearbeitet)
    })


    # Return the feedback data so it can be used outside this module if needed.
    return(list(
      feedback_data = feedback_data_reactive,
      bearbeitet = bearbeitet_reactive,
      all_data = all_data_reactive,
      todays_practice = todays_practice,
      overall_practice = overall_practice

    ))

  })
}



## To be copied in the UI
# mod_response_analysis_ui("response_analysis_1")

## To be copied in the server
# mod_response_analysis_server("response_analysis_1")
