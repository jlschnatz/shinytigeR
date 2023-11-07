response_analysis <- function(data_item, credentials, check_button) {
  # Fetch user data everytime the check button is pressed
  user_data <- reactive({
    # Fetch the user data from the SQLite database for the specific user
    req(credentials()$user_auth)
    # This line forces the reactive to re-evaluate whenever the button is clicked
    check_button()
    db_get_userdata(as.character(credentials()$info$user_name))
  })

  # Feedback generation (estimate IRT scores theta)
  feedback_data_reactive <- reactive({

    feedback_data <- with(
      user_data(),
      by(
        user_data(), learning_area,
        function(x) estimate_theta(x$bool_correct, data_item$irt_discr[x$id_item], data_item$irt_diff[x$id_item])
      )
    )

    feedback_data <- as.data.frame(feedback_data)
    colnames(feedback_data) <- "theta"
    feedback_data$Lerneinheit <- factor(rownames(feedback_data), levels = unique(data_item$learning_area), labels = unique(data_item$learning_area))
    rownames(feedback_data) <- 1:length(feedback_data$Lerneinheit)
    feedback_data$theta <- as.numeric(feedback_data$theta)

    # Return the feedback data
    return(feedback_data)
  })

  # Combine user data and course data
  all_data_reactive <- reactive({

    sample_data <- with(
      data_item,
      by(
        data_item, learning_area,
        function(x) estimate_theta(x$ia_diff, x$irt_discr, x$irt_diff)
      )
    )

    sample_data <- as.data.frame(sample_data)
    colnames(sample_data) <- "theta_sample"
    sample_data$Lerneinheit <- rownames(sample_data)
    sample_data$Lerneinheit <- factor(rownames(sample_data), levels = unique(data_item$learning_area), labels = unique(data_item$learning_area))
    rownames(sample_data) <- 1:length(sample_data$Lerneinheit)
    sample_data$theta_sample <- as.numeric(sample_data$theta_sample)

    all_data <- merge(feedback_data_reactive(), sample_data, by = "Lerneinheit", all = TRUE)

    #       course_data <- with(all_users_data(),
    #                           by(data_item, learning_area,
    #                              function(x) estimate_theta(x$ia_diff, x$irt_discr, x$irt_diff)))
    #
    #       course_data <- as.data.frame(course_data)
    #       colnames(course_data) <- "theta_course"
    #       course_data$Lerneinheit <- rownames(course_data)
    #       course_data$Lerneinheit <- factor(rownames(course_data), levels = unique(data_item$learning_area), labels = unique(data_item$learning_area))
    #       rownames(course_data) <- 1:length(course_data$Lerneinheit)
    #       course_data$theta_sample <- as.numeric(course_data$theta_sample)
    #
    #       all_data <- merge(all_data_tmp, course_data, by = "Lerneinheit", all = TRUE)

    # Return all data
    return(all_data)
  })

  # Fetch all users data from the database
  # all_users_data <- reactive({
  #   # Fetch the user data from the SQLite database for the specific user
  #   print("Fetching all data...")
  #   req(credentials()$user_auth)
  #   # This line forces the reactive to re-evaluate whenever the button is clicked
  #   check_button()
  #
  #   db_get_userdata(as.character(credentials()$info$user_name), .fetch_all = TRUE)
  # })

  bearbeitet_reactive <- reactive({

    bearbeitet <- ifelse(levels(feedback_data_reactive()$Lerneinheit) %in% feedback_data_reactive()$Lerneinheit, "black", "grey")
    # Return bearbeitet
    return(bearbeitet)
  })

  return(
    list(
      feedback_data = feedback_data_reactive,
      bearbeitet = bearbeitet_reactive,
      all_data = all_data_reactive,
      user_data = user_data
    )
  )
}
