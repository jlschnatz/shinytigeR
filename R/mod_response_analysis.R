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
    uiOutput(ns("response_analysis"))
    # actionButton(ns("refresh_data"), "Refresh Data")
  )
}

#' response_analysis Server Functions
#'
#' @noRd
mod_response_analysis_server <- function(id, input, output, session, data_item, credentials, check_button){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # user_data <- data.frame("id_user" = as.factor(rep("user1",8)),
    #                         "id_session" = as.factor(c(rep("sess1",3), rep("sess2",2), rep("sess3",3))),
    #                         "id_item" = as.factor(c(1, 2, 3, 4, 5, 6, 7, 8)),
    #                         "learning_area" = factor(as.factor(rep(c("Datenrepräsentationen", "Maße zentraler Tendenz"), each = 4)),
    #                                                  levels = unique(data_item$learning_area), labels = unique(data_item$learning_area)),
    #                         "selected_option" = c(3, 3, 4, 4, 3, 1, 4, 2),
    #                         "answer_correct" = c(1, 4, 4, 4, 3, 1, 4, 2),
    #                         "bool_correct" = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE),
    #                         "id_date" = rep(Sys.Date(), 8))


    # Fetch user's data from the database
    user_data <- reactive({
      # Fetch the user data from the SQLite database for the specific user
      print("Fetching data...")
      req(credentials()$user_auth)
      # This line forces the reactive to re-evaluate whenever the button is clicked
      check_button()

      db_get_userdata(as.character(credentials()$info$user_name))
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
      if (format(Sys.Date(), "%Y") == 2023){
        xmas <- as.Date("2023-12-24")
      } else {
        xmas <- as.Date("2024-12-24")
      }

      today <- Sys.Date()
      length(seq(from = today, to = xmas, by = 'day')) - 1
    })


    # Feedback generation
    feedback_data_reactive <- reactive({

      feedback_data <- with(user_data(),
                            by(user_data(), learning_area,
                               function(x) estimate_theta(x$bool_correct, data_item$irt_discr[x$id_item], data_item$irt_diff[x$id_item])))

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

      sample_data <- with(data_item,
                          by(data_item, learning_area,
                             function(x) estimate_theta(x$ia_diff, x$irt_discr, x$irt_diff)))

      sample_data <- as.data.frame(sample_data)
      colnames(sample_data) <- "theta_sample"
      sample_data$Lerneinheit <- rownames(sample_data)
      sample_data$Lerneinheit <- factor(rownames(sample_data), levels = unique(data_item$learning_area), labels = unique(data_item$learning_area))
      rownames(sample_data) <- 1:length(sample_data$Lerneinheit)
      sample_data$theta_sample <- as.numeric(sample_data$theta_sample)

      all_data <- merge(feedback_data_reactive(), sample_data, by = "Lerneinheit", all = TRUE)


#
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

      print(all_data)
      print("all data end")

      # Return all data
      return(all_data)
    })


    bearbeitet_reactive <- reactive({
      bearbeitet <- ifelse(levels(feedback_data_reactive()$Lerneinheit) %in% feedback_data_reactive()$Lerneinheit, "black", "grey")
      # Return bearbeitet
      return(bearbeitet)
    })


    output$response_analysis <- renderUI({
      bslib::layout_columns(
        bslib::value_box(
          title = "Heute bearbeitete Aufgaben", value = textOutput(ns("todays_practice")),
          shiny::markdown("Super, weiter so!"),
          # theme = bslib::value_box_theme(bg = "#860047", fg = "#FFFFFF"),
          style = "background-color: #860047!important; padding-left: 10px;",
          showcase = bsicons::bs_icon("emoji-smile", style = "font-size: 45px; color: white"),
          # showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = "150px"
        ),
        bslib::value_box(
          title = "Insgesamt bearbeitete Aufgaben", value = textOutput(ns("total_practice")),
          # theme = value_box_theme(bg = "#FFFFFF", fg = "#C96215"),
          style = "background-color: #C96215!important;",
          showcase = bsicons::bs_icon("bar-chart-fill", style = "font-size: 45px; color: white"), # showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = "150px"
        ),
        bslib::value_box(
          title = "Schon", value = textOutput(ns("session_practice")), #tags$span(textOutput(ns("session_practice"))," mal"),
          shiny::markdown("mal warst du auf tigeR aktiv"),
          #  theme = value_box_theme(bg = "#737C45", fg = "#000000"),
          style = "background-color: #737C45!important; padding-right: 10px;",
          showcase = bsicons::bs_icon("calendar4-week", style = "font-size: 45px; color: white"), # showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = "150px"
        ),
        bslib::value_box(
          title = "Noch", value = textOutput(ns("xmas_countdown")),
          shiny::markdown("Tage bis Weihnachten"),
          # theme = value_box_theme(bg = "#FFFFFF", fg = "#B3062C"),
          style = "background-color: #B3062C!important;",
          showcase = fontawesome::fa_i("candy-cane", style = "font-size: 45px; color: white"), # showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = "150px"
        )
      )
    })




    # Return the feedback data so it can be used outside this module if needed.
    return(list(
      feedback_data = feedback_data_reactive,
      bearbeitet = bearbeitet_reactive,
      all_data = all_data_reactive

    ))

  })
}



## To be copied in the UI
# mod_response_analysis_ui("response_analysis_1")

## To be copied in the server
# mod_response_analysis_server("response_analysis_1")
