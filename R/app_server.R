#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  # Your application server logic
  # hack to add the logout button and timer to the navbar on app launch
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    # logout button
    ui = tags$ul(
      class = "nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-top: 17px; padding-bottom: 10px; color: white;",
          uiOutput("time_spent")
          )
      ),
      # timer
      tags$li(
        div(
          style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
          shinyauthr::logoutUI("logout", class = "btn-danger", icon = icon("right-from-bracket"))
        )
      )
    )
  )

  # Timer Logic ----

  observeEvent(credentials()$user_auth, {
    start_time <- Sys.time()
    current_time <- reactiveVal(NULL)

    observe({
      invalidateLater(1000, session)
      current_time(Sys.time())
    })

    time_parsed <- reactive(parse_clock(parse_difftime(current_time(), start_time)))

    output$time_spent <- renderUI({
      req(credentials()$user_auth)
      fluidRow(
        span(
          HTML("Zeit:&nbsp;"),
          bsicons::bs_icon("clock-history"),
          time_parsed()
        ))
    })
  })

  # Login Functionality ----

  user_base <- db_get_credentialdata()

  # call the shinyauthr login and logout server modules
  credentials <- loginServer(
    id = "login",
    data = user_base,
    user_col = "user_name",
    pwd_col = "password_hashed",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # Load Itemdata ----

  data_item <- db_get_itemdata(.drv = RSQLite::SQLite(), .db_name = "db_item.sqlite") %>%
    dplyr::mutate(dplyr::across(stimulus_image:answeroption_05, ~ stringr::str_replace(.x, "www/", "www/img_item/"))) %>%
    dplyr::mutate(learning_area = forcats::fct(learning_area)) %>%
    dplyr::filter(!is.na(stimulus_text)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with("if_answeroption"),
      .fns = ~ check_na_feedback(.x, answer_correct)
    )) %>%
    dplyr::ungroup()



  # additional tabs to be added after login

  shinyjs::disable(selector = '.navbar-nav a[data-value="train_panel"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="home_panel"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="progress_panel"')
  shinyjs::disable(selector = '.nav-item a[data-value="item"]') # disable nav-item for training

  observeEvent(mod1_select$submit_btn_value(), {
    if (!is.null(mod1_select$selected_topics())) {
      shiny::updateTabsetPanel(session = session, "navset_train", "item")
    }
  })

  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
    shinyjs::enable(selector = '.navbar-nav a[data-value="train_panel"')
    shinyjs::enable(selector = '.navbar-nav a[data-value="home_panel"')
    shinyjs::enable(selector = '.navbar-nav a[data-value="progress_panel"')
    shinyjs::disable(selector = '.navbar-nav a[data-value="login_panel"')
    bslib::nav_select("tabs", "home_panel")

    response_analysis <- mod_response_analysis_server("response_analysis_1", data_item = data_item, credentials = credentials,
                                                      check_button = mod3_check$check_button_value)
    mod_progress_dashboard_server("progress_dashboard_1",
                                  feedback_data = response_analysis$feedback_data(),
                                  bearbeitet = response_analysis$bearbeitet(), all_data = response_analysis$all_data(),
                                  credentials = credentials
    )
    }
  })

  bslib::nav_select("test", "auswahl")

  observeEvent(mod1_select$submit_btn_value, {
    shiny::updateTabsetPanel(session = session, inputId = "navset_train", "auswahl")
  })


  # set global reactiveValues for indexing trough the items
  output$cardheader_train <- renderText({paste0("Item ", data_item$id_item[mod2_display$cur_item_id()])})


  mod_home_server("home_1", credentials)

  mod1_select <- mod_select_item_server(
    id = "select_item_1",
    data_item = data_item,
    credentials = credentials
  )

  mod2_display <- mod_display_item_server(
    id = "display_item_1",
    data_item = data_item,
    index_display = mod1_select$index_display,
    check_button_value = mod3_check$check_button_value,
    credentials = credentials
  )

  mod3_check <- mod_check_item_server(
    id = "check_item_1",
    data_item = data_item,
    index_display = mod1_select$index_display,
    cur_item_id = mod2_display$cur_item_id,
    cur_answer_txt = mod2_display$cur_answer_txt,
    cur_answer_id = mod2_display$cur_answer_id,
    submit_btn_value = mod1_select$submit_btn_value,
    credentials = credentials
  )

}
