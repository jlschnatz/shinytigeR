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
          style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
          shinyauthr::logoutUI("logout", label = "Ausloggen", class = "btn-danger", icon = icon("right-from-bracket"))
        )
      )
    )
  )

  shiny::observeEvent(input$link_mail, {
    shinyalert::shinyalert(
    title = "Mail",
    text = "Möchtest du eine Mail schreiben? Dann klick <a href='mailto:schultze@psych.uni-frankfurt.de'>hier</a> um direkt weitergeleitet zu werden.",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    type = "info",
    showConfirmButton = FALSE,
    showCancelButton = TRUE,
    cancelButtonText = "Cancel",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
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
  data_item <- db_get_itemdata(.drv = RSQLite::SQLite(), .db_name = "db_item.sqlite")  |>
    dplyr::mutate(dplyr::across(stimulus_image:answeroption_06, ~stringr::str_replace(.x, "www/", "www/img_item/")))  |>
    dplyr::mutate(type_item = ifelse(type_item == "content", "Inhaltliche Aufgaben", "R-Aufgaben")) |>
    dplyr::mutate(learning_area = factor(learning_area, levels = c("Deskriptivstatistik", "Wahrscheinlichkeit", "Grundlagen der Inferenzstatistik", "Gruppenvergleiche", "Poweranalyse", "Zusammenhangsmaße", "Regression")))

  # Tab (Dis)-able Logic ----

  # additional tabs to be added after login
  shinyjs::disable(selector = '.navbar-nav a[data-value="train_panel"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="home_panel"')
  shinyjs::disable(selector = '.navbar-nav a[data-value="progress_panel"')
  shinyjs::disable(selector = '.nav-item a[data-value="item"]') 
  shinyjs::disable(selector = '.nav-item a[data-value="data_panel"]') 

  # update
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      panels <- sprintf('.navbar-nav a[data-value="%s"', c("train_panel", "home_panel", "progress_panel", "data_panel"))
      lapply(X = panels, FUN = function(x) shinyjs::enable(selector = x))
      lapply(X = panels, function(x) shinyjs::show(selector = x))
      shinyjs::disable(selector = '.navbar-nav a[data-value="login_panel"')
      shinyjs::hide(selector = '.navbar-nav a[data-value="login_panel"')
      bslib::nav_select("tabs", "home_panel")
    }
  })

# Train Panel Accordion Switch between Selection and Training Panel
  shiny::observeEvent(
    eventExpr = mod1_select$submit_btn_value(),
    handlerExpr = {
      req(credentials()$user_auth)
      if(length(mod1_select$index_display()) > 0 & mod1_select$n_item() <= mod1_select$max_item() & mod1_select$n_item() != 0) {
        bslib::accordion_panel_close(id = "accordion_1", values = "acc_panel_1")
        shinyjs::hide("accordion_1", time = 0.5, anim = TRUE, animType = "slide")
        shinyjs::show("accordion_2", time = 0.5, anim = TRUE, animType = "fade")
        bslib::accordion_panel_open(id = "accordion_2", values = "acc_panel_2")    
      }
    }
  )

  shiny::observeEvent(
    eventExpr = mod3_check$back_button_value(),
    handlerExpr = {
      req(credentials()$user_auth)
      if(length(mod1_select$index_display()) > 0) {
        bslib::accordion_panel_close(id = "accordion_2", values = "acc_panel_2")
        shinyjs::hide("accordion_2", time = 0.5, anim = TRUE, animType = "slide")
        shinyjs::show("accordion_1", time = 0.5, anim = TRUE, animType = "fade")
        bslib::accordion_panel_open(id = "accordion_1", values = "acc_panel_1")    
      }
    }
  )

  #data_sleep <- read.csv("inst/app/www/data_sleep.csv")
  output$sleep_data <- DT::renderDataTable(
    DT::datatable(
      data = data_sleep, 
      options = list(
        columnDefs = list(list(targets = "_all", orderable = FALSE, className = "dt-nowrap", width = "300px", autoWidth = FALSE)),
        dom = 't',
        fixedColumns = FALSE
      ), 
      select = "none", 
      rownames = FALSE)
  )

  # Call Modules ----

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

  res_response_analysis <- response_analysis(
    data_item = data_item,
    credentials = credentials,
    check_button = mod3_check$check_button_value
  )

  mod_progress_dashboard_server(
    "progress_dashboard_1",
    feedback_data = res_response_analysis$feedback_data(),
    bearbeitet = res_response_analysis$bearbeitet(),
    all_data = res_response_analysis$all_data(),
    credentials = credentials,
    user_data = res_response_analysis$user_data
  )
}
