#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  # Your application server logic
  # hack to add the logout button to the navbar on app launch
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class = "nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
          shinyauthr::logoutUI("logout")
        )
      )
    )
  )

  # Login Functionality ----

  # user database for logins (to be added to the db in the server)
  user_base <- tibble::tibble(
    user = c("user1", "user2"),
    password = purrr::map_chr(c("pass1", "pass2"), sodium::password_store),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
  )

  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )

  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )


  # Load data from item database

  data_item <- db_get_itemdata(.drv = RSQLite::SQLite(), .db_name = "db_item.sqlite")

  # additional tabs to be added after login
  home_tab <- bslib::nav_panel(
    title = "Home",
    value = "home",
    icon = bsicons::bs_icon("house-fill", size = 15),
    fluidRow(
      column(width = 6,
             tags$h4("Hallo und herzlich willkommen bei tigeR!"),
             tags$p("Hallo und herzlich willkommen bei tigeR! Hier findest du Übungsaufgaben begleitend zu den Inhalten des Moduls PsyBSc2."),
             tags$p("Unter dem Reiter Üben geht es direkt zu den Aufgaben und unter Fortschritt erhältst du einen Überblick über deine bisherigen Aktivitäten in tigeR."),
             tags$p("Bei Fragen, Problemen oder Anmerkungen kannst du dich jederzeit an Julia Beitner unter beitner@psych.uni-frankfurt.de wenden."),
             tags$p("Viel Spaß :)")
      ),
      column(width = 6,
             # You can add an image or any other content in the right column
             tags$img(src = "www/tigeR_Logo_transparent.png", width = "30%")
      )
    )
  )

  item_tab <- bslib::nav_panel(
    title = "Üben",
    value = "data",
    icon = bsicons::bs_icon("ui-radios", size = 15),
    fluidRow(
      mod_select_item_ui("select_item_1", unique(data_item$learning_area)),
      mod_display_item_ui("display_item_1"),
      mod_check_item_ui("check_item_1")
    )
  )

  progress_tab <- bslib::nav_panel(
    title = "Fortschritt",
    value = "data",
    icon = bsicons::bs_icon("check-circle-fill", size = 15),
    fluidRow(
      h3("Fortschritt"),
      tags$a("Hier der Fortschritt mit Dashboard"),
      mod_response_analysis_ui("response_analysis_1"),
      mod_progress_dashboard_ui("progress_dashboard_1")
    )
  )

  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (credentials()$user_auth) {
      # remove the login tab
      bslib::nav_hide("tabs", "login")
      # append tabs
      bslib::nav_insert("tabs", home_tab, select = TRUE)
      bslib::nav_insert("tabs", item_tab)
      bslib::nav_insert("tabs", progress_tab)
    }
  })

  mod1 <- mod_select_item_server("select_item_1", data_item)
  #mod_display_item_server("display_item_1")
}
