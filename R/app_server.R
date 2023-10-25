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
          shinyauthr::logoutUI("logout", class = "btn-danger", icon = icon("right-from-bracket"))
        )
      )
    )
  )

  # Login Functionality ----

  user_base <- db_get_credentialdata()

  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
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

  # Load data from item database

  data_item <- db_get_itemdata(.drv = RSQLite::SQLite(), .db_name = "db_item.sqlite") %>%
    dplyr::mutate(dplyr::across(stimulus_image:answeroption_05, ~stringr::str_replace(.x, "www/", "www/img_item/"))) %>%
    dplyr::mutate(learning_area = forcats::fct(learning_area)) %>%
    dplyr::filter(!is.na(stimulus_text)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with("if_answeroption"),
      .fns = ~ check_na_feedback(.x, answer_correct)
    )) %>%
    dplyr::ungroup()

  user_id <- "user1"
  response_analysis <- mod_response_analysis_server("response_analysis_1", data_item = data_item, user_id = user_id)

  # additional tabs to be added after login
  home_tab <- bslib::nav_panel(
    title = "Home",
    value = "home",
    icon = bsicons::bs_icon("house-fill", size = 15),
    fluidRow(
      column(width = 6,
             tags$h4("Hallo und herzlich willkommen bei tigeR!"),
             tags$p("Hier findest du Übungsaufgaben begleitend zu den Inhalten des Moduls PsyBSc2."),
             tags$p("Unter dem Reiter Üben geht es direkt zu den Aufgaben und unter Fortschritt erhältst du einen Überblick über deine bisherigen Aktivitäten in tigeR."),
             tags$p("Bei Fragen, Problemen oder Anmerkungen kannst du dich jederzeit an Julia Beitner unter beitner@psych.uni-frankfurt.de wenden."),
             tags$p("Viel Spaß :)")
      ),
      column(width = 6,
             # You can add an image or any other content in the right column
             tags$img(src = "www/img_logo/tigeR_Logo_transparent.png", width = "30%")
      )
    )
  )

  item_tab <- bslib::nav_panel(
    title = "Üben",
    value = "data",
    icon = bsicons::bs_icon("ui-radios", size = 15),
    tagList(
      mod_select_item_ui("select_item_1", data_item),
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
      tags$a("Hier erfährst du mehr über deinen bisherigen Fortschritt in tigeR"),
      div(style = "padding-top: 8px; padding-bottom: 30px;"),

      bslib::layout_columns(
        bslib::value_box(
          title = "Heute bearbeitete Aufgaben", value = isolate(response_analysis$todays_practice()),
          shiny::markdown("Super, weiter so!"),
          #theme = bslib::value_box_theme(bg = "#860047", fg = "#FFFFFF"),
          style = 'background-color: #860047!important; padding-left: 10px;',
          showcase = bsicons::bs_icon("emoji-smile", style="font-size: 45px; color: white"),
          #showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE,
          height = NULL
        ),
        bslib::value_box(
          title = "Insgesamt bearbeitete Aufgaben", value = isolate(response_analysis$total_practice()), #,
          #theme = value_box_theme(bg = "#FFFFFF", fg = "#C96215"),
          style = 'background-color: #C96215!important;',
          showcase = bsicons::bs_icon("bar-chart-fill", style="font-size: 45px; color: white"), #showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = NULL
        ),
        bslib::value_box(
          title = "Noch", value = isolate(response_analysis$xmas_countdown()), shiny::markdown("Tage bis Weihnachten"),
         # theme = value_box_theme(bg = "#FFFFFF", fg = "#B3062C"),
          style = 'background-color: #B3062C!important;',
          showcase = fontawesome::fa_i("candy-cane", style="font-size: 45px; color: white"), #showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = NULL
        ),
        bslib::value_box(
          title = " ", value = paste0(isolate(response_analysis$session_practice())," mal"), shiny::markdown("warst du schon auf tigeR aktiv"),
        #  theme = value_box_theme(bg = "#737C45", fg = "#000000"),
          style = 'background-color: #737C45!important; padding-right: 10px;',
          showcase = bsicons::bs_icon("calendar4-week", style="font-size: 45px; color: white"), #showcase_layout = "left center",
          full_screen = FALSE, fill = TRUE, height = NULL
        )
      ),
      div(style = "padding-bottom: 30px;"),


      column(10,   # Left column
             tags$head(
               tags$style(HTML("
          .shiny-input-container:not(.shiny-input-container-inline) {
            width:100%;
          }
          #feedback_plot, #comparison_plot {
            padding-bottom: 100px;
          }"))
             ),

      tags$div(style="margin-bottom:40px;"),

      #mod_response_analysis_ui("response_analysis_1"),
      mod_progress_dashboard_ui("progress_dashboard_1")
    ),
    column(1,   # right column

           # Add your action buttons here
           #   actionButton(ns("plot1_button"), "Plot 1"),
           #  actionButton(ns("plot2_button"), "Plot 2")
    )
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

  # set global reactiveValues for indexing trough the items

  mod1_select <- mod_select_item_server(
    id = "select_item_1",
    data_item = data_item
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

#  user_id <- "user1"
#  response_analysis <- mod_response_analysis_server("response_analysis_1", data_item = data_item, user_id = user_id)
  mod_progress_dashboard_server("progress_dashboard_1", feedback_data = response_analysis$feedback_data(),
             bearbeitet = response_analysis$bearbeitet(), all_data = response_analysis$all_data())


  #bslib::bs_themer()
}
