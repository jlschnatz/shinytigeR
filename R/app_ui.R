#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = shiny::withTags(b("tigeR", class = "fw-bolder", style = "padding-left: 10px;")),
      id = "tabs", # must add id here to add/remove tabs in server
      #collapsible = TRUE,
      navbar_options = bslib::navbar_options(collapsible = TRUE, bg = "black"),
      fluid = TRUE,
      theme = theme_tiger,
      # login tab to be rendered on launch (hiding the other tabs)
      bslib::nav_panel(
        title = "Login",
        value = "login_panel",
        icon = bsicons::bs_icon("lock-fill", size = 15),
        loginUI(
          id = "login",
          title = "Login",
          user_title = "Benutzername",
          pass_title = "Passwort",
          login_title = "Einloggen",
          error_message = "Invalider Benutzername oder Passwort!",
          additional_ui = shiny::tagList(
            htmltools::withTags(
              div(
                p(
                  "Du erhälst deine Zugangsdaten im R-Praktikum. Falls du sie vergessen hast, wende dich an", 
                  shiny::actionLink("link_mail", "Martin Schultze."), 
                  "Zum Ausprobieren kannst du folgende Zugangsdaten nutzen:"
                  )
              )
            ),
            shiny::HTML(
              knitr::kable(
                x = data.frame(a = c("<b style='font-weight: 500;'>Benutzername </b>", "<b style='font-weight: 500;'>Passwort</b>"), b = c("test", "test123")), 
                escape = FALSE, format = "html", col.names = NULL, table.attr = "style='width:50%;'")
              )
          )
        )
      ),

      # Home Panel
      bslib::nav_panel(
        id = "home_test",
        title = "Home",
        value = "home_panel",
        icon = bsicons::bs_icon("house-fill", size = 15),
        mod_home_ui("home_1")
      ),

      # Training Panel
      bslib::nav_panel(
        title = "Üben",
        value = "train_panel",
        id = "train",
        icon = bsicons::bs_icon("ui-radios", size = 15),
        shiny::fluidRow(
          col_1(),
          col_10(
            bslib::accordion(
              bslib::accordion_panel(
                title = div("Auswahlbereich", class = "fw-bolder"),
                mod_select_item_ui("select_item_1"),
                value = "acc_panel_1",
                icon = bsicons::bs_icon("funnel")
              ),
            id = "accordion_1",
            open = TRUE
          ),
          shinyjs::hidden(bslib::accordion(
            bslib::accordion_panel(
              title = "Übungsbereich",
              shiny::tagList(
                mod_display_item_ui("display_item_1"),
                mod_check_item_ui("check_item_1")
              ),
              value = "acc_panel_2",
              icon = bsicons::bs_icon("ui-radios")
            ),
            id = "accordion_2",
            open = FALSE
          ))
        ),
        col_1()
      )
      ),

      # Progress panel
      bslib::nav_panel(
        title = "Fortschritt",
        value = "progress_panel",
        icon = bsicons::bs_icon("check-circle-fill", size = 15),
        fluidRow(
        col_1(),
        col_10(mod_progress_dashboard_ui("progress_dashboard_1")),
        col_1()
        )
      ),

    bslib::nav_panel(
      title = "Datenset",
      value = "data_panel",
      icon = bsicons::bs_icon("database-fill"),
      fluidRow(
        col_1(),
        col_12(
          h4("Download"),
          div("Um die R Aufgaben lösen zu können, benötigst du einen von uns bereit gestellten Datensatz. Diesen Datensatz haben wir extra für tigeR simuliert.
                 Das heißt, die Daten sind ausgedacht und nicht echt. Mithilfe dieses Datensatzes kannst du dann die für die Fragen erforderlichen Analysen
                 durchführen und so deine R-Fähigkeiten üben. Der Datensatz kann mittels folgendem Befehl heruntergeladen werden:",
              tags$code('load(url("https://pandar.netlify.app/daten/df_tiger.rda"))')),
        ),
        col_1()
      ),
      fluidRow(
        #col_4(downloadButton("download", label = "Datensatz herunterladen")),
        rep_br(2),
        tags$p("Folgende Pakete werden empfohlen:", code("car"), code("WebPower"), "."),
        div(
          h4("Codebuch"),
         "Der vorliegende Datensatz",
         tags$code("df_tiger"),
         "befasst sich mit den Schlafgewohnheiten, der akademischen Leistung und den Lebensgewohnheiten von Studierenden.
         Dazu wurden weitere verschiedene Aspekte erhoben, wie sportliche Aktivitäten und generelles Wohlbefinden.
         Hier ist eine Übersicht der Variablen."
         )
        ),
      fluidRow(
        DT::DTOutput("sleep_data")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinytigeR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}