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
      title = shiny::tags$b("PsyBSc2 — tigeR"),
      id = "tabs", # must add id here to add/remove tabs in server
      collapsible = TRUE,
      fluid = TRUE,
      theme = bslib::bs_theme(
        bootswatch = "zephyr",
        primary = "#285f8a",
        "navbar-bg" = "#285f8a",
        danger = rgb(274 / 355, 0, 0)
      ),

      # login tab to be rendered on launch (hiding the other tabs)
      bslib::nav_panel(
        title = "Login",
        value = "login_panel",
        icon = bsicons::bs_icon("lock-fill", size = 15),
        loginUI(
          id = "login",
          title = "Benutzer Login",
          user_title = "Benutzername",
          pass_title = "Passwort",
          login_title = "Einloggen",
          error_message = "Invalider Benutzername oder Passwort!",
          additional_ui = tagList(
            tags$p(HTML("Bitte melde dich mit deinem im R-Praktikum erhaltenen Benutzernamen und Passwort an.
               Wenn du deinen Benutzernamen/Passwort vergessen haben solltest,
               wende dich an <a href='mailto:beitner@psych.uni-frankfurt.de'>Julia Beitner</a>,
               um einen neuen Zugang zu erhalten."), class = "text-left"),
            tags$p(
              HTML(
                knitr::kable(
                  data.frame(
                    Benutzername = "test",
                    Passwort = "test123"
                    ),
                  format = "html",
                  col.names = c("Benutzername", "Passwort"),
                  table.attr = "style='width:100%;'"
                  )
                ),
              class = "center"
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
        tagList(
          mod_home_ui("home_1")
        )
      ),

      # Training Panel
      bslib::nav_panel(
        title = "Üben",
        value = "train_panel",
        icon = bsicons::bs_icon("ui-radios", size = 15),
        tagList(
          bslib::navset_pill(
            bslib::nav_panel(
              title = bslib::tooltip(
                span(
                  HTML("Itemselektion &nbsp;"),
                  bsicons::bs_icon("info-circle")
                ),
                "Hier wählst du deine aus, die du üben möchtest.",
                placement = "bottom",
              ),
              mod_select_item_ui("select_item_1")
            ),
            bslib::nav_panel(
              title = "Übungen",
              mod_display_item_ui("display_item_1"),
              mod_check_item_ui("check_item_1")
            ),
          )
        )
      ),
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
