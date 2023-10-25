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
      id = "tabs", # must fivw id here to add/remove tabs in server
      collapsible = TRUE,
      # login tab to be rendered on launch (hiding the other tabs)
      bslib::nav_panel(
        title = "Login",
        value = "login_panel",
        icon = bsicons::bs_icon("lock-fill", size = 15),
        shinyauthr::loginUI(
          id = "login",
          title = "Benutzer Login",
          user_title = "Benutzername",
          pass_title = "Passwort",
          login_title = "Einloggen"
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
          mod_select_item_ui("select_item_1"),
          mod_display_item_ui("display_item_1"),
          mod_check_item_ui("check_item_1")
        )
      ),
      fluid = TRUE,
      theme = bslib::bs_theme(
        bootswatch = "zephyr",
        primary = "#285f8a",
        "navbar-bg" = "#285f8a",
        danger = rgb(274/355, 0, 0)
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
