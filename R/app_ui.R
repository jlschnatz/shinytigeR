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
      title = tags$b("PsyBSc2 — tigeR"),
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "minty",
        base_font = sass::font_google("Open Sans"),
        heading_font = sass::font_google("Roboto")
      ),
      bslib::nav_panel(
        title = "Übersicht",
        icon = bsicons::bs_icon("house-fill", size = 15),
        fluidRow(
          h3("Willkommen!"),
          tags$a("Das wird die Übersicht.")
        )
        ),
      bslib::nav_panel(
        title = "Üben",
        icon = bsicons::bs_icon("ui-radios", size = 15),
        fluidRow(
          h3("Items"),
          tags$a("Test"),
          mod_select_item_ui("select_item_1"),
          mod_display_item_ui("display_item_1"),
          mod_check_item_ui("check_item_1")
          )
        ),
      bslib::nav_panel(
        title = "Fortschritt",
        icon = bsicons::bs_icon("check-circle-fill", size = 15),
        fluidRow(
          h3("Fortschritt"),
          tags$a("Hier der Fortschritt mit Dashboard"),
          mod_response_analysis_ui("response_analysis_1"),
          mod_progress_dashboard_ui("progress_dashboard_1")
          )
        ),
      bslib::nav_spacer(),
      bslib::nav_item(tags$a(bsicons::bs_icon("person", size = 15), "Mein Account", target = "_blank")),
      bslib::nav_item(tags$a(bsicons::bs_icon("box-arrow-right", size = 15), "Logout", target = "_blank"))
      )
    # fluidPage(
    #   mod_login_author_ui("login_author_1"),
    #   mod_select_item_ui("select_item_1"),
    #   mod_display_item_ui("display_item_1"),
    #   mod_check_item_ui("check_item_1"),
    #   mod_response_analysis_ui("response_analysis_1"),
    #   mod_progress_dashboard_ui("progress_dashboard_1")
    )
  #)
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
  add_resource_path(
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
