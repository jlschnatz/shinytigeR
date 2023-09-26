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
    fluidPage(
      h1("shinytigeR"),
      mod_login_author_ui("login_author_1"),
      mod_select_item_ui("select_item_1"),
      mod_display_item_ui("display_item_1"),
      mod_check_item_ui("check_item_1"),
      mod_response_analysis_ui("response_analysis_1"),
      mod_progress_dashboard_ui("progress_dashboard_1")
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
