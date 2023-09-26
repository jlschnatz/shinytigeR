#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Modules
  mod_login_author_server("login_author_1")
  mod_select_item_server("select_item_1")
  mod_display_item_server("display_item_1")
  mod_check_item_server("check_item_1")
  mod_response_analysis_server("response_analysis_1")
  mod_progress_dashboard_server("progress_dashboard_1")

}
