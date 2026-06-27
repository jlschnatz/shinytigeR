#' Run the shinytigeR application
#'
#' @param ... Arguments passed to [shiny::runApp()].
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "shinytigeR", mustWork = TRUE)
  shiny::runApp(app_dir, ...)
}
