#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("date", "theta", "area_short"))

## usethis namespace: start
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyjs
#' @import ggplot2
#' @import plotly
#' @importFrom DBI dbConnect dbDisconnect dbReadTable dbWriteTable dbExistsTable dbListTables dbGetQuery dbExecute dbAppendTable dbCreateTable
#' @importFrom RSQLite SQLite
#' @importFrom shinyauthr loginUI loginServer logoutUI logoutServer
#' @importFrom sodium password_verify password_store
#' @importFrom markdown markdownToHTML
#' @importFrom rclipboard rclipboardSetup rclipButton
#' @importFrom stats optim setNames
#' @importFrom utils head read.csv
## usethis namespace: end
NULL
