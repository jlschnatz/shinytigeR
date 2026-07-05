#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("item_num", "roll_acc", "area_short"))

## usethis namespace: start
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyjs
#' @import ggplot2
#' @importFrom DBI dbConnect dbDisconnect dbReadTable dbWriteTable dbExistsTable dbListTables dbGetQuery dbExecute dbAppendTable dbCreateTable
#' @importFrom RSQLite SQLite
#' @importFrom shinyauthr loginUI loginServer logoutUI logoutServer
#' @importFrom sodium password_verify password_store
#' @importFrom markdown markdownToHTML
#' @importFrom stats optim setNames
#' @importFrom utils head read.csv
## usethis namespace: end
NULL
