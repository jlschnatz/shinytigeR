#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("item_num", "roll_acc", "area_short"))

## usethis namespace: start
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import shinyjs
#' @import ggplot2
#' @importFrom DBI dbConnect dbDisconnect dbReadTable dbWriteTable dbExistsTable dbListTables
#' @importFrom RSQLite SQLite
#' @importFrom shinyauthr loginUI loginServer logoutUI logoutServer
#' @importFrom sodium password_verify
#' @importFrom markdown markdownToHTML
#' @importFrom scales label_percent
#' @importFrom stats optim setNames
#' @importFrom utils head read.csv
## usethis namespace: end
NULL
