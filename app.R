library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(DBI)
library(RSQLite)
library(shinyauthr)
library(sodium)
library(ggplot2)
library(markdown)

for (f in c(
  "R/constants.R",
  "R/db.R",
  "R/irt.R",
  "R/utils.R",
  "R/mod_selector.R",
  "R/mod_practice.R",
  "R/mod_dashboard.R",
  "R/ui.R",
  "R/server.R"
)) {
  source(f, local = FALSE)
}

# Register static asset paths explicitly — avoids symlink and working-dir issues
shiny::addResourcePath("img",      normalizePath("www/img",      mustWork = TRUE))
shiny::addResourcePath("img_item", normalizePath("www/img_item", mustWork = TRUE))
shiny::addResourcePath("css",      normalizePath("www/css",      mustWork = TRUE))

shinyApp(ui = app_ui(), server = app_server)
