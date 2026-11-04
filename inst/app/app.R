# Qualified calls, not library(shinytigeR): run_app() only loads the package's
# namespace without attaching it (the Docker CMD calls shinytigeR::run_app()),
# so bare app_ui/app_server weren't found there. `::` works both for the
# installed package and under pkgload::load_all() in dev.
shiny::shinyApp(ui = shinytigeR::app_ui(), server = shinytigeR::app_server)
