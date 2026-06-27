Sys.setenv(TIGER_DB_DIR = normalizePath("."))
pkgload::load_all(quiet = TRUE)
shiny::runApp("inst/app", port = 7331, launch.browser = TRUE)
