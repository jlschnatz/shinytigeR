Sys.setenv(TIGER_DB_DIR = normalizePath("."))
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
} else {
  install.packages(".", repos = NULL, type = "source",
                   INSTALL_opts = "--no-docs --no-multiarch --no-test-load",
                   quiet = TRUE)
}
shiny::runApp("inst/app", port = 7331, launch.browser = TRUE)
