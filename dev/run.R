# The DB folder next to the repo (../db: databases, reg_code.txt, img_item/),
# laid out like the server's shinytigeR-DB, which is mounted at /opt/shinyapp.
# Another folder, e.g. a backup downloaded from the server:
#   TIGER_DB_DIR=/path/to/folder rv run dev/run.R
if (!nzchar(Sys.getenv("TIGER_DB_DIR"))) Sys.setenv(TIGER_DB_DIR = "../db")
Sys.setenv(TIGER_DB_DIR = normalizePath(Sys.getenv("TIGER_DB_DIR"), mustWork = TRUE))
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
} else {
  install.packages(
    ".",
    repos = NULL,
    type = "source",
    INSTALL_opts = "--no-docs --no-multiarch --no-test-load",
    quiet = TRUE
  )
}
shiny::runApp("inst/app", port = 7331, launch.browser = TRUE)
