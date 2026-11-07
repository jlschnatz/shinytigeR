# Docker-only entrypoint. Like dev/run.R, but binds to 0.0.0.0 (required for
# the container's port mapping to reach it) and skips launch.browser (no
# browser inside the container).
# The DB folder (../db on the host) is mounted at /db, see docker-compose.yml
Sys.setenv(TIGER_DB_DIR = "/db")
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
shiny::runApp("inst/app", port = 7331, host = "0.0.0.0", launch.browser = FALSE)
