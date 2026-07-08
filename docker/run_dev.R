# Docker-only entrypoint. Like dev/run.R, but binds to 0.0.0.0 (required for
# the container's port mapping to reach it) and skips launch.browser (no
# browser inside the container).
Sys.setenv(TIGER_DB_DIR = normalizePath("."))
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
