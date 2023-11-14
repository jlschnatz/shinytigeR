glue_sys_reqs <- function(.pkgs) {
  rlang::check_installed("curl")
  rspm <- Sys.getenv("RSPM_ROOT", "https://packagemanager.rstudio.com")
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", 1)
  rspm_repo_url <- glue::glue("{rspm}/__api__/repos/{rspm_repo_id}")

  pkgnames <- glue::glue_collapse(unique(.pkgs), sep = "&pkgname=")

  req_url <- glue::glue(
    "{rspm_repo_url}/sysreqs?all=false",
    "&pkgname={pkgnames}&distribution=ubuntu&release=22.04"
  )
  res <- curl::curl_fetch_memory(req_url)
  sys_reqs <- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
  if (!is.null(sys_reqs$error)) rlang::abort(sys_reqs$error)

  sys_reqs <- purrr::map(sys_reqs$requirements, purrr::pluck, "requirements", "packages")
  sys_reqs <- sort(unique(unlist(sys_reqs)))
  sys_reqs <- glue::glue_collapse(sys_reqs, sep = " \\\n    ")
  glue::glue(
    "apt-get update -qq && \\ \n",
    "  apt-get install -y --no-install-recommends \\\n    ",
    sys_reqs,
    "\ && \\\n",
    "  apt-get clean && \\ \n",
    "  rm -rf /var/lib/apt/lists/*",
    .trim = FALSE
  )
}

write_dockerfile_renv <- function(
    source_folder = ".", lockfile = "renv.lock",
    output_dir = "deploy", update_tar_gz = TRUE,
    base_name = "builder", open_file = FALSE) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  if (normalizePath(dirname(output_dir)) == normalizePath(source_folder)) {
    rlang::check_installed("usethis", version = "1.6.0")
    usethis::use_build_ignore(output_dir)
  }

  if (is.null(lockfile)) {
    cli::cli_abort("Please provide a lock.file!")
  }

  file.copy(from = lockfile, to = output_dir)

  if (update_tar_gz) {
    old_version <- list.files(
      path = output_dir,
      pattern = paste0(
        golem::get_golem_name(),
        "_*.*.tar.gz"
      ),
      full.names = TRUE
    )
    if (length(old_version) > 0) {
      lapply(old_version, file.remove)
      lapply(old_version, unlink, force = TRUE)
      cli::cat_bullet(sprintf(
        "%s were removed from folder",
        paste(old_version, collapse = ", ")
      ), bullet = "bullet", bullet_col = "red")
    }
    if (isTRUE(requireNamespace("pkgbuild", quietly = TRUE))) {
      out <- pkgbuild::build(
        path = source_folder, dest_path = output_dir,
        vignettes = FALSE
      )
      if (missing(out)) {
        cli::cat_bullet("Error during tar.gz building", bullet = "bullet", bullet_col = "red")
      } else {
        cli::cat_bullet(sprintf(" %s created.", out), bullet = "tick", bullet_col = "green")
      }
    } else {
      stop("please install {pkgbuild}")
    }
  }

  from_r_version <- glue::glue("rocker/r-ver:{getRversion()}")
  lock <- getFromNamespace("lockfile", "renv")(lockfile)
  pkgs <- names(lock$data()$Packages)

  dockerfile <- dockerfiler::Dockerfile$new(FROM = from_r_version, AS = base_name)
  dockerfile$MAINTAINER("Julia Beitner", "beitner@psych.uni-frankfurt.de")
  sys_reqs <- glue_sys_reqs(pkgs)
  dockerfile$RUN(sys_reqs)
  dockerfile$RUN("mkdir -p /usr/local/lib/R/etc /usr/lib/R/etc/")
  dockerfile$COPY(basename(lockfile), "renv.lock")
  dockerfile$RUN("echo \"options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)\" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site")
  dockerfile$RUN("R -e 'install.packages(c(\"renv\",\"remotes\"))'")
  dockerfile$RUN(dockerfiler::r(renv::restore()))
  dockerfile$RUN("rm renv.lock")
  dockerfile$custom(glue::glue("FROM {from_r_version}"), cmd = "")
  dockerfile$custom(glue::glue("COPY --from={base_name}"), "/usr/local/lib/R /usr/local/lib/R")
  dockerfile$COPY("shinytigeR_*.tar.gz", "/app.tar.gz")
  dockerfile$RUN(dockerfiler::r(remotes::install_local("/app.tar.gz", upgrade = "never")))
  dockerfile$RUN("rm /app.tar.gz")
  dockerfile$RUN('addgroup --gid 1003 beitner; adduser --shell /bin/false --no-create-home --uid 1002 --gecos "Julia Beitner,,," --gid 1003 --disabled-password beitner')
  dockerfile$VOLUME('[ "/opt/shinyapp" ]')
  dockerfile$WORKDIR("/opt/shinyapp")
  dockerfile$EXPOSE(3838)
  dockerfile$USER("beitner")
  dockerfile$CMD(dockerfiler::r(options("shiny.port" = 3838, shiny.host = "0.0.0.0")))
  dockerfile$CMD(dockerfiler::r(shinytigeR::run_app()))

  dockerfile$write(as = file.path(output_dir, "Dockerfile"))
  cli::cli_alert_success(" Dockerfile written!")
  out <- glue::glue("sudo docker build -t shinytiger {output_dir}/")
  cat(out, file = file.path(output_dir, "README"))

  if (open_file) {
    rstudioapi::navigateToFile(file.path(output_dir, "README"))
  } else {
    cli::cat_bullet(sprintf(" Go to %s", file.path(output_dir, "README")), bullet_col = "red")
  }
}

build_deploy <- function(.deploy_path = "deploy", .lock_file = "renv.lock") {
  devtools::build(path = .deploy_path)
  cli::cli_alert_info("Copying lockfile to deploy path")
  file.copy(.lock_file, file.path(.deploy_path, .lock_file))
  cli::cli_alert_success("Done!")
}
