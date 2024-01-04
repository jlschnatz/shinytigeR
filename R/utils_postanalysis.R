#' Download Files from a Remote Server using Duck
#'
#' This function allows the user to download files from a remote server using the Duck command-line tool.
#' Duck is a command-line SFTP client that facilitates secure file transfers.
#'
#' @export
#' @param .x A character vector of relative filepaths on the remote server to be downloaded.
#' @param .user A character specifying the username for the server connection.
#' @param .cmd A character specifying the path to the Duck executable on the local machine.
#' @param .rsa_path A character specifying the path to the RSA private key for authentication.
#' @param .return_path A character vector specifying the local destination paths to save the downloaded files.
#'
#' @details
#' The function connects to the remote server using the provided credentials and downloads files specified by the filepaths in \code{.x}.
#' The downloaded files are saved locally at the paths specified in \code{.return_path}.
#'
#' Note: This function requires a connection to the Goethe University VPN to access the remote server.
#' @examples
#' \dontrun{
#' # Example usage:
#' .cmd <- "/opt/homebrew/bin/duck"
#' .rsa_path <- "/Users/luca/.ssh/id_rsa"
#' .return_path <- "/Users/luca/Desktop/db_user.sqlite"
#' .user <- "beitner"
#' .x <- "shinyapps/shinytigeR-DB/db_user.sqlite"
#'
#' duck_download_files(.x, .user, .cmd, .rsa_path, .return_path)
#' }
#'
#' @importFrom cli cli_abort cli_alert_info cli_h1 cli_progress_step
#' @importFrom processx run
#' @seealso \code{\link{pool_connect}}
duck_download_files <- function(.x, .user, .cmd, .rsa_path, .return_path) {
  server_path <- "sftp://tiger.uni-frankfurt.de"
  if(missing(.x)) {
    cli::cli_abort("Argument .x is missing. Please provide at least one path to a file on the remote server to download.")
  } else if (length(.x) > 0) {
    for (i in seq_along(.x)) {
      if (grepl(pattern = "^/", x = .x[i])) cli::cli_abort(".x must not start with a foward slash `/`!")
      .dpath <- file.path(server_path, .x[i])
      arg <- c("-u", .user, "-i", .rsa_path, "--download", .dpath, .return_path[i], "-e", "overwrite")
      process_run(.x[i], .cmd, arg, .user, .rsa_path, .dpath, .return_path[i])
    }
  }
}

#' Execute External Process for File Download
#'
#' This function executes an external process using the specified command and arguments for downloading files.
#' It is internally used by the \code{\link{duck_download_files}} function to perform the actual file download.
#'
#' @param .x A character specifying the filepath on the remote server to be downloaded.
#' @param .cmd A character specifying the path to the external command-line executable.
#' @param arg A character vector specifying the arguments for the external command.
#' @param .user A character specifying the username for the server connection.
#' @param .rsa_path A character specifying the path to the RSA private key for authentication.
#' @param .dpath A character specifying the destination path on the remote server for the downloaded file.
#' @param .return_path A character specifying the local destination path to save the downloaded file.
#'
#' @details
#' This function is responsible for initiating the download process by executing an external command (e.g., Duck).
#' It provides visual feedback using the \code{cli} package to inform the user about the ongoing download.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' .cmd <- "/opt/homebrew/bin/duck"
#' .rsa_path <- "/Users/luca/.ssh/id_rsa"
#' .return_path <- c("/Users/luca/Desktop/db_user.sqlite")
#' .user <- "beitner"
#' .x <- "shinyapps/shinytigeR-DB/db_user.sqlite"
#' .dpath <- "sftp://tiger.uni-frankfurt.de/shinyapps/shinytigeR-DB/db_user.sqlite"
#' arg <- c("-u", .user, "-i", .rsa_path, "-d", .dpath, .return_path, "-e", "overwrite")
#'
#' process_run(.x, .cmd, arg, .user, .rsa_path, .dpath, .return_path)
#' }
#'
#' @importFrom cli cli_h1 cli_alert_info cli_progress_step cli_progress_done
#' @importFrom processx run
#' @keywords external process execution download visual feedback
#' @seealso \code{\link{duck_download_files}}
process_run <- function(.x, .cmd, arg, .user, .rsa_path, .dpath, .return_path) {
  cli::cli_h1("Starting Download")
  cli::cli_alert_info(
    "Running {.path { .cmd}} {cli::col_br_magenta('\\\\')}
    {cli::col_br_magenta('-u')} { .user} {cli::col_br_magenta('\\\\')}
    {cli::col_br_magenta('-i')} {.path { .rsa_path}} {cli::col_br_magenta('\\\\')}
    {cli::col_br_magenta('-d')} {.path { .dpath}} {cli::col_br_magenta('\\\\')}
    {.path { .return_path}} {cli::col_br_magenta('\\\\')}
    {cli::col_br_magenta('-e')} overwrite",
    wrap = FALSE
  )
  cli::cli_progress_step(
    "Downloading file {.strong { .x}} to { .return_path}",
    spinner = TRUE
  )
  invisible(processx::run(
    command = .cmd,
    args = arg,
    echo_cmd = FALSE,
    echo = FALSE,
    spinner = TRUE
  ))
  cli::cli_progress_done()
}

#' Establish a Database Connection Pool
#'
#' This function creates a database connection pool using the RSQLite and pool package.
#' The pool allows efficient management of multiple database connections, enhancing performance and resource utilization.
#'
#' @param .return_path A character vector specifying the local paths of SQLite database files.
#'
#' @details
#' The function checks for the presence of files with a ".sqlite" extension in the specified directory.
#' It then creates a database connection pool using the RSQLite package for each valid SQLite file found.
#' The resulting pool can be used to query and manipulate data from the connected databases.
#'
#' @return A database connection pool object.
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom pool dbPool
#' @importFrom DBI dbListTables
#' @importFrom dplyr collect tbl
#' @importFrom purrr map_dfr
#' @keywords database connection pool SQLite
#' @seealso \code{\link{duck_download_files}}
#'
pool_connect <- function(.return_path) {
  if(!any(grepl(".sqlite$", list.files(path = dirname(.return_path))))) {
    cli::cli_abort("No file that ends with {.emph .sqlite} in given directory {.file { unique(dirname(.return_path))}}")
  }
  index <- grep("db_user.sqlite$", .return_path)
  pool <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = .return_path[index]
  )
  usernames <- DBI::dbListTables(pool)
  user_data <- purrr::map_dfr(usernames, ~dplyr::collect(dplyr::tbl(pool, .x)))
  return(user_data)
}


duck_update_tarball <- function(.local_path, .user, .cmd, .rsa_path, .server_path) {
  server_path <- "sftp://tiger.uni-frankfurt.de"
  if(missing(.tar_path)) {
    cli::cli_abort("Argument .tar_path is missing. Please provide at least one path to a file on the remote server to download.")
  } else if (length(.tar_path) > 0) {
    for (i in seq_along(.tar_path)) {
      if (grepl(pattern = "^/", x = .tar_path[i])) cli::cli_abort(".tar_path must not start with a foward slash `/`!")
      .dpath <- file.path(server_path, .tar_path[i])
      arg <- c("-u", .user, "-i", .rsa_path, "--upload", .dpath, .return_path[i], "-e", "overwrite")
      process_run(.tar_path[i], .cmd, arg, .user, .rsa_path, .dpath, .return_path[i])
    }
  }

}
