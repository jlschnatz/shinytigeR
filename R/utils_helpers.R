#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_na_feedback <- function(.feedback_col, .answer_correct) {
  fb_enq <- rlang::enquo(.feedback_col)
  fb_name <- rlang::quo_name(fb_enq)
  assertthat::assert_that(
    grepl("^if_answeroption_[0-9]{2}$", fb_name),
    msg = "Column does not match the naming convention if_answeroption_0<int>"
  )
  assertthat::assert_that(dplyr::between(.answer_correct, 1, 5))
  fb_number <- substr_nth(fb_name, n = 1, side = "right")

  if (is.na(.feedback_col)) {
    return(dplyr::if_else(
      fb_number == .answer_correct,
      paste0("Richtige Antwort. Gut gemacht! ", icon("smile")),
      "Falsche Antwort."
    ))
  } else {
    return(.feedback_col)
  }
}

# image validation function
is_image <- function(x) {
  pattern <- "(([\\/a-zA-Z0-9äöüÄÖÜ_-]*?)\\.(svg|pdf|png|jpeg|jpg|tiff))$"
  return(grepl(pattern = pattern, x))
}


# Useful helpers
substr_nth <- function(x, n = 1, side = "right") {
  rlang::arg_match(side, c("left", "right"))
  assertthat::assert_that(is.character(x))
  assertthat::assert_that(n < nchar(x))
  out <- switch(side,
                "right" = substr(x, nchar(x) - n + 1, nchar(x)),
                "left"  = substr(x, 1, n)
  )

  return(out)
}

get_answeroptions <- function(.df, .row) {
  na.omit(as.character(.df[.row, paste0("answeroption_0", seq_len(5))]))
}

db_get_itemdata <- function(.drv = RSQLite::SQLite(), .db_name = "db_item.sqlite") {

  pool <- pool::dbPool(
    drv = .drv,
    dbname = .db_name
  )

  con <- pool::poolCheckout(pool)
  pool::poolReturn(con)

  data_item <- dplyr::collect(dplyr::tbl(con, "item_db"))

  pool::poolClose(pool)

  return(data_item)
}


db_get_userdata <- function(id_user, .drv = RSQLite::SQLite(), .db_name = "db_user.sqlite", .fetch_all = FALSE) {

  pool <- pool::dbPool(
    drv = .drv,
    dbname = .db_name
  )

  con <- pool::poolCheckout(pool)

  if (.fetch_all) {
    vec_username <- DBI::dbListTables(con)
    user_data <- purrr::map(vec_username, ~dplyr::collect(dplyr::tbl(con, .x)) %>% purrr::list_rbind())
  } else {
    # Fetch data for the specific user
    user_data <- dplyr::collect(dplyr::tbl(con, id_user))
  }

  pool::poolReturn(con)
  pool::poolClose(pool)

  return(user_data)
}

db_get_credentialdata <- function(.drv = RSQLite::SQLite(), .db_name = "db_credentials.sqlite") {

  pool <- pool::dbPool(
    drv = .drv,
    dbname = .db_name
  )

  con <- pool::poolCheckout(pool)
  pool::poolReturn(con)

  data_credentials <- dplyr::collect(dplyr::tbl(con, "credentials_db"))

  pool::poolClose(pool)

  return(data_credentials)
}


# ## IRT functions
# # 2PL model function
# prob_2pl <- function(theta, discrimination, difficulty) {
#   return(1 / (1 + exp(-discrimination * (theta - difficulty))))
# }
#
# # Negative log-likelihood function for the 2PL model
# negative_log_likelihood <- function(theta, responses, discrimination, difficulty) {
#   p <- prob_2pl(theta, discrimination, difficulty)
#   return(-sum(responses * log(p) + (1 - responses) * log(1 - p)))
# }
#
# # Function to estimate theta for a person using Maximum Likelihood
# estimate_theta <- function(responses, discrimination, difficulty) {
#   result <- optim(0, negative_log_likelihood, method="L-BFGS-B",
#                   lower=-4, upper=4, responses=responses,
#                   discrimination=discrimination, difficulty=difficulty)
#   return(result$par)
# }
