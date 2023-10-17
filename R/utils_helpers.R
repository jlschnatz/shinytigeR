#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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
