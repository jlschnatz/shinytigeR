#' @import dbplyr
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

#' @import dbplyr
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

#' @import dbplyr
db_check_userexists <- function(id_user, .drv = RSQLite::SQLite(), .db_name = "db_user.sqlite") {

  pool <- pool::dbPool(
    drv = .drv,
    dbname = .db_name
  )

  con <- pool::poolCheckout(pool)
  vec_username <- DBI::dbListTables(con)
  pool::poolReturn(con)
  pool::poolClose(pool)

  return(as.logical(id_user %in% vec_username))
}

#' @import dbplyr
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


#' @import dbplyr
write_userdata_db <- function(id_user, .response_data_df, .drv = RSQLite::SQLite(), .db = "db_user.sqlite") {
  pool <- pool::dbPool(drv = .drv, dbname = .db)
  user_vec <- pool::dbListTables(pool)

  if (!id_user %in% user_vec) {
    col_names <- colnames(.response_data_df)
    pool::dbCreateTable(
      conn = pool,
      name = id_user,
      fields = setNames(col_names, col_names)
    )
  }

  pool::dbAppendTable(conn = pool, name = id_user, value = .response_data_df)
  pool::poolClose(pool)
}


