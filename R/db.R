db_with <- function(path, fn, wal = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  if (wal) DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

db_get_items <- function(path = DB_ITEMS) {
  df <- db_with(path, function(con) {
    DBI::dbGetQuery(con, "SELECT * FROM item_db")
  })
  # Adjust image paths: DB stores "www/foo.png"; Shiny serves www/ as root
  # so we strip the "www/" prefix.  img_item/ is symlinked under www/.
  img_cols <- grep("^(stimulus_image|answeroption_)", names(df), value = TRUE)
  for (col in img_cols) {
    df[[col]] <- sub("^www/", "img_item/", df[[col]])
  }
  df$learning_area <- factor(df$learning_area, levels = LEARNING_AREA_LEVELS)
  df
}

db_get_userdata <- function(user_id, path = DB_USERS) {
  db_with(path, function(con) {
    if (!DBI::dbExistsTable(con, user_id)) return(data.frame())
    DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"', user_id))
  }, wal = TRUE)
}

db_user_exists <- function(user_id, path = DB_USERS) {
  db_with(path, function(con) {
    DBI::dbExistsTable(con, user_id)
  }, wal = TRUE)
}

db_write_response <- function(user_id, df, path = DB_USERS) {
  db_with(path, function(con) {
    if (!DBI::dbExistsTable(con, user_id)) {
      DBI::dbCreateTable(con, user_id, df)
    }
    DBI::dbAppendTable(con, user_id, df)
  }, wal = TRUE)
}

db_get_credentials <- function(path = DB_CREDS) {
  db_with(path, function(con) {
    DBI::dbGetQuery(con, "SELECT * FROM credentials_db")
  })
}
