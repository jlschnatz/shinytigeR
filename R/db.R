db_with <- function(path, fn, wal = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (wal) {
    DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
    # Wait up to 5 s before returning SQLITE_BUSY, instead of failing instantly.
    # Matters when two sessions write concurrently (e.g. simultaneous registrations).
    DBI::dbExecute(con, "PRAGMA busy_timeout=5000;")
  }
  fn(con)
}

db_get_items <- function(path = DB_ITEMS()) {
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

db_get_userdata <- function(user_id, path = DB_USERS()) {
  db_with(
    path,
    function(con) {
      if (!DBI::dbExistsTable(con, user_id)) {
        return(data.frame())
      }
      DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"', user_id))
    },
    wal = TRUE
  )
}

db_user_exists <- function(user_id, path = DB_USERS()) {
  db_with(
    path,
    function(con) {
      DBI::dbExistsTable(con, user_id)
    },
    wal = TRUE
  )
}

db_write_response <- function(user_id, df, path = DB_USERS()) {
  db_with(
    path,
    function(con) {
      if (!DBI::dbExistsTable(con, user_id)) {
        DBI::dbCreateTable(con, user_id, df)
      } else {
        # Per-user tables are created lazily from whatever build_response_row()
        # produced on that user's first-ever write, so a table created before
        # a new response column existed (e.g. typed_value, added for numeric
        # items) won't have it. Add any missing columns before appending,
        # rather than requiring a one-off migration of every existing table.
        existing_cols <- DBI::dbListFields(con, user_id)
        missing_cols <- setdiff(names(df), existing_cols)
        for (col in missing_cols) {
          sql_type <- if (is.numeric(df[[col]])) "REAL" else "TEXT"
          DBI::dbExecute(
            con,
            sprintf('ALTER TABLE "%s" ADD COLUMN "%s" %s', user_id, col, sql_type)
          )
        }
      }
      DBI::dbAppendTable(con, user_id, df)
    },
    wal = TRUE
  )
}

db_get_credentials <- function(path = DB_CREDS()) {
  db_with(path, function(con) {
    DBI::dbGetQuery(con, "SELECT * FROM credentials_db")
  })
}

db_username_exists <- function(username, path = DB_CREDS()) {
  db_with(path, function(con) {
    res <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM credentials_db WHERE user_name = ?",
      params = list(username)
    )
    res$n > 0L
  })
}

db_register_user <- function(username, password_plain, path = DB_CREDS()) {
  db_with(path, wal = TRUE, function(con) {
    DBI::dbExecute(con, "BEGIN IMMEDIATE")
    exists <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM credentials_db WHERE user_name = ?",
      params = list(username)
    )$n > 0L
    if (exists) {
      DBI::dbExecute(con, "ROLLBACK")
      stop("username_taken")
    }
    hashed <- sodium::password_store(password_plain)
    DBI::dbExecute(
      con,
      "INSERT INTO credentials_db (user_name, password_hashed) VALUES (?, ?)",
      params = list(username, hashed)
    )
    DBI::dbExecute(con, "COMMIT")
    invisible(TRUE)
  })
}
