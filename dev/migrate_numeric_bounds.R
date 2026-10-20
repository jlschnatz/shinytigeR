# Adds the per-option numeric range columns lower_answeroption_01..06 and
# upper_answeroption_01..06 (REAL) to db_item.sqlite's item_db table, and makes
# sure answer_correct is a TEXT column (numeric items may list several correct
# options, e.g. "1;3"). See "Numeric item rules" in CLAUDE.md for what the
# columns mean. Idempotent — safe to run multiple times, INCLUDING against the
# real production DB. Existing data is copied over unchanged; the new columns
# start out NULL (= exact match only). Does NOT insert any items.
#
# Why a table rebuild instead of ALTER TABLE ADD COLUMN: SQLite can only append
# columns at the end, which would put the bounds after the irt_* columns. For
# readability (item generation/validation works on these rows directly) they
# go right after if_answeroption_06, so each option's value / feedback / range
# columns sit together. The rebuild runs in a single transaction.
#
# Usage, from the project root:
#
#   rv run dev/migrate_numeric_bounds.R                              # local db_item.sqlite
#   TIGER_DB_DIR=/opt/shinyapp rv run dev/migrate_numeric_bounds.R    # production, e.g. the ShinyProxy-mounted volume

BOUND_COLS <- c(
  sprintf("lower_answeroption_%02d", 1:6),
  sprintf("upper_answeroption_%02d", 1:6)
)

migrate_numeric_bounds <- function(path) {
  if (!file.exists(path)) {
    stop("No db_item.sqlite found at ", path, call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  info <- DBI::dbGetQuery(con, "PRAGMA table_info(item_db)")
  missing <- setdiff(BOUND_COLS, info$name)
  correct_is_text <- identical(toupper(info$type[info$name == "answer_correct"]), "TEXT")

  if (length(missing) == 0L && correct_is_text) {
    message("Numeric bound columns already migrated in ", path, " — nothing to do.")
    return(invisible(FALSE))
  }

  indexes <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'item_db' AND sql IS NOT NULL"
  )$name
  if (length(indexes) > 0L) {
    stop(
      "item_db has indexes (", paste(indexes, collapse = ", "),
      ") which this rebuild would drop — extend the script to recreate them first.",
      call. = FALSE
    )
  }

  # New column order: existing columns, with any missing bound columns inserted
  # after if_answeroption_06 (or appended, if that column doesn't exist).
  old_cols <- info$name
  types <- stats::setNames(info$type, info$name)
  types[["answer_correct"]] <- "TEXT"
  types[missing] <- "REAL"
  anchor <- match("if_answeroption_06", old_cols)
  new_cols <- if (is.na(anchor)) {
    c(old_cols, missing)
  } else {
    append(old_cols, missing, after = anchor)
  }

  col_defs <- paste(
    sprintf("`%s` %s", new_cols, types[new_cols]),
    collapse = ",\n  "
  )
  old_list <- paste(sprintf("`%s`", old_cols), collapse = ", ")

  message(
    "Rebuilding item_db in ", path, ": adding ", length(missing),
    " bound column(s)", if (!correct_is_text) ", converting answer_correct to TEXT", "..."
  )
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, sprintf("CREATE TABLE item_db_new (\n  %s\n)", col_defs))
    DBI::dbExecute(
      con,
      sprintf("INSERT INTO item_db_new (%s) SELECT %s FROM item_db", old_list, old_list)
    )
    n_old <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db")$n
    n_new <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db_new")$n
    if (n_old != n_new) {
      stop("Row count mismatch after copy (", n_old, " vs ", n_new, ") — rolled back.", call. = FALSE)
    }
    DBI::dbExecute(con, "DROP TABLE item_db")
    DBI::dbExecute(con, "ALTER TABLE item_db_new RENAME TO item_db")
  })
  message("Done — ", length(new_cols), " columns.")
  invisible(TRUE)
}

# Only auto-run when invoked directly (rv run/Rscript). dev/add_numeric_samples.R
# sets .migrate_numeric_bounds_sourced_only before source()-ing this file.
if (!exists(".migrate_numeric_bounds_sourced_only", inherits = FALSE)) {
  migrate_numeric_bounds(file.path(Sys.getenv("TIGER_DB_DIR", unset = "."), "db_item.sqlite"))
}
