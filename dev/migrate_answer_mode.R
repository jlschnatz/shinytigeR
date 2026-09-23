# Adds the answer_mode column (mc/num) to db_item.sqlite's item_db table if
# it doesn't exist yet, and backfills any NULL values to "mc". Idempotent —
# safe to run multiple times, INCLUDING against the real production DB. Does
# NOT insert any sample/dummy items — see dev/add_numeric_samples.R for that
# (dev-only; sources this file to reuse migrate_answer_mode() below rather
# than duplicating the migration logic).
#
# Why this has to run before deploying a build that includes numeric items:
# mod_mc_answer.R and mod_numeric_answer.R both gate their rendering on
# identical(item$answer_mode[1], "mc"/"num"). If the column doesn't exist at
# all yet, item$answer_mode resolves to NULL rather than throwing an error —
# so BOTH checks are FALSE, and NEITHER module renders anything. The result
# isn't a crash: the stimulus and progress bar still show, but the answer
# area is silently blank and "Antwort prüfen" never enables, for EVERY item,
# MC included — with no error surfaced anywhere. Run this first.
#
# Usage, from the project root:
#
#   rv run dev/migrate_answer_mode.R                              # local db_item.sqlite
#   TIGER_DB_DIR=/opt/shinyapp rv run dev/migrate_answer_mode.R    # production, e.g. the ShinyProxy-mounted volume

migrate_answer_mode <- function(path) {
  if (!file.exists(path)) {
    stop("No db_item.sqlite found at ", path, call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cols <- DBI::dbListFields(con, "item_db")
  if (!"answer_mode" %in% cols) {
    message("Adding answer_mode column to ", path, "...")
    DBI::dbExecute(con, "ALTER TABLE item_db ADD COLUMN answer_mode TEXT")
  }

  n_null <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM item_db WHERE answer_mode IS NULL"
  )$n
  if (n_null > 0L) {
    message("Backfilling ", n_null, " row(s) with answer_mode IS NULL to 'mc' in ", path, "...")
    DBI::dbExecute(con, "UPDATE item_db SET answer_mode = 'mc' WHERE answer_mode IS NULL")
  } else {
    message("answer_mode already migrated in ", path, " — nothing to do.")
  }
  invisible(n_null)
}

# Only auto-run when invoked directly (rv run/Rscript). dev/add_numeric_samples.R
# sets .migrate_answer_mode_sourced_only before source()-ing this file, so it
# gets migrate_answer_mode() without this file running the migration a second
# time redundantly.
if (!exists(".migrate_answer_mode_sourced_only", inherits = FALSE)) {
  migrate_answer_mode(file.path(Sys.getenv("TIGER_DB_DIR", unset = "."), "db_item.sqlite"))
}
