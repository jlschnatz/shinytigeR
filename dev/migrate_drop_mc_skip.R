# Removes the baked-in "skip" answer option from every MC item in
# db_item.sqlite's item_db table. Idempotent — safe to run multiple times,
# INCLUDING against the real production DB. Does NOT insert any items.
#
# Background: MC items used to carry a skip pseudo-option as their LAST
# non-empty option ("Frage überspringen." for text answers, "www/skip.png" for
# image answers), with the fixed feedback "Alles klar! Du hast die Aufgabe
# übersprungen.". The app now has one dedicated "Aufgabe überspringen" button
# for all items (mod_practice.R) and the feedback text lives in SKIP_FEEDBACK
# (constants.R), so the option is redundant — and with the new app it would
# show up as an ordinary answer option, scored as incorrect. Run this before
# deploying a build with the skip button.
#
# Only the last non-empty option of an `answer_mode = 'mc'` item is touched, and
# only if its value is exactly one of the two skip values; its
# if_answeroption_0X feedback is cleared along with it. Aborts without changing
# anything if any such skip option is listed in answer_correct.
#
# Existing response rows in db_user.sqlite need no migration: old MC skip rows
# keep selected_option = <index of the former skip option>, but `skipped` (TRUE)
# and `bool_correct` (NA) — which is what the app reads — are already right.
#
# Usage, from the project root:
#
#   rv run dev/migrate_drop_mc_skip.R                                  # local db_item.sqlite
#   TIGER_DB_DIR=/opt/shinyapp rv run dev/migrate_drop_mc_skip.R        # production, e.g. the ShinyProxy-mounted volume
#   rv run dev/migrate_drop_mc_skip.R dev/db_item_sample.sqlite         # any other file, by path

MC_SKIP_VALUES <- c("Frage überspringen.", "www/skip.png")

migrate_drop_mc_skip <- function(path) {
  if (!file.exists(path)) {
    stop("No item database found at ", path, call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  opt_cols <- sprintf("answeroption_%02d", 1:6)
  items <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT id_item, answer_correct,",
      paste(opt_cols, collapse = ", "),
      "FROM item_db WHERE answer_mode = 'mc'"
    )
  )

  # Per item: column position of the last non-empty option, if it's a skip
  hits <- do.call(rbind, lapply(seq_len(nrow(items)), function(r) {
    vals <- unlist(items[r, opt_cols], use.names = FALSE)
    filled <- which(!is.na(vals) & nzchar(vals))
    if (length(filled) == 0L) return(NULL)
    last <- max(filled)
    if (!vals[last] %in% MC_SKIP_VALUES) return(NULL)
    correct <- suppressWarnings(as.integer(strsplit(
      as.character(items$answer_correct[r]), ";", fixed = TRUE
    )[[1]]))
    data.frame(
      id_item = items$id_item[r],
      pos = last,
      # answer_correct indexes the compacted option list (see
      # get_answeroptions()), so compare against the skip's compacted index
      skip_is_correct = length(filled) %in% correct
    )
  }))

  if (is.null(hits) || nrow(hits) == 0L) {
    message("No MC skip options left in ", path, " — nothing to do.")
    return(invisible(0L))
  }
  if (any(hits$skip_is_correct)) {
    stop(
      "Aborting, nothing changed: the skip option is marked correct for item(s) ",
      paste(hits$id_item[hits$skip_is_correct], collapse = ", "),
      call. = FALSE
    )
  }

  DBI::dbWithTransaction(con, {
    for (pos in unique(hits$pos)) {
      ids <- hits$id_item[hits$pos == pos]
      DBI::dbExecute(
        con,
        sprintf(
          "UPDATE item_db SET answeroption_%02d = NULL, if_answeroption_%02d = NULL WHERE id_item IN (%s)",
          pos, pos, paste(ids, collapse = ", ")
        )
      )
    }
  })
  message("Removed the skip option from ", nrow(hits), " MC item(s) in ", path, ".")
  invisible(nrow(hits))
}

# Only auto-run when invoked directly (rv run/Rscript). Tests set
# .migrate_drop_mc_skip_sourced_only before source()-ing this file to get
# migrate_drop_mc_skip() without running it against a real DB.
if (!exists(".migrate_drop_mc_skip_sourced_only", inherits = FALSE)) {
  args <- commandArgs(trailingOnly = TRUE)
  migrate_drop_mc_skip(
    if (length(args) >= 1L) {
      args[1]
    } else {
      file.path(Sys.getenv("TIGER_DB_DIR", unset = "."), "db_item.sqlite")
    }
  )
}
