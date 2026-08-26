# Inserts a synthetic user ("mockuser") with a practice response for every
# item in the local db_item.sqlite, across every learning area — useful for
# exercising dashboard/competency-map/ability-persistence features locally
# without needing real (and privacy-sensitive) student data.
#
# With --irt-backfill, also fills in placeholder irt_discr/irt_diff for any
# item still missing them (never overwrites an existing value), so every
# learning area actually produces a theta locally. OFF BY DEFAULT and
# deliberately opt-in: your local db_item.sqlite is very likely your real
# working copy of the item pool (not just the small committed sample), and
# permanently mixing fake numbers into items you may be tracking as "still
# needs real calibration" is exactly the kind of silent, hard-to-notice
# mistake this flag exists to prevent. These are dev-only placeholder
# values either way, not a real calibration — see ../tiger_calibration (a
# separate, uncommitted project) for that.
#
# Idempotent: re-running drops and recreates "mockuser"'s response table, and
# --irt-backfill only ever fills NA cells (never overwrites an existing
# irt_discr/irt_diff). Never touches any other user's data.
#
# dev-only — never run against a production db_user.sqlite or db_item.sqlite.
#
# Usage:
#   rv run dev/add_mock_user.R                    # responses only
#   rv run dev/add_mock_user.R --irt-backfill      # + fill missing IRT params

MOCK_USER <- "mockuser"
MOCK_PW <- "mockuser123"
DO_IRT_BACKFILL <- "--irt-backfill" %in% commandArgs(trailingOnly = TRUE)

db_dir <- Sys.getenv("TIGER_DB_DIR", unset = ".")
items_path <- file.path(db_dir, "db_item.sqlite")
users_path <- file.path(db_dir, "db_user.sqlite")
creds_path <- file.path(db_dir, "db_credentials.sqlite")

if (!file.exists(items_path)) {
  stop("No db_item.sqlite found at ", items_path, call. = FALSE)
}
if (!file.exists(creds_path)) {
  stop(
    "No db_credentials.sqlite found at ", creds_path,
    " — run dev/seed_db.R first.",
    call. = FALSE
  )
}

# ── Credentials: add a login for the mock user if it doesn't exist yet ────
con_c <- DBI::dbConnect(RSQLite::SQLite(), creds_path)
exists <- DBI::dbGetQuery(
  con_c,
  "SELECT COUNT(*) AS n FROM credentials_db WHERE user_name = ?",
  params = list(MOCK_USER)
)$n > 0L
if (!exists) {
  DBI::dbExecute(
    con_c,
    "INSERT INTO credentials_db (user_name, password_hashed, permissions) VALUES (?, ?, ?)",
    params = list(MOCK_USER, sodium::password_store(MOCK_PW), "standard")
  )
}
DBI::dbDisconnect(con_c)

# ── Optional: backfill placeholder IRT params for items missing them ──────
con_i <- DBI::dbConnect(RSQLite::SQLite(), items_path)
items <- DBI::dbGetQuery(con_i, "SELECT id_item, learning_area, irt_discr, irt_diff FROM item_db")

missing <- is.na(items$irt_discr) | is.na(items$irt_diff)
if (DO_IRT_BACKFILL && any(missing)) {
  # A small varied set of plausible (a, b) pairs, cycled by row — not a real
  # calibration, just enough spread that a mock competency map doesn't look
  # perfectly uniform.
  placeholder_pairs <- list(
    c(a = 0.6, b = -1.2), c(a = 0.9, b = 0.0),
    c(a = 1.1, b = 1.0), c(a = 0.75, b = -0.4)
  )
  idx <- which(missing)
  for (k in seq_along(idx)) {
    pair <- placeholder_pairs[[((k - 1) %% length(placeholder_pairs)) + 1]]
    DBI::dbExecute(
      con_i,
      "UPDATE item_db SET irt_discr = ?, irt_diff = ? WHERE id_item = ?",
      params = list(pair[["a"]], pair[["b"]], items$id_item[idx[k]])
    )
  }
  message("Backfilled placeholder irt_discr/irt_diff for ", length(idx), " item(s) (--irt-backfill).")
} else if (any(missing)) {
  areas_affected <- unique(items$learning_area[missing])
  message(
    length(which(missing)), " item(s) still missing irt_discr/irt_diff (",
    paste(areas_affected, collapse = ", "),
    ") — those areas will show 'Keine Daten' for Kompetenz. ",
    "Re-run with --irt-backfill to fill in dev-only placeholder values."
  )
}

items <- DBI::dbGetQuery(con_i, "SELECT id_item, learning_area FROM item_db")
DBI::dbDisconnect(con_i)

# ── Mock responses: one per item, ~70% correct, spread over the last few days ──
set.seed(42) # reproducible mock data across re-runs
n <- nrow(items)
correct <- runif(n) < 0.7
now <- as.integer(Sys.time())
rows <- data.frame(
  id_user = MOCK_USER,
  id_session = paste0("mock_sess_", rep(1:5, length.out = n)),
  id_date = as.integer(Sys.Date() - sample(0:6, n, replace = TRUE)),
  id_datetime = now - sample(0:518400, n), # spread over ~6 days
  id_item = items$id_item,
  learning_area = items$learning_area,
  selected_option = ifelse(correct, 1L, 2L),
  answer_correct = 1L,
  bool_correct = as.integer(correct),
  skipped = 0L,
  stringsAsFactors = FALSE
)

con_u <- DBI::dbConnect(RSQLite::SQLite(), users_path)
if (DBI::dbExistsTable(con_u, MOCK_USER)) {
  DBI::dbRemoveTable(con_u, MOCK_USER)
}
DBI::dbWriteTable(con_u, MOCK_USER, rows)
DBI::dbDisconnect(con_u)

message(
  "Seeded ", nrow(rows), " mock response(s) for user '", MOCK_USER,
  "' across ", length(unique(rows$learning_area)), " learning area(s) in ", users_path,
  "\nLog in as '", MOCK_USER, "' / '", MOCK_PW, "'."
)
