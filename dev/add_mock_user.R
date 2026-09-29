# Inserts a synthetic user ("mockuser") with a practice response for every
# item in the local db_item.sqlite, across every learning area — useful for
# exercising dashboard/competency-map/ability-persistence features locally
# without needing real (and privacy-sensitive) student data. About 15% of
# items also get a later second attempt, some of which are skips, plus a few
# items that were only ever skipped — so the "latest non-skipped attempt"
# rule in compute_and_save_ability() (R/irt.R) is exercised too.
#
# With --irt-backfill, also fills in a placeholder irt_diff for any item
# still missing one (never overwrites an existing value), so every learning
# area actually produces a theta locally. The app uses a 1PL model, so only
# the difficulty irt_diff matters; irt_discr is left alone. OFF BY DEFAULT and
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
# irt_diff). Never touches any other user's data.
#
# dev-only — never run against a production db_user.sqlite or db_item.sqlite.
#
# Usage:
#   rv run dev/add_mock_user.R                    # responses only
#   rv run dev/add_mock_user.R --irt-backfill      # + fill missing irt_diff

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

# ── Optional: backfill placeholder difficulties for items missing them ────
con_i <- DBI::dbConnect(RSQLite::SQLite(), items_path)
items <- DBI::dbGetQuery(con_i, "SELECT id_item, learning_area, irt_diff FROM item_db")

missing <- is.na(items$irt_diff)
if (DO_IRT_BACKFILL && any(missing)) {
  # A small varied set of plausible difficulties, cycled by row — not a real
  # calibration, just enough spread that a mock competency map doesn't look
  # perfectly uniform.
  placeholder_b <- c(-1.2, 0.0, 1.0, -0.4)
  idx <- which(missing)
  for (k in seq_along(idx)) {
    DBI::dbExecute(
      con_i,
      "UPDATE item_db SET irt_diff = ? WHERE id_item = ?",
      params = list(
        placeholder_b[((k - 1) %% length(placeholder_b)) + 1],
        items$id_item[idx[k]]
      )
    )
  }
  message("Backfilled placeholder irt_diff for ", length(idx), " item(s) (--irt-backfill).")
} else if (any(missing)) {
  areas_affected <- unique(items$learning_area[missing])
  message(
    length(which(missing)), " item(s) still missing irt_diff (",
    paste(areas_affected, collapse = ", "),
    ") — those items don't count towards Kompetenz. ",
    "Re-run with --irt-backfill to fill in dev-only placeholder values."
  )
}

items <- DBI::dbGetQuery(con_i, "SELECT * FROM item_db")
DBI::dbDisconnect(con_i)

# Number of answer options per item; the last one is always "Überspringen"
opt_cols <- grep("^answeroption_0[1-6]$", names(items), value = TRUE)
n_opts <- rowSums(!is.na(items[, opt_cols, drop = FALSE]) & items[, opt_cols, drop = FALSE] != "")

# One response row per (item index, outcome), encoded the way the app's
# build_response_row() does: skip = last option, bool_correct NA.
make_rows <- function(i, outcome, dt) {
  # answer_correct is text and may list several options ("1;2"); use the first
  correct_opt <- as.integer(strsplit(as.character(items$answer_correct[i]), ";")[[1]][1])
  wrong_opt <- setdiff(seq_len(n_opts[i] - 1L), correct_opt)[1]
  selected <- switch(outcome,
    correct = correct_opt,
    incorrect = wrong_opt,
    skip = as.integer(n_opts[i])
  )
  data.frame(
    id_user = MOCK_USER,
    id_session = paste0("mock_sess_", (dt %/% 86400L) %% 5L + 1L),
    id_date = as.integer(dt %/% 86400L),
    id_datetime = as.integer(dt),
    id_item = items$id_item[i],
    learning_area = items$learning_area[i],
    selected_option = selected,
    answer_correct = items$answer_correct[i],
    bool_correct = switch(outcome, correct = 1L, incorrect = 0L, skip = NA_integer_),
    skipped = as.integer(outcome == "skip"),
    stringsAsFactors = FALSE
  )
}

# ── Mock responses: one per item, ~70% correct, spread over the last 6 days ──
set.seed(42) # reproducible mock data across re-runs
n <- nrow(items)
now <- as.integer(Sys.time())
first_dt <- now - sample(86400L:518400L, n, replace = TRUE) # 1-6 days ago
first_outcome <- ifelse(runif(n) < 0.7, "correct", "incorrect")

# A few items were only ever skipped (they must not count at all)
only_skipped <- sample(n, max(1L, round(0.03 * n)))
first_outcome[only_skipped] <- "skip"

# ~15% of the others get a second, later attempt: half of those are skips
# (the first real answer should count), the rest a fresh answer (it wins)
repeat_idx <- sample(setdiff(seq_len(n), only_skipped), round(0.15 * n))
second_outcome <- ifelse(
  seq_along(repeat_idx) %% 2L == 0L,
  "skip",
  ifelse(runif(length(repeat_idx)) < 0.8, "correct", "incorrect")
)
second_dt <- pmin(first_dt[repeat_idx] + sample(3600L:86400L, length(repeat_idx), replace = TRUE), now)

rows <- do.call(rbind, c(
  lapply(seq_len(n), function(i) make_rows(i, first_outcome[i], first_dt[i])),
  Map(make_rows, repeat_idx, second_outcome, second_dt)
))
rows <- rows[order(rows$id_datetime), ]

con_u <- DBI::dbConnect(RSQLite::SQLite(), users_path)
if (DBI::dbExistsTable(con_u, MOCK_USER)) {
  DBI::dbRemoveTable(con_u, MOCK_USER)
}
DBI::dbWriteTable(con_u, MOCK_USER, rows)
DBI::dbDisconnect(con_u)

message(
  "Seeded ", nrow(rows), " mock response(s) for user '", MOCK_USER,
  "' across ", length(unique(rows$learning_area)), " learning area(s) in ", users_path,
  "\n(", length(repeat_idx), " items answered twice, ", sum(second_outcome == "skip"),
  " of them skipped the second time; ", length(only_skipped), " items only skipped)",
  "\nLog in as '", MOCK_USER, "' / '", MOCK_PW, "'."
)
