# Creates the two databases a contributor needs to run the app locally:
#
#   db_credentials.sqlite  — a single test account (test / test123)
#   db_user.sqlite         — empty; tables are created per user on first write
#
# Neither file is tracked by git (see .gitignore). They are derived artifacts:
# regenerate them rather than committing them, so that local practice sessions
# writing to db_user.sqlite never show up as changes to commit.
#
# If db_item.sqlite is missing, the committed 14-item sample pool
# (dev/db_item_sample.sqlite) is copied into place so the app runs immediately.
# An existing db_item.sqlite is never touched — maintainers keep the full pool.
#
# Usage, from the project root:
#
#   rv run dev/seed_db.R           # creates missing files, refuses to clobber
#   rv run dev/seed_db.R --force   # overwrites existing files

TEST_USER <- "test"
TEST_PW <- "test123"

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

db_dir <- Sys.getenv("TIGER_DB_DIR", unset = ".")
creds_path <- file.path(db_dir, "db_credentials.sqlite")
users_path <- file.path(db_dir, "db_user.sqlite")

# ── Guard ───────────────────────────────────────────────────────────────────
# The real credentials DB holds ~250 student password hashes and the real user
# DB holds three years of responses. Overwriting either by accident is not
# recoverable from this repo, so require an explicit --force.
existing <- Filter(file.exists, c(creds_path, users_path))
if (length(existing) > 0L && !force) {
  stop(
    "Refusing to overwrite existing database(s):\n  ",
    paste(existing, collapse = "\n  "),
    "\n\nIf these are the real databases, back them up first.",
    "\nTo replace them with fresh seed data, re-run with --force.",
    call. = FALSE
  )
}

# ── Credentials: one test account ───────────────────────────────────────────
if (file.exists(creds_path)) file.remove(creds_path)

con <- DBI::dbConnect(RSQLite::SQLite(), creds_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)

invisible(DBI::dbExecute(
  con,
  "CREATE TABLE `credentials_db` (
     `user_name`       TEXT,
     `password_hashed` TEXT,
     `permissions`     TEXT
   )"
))
invisible(DBI::dbExecute(
  con,
  "INSERT INTO `credentials_db` (user_name, password_hashed, permissions)
   VALUES (?, ?, ?)",
  params = list(
    TEST_USER,
    sodium::password_store(TEST_PW),
    "standard"
  )
))
DBI::dbDisconnect(con)
on.exit(NULL)

# ── User responses: empty ───────────────────────────────────────────────────
# db_write_response() creates a per-user table on first write, so an empty
# database file is all that is needed.
if (file.exists(users_path)) file.remove(users_path)
con_u <- DBI::dbConnect(RSQLite::SQLite(), users_path)
DBI::dbDisconnect(con_u)

# ── Items: sample pool, only if no pool is present ──────────────────────────
# Deliberately not covered by --force: the full pool is not reproducible from
# this repository, so this never overwrites an existing db_item.sqlite.
items_path <- file.path(db_dir, "db_item.sqlite")
sample_path <- file.path("dev", "db_item_sample.sqlite")
items_note <- if (file.exists(items_path)) {
  paste0("  ", items_path, "  (left as is)\n")
} else if (file.exists(sample_path)) {
  file.copy(sample_path, items_path)
  paste0("  ", items_path, "  (copied from sample pool, 14 items)\n")
} else {
  paste0("  ", items_path, "  MISSING — and no sample found. See SETUP.md.\n")
}

message(
  "Seeded:\n",
  "  ", creds_path, "  (1 account: ", TEST_USER, " / ", TEST_PW, ")\n",
  "  ", users_path, "  (empty)\n",
  items_note,
  "\nStart the app with:  rv run dev/run.R\n"
)
