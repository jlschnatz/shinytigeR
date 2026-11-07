# Creates the databases a contributor needs to run the app locally:
#
#   db_credentials.sqlite  — a single test account (test / test123)
#   db_user.sqlite         — empty; tables are created per user on first write
#   db_ability.sqlite      — empty; tables are created per user on first write
#
# They go into the DB folder: TIGER_DB_DIR if set, else ../db, i.e. app/db
# next to the repo when run from the project root (created if missing). The
# same folder dev/run.R uses, laid out like the server's shinytigeR-DB. It's
# outside the repo, so nothing in it can end up in git.
#
# If db_item.sqlite is missing, the committed sample pool
# (dev/db_item_sample.sqlite, 18 items) is copied into place with its images
# (dev/img_item_sample/ -> img_item/), so the app runs immediately. An existing
# db_item.sqlite is never touched — maintainers keep the full pool.
#
# Usage, from the project root:
#
#   rv run dev/seed_db.R                          # into ../db; creates missing files, refuses to clobber
#   rv run dev/seed_db.R --force                  # overwrites existing files (not db_item.sqlite)
#   TIGER_DB_DIR=<folder> rv run dev/seed_db.R    # into another folder

TEST_USER <- "test"
TEST_PW <- "test123"

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

db_dir <- Sys.getenv("TIGER_DB_DIR", unset = "../db")
dir.create(db_dir, showWarnings = FALSE, recursive = TRUE)
creds_path <- file.path(db_dir, "db_credentials.sqlite")
users_path <- file.path(db_dir, "db_user.sqlite")
ability_path <- file.path(db_dir, "db_ability.sqlite")

# ── Guard ───────────────────────────────────────────────────────────────────
# The real credentials DB holds ~250 student password hashes and the real user
# DB holds three years of responses. Overwriting either by accident is not
# recoverable from this repo, so require an explicit --force.
existing <- Filter(file.exists, c(creds_path, users_path, ability_path))
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

# ── Ability estimates: empty ────────────────────────────────────────────────
# db_write_ability() creates a per-user table on first write, same as above.
if (file.exists(ability_path)) file.remove(ability_path)
con_a <- DBI::dbConnect(RSQLite::SQLite(), ability_path)
DBI::dbDisconnect(con_a)

# ── Items: sample pool, only if no pool is present ──────────────────────────
# Deliberately not covered by --force: the full pool is not reproducible from
# this repository, so this never overwrites an existing db_item.sqlite.
items_path <- file.path(db_dir, "db_item.sqlite")
sample_path <- file.path("dev", "db_item_sample.sqlite")
items_note <- if (file.exists(items_path)) {
  paste0("  ", items_path, "  (left as is)\n")
} else if (file.exists(sample_path)) {
  file.copy(sample_path, items_path)
  # The sample items' images go next to it, like on the server (img_item/)
  img_dir <- file.path(db_dir, "img_item")
  dir.create(img_dir, showWarnings = FALSE)
  file.copy(list.files(file.path("dev", "img_item_sample"), full.names = TRUE), img_dir)
  paste0("  ", items_path, "  (copied from sample pool, with its images in img_item/)\n")
} else {
  paste0("  ", items_path, "  MISSING — and no sample found. See SETUP.md.\n")
}

message(
  "Seeded:\n",
  "  ", creds_path, "  (1 account: ", TEST_USER, " / ", TEST_PW, ")\n",
  "  ", users_path, "  (empty)\n",
  "  ", ability_path, "  (empty)\n",
  items_note,
  "\nStart the app with:  rv run dev/run.R\n"
)
