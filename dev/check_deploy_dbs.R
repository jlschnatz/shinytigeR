# Checks a DB folder before it's uploaded to the server. Read-only.
#
#   rv run dev/check_deploy_dbs.R <folder>
#
# Reports what's missing or not migrated; exits with status 1 if anything
# would break the app. Fix item-DB problems with the three migrations:
#   TIGER_DB_DIR=<folder> rv run dev/migrate_answer_mode.R
#   TIGER_DB_DIR=<folder> rv run dev/migrate_numeric_bounds.R
#   TIGER_DB_DIR=<folder> rv run dev/migrate_drop_mc_skip.R

args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[1] else Sys.getenv("TIGER_DB_DIR", "../db")
dir <- normalizePath(dir, mustWork = TRUE)
cat("Checking", dir, "\n\n")

problems <- 0L
ok <- function(msg) cat("  ok    ", msg, "\n")
warn <- function(msg) cat("  note  ", msg, "\n")
fail <- function(msg) {
  cat("  FAIL  ", msg, "\n")
  problems <<- problems + 1L
}
with_db <- function(file, fn) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir, file), flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con))
  fn(con)
}

# ── Files ─────────────────────────────────────────────────────────────────────
for (f in c("db_item.sqlite", "db_user.sqlite", "db_credentials.sqlite", "db_ability.sqlite")) {
  if (file.exists(file.path(dir, f))) ok(f) else fail(paste(f, "is missing (an empty file is fine for db_user/db_ability)"))
}
code_file <- file.path(dir, "reg_code.txt")
if (file.exists(code_file) && any(nzchar(trimws(readLines(code_file, warn = FALSE))))) {
  ok("reg_code.txt (semester code)")
} else {
  warn("no reg_code.txt: self-registration only works if TIGER_REG_CODE is set in ShinyProxy")
}

# ── Item DB ───────────────────────────────────────────────────────────────────
if (file.exists(file.path(dir, "db_item.sqlite"))) {
  with_db("db_item.sqlite", function(con) {
    cols <- DBI::dbListFields(con, "item_db")
    n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db")$n
    cat("\n  item_db:", n, "items\n")

    if ("answer_mode" %in% cols) {
      bad <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE answer_mode IS NULL OR answer_mode NOT IN ('mc', 'num')")$n
      if (bad == 0L) ok("answer_mode set on every item") else fail(sprintf("%d item(s) without a valid answer_mode: run migrate_answer_mode.R", bad))
    } else {
      fail("no answer_mode column: run migrate_answer_mode.R (without it no item can be answered)")
    }

    if (all(sprintf("lower_answeroption_%02d", 1:6) %in% cols)) {
      ok("numeric range columns")
    } else {
      warn("no lower_/upper_ range columns: run migrate_numeric_bounds.R (numeric items fall back to exact match)")
    }

    skip_vals <- c("Frage überspringen.", "www/skip.png")
    opt_cols <- intersect(sprintf("answeroption_%02d", 1:6), cols)
    where <- paste(sprintf("%s IN ('%s')", opt_cols, paste(skip_vals, collapse = "', '")), collapse = " OR ")
    n_skip <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM item_db WHERE", where))$n
    if (n_skip == 0L) ok("no old skip options") else fail(sprintf("%d item(s) still have the old skip option: run migrate_drop_mc_skip.R", n_skip))

    samples <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE id_item BETWEEN 90001 AND 90004")$n
    if (samples == 0L) ok("no dev sample items") else fail("contains the dev sample items 90001-90004 (dev/add_numeric_samples.R)")

    no_diff <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE irt_diff IS NULL")$n
    if (no_diff > 0L) warn(sprintf("%d item(s) without irt_diff: they don't count towards the estimate", no_diff))
    if ("irt_provisional" %in% cols) {
      n_prov <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE irt_provisional = 1")$n
      warn(sprintf("%d item(s) with a preliminary difficulty (irt_provisional = 1)", n_prov))
    } else {
      warn("no irt_provisional column: every difficulty counts as calibrated")
    }

    # Every image an item refers to (stimulus and image answer options) must be
    # in the folder's img_item/, which the app serves (add_item_image_path())
    img_cols <- intersect(c("stimulus_image", sprintf("answeroption_%02d", 1:6)), cols)
    vals <- unlist(DBI::dbGetQuery(con, paste("SELECT", paste(img_cols, collapse = ", "), "FROM item_db")), use.names = FALSE)
    imgs <- unique(basename(vals[!is.na(vals) & grepl("\\.(png|jpe?g|svg|gif)$", vals, ignore.case = TRUE)]))
    img_dir <- file.path(dir, "img_item")
    if (length(imgs) == 0L) {
      ok("no item images referenced")
    } else if (!dir.exists(img_dir)) {
      fail(sprintf("%d item image(s) referenced, but there is no img_item/ folder", length(imgs)))
    } else {
      missing <- imgs[!file.exists(file.path(img_dir, imgs))]
      if (length(missing) == 0L) ok(sprintf("all %d item images exist in img_item/", length(imgs)))
      else fail(sprintf("%d item image(s) not in img_item/, e.g. %s", length(missing), missing[1]))
    }
  })
}

# ── Credentials ───────────────────────────────────────────────────────────────
if (file.exists(file.path(dir, "db_credentials.sqlite"))) {
  with_db("db_credentials.sqlite", function(con) {
    if (DBI::dbExistsTable(con, "credentials_db")) {
      cat("\n")
      ok(sprintf("credentials_db: %d accounts", DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM credentials_db")$n))
    } else {
      fail("db_credentials.sqlite has no credentials_db table")
    }
  })
}

cat("\n", if (problems == 0L) "Ready to upload." else sprintf("%d problem(s).", problems), "\n")
quit(status = if (problems == 0L) 0L else 1L)
