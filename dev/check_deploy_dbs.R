# Checks a DB folder before it's uploaded to the server. Read-only.
#
#   rv run dev/check_deploy_dbs.R <folder>
#   rv run dev/check_deploy_dbs.R --items-only <folder>
#
# Reports what's missing or in an old format; exits with status 1 if anything
# would break the app. An item DB in an old format (missing columns, old skip
# options) comes from an old export: build the pool again with tigertools
# (build_release()). (The migrations that used to fix old pools in place were
# removed in Nov 2026, see git history.)
#
# --items-only checks just db_item.sqlite and img_item/, e.g. a freshly built
# pool in a release folder that has no user or credentials databases.
# Otherwise the AI feedback's ai_config.json and ai_lookup.csv are checked too
# (if there's a config; without one the feature is just off).

args <- commandArgs(trailingOnly = TRUE)
items_only <- "--items-only" %in% args
args <- setdiff(args, "--items-only")
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
db_files <- if (items_only) "db_item.sqlite" else c("db_item.sqlite", "db_user.sqlite", "db_credentials.sqlite", "db_ability.sqlite")
for (f in db_files) {
  if (file.exists(file.path(dir, f))) ok(f) else fail(paste(f, "is missing (an empty file is fine for db_user/db_ability)"))
}
code_file <- file.path(dir, "reg_code.txt")
if (!items_only) {
  if (file.exists(code_file) && any(nzchar(trimws(readLines(code_file, warn = FALSE))))) {
    ok("reg_code.txt (semester code)")
  } else {
    warn("no reg_code.txt: self-registration only works if TIGER_REG_CODE is set in ShinyProxy")
  }
}

# ── Item DB ───────────────────────────────────────────────────────────────────
if (file.exists(file.path(dir, "db_item.sqlite"))) {
  with_db("db_item.sqlite", function(con) {
    cols <- DBI::dbListFields(con, "item_db")
    n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db")$n
    cat("\n  item_db:", n, "items\n")

    # Pools built with tigertools::build_release() record their inputs
    if (DBI::dbExistsTable(con, "build_info")) {
      info <- DBI::dbGetQuery(con, "SELECT * FROM build_info")
      for (i in seq_len(nrow(info))) {
        hash <- if (is.na(info$md5[i])) "" else sprintf(" (md5 %s)", substr(info$md5[i], 1, 8))
        ok(sprintf("built from %s: %s%s", info$input[i], info$file[i], hash))
      }
    } else {
      warn("no build_info table: not built with tigertools::build_release()")
    }

    if ("answer_mode" %in% cols) {
      bad <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE answer_mode IS NULL OR answer_mode NOT IN ('mc', 'num')")$n
      if (bad == 0L) ok("answer_mode set on every item") else fail(sprintf("%d item(s) without a valid answer_mode: old item DB format, export it again with tigertools", bad))
    } else {
      fail("no answer_mode column: old item DB format, export it again with tigertools (without it no item can be answered)")
    }

    if (all(sprintf("lower_answeroption_%02d", 1:6) %in% cols)) {
      ok("numeric range columns")
    } else {
      warn("no lower_/upper_ range columns: old item DB format, export it again with tigertools (numeric items fall back to exact match)")
    }

    skip_vals <- c("Frage überspringen.", "www/skip.png")
    opt_cols <- intersect(sprintf("answeroption_%02d", 1:6), cols)
    where <- paste(sprintf("%s IN ('%s')", opt_cols, paste(skip_vals, collapse = "', '")), collapse = " OR ")
    n_skip <- DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM item_db WHERE", where))$n
    if (n_skip == 0L) ok("no old skip options") else fail(sprintf("%d item(s) still have the old skip option: old item DB format, export it again with tigertools", n_skip))

    samples <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE id_item BETWEEN 90001 AND 90004")$n
    if (samples == 0L) ok("no dev sample items") else fail("contains the dev sample items 90001-90004 (the dev sample pool, dev/db_item_sample.sqlite)")

    no_diff <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE irt_diff IS NULL")$n
    if (no_diff > 0L) warn(sprintf("%d item(s) without irt_diff: they don't count towards the estimate", no_diff))
    if ("irt_provisional" %in% cols) {
      n_prov <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM item_db WHERE irt_provisional = 1")$n
      warn(sprintf("%d item(s) with a preliminary difficulty (irt_provisional = 1)", n_prov))
    } else {
      warn("no irt_provisional column: every difficulty counts as calibrated")
    }

    # Every formula must render with KaTeX (render_math() in R/utils.R, the
    # same pipeline as the app): one it can't parse shows as red TeX
    if (requireNamespace("pkgload", quietly = TRUE)) {
      suppressMessages(pkgload::load_all(quiet = TRUE))
      txt_cols <- intersect(c("stimulus_text", sprintf("answeroption_%02d", 1:6), sprintf("if_answeroption_%02d", 1:6)), cols)
      items <- DBI::dbGetQuery(con, paste("SELECT id_item,", paste(txt_cols, collapse = ", "), "FROM item_db"))
      bad_ids <- integer(0)
      n_math <- 0L
      for (col in txt_cols) {
        has <- !is.na(items[[col]]) & grepl("$", items[[col]], fixed = TRUE)
        n_math <- n_math + sum(has)
        for (i in which(has)) {
          html <- suppressWarnings(render_md(items[[col]][i]))
          if (grepl("katex-error", html, fixed = TRUE)) bad_ids <- c(bad_ids, items$id_item[i])
        }
      }
      bad_ids <- unique(bad_ids)
      if (length(bad_ids) == 0L) ok(sprintf("all formulas render (%d texts with math)", n_math))
      else warn(sprintf("%d item(s) with a formula KaTeX can't render (shown as red TeX): %s", length(bad_ids), paste(head(bad_ids, 10), collapse = ", ")))
    } else {
      warn("pkgload not available: formulas not checked")
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
if (!items_only && file.exists(file.path(dir, "db_credentials.sqlite"))) {
  with_db("db_credentials.sqlite", function(con) {
    if (DBI::dbExistsTable(con, "credentials_db")) {
      cat("\n")
      ok(sprintf("credentials_db: %d accounts", DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM credentials_db")$n))
    } else {
      fail("db_credentials.sqlite has no credentials_db table")
    }
  })
}

# ── AI feedback (optional) ────────────────────────────────────────────────────
# Without ai_config.json the feature is off, which is fine. With one, it must
# be usable as it is: readable, models listed, a real cooldown (0 is for local
# testing), and a valid source list (ai_lookup.csv).
if (!items_only) {
  cat("\n")
  cfg_file <- file.path(dir, "ai_config.json")
  if (!file.exists(cfg_file)) {
    warn("no ai_config.json: AI feedback is off")
  } else {
    if (!exists("ai_config", mode = "function")) suppressMessages(pkgload::load_all(quiet = TRUE))
    raw <- tryCatch(jsonlite::fromJSON(cfg_file, simplifyVector = TRUE), error = function(e) e)
    if (inherits(raw, "error") || !is.list(raw)) {
      fail("ai_config.json can't be read (invalid JSON): AI feedback would be off")
    } else {
      unknown <- setdiff(names(raw), names(AI_CONFIG_DEFAULTS))
      if (length(unknown) > 0L) {
        fail(sprintf("ai_config.json: unknown key(s) %s (ignored by the app; misspelt or renamed?)", paste(unknown, collapse = ", ")))
      }
      cfg <- ai_config(dir)
      if (!isTRUE(raw$enabled)) {
        warn("ai_config.json: \"enabled\" is false, AI feedback is off")
      } else if (length(cfg$models) == 0L) {
        fail("ai_config.json: enabled, but no models listed (AI feedback would be off)")
      } else {
        ok(sprintf("AI feedback on, models in this order: %s", paste(cfg$models, collapse = ", ")))
        no_label <- setdiff(cfg$models, names(cfg$model_labels))
        if (length(no_label) > 0L) {
          warn(sprintf("no model_labels entry for %s: students see the API id", paste(no_label, collapse = ", ")))
        }
        if (cfg$cooldown_minutes <= 0) {
          fail("cooldown_minutes is 0: that's the local testing value (default 240)")
        } else if (cfg$cooldown_minutes < 60) {
          warn(sprintf("cooldown_minutes is only %s (default 240)", cfg$cooldown_minutes))
        } else {
          ok(sprintf("cooldown %s min, daily limit %s API calls", cfg$cooldown_minutes, cfg$daily_limit))
        }
        lookup_problems <- ai_lookup_problems(read_ai_lookup(file.path(dir, AI_LOOKUP_FILE)))
        if (length(lookup_problems) == 0L) {
          ok(sprintf("%s: %d sources", AI_LOOKUP_FILE, nrow(read_ai_lookup(file.path(dir, AI_LOOKUP_FILE)))))
        } else {
          for (p in lookup_problems) fail(sprintf("%s: %s (dev/export_ai_lookup.R)", AI_LOOKUP_FILE, p))
        }
        key_file <- file.path(dir, AI_KEY_FILE)
        if (file.exists(key_file) && any(nzchar(trimws(readLines(key_file, warn = FALSE))))) {
          warn(sprintf("API key in %s: fine, but TIGER_LLM_KEY as ShinyProxy container-env is preferred (keeps it off the data volume)", AI_KEY_FILE))
        } else {
          warn(sprintf("no %s: TIGER_LLM_KEY must be set in ShinyProxy (container-env), else the feature is off", AI_KEY_FILE))
        }
      }
    }
  }
  if (file.exists(file.path(dir, "db_feedback.sqlite"))) {
    warn("db_feedback.sqlite in this folder: don't upload it over the server's (the app creates it)")
  }
}

cat("\n", if (problems == 0L) "Ready to upload." else sprintf("%d problem(s).", problems), "\n")
quit(status = if (problems == 0L) 0L else 1L)
