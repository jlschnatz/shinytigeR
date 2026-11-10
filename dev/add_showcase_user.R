# A showcase account for presenting the app (e.g. in the lecture): a
# simulated student with five weeks of practice, so every part of the
# dashboard has something to show. Simulated through the app's own functions
# (build_response_row(), estimate_competency(), build_ability_rows()), so the
# data looks exactly like a real student's:
#
# - 14 sessions over five weeks, 1-2 areas each, plus three retry sessions
#   ("Fehler nochmal üben", practice_mode = "retry")
# - true ability per area that grows over time; answers drawn from the 1PL
#   model with the pool's difficulties (P = plogis(theta - b))
# - after every session an ability update, as the dashboard's button would
#   save it (only the areas practised since the previous one), dated then:
#   the "Fähigkeitsverlauf" has points over all three time windows
# - Regression clearly weakest, at "Aufbau" ("Zum Nacharbeiten"), Poweranalyse
#   with only 3 items and even lower (the note below it), Deskriptivstatistik
#   strong, Wahrscheinlichkeit with few items (one certainty dot), the others
#   in between; the script tries seeds until the result shows all of that
#   (show_off()), so it still works when the pool changes
# - a few skips, numeric answers (some matching a typical mistake, some
#   unmatched), items answered again later; about 40 % of every area stays
#   new for the live demo
# - the last session is the day before the script runs, so open mistakes are
#   retryable (RETRY_MIN_DAYS) and the login catch-up has nothing to do
#
# Writes into the DB folder TIGER_DB_DIR (default ../db): adds the account to
# db_credentials.sqlite, replaces its tables in db_user.sqlite and
# db_ability.sqlite (other users are untouched), reads db_item.sqlite. Run it
# on the folder whose db_item.sqlite goes live, then upload the three files.
# The password is random and written to SHOWCASE_ACCOUNT.txt in that folder
# (readable only by you), never to the console.
#
#   TIGER_DB_DIR=<folder> rv run dev/add_showcase_user.R [username]

suppressWarnings(pkgload::load_all(quiet = TRUE))

USER <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(USER)) USER <- "tiger_demo"
stopifnot(grepl(REG_USERNAME_PATTERN, USER))

dir <- normalizePath(Sys.getenv("TIGER_DB_DIR", "../db"), mustWork = TRUE)
Sys.setenv(TIGER_DB_DIR = dir)
for (f in c("db_item.sqlite", "db_credentials.sqlite")) {
  if (!file.exists(file.path(dir, f))) stop("No ", f, " in ", dir, call. = FALSE)
}
items <- db_get_items()

A <- LEARNING_AREA_LEVELS
# true ability at the start and at the end of the five weeks, per area
TRUE_START <- c(0.6, -0.4, -1.0, -0.3, -2.4, -0.3, -2.6)
TRUE_END   <- c(1.9,  0.3, -0.1,  0.9, -2.2,  0.6, -1.6)
names(TRUE_START) <- names(TRUE_END) <- A

# sessions: days before the last one (0 = yesterday), areas and item counts;
# "retry" = a retry queue from the dashboard
SESSIONS <- list(
  list(34, c(Deskriptivstatistik = 10)),
  list(33, c(Deskriptivstatistik = 6, Wahrscheinlichkeit = 3)),
  list(30, c(Wahrscheinlichkeit = 2, `Grundlagen der Inferenzstatistik` = 6)),
  list(27, c(`Grundlagen der Inferenzstatistik` = 5)),
  list(24, c(Deskriptivstatistik = 6, Gruppenvergleiche = 6)),
  list(21, "retry"),
  list(20, c(Gruppenvergleiche = 5, Zusammenhangsmaße = 4)),
  list(17, c(Regression = 6)),
  list(14, c(`Grundlagen der Inferenzstatistik` = 4, Regression = 5)),
  list(12, "retry"),
  list(9, c(Zusammenhangsmaße = 3, Poweranalyse = 3)),
  list(6, c(Regression = 4, Deskriptivstatistik = 5)),
  list(3, c(Gruppenvergleiche = 4, `Grundlagen der Inferenzstatistik` = 3)),
  list(0, c(Regression = 3, Deskriptivstatistik = 5))
)
# the last session also gets a short retry queue before it
RETRY_BEFORE_LAST <- TRUE

simulate <- function(seed) {
  set.seed(seed)
  last_day <- Sys.Date() - 1L
  span <- max(vapply(SESSIONS, `[[`, numeric(1), 1))
  rows <- list()
  snaps <- list()
  hist <- NULL # all rows so far
  since <- -Inf

  true_theta <- function(area, day_ago) {
    p <- 1 - day_ago / span
    TRUE_START[[area]] + p * (TRUE_END[[area]] - TRUE_START[[area]])
  }

  answer <- function(item, theta, t, token, mode, boost = 0) {
    item <- as.list(item)
    p <- plogis(theta + boost - item$irt_diff)
    # a few skips, only in the large areas (the small ones keep their items)
    big <- c("Deskriptivstatistik", "Grundlagen der Inferenzstatistik", "Gruppenvergleiche")
    if (mode == "standard" && item$learning_area %in% big && runif(1) < 0.05) {
      r <- build_response_row(item, NA_integer_, USER, token, skipped = TRUE)
    } else if (identical(item$answer_mode, "num")) {
      opts <- get_numeric_options(item)
      if (runif(1) < p) {
        o <- opts[opts$is_correct, ][1, ]
        r <- build_response_row(item, o$idx, USER, token, typed_value = o$value, practice_mode = mode)
      } else if (any(!opts$is_correct) && runif(1) < 0.6) {
        o <- opts[!opts$is_correct, ][sample(sum(!opts$is_correct), 1), ]
        r <- build_response_row(item, o$idx, USER, token, typed_value = o$value, practice_mode = mode)
      } else {
        v <- round(opts$value[1] * runif(1, 1.2, 1.6) + 1, 2)
        r <- build_response_row(item, NA_integer_, USER, token, typed_value = v, practice_mode = mode)
      }
    } else {
      correct <- parse_answer_correct(item$answer_correct)
      n_opt <- length(get_answeroptions(item))
      idx <- if (runif(1) < p) correct[1] else sample(setdiff(seq_len(n_opt), correct), 1)
      r <- build_response_row(item, idx, USER, token, practice_mode = mode)
    }
    r$id_datetime <- as.integer(t)
    r$id_date <- as.integer(local_date(t))
    r
  }

  retry_queue <- function(day_ago, t0, token, n = 6L) {
    la <- latest_attempts(hist[!hist$skipped, , drop = FALSE])
    open <- la[!la$bool_correct & local_date(la$id_datetime) <= last_day - day_ago - RETRY_MIN_DAYS, ]
    ids <- safe_sample(open$id_item, min(n, nrow(open)))
    out <- list()
    for (k in seq_along(ids)) {
      it <- items[items$id_item == ids[k], ]
      out[[k]] <- answer(it, true_theta(it$learning_area, day_ago), t0 + 70 * k, token, "retry", boost = 0.6)
    }
    do.call(rbind, out)
  }

  update <- function(t) {
    ud <- hist[!is.na(hist$bool_correct), , drop = FALSE]
    practised <- intersect(A, ud$learning_area[ud$id_datetime > since])
    if (!length(practised)) return(NULL)
    la <- latest_attempts(ud)
    la$learning_area <- factor(la$learning_area, levels = A)
    ab <- build_ability_rows(estimate_competency(la, items, areas = practised), USER, paste0("showcase-", t))
    ab$computed_at <- as.integer(t)
    since <<- t
    ab
  }

  for (s in SESSIONS) {
    day_ago <- s[[1]]
    day <- last_day - day_ago
    t <- as.numeric(as.POSIXct(paste(day, sprintf("%02d:%02d:00", sample(16:19, 1), sample(0:59, 1)))))
    token <- paste0("showcase-", day)
    new <- NULL
    if (identical(s[[2]], "retry") || (day_ago == 0 && RETRY_BEFORE_LAST)) {
      new <- retry_queue(day_ago, t, token, if (identical(s[[2]], "retry")) 8L else 4L)
      if (!is.null(new)) t <- max(new$id_datetime) + 120
    }
    if (!identical(s[[2]], "retry")) {
      for (area in names(s[[2]])) {
        seen <- if (is.null(hist)) integer(0) else hist$id_item
        pool <- items[items$learning_area == area & !is.na(items$irt_diff), ]
        fresh <- pool$id_item[!pool$id_item %in% seen]
        # mostly new items, in the large areas now and then one answered
        # before (selector without "Nur neue Aufgaben")
        n <- s[[2]][[area]]
        n_old <- if (length(seen) && nrow(pool) >= 20L && runif(1) < 0.5) 1L else 0L
        old <- intersect(pool$id_item, seen)
        ids <- c(safe_sample(fresh, n - n_old), safe_sample(old, min(n_old, length(old))))
        for (id in ids) {
          t <- t + sample(40:150, 1)
          new <- rbind(new, answer(items[items$id_item == id, ], true_theta(area, day_ago), t, token, "standard"))
        }
      }
    }
    hist <- rbind(hist, new)
    snaps[[length(snaps) + 1]] <- update(t + 60)
  }
  list(rows = hist, ability = do.call(rbind, snaps))
}

# the final state as the dashboard would show it
final_state <- function(sim) {
  ab <- sim$ability
  ab <- ab[order(ab$computed_at), ]
  ab[!duplicated(ab$learning_area, fromLast = TRUE), ]
}

# Does the final state show off every part of the dashboard?
show_off <- function(sim) {
  fin <- final_state(sim)
  f <- function(area) fin[fin$learning_area == area, ]
  lvl <- function(r) competency_label(r$theta, r$n_items)
  dots <- function(r) certainty_dot_count(r$theta, r$se, r$n_items, lvl(r))
  reg <- f("Regression"); pow <- f("Poweranalyse")
  des <- f("Deskriptivstatistik"); wah <- f("Wahrscheinlichkeit")
  ranked <- rank_weak_areas(fin)
  traj <- sim$ability[sim$ability$learning_area == "Regression", ]
  all_dots <- vapply(seq_len(nrow(fin)), function(i) dots(fin[i, ]), integer(1))
  others <- fin[fin$learning_area != "Poweranalyse", ]
  la <- latest_attempts(sim$rows[!sim$rows$skipped, ])
  last <- sim$rows[!sim$rows$skipped & sim$rows$id_date == max(sim$rows$id_date), ]
  nrow(fin) == 7L && mean(last$bool_correct) >= 0.5 &&
    all(abs(others$theta) < 2.8) && wah$n_items >= 5L && sum(!la$bool_correct) <= 30L &&
    identical(ranked[1], "Regression") && lvl(reg)$level == 1L && dots(reg) == 3L &&
    nrow(pow) == 1L && pow$n_items == 3L && pow$theta < reg$theta &&
    lvl(des)$level >= 5L && dots(wah) == 1L && 2L %in% all_dots &&
    traj$theta[nrow(traj)] > traj$theta[1] + 0.3
}

seed <- 0L
repeat {
  seed <- seed + 1L
  if (seed > 500L) stop("No seed shows everything; adjust TRUE_START/TRUE_END or SESSIONS", call. = FALSE)
  sim <- simulate(seed)
  if (show_off(sim)) break
}
fin <- final_state(sim)

# ── write ───────────────────────────────────────────────────────────────────
pw <- paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
con <- DBI::dbConnect(RSQLite::SQLite(), DB_CREDS())
invisible(DBI::dbExecute(con, "DELETE FROM credentials_db WHERE user_name = ?", params = list(USER)))
DBI::dbDisconnect(con)
invisible(db_register_user(USER, pw))
for (p in c(DB_USERS(), DB_ABILITY())) {
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  if (DBI::dbExistsTable(con, USER)) DBI::dbRemoveTable(con, USER)
  DBI::dbDisconnect(con)
}
invisible(db_write_response(USER, sim$rows))
invisible(db_write_ability(USER, sim$ability))

acc <- file.path(dir, "SHOWCASE_ACCOUNT.txt")
writeLines(c(
  "Showcase account (store it in your password manager, then delete this file):",
  paste("  username:", USER), paste("  password:", pw)
), acc)
Sys.chmod(acc, "600")

# ── summary ─────────────────────────────────────────────────────────────────
ud <- sim$rows
cat("\nShowcase account", USER, "in", dir, "(seed", seed, ")\n")
cat(sprintf("  %d answers on %d items (pool: %d) on %d days, %d updates of the estimate\n",
            nrow(ud), length(unique(ud$id_item)), nrow(items),
            length(unique(ud$id_date)), length(unique(sim$ability$computed_at))))
cat(sprintf("  %d skipped, %d retry answers, %d numeric answers\n",
            sum(ud$skipped), sum(ud$practice_mode == "retry"), sum(!is.na(ud$typed_value))))
la <- latest_attempts(ud[!ud$skipped, ])
cat(sprintf("  open mistakes: %d, fixed mistakes: %d\n",
            sum(!la$bool_correct), length(fixed_ids <- intersect(
              ud$id_item[!ud$skipped & !ud$bool_correct], la$id_item[la$bool_correct]))))
cat("\n  Final estimate per area:\n")
for (i in seq_len(nrow(fin))) {
  cl <- competency_label(fin$theta[i], fin$n_items[i])
  cat(sprintf("    %-34s theta %5.2f  se %4.2f  items %2d  %-15s dots %d\n",
              fin$learning_area[i], fin$theta[i], fin$se[i], fin$n_items[i], cl$label,
              certainty_dot_count(fin$theta[i], fin$se[i], fin$n_items[i], cl)))
}
cat("\nPassword written to", acc, "\n")
