# Report on the AI feedback (db_feedback.sqlite): how it's used, how it's
# rated, per model and prompt version, the API's errors and speed, and which
# sources get clicked or reported. Read-only.
#
#   rv run dev/ai_feedback_report.R [folder] [--days N]
#
# folder   the DB folder (default TIGER_DB_DIR, else ../db), e.g. a backup
#          downloaded from the server
# --days   period for the API calls and the latest comments (default 30)
#
# Usernames are never printed.

args <- commandArgs(trailingOnly = TRUE)
days <- 30L
if ("--days" %in% args) {
  i <- match("--days", args)
  days <- as.integer(args[i + 1L])
  args <- args[-c(i, i + 1L)]
}
dir <- if (length(args)) args[1] else Sys.getenv("TIGER_DB_DIR", "../db")
if (!nzchar(dir)) dir <- "../db"
dir <- normalizePath(dir, mustWork = TRUE)
suppressMessages(suppressWarnings(pkgload::load_all(quiet = TRUE)))
options(width = 200) # tables on one line

path <- file.path(dir, "db_feedback.sqlite")
if (!file.exists(path)) {
  cat("No db_feedback.sqlite in", dir, "(no feedback created yet)\n")
  quit(status = 0L)
}
con <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
on.exit(DBI::dbDisconnect(con), add = TRUE)
q <- function(sql, ...) DBI::dbGetQuery(con, sql, params = if (...length()) list(...) else NULL)
has <- function(table) DBI::dbExistsTable(con, table)
since <- as.integer(Sys.time()) - days * 86400L

heading <- function(x) cat("\n", x, "\n", strrep("─", nchar(x)), "\n", sep = "")
pct <- function(x, n) ifelse(n > 0, sprintf("%.0f %%", 100 * x / n), "–")
show <- function(df) {
  if (nrow(df) == 0L) cat("  (none)\n") else print(df, row.names = FALSE, right = FALSE)
}
day <- function(t) format(as.POSIXct(t, origin = "1970-01-01"), "%Y-%m-%d")
short <- function(x, n = 300L) ifelse(nchar(x) > n, paste0(substr(x, 1L, n), " …"), x)

cfg <- ai_config(dir)
label <- function(model) vapply(model, function(m) ai_model_label(m, cfg), character(1))

cat("AI feedback report:", path, "\n")
cat("Generated", format(Sys.time(), "%Y-%m-%d %H:%M"), "| period for calls and comments: last", days, "days\n")

# ── Overview ──────────────────────────────────────────────────────────────────
heading("Overview")
ov <- q("SELECT COUNT(*) AS n, COUNT(DISTINCT id_user) AS users, MIN(created_at) AS first,
               MAX(created_at) AS last FROM feedback")
rt <- q("SELECT COUNT(*) AS n, SUM(helpful) AS up FROM rating")
cat(sprintf("  %d feedbacks from %d students", ov$n, ov$users))
if (ov$n > 0) cat(sprintf(" (%s to %s)", day(ov$first), day(ov$last)))
cat(sprintf("\n  rated: %d (%s of the feedbacks), of these helpful: %s\n",
  rt$n, pct(rt$n, ov$n), pct(rt$up, rt$n)))
if (has("link_click")) {
  lc <- q("SELECT COUNT(*) AS n, COUNT(DISTINCT id_feedback) AS fb FROM link_click")
  cat(sprintf("  link clicks: %d, in %s of the feedbacks\n", lc$n, pct(lc$fb, ov$n)))
}

# ── Per model ─────────────────────────────────────────────────────────────────
heading("Per model (all feedbacks)")
m <- q("
  SELECT f.model, COUNT(*) AS feedbacks,
         SUM(f.used_fallback) AS fallback, SUM(1 - f.format_ok) AS format_broken,
         COUNT(r.helpful) AS rated, SUM(r.helpful) AS helpful,
         AVG(f.duration_seconds) AS secs
  FROM feedback f LEFT JOIN rating r ON r.id_feedback = f.id_feedback
  GROUP BY f.model ORDER BY feedbacks DESC")
tok <- q("SELECT model, AVG(input_tokens) AS tin, AVG(output_tokens) AS tout, AVG(reasoning_tokens) AS treas
          FROM api_call WHERE outcome IN ('ok', 'format') GROUP BY model")
if (nrow(m) > 0) {
  m <- merge(m, tok, by = "model", all.x = TRUE, sort = FALSE)
  show(data.frame(
    model = label(m$model), feedbacks = m$feedbacks,
    `as fallback` = m$fallback, `format broken` = m$format_broken,
    rated = m$rated, helpful = pct(m$helpful, m$rated),
    `avg s` = sprintf("%.1f", m$secs),
    `tokens in/out` = sprintf("%.0f / %.0f", m$tin, m$tout),
    check.names = FALSE
  ))
} else {
  show(m)
}

# ── Per prompt version ────────────────────────────────────────────────────────
heading("Per prompt version")
pv <- q("
  SELECT f.prompt_version AS version, MIN(f.created_at) AS first, COUNT(*) AS feedbacks,
         COUNT(r.helpful) AS rated, SUM(r.helpful) AS helpful
  FROM feedback f LEFT JOIN rating r ON r.id_feedback = f.id_feedback
  GROUP BY f.prompt_version ORDER BY first")
if (nrow(pv) > 0) {
  stored <- if (has("prompt")) q("SELECT version FROM prompt")$version else character(0)
  show(data.frame(
    version = pv$version, since = day(pv$first), feedbacks = pv$feedbacks,
    rated = pv$rated, helpful = pct(pv$helpful, pv$rated),
    `text stored` = ifelse(pv$version %in% stored, "yes", "no"),
    check.names = FALSE
  ))
  cat("  (a version's text: SELECT text FROM prompt WHERE version = '...')\n")
} else {
  show(pv)
}

# ── API calls ─────────────────────────────────────────────────────────────────
heading(sprintf("API calls, last %d days", days))
calls <- q("SELECT model, outcome, COUNT(*) AS n, AVG(duration_seconds) AS secs, MAX(duration_seconds) AS max_secs
            FROM api_call WHERE created_at >= ? GROUP BY model, outcome", since)
if (nrow(calls) > 0) {
  wide <- stats::xtabs(n ~ model + outcome, data = calls)
  tab <- as.data.frame.matrix(wide)
  tab <- data.frame(model = label(rownames(tab)), tab, check.names = FALSE)
  good <- rowSums(wide[, intersect(colnames(wide), c("ok", "format")), drop = FALSE])
  tab$`success` <- pct(good, rowSums(wide))
  ok_rows <- calls[calls$outcome %in% c("ok", "format"), ]
  tab$`max s (ok)` <- vapply(rownames(wide), function(mm) {
    x <- ok_rows$max_secs[ok_rows$model == mm]
    if (length(x)) sprintf("%.1f", max(x)) else "–"
  }, character(1))
  show(tab)
  errs <- q("SELECT created_at, model, outcome, message FROM api_call
             WHERE created_at >= ? AND outcome NOT IN ('ok', 'format')
             ORDER BY created_at DESC LIMIT 5", since)
  if (nrow(errs) > 0) {
    cat("\n  latest errors:\n")
    for (i in seq_len(nrow(errs))) {
      msg <- gsub("\\s+", " ", substr(errs$message[i], 1, 110))
      cat(sprintf("  %s  %-24s %-11s %s\n", day(errs$created_at[i]), errs$model[i], errs$outcome[i], msg))
    }
  }
} else {
  show(calls)
}

# ── Reasons and comments ──────────────────────────────────────────────────────
heading("Reasons ticked with a rating")
reasons <- q("SELECT reason_code, COUNT(*) AS n FROM rating_reason GROUP BY reason_code ORDER BY n DESC")
all_reasons <- c(AI_REASONS_NEGATIVE, AI_REASONS_POSITIVE)
if (nrow(reasons) > 0) {
  show(data.frame(
    thumb = ifelse(reasons$reason_code %in% names(AI_REASONS_POSITIVE), "up", "down"),
    reason = ifelse(reasons$reason_code %in% names(all_reasons), all_reasons[reasons$reason_code], reasons$reason_code),
    code = reasons$reason_code, n = reasons$n
  ))
} else {
  show(reasons)
}

heading(sprintf("Comments, last %d days", days))
com <- q("SELECT r.updated_at AS t, r.helpful, f.model, r.comment
          FROM rating r JOIN feedback f ON f.id_feedback = r.id_feedback
          WHERE r.comment IS NOT NULL AND r.updated_at >= ? ORDER BY r.updated_at DESC", since)
if (nrow(com) == 0L) cat("  (none)\n")
for (i in seq_len(nrow(com))) {
  cat(sprintf("  %s  %s  %s\n    %s\n", day(com$t[i]), if (com$helpful[i] == 1L) "up  " else "down",
    label(com$model[i]), paste(strwrap(short(com$comment[i]), 90), collapse = "\n    ")))
}

# ── Sources ───────────────────────────────────────────────────────────────────
heading("Sources: recommended, clicked, reported")
src <- q("
  SELECT rec.resource_id AS id, COUNT(*) AS recommended,
         SUM(CASE WHEN lc.n > 0 THEN 1 ELSE 0 END) AS clicked,
         SUM(COALESCE(rp.broken, 0)) AS link_broken, SUM(COALESCE(rp.unfit, 0)) AS unfit,
         SUM(COALESCE(rp.other, 0)) AS other
  FROM recommendation rec
  LEFT JOIN (SELECT id_feedback, position, COUNT(*) AS n FROM link_click GROUP BY id_feedback, position) lc
    ON lc.id_feedback = rec.id_feedback AND lc.position = rec.position
  LEFT JOIN (SELECT id_feedback, position,
                    SUM(reason_code = 'link_broken') AS broken,
                    SUM(reason_code = 'materials_unfit') AS unfit,
                    SUM(reason_code = 'other') AS other
             FROM recommendation_report GROUP BY id_feedback, position) rp
    ON rp.id_feedback = rec.id_feedback AND rp.position = rec.position
  GROUP BY rec.resource_id")
lookup <- read_ai_lookup(file.path(dir, AI_LOOKUP_FILE))
title <- function(id) {
  if (is.null(lookup)) return(rep("", length(id)))
  t <- lookup$display_name[match(id, lookup$id)]
  ifelse(is.na(t), "(no longer in the source list)", substr(t, 1, 60))
}
if (nrow(src) > 0) {
  src$title <- title(src$id)
  src$`click rate` <- pct(src$clicked, src$recommended)
  reported <- src[src$link_broken + src$unfit + src$other > 0, ]
  cat("  To check (reported at least once):\n")
  show(reported[order(-reported$link_broken, -reported$unfit), c("id", "title", "recommended", "link_broken", "unfit", "other")])
  cat("\n  Most recommended:\n")
  show(utils::head(src[order(-src$recommended), c("id", "title", "recommended", "clicked", "click rate")], 10))
  rep_com <- q("SELECT rec.resource_id AS id, rp.reason_code, rp.comment FROM recommendation_report rp
                JOIN recommendation rec ON rec.id_feedback = rp.id_feedback AND rec.position = rp.position
                WHERE rp.comment IS NOT NULL ORDER BY rp.created_at DESC LIMIT 10")
  if (nrow(rep_com) > 0) {
    cat("\n  Latest report comments:\n")
    for (i in seq_len(nrow(rep_com))) {
      cat(sprintf("  %s (%s): %s\n", rep_com$id[i], rep_com$reason_code[i], gsub("\\s+", " ", short(rep_com$comment[i]))))
    }
  }
} else {
  show(src)
}
cat("\n")
