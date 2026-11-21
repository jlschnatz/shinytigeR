# AI feedback ("Persönliches Feedback" on the dashboard): a short personal
# summary plus up to three recommended sources, written by a language model
# from the student's saved ability snapshot. Ported from the kiwi prototype
# (kiwi/ai_feedback). In the order of the workflow:
#
#   1. Config, key, source list (all in the DB folder, read on every request)
#   2. Learning status, focus topics, preselected sources
#   3. Prompt and response schema
#   4. db_feedback.sqlite: schema, call log, saving, loading
#   5. May the student ask for feedback now? (ai_feedback_status())
#   6. Calling the model (asynchronously, so the session isn't blocked)
#   7. Validating the answer
#   8. Everything together: ai_feedback_prepare() + ai_feedback_run()
#
# Only levels go to the model, as words: no theta, no SE, no user id. The
# model picks sources by id from a preselected list; titles and links always
# come from the source list.

# ── 1. Config, key, source list ───────────────────────────────────────────────

# AI_CONFIG_FILE from the DB folder, merged onto AI_CONFIG_DEFAULTS. Read on
# every request, so a changed file applies at once. Missing or unreadable
# file, or no models = feature off (never a flood of calls by mistake).
ai_config <- function(dir = .db_dir()) {
  cfg <- AI_CONFIG_DEFAULTS
  path <- file.path(dir, AI_CONFIG_FILE)
  if (!file.exists(path)) {
    return(cfg)
  }
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(raw)) {
    warning(AI_CONFIG_FILE, " can't be read; AI feedback is off.", call. = FALSE)
    return(cfg)
  }
  known <- intersect(names(raw), names(cfg))
  cfg[known] <- raw[known]
  cfg$models <- as.character(unlist(cfg$models))
  cfg$enabled <- isTRUE(cfg$enabled) && length(cfg$models) > 0L
  cfg
}

ai_api_key <- function() read_secret(AI_KEY_ENV, AI_KEY_FILE)

# The name students see for a model (config's model_labels), else its id
ai_model_label <- function(model, cfg) {
  label <- cfg$model_labels[[model]]
  if (is.null(label) || !nzchar(label)) model else as.character(label)
}

# TRUE/FALSE from a source-list cell: TRUE, WAHR, 1 or x count as TRUE
# (Excel exports the logical columns differently depending on its language)
as_flag <- function(x, n) {
  if (is.null(x)) {
    return(rep(FALSE, n))
  }
  toupper(trimws(as.character(x))) %in% c("TRUE", "WAHR", "1", "X")
}

# The source list (AI_LOOKUP_FILE in the DB folder; exported from the Excel
# file with dev/export_ai_lookup.R). NULL if it's missing.
read_ai_lookup <- function(path = file.path(.db_dir(), AI_LOOKUP_FILE)) {
  if (!file.exists(path)) {
    return(NULL)
  }
  df <- utils::read.csv(
    path,
    stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8",
    check.names = FALSE, colClasses = "character"
  )
  for (col in AI_LOOKUP_COLUMNS) {
    df[[col]] <- as_flag(df[[col]], nrow(df))
  }
  df$competence_min <- suppressWarnings(as.numeric(df$competence_min))
  df$competence_max <- suppressWarnings(as.numeric(df$competence_max))
  df
}

AI_LOOKUP_REQUIRED <- c(
  "id", "display_name", "link", "resource_type", "competence_min",
  "competence_max", "learning_function", "difficulty", "effort"
)

# Problems with a source list, as sentences (empty = fine). For
# dev/check_deploy_dbs.R and the tests.
ai_lookup_problems <- function(df) {
  if (is.null(df)) {
    return(paste(AI_LOOKUP_FILE, "is missing"))
  }
  missing <- setdiff(c(AI_LOOKUP_REQUIRED, AI_LOOKUP_COLUMNS), names(df))
  if (length(missing) > 0L) {
    return(paste("missing columns:", paste(missing, collapse = ", ")))
  }
  out <- character(0)
  if (anyNA(df$id) || any(!nzchar(df$id))) out <- c(out, "empty id")
  dup <- unique(df$id[duplicated(df$id)])
  if (length(dup) > 0L) out <- c(out, paste("duplicate ids:", paste(dup, collapse = ", ")))
  bad_link <- df$id[is.na(df$link) | !grepl("^https?://", df$link)]
  if (length(bad_link) > 0L) out <- c(out, paste("missing or invalid link:", paste(bad_link, collapse = ", ")))
  n_lvl <- nrow(COMPETENCY_SCALE)
  bad_lvl <- df$id[is.na(df$competence_min) | is.na(df$competence_max) |
    df$competence_min < 1 | df$competence_max > n_lvl | df$competence_min > df$competence_max]
  if (length(bad_lvl) > 0L) {
    out <- c(out, sprintf("level range not within 1-%d: %s", n_lvl, paste(bad_lvl, collapse = ", ")))
  }
  no_area <- df$id[rowSums(as.matrix(df[, AI_LOOKUP_COLUMNS])) == 0]
  if (length(no_area) > 0L) out <- c(out, paste("no learning area:", paste(no_area, collapse = ", ")))
  for (area in names(AI_LOOKUP_COLUMNS)) {
    if (!any(df[[AI_LOOKUP_COLUMNS[[area]]]])) out <- c(out, paste("no source for", area))
  }
  out
}

# ── 2. Learning status, focus topics, sources ─────────────────────────────────

# The saved snapshot (latest_ability(), as the dashboard's competency()) as
# levels: one row per area with an estimate, in LEARNING_AREA_LEVELS order.
# level/competence and dots are exactly what the "Dein Lernstand" table
# shows (competency_label(), certainty_dot_count()).
ai_learning_status <- function(comp) {
  empty <- data.frame(
    learning_area = character(0), level = integer(0), competence = character(0),
    dots = integer(0), evidence = character(0), stringsAsFactors = FALSE
  )
  if (is.null(comp) || nrow(comp) == 0L) {
    return(empty)
  }
  ok <- !is.na(comp$theta) & !is.na(comp$n_items) & comp$n_items > 0
  comp <- comp[ok, , drop = FALSE]
  if (nrow(comp) == 0L) {
    return(empty)
  }
  comp <- comp[order(match(as.character(comp$learning_area), LEARNING_AREA_LEVELS)), , drop = FALSE]
  se <- if ("se" %in% names(comp)) comp$se else rep(NA_real_, nrow(comp))
  level <- integer(nrow(comp))
  dots <- integer(nrow(comp))
  for (i in seq_len(nrow(comp))) {
    cl <- competency_label(comp$theta[i], comp$n_items[i])
    level[i] <- cl$level
    dots[i] <- certainty_dot_count(comp$theta[i], se[i], comp$n_items[i], cl)
  }
  data.frame(
    learning_area = as.character(comp$learning_area),
    level = level,
    competence = COMPETENCY_SCALE$label[level],
    dots = dots,
    evidence = AI_EVIDENCE_WORDS[pmax(dots, 1L)],
    stringsAsFactors = FALSE
  )
}

# The focus topics: the first AI_FOCUS_TOPICS areas of rank_weak_areas(), the
# ranking "Zum Nacharbeiten" uses (so both cards talk about the same areas;
# areas with fewer than WEAK_AREA_MIN_ITEMS items can't be focus topics, but
# stay in the learning status). Which one to start with is the model's call.
ai_focus_topics <- function(status, comp) {
  areas <- head(rank_weak_areas(comp), AI_FOCUS_TOPICS)
  areas <- areas[areas %in% status$learning_area]
  status[match(areas, status$learning_area), , drop = FALSE]
}

# For each focus topic, the sources fitting the area and the student's level
# there, shuffled (so the next feedback can suggest other ones), one per
# learning function first (so none is lost when cutting to `n_max`).
# NULL if there's none at all.
ai_select_resources <- function(focus, lookup, n_max = AI_MAX_RESOURCES_PER_TOPIC) {
  if (is.null(lookup) || nrow(focus) == 0L) {
    return(NULL)
  }
  cols <- c("id", "display_name", "resource_type", "learning_function", "difficulty", "effort")
  rows <- lapply(seq_len(nrow(focus)), function(i) {
    flag <- lookup[[AI_LOOKUP_COLUMNS[[focus$learning_area[i]]]]]
    lvl <- focus$level[i]
    fits <- which(flag & lookup$competence_min <= lvl & lookup$competence_max >= lvl)
    if (length(fits) == 0L) {
      return(NULL)
    }
    cand <- lookup[fits[sample.int(length(fits))], cols, drop = FALSE]
    first <- !duplicated(cand$learning_function)
    cand <- head(rbind(cand[first, , drop = FALSE], cand[!first, , drop = FALSE]), n_max)
    cand$topic <- focus$learning_area[i]
    cand
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) NULL else `rownames<-`(out, NULL)
}

# ── 3. Prompt and schema ──────────────────────────────────────────────────────

ai_system_prompt_path <- function(dir = .db_dir()) file.path(dir, AI_SYSTEM_PROMPT_FILE)

# The system prompt (from the DB folder, read on every request) with the
# evidence words filled in, and its version (ai_prompt_version()), stored
# with every feedback. NULL if the file is missing (the feature is off then).
ai_system_prompt <- function(path = ai_system_prompt_path()) {
  if (!file.exists(path)) {
    return(NULL)
  }
  txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (i in seq_along(AI_EVIDENCE_WORDS)) {
    txt <- gsub(sprintf("[[EVIDENCE_%d]]", i), AI_EVIDENCE_WORDS[i], txt, fixed = TRUE)
  }
  list(text = txt, version = ai_prompt_version(txt))
}

# What's missing from a prompt file the code relies on (AI_PROMPT_REQUIRED),
# as sentences; empty = fine. For dev/check_deploy_dbs.R: an edited prompt
# that drifts from the code wouldn't fail, the feedback would just get worse.
ai_prompt_problems <- function(path = ai_system_prompt_path()) {
  if (!file.exists(path)) {
    return(paste(AI_SYSTEM_PROMPT_FILE, "is missing"))
  }
  txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  missing <- AI_PROMPT_REQUIRED[!vapply(AI_PROMPT_REQUIRED, grepl, logical(1), x = txt, fixed = TRUE)]
  if (length(missing) == 0L) character(0) else paste("doesn't mention:", paste(missing, collapse = ", "))
}

# MD5 prefix of the text as sent (evidence words filled in): two different
# texts never share a version. Stored with every feedback; the text itself
# goes to the `prompt` table once (ai_save_feedback()).
ai_prompt_version <- function(text) {
  substr(unname(tools::md5sum(bytes = charToRaw(enc2utf8(text)))), 1L, 8L)
}

# The request text (German, like the system prompt): LERNSTAND (every area
# with an estimate, so the model can name strengths), FOKUSTHEMEN,
# RESSOURCEN. Levels and words only, no numbers.
ai_user_prompt <- function(status, focus, resources) {
  lines_status <- paste0(status$learning_area, ": ", status$competence, " | Evidenz: ", status$evidence)
  focus_areas <- status$learning_area[status$learning_area %in% focus$learning_area]
  lines_res <- vapply(unique(resources$id), function(id) {
    hits <- resources[resources$id == id, , drop = FALSE]
    paste(
      id, hits$display_name[1], gsub("_", " ", hits$resource_type[1]),
      paste(hits$topic, collapse = ", "), hits$learning_function[1],
      hits$difficulty[1], hits$effort[1],
      sep = " | "
    )
  }, character(1))
  paste(c(
    "LERNSTAND", lines_status, "",
    "FOKUSTHEMEN", paste(focus_areas, collapse = ", "), "",
    "RESSOURCEN", unname(lines_res)
  ), collapse = "\n")
}

# Response schema, built per request: topic and resource_id can only be the
# focus topics and the preselected ids (the model can't name other sources).
# The descriptions are sent to the model, hence German.
ai_schema <- function(focus, resources) {
  ellmer::type_object(
    summary = ellmer::type_string("Zusammenfassung des Lernstands."),
    recommendations = ellmer::type_array(
      ellmer::type_object(
        topic = ellmer::type_enum(focus$learning_area, "Ein Themengebiet aus FOKUSTHEMEN."),
        resource_id = ellmer::type_enum(unique(resources$id), "ID einer Zeile aus RESSOURCEN."),
        text = ellmer::type_string("Begründung und konkrete Handlung, ein bis zwei Sätze.")
      ),
      "Eine Empfehlung pro Themengebiet aus FOKUSTHEMEN."
    )
  )
}

# ── 4. db_feedback.sqlite ─────────────────────────────────────────────────────

# Shared tables (one row per feedback/call/rating, with an id_user column),
# not one table per user like db_user.sqlite. Created on first use.
AI_DB_SCHEMA <- c(
  # one row per generated feedback
  "CREATE TABLE IF NOT EXISTS feedback (
    id_feedback INTEGER PRIMARY KEY AUTOINCREMENT,
    id_user TEXT, id_session TEXT,
    created_at INTEGER,          -- Unix time
    ability_computed_at INTEGER, -- the ability snapshot it is based on
    model TEXT,                  -- the model that answered
    used_fallback INTEGER,       -- 1 = not the first model in the config
    format_ok INTEGER,           -- 0 = broke the length/word rules (best there was)
    prompt_version TEXT,         -- MD5 prefix of the system prompt as sent (see table prompt)
    summary TEXT,
    user_prompt TEXT,            -- what was sent (levels, focus topics, sources)
    duration_seconds REAL        -- click to answer, all models tried
  )",
  # its up to three recommendations
  "CREATE TABLE IF NOT EXISTS recommendation (
    id_feedback INTEGER, position INTEGER, topic TEXT, resource_id TEXT, text TEXT
  )",
  # every call to the API, successful or not
  "CREATE TABLE IF NOT EXISTS api_call (
    id_call INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at INTEGER, id_user TEXT,
    id_feedback INTEGER,         -- set for the call whose answer was used
    model TEXT,
    outcome TEXT,                -- ok, format, invalid, rate_limit, timeout, connection, unavailable
    message TEXT,                -- error message
    duration_seconds REAL,
    input_tokens INTEGER, output_tokens INTEGER, cached_tokens INTEGER,
    reasoning_tokens INTEGER,    -- if the API reports them
    finish_reason TEXT,          -- e.g. stop, length (= hit max_tokens)
    response_model TEXT,         -- the model the API says answered
    response_id TEXT
  )",
  # a student's rating of a feedback: one per feedback and student, updated
  "CREATE TABLE IF NOT EXISTS rating (
    id_feedback INTEGER, id_user TEXT,
    helpful INTEGER,             -- 1 = thumbs up, 0 = thumbs down
    comment TEXT,
    created_at INTEGER, updated_at INTEGER,
    PRIMARY KEY (id_feedback, id_user)
  )",
  # the reasons ticked with a rating (codes, not labels)
  "CREATE TABLE IF NOT EXISTS rating_reason (
    id_feedback INTEGER, id_user TEXT, reason_code TEXT
  )",
  # a problem reported for one recommendation
  "CREATE TABLE IF NOT EXISTS recommendation_report (
    id_feedback INTEGER, position INTEGER, id_user TEXT,
    created_at INTEGER, reason_code TEXT, comment TEXT
  )",
  # every system prompt version once, with its full text (the MD5 prefix in
  # feedback.prompt_version alone can't tell what a version said)
  "CREATE TABLE IF NOT EXISTS prompt (
    version TEXT PRIMARY KEY,
    text TEXT,
    first_used_at INTEGER
  )",
  # a click on a recommended source
  "CREATE TABLE IF NOT EXISTS link_click (
    id_feedback INTEGER, position INTEGER, id_user TEXT, created_at INTEGER
  )"
)

ai_db <- function(fn, path = DB_FEEDBACK()) {
  db_with(path, function(con) {
    for (sql in AI_DB_SCHEMA) DBI::dbExecute(con, sql)
    fn(con)
  }, wal = TRUE)
}

# Unix time of today's local midnight
local_midnight <- function(now = Sys.time()) {
  as.integer(as.POSIXct(format(now, "%Y-%m-%d")))
}

ai_log_call <- function(user_id, model, outcome, duration, meta = list(), message = NA_character_) {
  m <- function(name, na) {
    v <- meta[[name]]
    if (is.null(v) || length(v) == 0L) na else v[[1]]
  }
  ai_db(function(con) {
    DBI::dbExecute(con, "
      INSERT INTO api_call (created_at, id_user, model, outcome, message, duration_seconds,
        input_tokens, output_tokens, cached_tokens, reasoning_tokens,
        finish_reason, response_model, response_id)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        as.integer(Sys.time()), user_id, model, outcome, substr(message, 1L, 1000L),
        round(duration, 2),
        as.integer(m("input_tokens", NA)), as.integer(m("output_tokens", NA)),
        as.integer(m("cached_tokens", NA)), as.integer(m("reasoning_tokens", NA)),
        as.character(m("finish_reason", NA)), as.character(m("response_model", NA)),
        as.character(m("response_id", NA))
      )
    )
    DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
  })
}

# Models that failed with a connection-type error (AI_BLOCKING_ERRORS) in the
# last AI_BLOCK_MINUTES, for any user
ai_blocked_models <- function(now = Sys.time()) {
  since <- as.integer(now) - AI_BLOCK_MINUTES * 60
  ai_db(function(con) {
    sql <- sprintf(
      "SELECT DISTINCT model FROM api_call WHERE created_at > ? AND outcome IN (%s)",
      paste(rep("?", length(AI_BLOCKING_ERRORS)), collapse = ", ")
    )
    DBI::dbGetQuery(con, sql, params = c(list(since), as.list(AI_BLOCKING_ERRORS)))$model
  })
}

# Calls to the API today, all users together (for the config's daily_limit)
ai_calls_today <- function(now = Sys.time()) {
  ai_db(function(con) {
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM api_call WHERE created_at >= ?",
      params = list(local_midnight(now))
    )$n
  })
}

ai_save_feedback <- function(req, session_token, chosen, duration) {
  ai_db(function(con) {
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, "
        INSERT INTO feedback (id_user, id_session, created_at, ability_computed_at, model,
          used_fallback, format_ok, prompt_version, summary, user_prompt, duration_seconds)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          req$user_id, session_token, as.integer(Sys.time()),
          as.integer(req$ability_computed_at), chosen$model,
          as.integer(!identical(chosen$model, req$cfg$models[1])),
          as.integer(chosen$format_ok), req$prompt_version, chosen$summary,
          req$user_prompt, round(duration, 2)
        )
      )
      id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
      DBI::dbExecute(con, "INSERT OR IGNORE INTO prompt (version, text, first_used_at) VALUES (?, ?, ?)",
        params = list(req$prompt_version, req$system_prompt, as.integer(Sys.time()))
      )
      rec <- chosen$recommendations
      for (i in seq_len(nrow(rec))) {
        DBI::dbExecute(con, "
          INSERT INTO recommendation (id_feedback, position, topic, resource_id, text)
          VALUES (?, ?, ?, ?, ?)",
          params = list(id, i, rec$topic[i], rec$resource_id[i], rec$text[i])
        )
      }
      DBI::dbExecute(con, "UPDATE api_call SET id_feedback = ? WHERE id_call = ?",
        params = list(id, chosen$id_call)
      )
      id
    })
  })
}

# The student's latest feedback, with title and link of each recommended
# source from the current source list (NA if a source was removed since).
# NULL if there's none.
ai_latest_feedback <- function(user_id, lookup = read_ai_lookup()) {
  ai_db(function(con) {
    fb <- DBI::dbGetQuery(con, "
      SELECT * FROM feedback WHERE id_user = ?
      ORDER BY created_at DESC, id_feedback DESC LIMIT 1",
      params = list(user_id)
    )
    if (nrow(fb) == 0L) {
      return(NULL)
    }
    rec <- DBI::dbGetQuery(con, "
      SELECT position, topic, resource_id, text FROM recommendation
      WHERE id_feedback = ? ORDER BY position",
      params = list(fb$id_feedback)
    )
    row <- if (is.null(lookup)) rep(NA_integer_, nrow(rec)) else match(rec$resource_id, lookup$id)
    rec$title <- if (is.null(lookup)) rep(NA_character_, nrow(rec)) else lookup$display_name[row]
    rec$link <- if (is.null(lookup)) rep(NA_character_, nrow(rec)) else lookup$link[row]
    c(as.list(fb[, c(
      "id_feedback", "created_at", "ability_computed_at", "model",
      "used_fallback", "summary"
    )]), list(recommendations = rec))
  })
}

# A click on a recommended source. The ids come from the browser, so it's
# only written if that feedback is the student's and has that position.
# TRUE if written.
ai_log_link_click <- function(user_id, id_feedback, position) {
  id <- suppressWarnings(as.integer(id_feedback))
  pos <- suppressWarnings(as.integer(position))
  if (length(id) != 1L || length(pos) != 1L || is.na(id) || is.na(pos)) {
    return(FALSE)
  }
  ai_db(function(con) {
    found <- DBI::dbGetQuery(con, "
      SELECT 1 FROM recommendation r JOIN feedback f ON f.id_feedback = r.id_feedback
      WHERE f.id_feedback = ? AND f.id_user = ? AND r.position = ?",
      params = list(id, user_id, pos)
    )
    if (nrow(found) == 0L) {
      return(FALSE)
    }
    DBI::dbExecute(con, "
      INSERT INTO link_click (id_feedback, position, id_user, created_at) VALUES (?, ?, ?, ?)",
      params = list(id, pos, user_id, as.integer(Sys.time()))
    )
    TRUE
  })
}

# "Aufgaben dazu üben" under a recommendation: up to `n` items of `area`,
# those not answered yet first, then open mistakes past their time-out
# (retry_eligible()), then the rest, each group in random order; the queue is
# shuffled at the end so it doesn't run group by group. `ud` is the
# student's responses (db_get_userdata()).
area_practice_items <- function(area, data_item, ud, n = AI_PRACTICE_ITEMS, today = Sys.Date()) {
  items <- data_item$id_item[as.character(data_item$learning_area) == area]
  if (length(items) == 0L) {
    return(integer(0))
  }
  answered <- unique(answered_rows(ud, data_item)$id_item)
  fresh <- setdiff(items, answered)
  wrong <- intersect(retry_eligible(open_items(ud, data_item), today)$id_item, items)
  rest <- setdiff(items, c(fresh, wrong))
  picked <- head(c(safe_sample(fresh), safe_sample(wrong), safe_sample(rest)), n)
  as.integer(safe_sample(picked))
}

# ── 5. May the student ask for feedback now? ──────────────────────────────────

# list(state, notice, minutes): state is "ready", or why not:
#   off          disabled in the config, no API key, or no system prompt
#   no_data      no area with enough items for a focus topic yet
#   cooldown     cooldown_minutes since the last feedback haven't passed. The
#                only limit per student: after it, a new feedback is possible
#                on the same estimate too (e.g. if the last one didn't help;
#                the sources are drawn anew each time)
#   daily_limit  the config's daily_limit of API calls is reached
# notice is the German text for the student (NULL when ready).
ai_feedback_status <- function(user_id, comp, cfg = ai_config(), now = Sys.time()) {
  out <- function(state, notice = NULL, minutes = 0L) {
    list(state = state, notice = notice, minutes = minutes)
  }
  if (!cfg$enabled || !nzchar(ai_api_key()) || !file.exists(ai_system_prompt_path())) {
    return(out("off", "Das persönliche Feedback ist zurzeit nicht verfügbar."))
  }
  status <- ai_learning_status(comp)
  if (nrow(ai_focus_topics(status, comp)) == 0L) {
    return(out("no_data", sprintf(
      "Bearbeite in mindestens einem Thema %d Aufgaben, dann kann ein Feedback erstellt werden.",
      WEAK_AREA_MIN_ITEMS
    )))
  }
  last <- ai_db(function(con) {
    DBI::dbGetQuery(con, "
      SELECT created_at FROM feedback WHERE id_user = ?
      ORDER BY created_at DESC LIMIT 1",
      params = list(user_id)
    )
  })
  if (nrow(last) == 1L) {
    until <- last$created_at + cfg$cooldown_minutes * 60
    seconds_left <- until - as.integer(now)
    if (seconds_left > 0) {
      minutes <- as.integer(ceiling(seconds_left / 60))
      return(out("cooldown", ai_cooldown_notice(until, now), minutes))
    }
  }
  if (ai_calls_today(now) >= cfg$daily_limit) {
    return(out("daily_limit", "Heute wurden schon sehr viele Feedbacks erstellt. Morgen geht es wieder."))
  }
  out("ready")
}

# "Ein neues Feedback ist ab 18:42 Uhr möglich." (later days with the date).
# The time it ends rather than a countdown: the text stays the same for the
# whole cooldown, so the card doesn't re-render every minute (which would
# wipe a comment being typed into the rating form).
ai_cooldown_notice <- function(until, now = Sys.time()) {
  until <- as.POSIXct(until, origin = "1970-01-01")
  day <- local_date(until)
  when <- if (day == local_date(now)) {
    format(until, "%H:%M")
  } else if (day == local_date(now) + 1L) {
    paste("morgen", format(until, "%H:%M"))
  } else {
    paste0(format_date_de(day), ", ", format(until, "%H:%M"))
  }
  sprintf("Ein neues Feedback ist ab %s Uhr möglich.", when)
}

# ── Students' rating of a feedback ────────────────────────────────────────────
# The ids come from the browser, so every write first checks that the
# feedback is the student's. Reasons are stored as codes (names of
# AI_REASONS_POSITIVE / AI_REASONS_NEGATIVE / AI_REPORT_REASONS), unknown
# codes are dropped.

as_int1 <- function(x) {
  x <- suppressWarnings(as.integer(unlist(x)))
  if (length(x) != 1L || is.na(x)) NA_integer_ else x
}

# A comment as stored: trimmed, at most AI_COMMENT_MAX_CHARS, NA if empty
ai_clean_comment <- function(x) {
  x <- unlist(x)
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  x <- trimws(as.character(x))
  if (!nzchar(x)) NA_character_ else substr(x, 1L, AI_COMMENT_MAX_CHARS)
}

ai_owns_feedback <- function(con, user_id, id) {
  nrow(DBI::dbGetQuery(con, "SELECT 1 FROM feedback WHERE id_feedback = ? AND id_user = ?",
    params = list(id, user_id)
  )) == 1L
}

# Saves the student's rating: one per feedback and student, updated. The
# thumb alone is saved on its click (reasons/comment NULL = keep them); the
# form then saves the reasons and the comment, each alone as it changes.
# Switching the thumb clears the
# reasons and the comment (they were about the other one). TRUE if saved.
ai_save_rating <- function(user_id, id_feedback, helpful, reasons = NULL, comment = NULL) {
  id <- as_int1(id_feedback)
  h <- as_int1(helpful)
  if (is.na(id) || is.na(h) || !h %in% 0:1) {
    return(FALSE)
  }
  allowed <- names(if (h == 1L) AI_REASONS_POSITIVE else AI_REASONS_NEGATIVE)
  ai_db(function(con) {
    if (!ai_owns_feedback(con, user_id, id)) {
      return(FALSE)
    }
    DBI::dbWithTransaction(con, {
      now <- as.integer(Sys.time())
      old <- DBI::dbGetQuery(con, "SELECT helpful FROM rating WHERE id_feedback = ? AND id_user = ?",
        params = list(id, user_id)
      )
      switched <- nrow(old) == 1L && !identical(as.integer(old$helpful), h)
      if (nrow(old) == 0L) {
        DBI::dbExecute(con, "
          INSERT INTO rating (id_feedback, id_user, helpful, comment, created_at, updated_at)
          VALUES (?, ?, ?, ?, ?, ?)",
          params = list(id, user_id, h, ai_clean_comment(comment), now, now)
        )
      } else {
        DBI::dbExecute(con, "UPDATE rating SET helpful = ?, updated_at = ? WHERE id_feedback = ? AND id_user = ?",
          params = list(h, now, id, user_id)
        )
        if (!is.null(comment) || switched) {
          DBI::dbExecute(con, "UPDATE rating SET comment = ? WHERE id_feedback = ? AND id_user = ?",
            params = list(if (is.null(comment)) NA_character_ else ai_clean_comment(comment), id, user_id)
          )
        }
      }
      if (!is.null(reasons) || switched) {
        DBI::dbExecute(con, "DELETE FROM rating_reason WHERE id_feedback = ? AND id_user = ?",
          params = list(id, user_id)
        )
        for (code in intersect(unique(as.character(unlist(reasons))), allowed)) {
          DBI::dbExecute(con, "INSERT INTO rating_reason (id_feedback, id_user, reason_code) VALUES (?, ?, ?)",
            params = list(id, user_id, code)
          )
        }
      }
    })
    TRUE
  })
}

# The student's rating of a feedback: list(helpful, reasons, comment), or NULL
ai_get_rating <- function(user_id, id_feedback) {
  ai_db(function(con) {
    r <- DBI::dbGetQuery(con, "SELECT helpful, comment FROM rating WHERE id_feedback = ? AND id_user = ?",
      params = list(as_int1(id_feedback), user_id)
    )
    if (nrow(r) == 0L) {
      return(NULL)
    }
    reasons <- DBI::dbGetQuery(con, "SELECT reason_code FROM rating_reason WHERE id_feedback = ? AND id_user = ?",
      params = list(as_int1(id_feedback), user_id)
    )$reason_code
    list(helpful = as.integer(r$helpful), reasons = reasons, comment = r$comment)
  })
}

# "Problem melden" on one recommendation: a reason (AI_REPORT_REASONS) and/or
# a comment (no reason but a comment = "other"). One per recommendation and
# student, a second one replaces it. TRUE if saved.
ai_save_report <- function(user_id, id_feedback, position, reason = NULL, comment = NULL) {
  id <- as_int1(id_feedback)
  pos <- as_int1(position)
  reason <- as.character(unlist(reason))
  reason <- if (length(reason) >= 1L && reason[1] %in% names(AI_REPORT_REASONS)) reason[1] else NA_character_
  comment <- ai_clean_comment(comment)
  if (is.na(id) || is.na(pos) || (is.na(reason) && is.na(comment))) {
    return(FALSE)
  }
  if (is.na(reason)) reason <- "other"
  ai_db(function(con) {
    found <- DBI::dbGetQuery(con, "
      SELECT 1 FROM recommendation r JOIN feedback f ON f.id_feedback = r.id_feedback
      WHERE f.id_feedback = ? AND f.id_user = ? AND r.position = ?",
      params = list(id, user_id, pos)
    )
    if (nrow(found) == 0L) {
      return(FALSE)
    }
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, "DELETE FROM recommendation_report WHERE id_feedback = ? AND position = ? AND id_user = ?",
        params = list(id, pos, user_id)
      )
      DBI::dbExecute(con, "
        INSERT INTO recommendation_report (id_feedback, position, id_user, created_at, reason_code, comment)
        VALUES (?, ?, ?, ?, ?, ?)",
        params = list(id, pos, user_id, as.integer(Sys.time()), reason, comment)
      )
    })
    TRUE
  })
}

# Positions of the recommendations the student reported a problem with
ai_reported_positions <- function(user_id, id_feedback) {
  ai_db(function(con) {
    DBI::dbGetQuery(con, "SELECT DISTINCT position FROM recommendation_report WHERE id_feedback = ? AND id_user = ?",
      params = list(as_int1(id_feedback), user_id)
    )$position
  })
}

# ── 6. Calling the model ──────────────────────────────────────────────────────

# Sends system and user prompt to ONE model. Returns a promise of
# list(data, meta): the structured answer and the call's metadata. Async, so
# the session stays usable while the model writes (no extra R process).
ai_query_model <- function(model, req) {
  cfg <- req$cfg
  extra <- cfg$model_args[[model]]
  if (is.null(extra)) extra <- list(reasoning_effort = cfg$reasoning_effort)
  # ellmer reads these when it sends the request. No retries of its own:
  # a failing model goes straight to the next one (429 is handled below).
  options(ellmer_timeout_s = cfg$timeout_seconds, ellmer_max_tries = 1L)
  chat <- ellmer::chat_openai_compatible(
    base_url = cfg$base_url,
    credentials = function() req$key,
    model = model,
    system_prompt = req$system_prompt,
    params = ellmer::params(max_tokens = cfg$max_tokens),
    api_args = extra,
    echo = "none"
  )
  promises::then(
    chat$chat_structured_async(req$user_prompt, type = req$schema),
    function(data) list(data = data, meta = ai_turn_meta(chat$last_turn()))
  )
}

# Token counts and the like from ellmer's last turn; NA where the API doesn't
# report them
ai_turn_meta <- function(turn) {
  get <- function(x) if (is.null(x) || length(x) == 0L) NA else x[[1]]
  tok <- tryCatch(turn@tokens, error = function(e) NULL)
  j <- tryCatch(turn@json, error = function(e) NULL)
  pick <- function(name, pos) {
    if (is.null(tok)) NA else if (!is.null(names(tok)) && name %in% names(tok)) tok[[name]] else get(tok[pos])
  }
  list(
    input_tokens = pick("input", 1L),
    output_tokens = pick("output", 2L),
    cached_tokens = pick("cached_input", 3L),
    reasoning_tokens = get(j$usage$completion_tokens_details$reasoning_tokens),
    finish_reason = get(j$choices[[1]]$finish_reason),
    response_model = get(j$model),
    response_id = get(j$id)
  )
}

# What went wrong, from the condition httr2/ellmer signal
ai_error_kind <- function(e) {
  msg <- conditionMessage(e)
  if (inherits(e, "httr2_http_429")) {
    "rate_limit"
  } else if (inherits(e, "httr2_http")) {
    "unavailable" # e.g. model removed (404), server error (5xx)
  } else if (grepl("timeout|timed out", msg, ignore.case = TRUE)) {
    "timeout"
  } else if (inherits(e, "httr2_failure") || inherits(e, "curl_error")) {
    "connection"
  } else {
    "invalid" # e.g. an answer ellmer couldn't parse
  }
}

ai_delay <- function(seconds) {
  promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), seconds))
}

# One call to one model, validated and logged. A promise that never rejects:
# it resolves to list(status, ...), status being "ok" (then also summary,
# recommendations, format_ok, id_call) or the outcome of the failure.
ai_call_once <- function(model, req) {
  t0 <- Sys.time()
  secs <- function() as.numeric(difftime(Sys.time(), t0, units = "secs"))
  p <- tryCatch(ai_query_model(model, req), error = function(e) promises::promise_reject(e))
  promises::then(
    p,
    onFulfilled = function(res) {
      v <- tryCatch(ai_validate_response(res$data, req$focus, req$resources), error = function(e) e)
      if (inherits(v, "error")) {
        ai_log_call(req$user_id, model, "invalid", secs(), res$meta, conditionMessage(v))
        return(list(status = "invalid", message = conditionMessage(v)))
      }
      fmt <- ai_check_format(v)
      id_call <- ai_log_call(req$user_id, model, if (fmt$passes) "ok" else "format", secs(), res$meta)
      c(v, list(status = "ok", model = model, format_ok = fmt$passes, id_call = id_call))
    },
    onRejected = function(e) {
      kind <- ai_error_kind(e)
      ai_log_call(req$user_id, model, kind, secs(), message = conditionMessage(e))
      list(status = kind, message = conditionMessage(e))
    }
  )
}

# One model; on "too many requests" wait and ask it again
ai_try_model <- coro::async(function(model, req) {
  for (attempt in seq_len(AI_RATE_LIMIT_TRIES)) {
    res <- await(ai_call_once(model, req))
    if (res$status != "rate_limit") {
      return(res)
    }
    if (attempt < AI_RATE_LIMIT_TRIES) await(ai_delay(AI_RATE_LIMIT_WAIT_S))
  }
  res
})

# ── 7. Validating the answer ──────────────────────────────────────────────────

# Drops everything not allowed: pairs of source and topic that weren't
# preselected, repeated sources or topics, empty texts. Error if no summary or
# no recommendation is left (then the next model is tried).
ai_validate_response <- function(response, focus, resources) {
  summary <- response$summary
  if (is.null(summary) || length(summary) != 1L || is.na(summary) || !nzchar(trimws(summary))) {
    stop("Response contains no summary.")
  }
  rec <- response$recommendations
  if (is.null(rec) || NROW(rec) == 0L) {
    stop("Response contains no recommendations.")
  }
  rec <- as.data.frame(rec, stringsAsFactors = FALSE)
  if (!all(c("topic", "resource_id", "text") %in% names(rec))) {
    stop("Recommendations do not have the expected fields.")
  }
  # ellmer returns enum fields as factors
  for (col in c("topic", "resource_id", "text")) rec[[col]] <- as.character(rec[[col]])
  rec <- rec[paste(rec$resource_id, rec$topic) %in% paste(resources$id, resources$topic), , drop = FALSE]
  rec <- rec[!duplicated(rec$resource_id), , drop = FALSE]
  rec <- rec[!duplicated(rec$topic), , drop = FALSE]
  rec <- rec[!is.na(rec$text) & nzchar(trimws(rec$text)), , drop = FALSE]
  rec <- head(rec, AI_FOCUS_TOPICS)
  if (nrow(rec) == 0L) {
    stop("None of the recommendations was valid.")
  }
  rec$text <- trimws(rec$text)
  rownames(rec) <- NULL
  list(summary = trimws(summary), recommendations = rec[, c("topic", "resource_id", "text")])
}

count_words <- function(text) {
  length(strsplit(trimws(text), "\\s+")[[1]])
}

# Length and the internal terms the feedback mustn't use. A word stem
# matches only at the start of a word ("Lernstands", not "Gesamtlernstand").
ai_check_format <- function(validated, tolerance = AI_FORMAT_TOLERANCE) {
  texts <- c(validated$summary, validated$recommendations$text)
  too_long <- count_words(validated$summary) > AI_MAX_WORDS_SUMMARY * tolerance ||
    max(vapply(validated$recommendations$text, count_words, integer(1))) >
      AI_MAX_WORDS_RECOMMENDATION * tolerance
  pattern <- paste0("(*UCP)\\b(", paste(AI_FORBIDDEN_WORDS, collapse = "|"), ")")
  hits <- gregexpr(pattern, texts, ignore.case = TRUE, perl = TRUE)
  n_forbidden <- sum(vapply(hits, function(h) sum(h > 0), integer(1)))
  list(too_long = too_long, forbidden_words = n_forbidden, passes = !too_long && n_forbidden == 0L)
}

# ── 8. Everything together ────────────────────────────────────────────────────

# Synchronous part, run on the click: checks, then builds the request.
# list(ok = FALSE, notice) or list(ok = TRUE, ...the request...).
# check_status = FALSE skips cooldown, snapshot and daily limit (only for
# dev/try_ai_feedback.R; the feature must still be on, with key and prompt).
ai_feedback_prepare <- function(user_id, comp, cfg = ai_config(), check_status = TRUE) {
  if (check_status) {
    st <- ai_feedback_status(user_id, comp, cfg)
    if (st$state != "ready") {
      return(list(ok = FALSE, notice = st$notice))
    }
  } else if (!cfg$enabled || !nzchar(ai_api_key())) {
    return(list(ok = FALSE, notice = "AI feedback is off (config) or there is no key."))
  }
  status <- ai_learning_status(comp)
  focus <- ai_focus_topics(status, comp)
  if (nrow(focus) == 0L) {
    return(list(ok = FALSE, notice = "No area has enough items for a focus topic yet."))
  }
  resources <- ai_select_resources(focus, read_ai_lookup())
  if (is.null(resources)) {
    return(list(ok = FALSE, notice = "Für deinen Lernstand sind gerade keine passenden Materialien hinterlegt."))
  }
  sp <- ai_system_prompt()
  if (is.null(sp)) {
    return(list(ok = FALSE, notice = "Das persönliche Feedback ist zurzeit nicht verfügbar."))
  }
  list(
    ok = TRUE, user_id = user_id, cfg = cfg, key = ai_api_key(),
    system_prompt = sp$text, prompt_version = sp$version,
    user_prompt = ai_user_prompt(status, focus, resources),
    schema = ai_schema(focus, resources),
    focus = focus, resources = resources,
    ability_computed_at = max(comp$computed_at, na.rm = TRUE)
  )
}

# Asynchronous part: tries the models in the config's order (skipping those
# blocked after a recent connection error, unless all are), takes the first
# answer that passes the format check, else the first usable one. Saves it.
# A promise of list(ok, notice, id_feedback): notice is German text for the
# student (also on success, if a fallback model answered).
ai_feedback_run <- coro::async(function(req, session_token) {
  start <- Sys.time()
  models <- req$cfg$models
  candidates <- setdiff(models, ai_blocked_models())
  if (length(candidates) == 0L) candidates <- models

  chosen <- NULL
  fallback <- NULL
  rate_limited <- FALSE
  for (model in candidates) {
    res <- await(ai_try_model(model, req))
    if (res$status == "rate_limit") {
      # the key's limit, another model won't help
      rate_limited <- TRUE
      break
    }
    if (res$status != "ok") next
    if (res$format_ok) {
      chosen <- res
      break
    }
    if (is.null(fallback)) fallback <- res
  }
  if (is.null(chosen)) chosen <- fallback
  ai_feedback_finish(req, session_token, chosen, rate_limited, start)
})

# After the models: saves the chosen answer and words the result. A plain
# function, because coro's async bodies can't hold `x <- if (...)`.
ai_feedback_finish <- function(req, session_token, chosen, rate_limited, start) {
  if (is.null(chosen)) {
    notice <- if (rate_limited) {
      "Gerade erstellen viele Personen gleichzeitig ein Feedback. Bitte versuch es in einer Minute noch einmal."
    } else {
      "Das persönliche Feedback ist gerade nicht verfügbar. Bitte versuch es später noch einmal."
    }
    return(list(ok = FALSE, notice = notice))
  }
  duration <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  id <- ai_save_feedback(req, session_token, chosen, duration)
  standard <- req$cfg$models[1]
  notice <- if (!identical(chosen$model, standard)) {
    sprintf(
      "Das Standardmodell (%s) war nicht erreichbar. Dieses Feedback hat ein Ersatzmodell geschrieben: %s.",
      ai_model_label(standard, req$cfg), ai_model_label(chosen$model, req$cfg)
    )
  }
  list(ok = TRUE, notice = notice, id_feedback = id)
}
