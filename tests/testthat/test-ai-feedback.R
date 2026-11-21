# AI feedback (R/ai_feedback.R). The model is never called: ai_query_model()
# is mocked, returning promises like the real one.

# ── Config and source list ────────────────────────────────────────────────────

test_that("a missing, broken or empty config leaves the feature off", {
  dir <- withr::local_tempdir()
  expect_false(ai_config(dir)$enabled)

  writeLines("{ not json", file.path(dir, AI_CONFIG_FILE))
  expect_warning(cfg <- ai_config(dir), "can't be read")
  expect_false(cfg$enabled)

  jsonlite::write_json(list(enabled = TRUE, models = list()), file.path(dir, AI_CONFIG_FILE), auto_unbox = TRUE)
  expect_false(ai_config(dir)$enabled)
})

test_that("the config is merged onto the defaults, model settings included", {
  dir <- withr::local_tempdir()
  writeLines('{
    "enabled": true, "models": ["a/x", "b/y"], "cooldown_minutes": 90,
    "model_labels": {"a/x": "Modell X"},
    "model_args": {"b/y": {"chat_template_kwargs": {"enable_thinking": false}}, "a/x": {}}
  }', file.path(dir, AI_CONFIG_FILE))
  cfg <- ai_config(dir)
  expect_true(cfg$enabled)
  expect_equal(cfg$models, c("a/x", "b/y"))
  expect_equal(cfg$cooldown_minutes, 90)
  expect_equal(cfg$daily_limit, AI_CONFIG_DEFAULTS$daily_limit)
  expect_equal(ai_model_label("a/x", cfg), "Modell X")
  expect_equal(ai_model_label("b/y", cfg), "b/y")
  expect_false(cfg$model_args[["b/y"]]$chat_template_kwargs$enable_thinking)
  expect_length(cfg$model_args[["a/x"]], 0L) # "{}": send nothing extra
})

test_that("the source list reads Excel's logical columns in any spelling", {
  dir <- withr::local_tempdir()
  lk <- make_lookup()
  lk$descriptive <- ifelse(lk$descriptive, "WAHR", "FALSCH")
  lk$probability <- as.integer(lk$probability)
  path <- file.path(dir, AI_LOOKUP_FILE)
  utils::write.csv(lk, path, row.names = FALSE)
  df <- read_ai_lookup(path)
  expect_type(df$descriptive, "logical")
  expect_equal(df$descriptive, make_lookup()$descriptive)
  expect_equal(df$probability, make_lookup()$probability)
  expect_null(read_ai_lookup(file.path(dir, "nope.csv")))
})

test_that("ai_lookup_problems() accepts a clean list and names the problems", {
  lk <- make_lookup()
  expect_length(ai_lookup_problems(lk), 0L)
  expect_match(ai_lookup_problems(NULL), "missing")

  bad <- lk
  bad$id[2] <- bad$id[1]
  bad$link[3] <- "example.org"
  bad$competence_max[4] <- 9
  bad$regression <- FALSE
  p <- ai_lookup_problems(bad)
  expect_true(any(grepl("duplicate ids: s11", p)))
  expect_true(any(grepl("invalid link: s13", p)))
  expect_true(any(grepl("level range.*s14", p)))
  expect_true(any(grepl("no source for Regression", p)))
  expect_true(any(grepl("no learning area", p)))
  expect_match(ai_lookup_problems(lk[, -1]), "missing columns: id")
})

# ── Learning status, focus topics, sources ────────────────────────────────────

test_that("the learning status uses the dashboard's levels and certainty dots", {
  comp <- make_comp(
    theta = c(0.4, -1.2, NA, 2.5, 0, 0, 0),
    se = c(0.2, 0.9, NA, 0.3, NA, 0.4, 0.4),
    n_items = c(20, 4, 0, 12, 2, 10, 10)
  )
  st <- ai_learning_status(comp)
  expect_equal(st$learning_area, LEARNING_AREA_LEVELS[-3]) # no estimate: dropped
  # 0.4 is "Solide" on COMPETENCY_SCALE (the prototype's thresholds said "Kompetent")
  expect_equal(st$competence[1], "Solide")
  expect_equal(st$competence[2], "Aufbau")
  expect_equal(st$competence[3], "Souverän")
  for (i in seq_len(nrow(st))) {
    k <- match(st$learning_area[i], LEARNING_AREA_LEVELS)
    cl <- competency_label(comp$theta[k], comp$n_items[k])
    expect_equal(st$dots[i], certainty_dot_count(comp$theta[k], comp$se[k], comp$n_items[k], cl))
  }
  expect_equal(st$evidence, AI_EVIDENCE_WORDS[st$dots])
  expect_equal(nrow(ai_learning_status(NULL)), 0L)
})

test_that("focus topics are the first three of rank_weak_areas()", {
  # Poweranalyse is the lowest but has too few items: in the status, not in focus
  comp <- make_comp(
    theta = c(1.2, 1.8, -0.6, 1.7, -2.5, 0.1, -0.2),
    n_items = c(10, 10, 10, 10, 3, 10, 10)
  )
  st <- ai_learning_status(comp)
  focus <- ai_focus_topics(st, comp)
  expect_equal(focus$learning_area, head(rank_weak_areas(comp), 3))
  expect_equal(focus$learning_area, c("Grundlagen der Inferenzstatistik", "Regression", "Zusammenhangsmaße"))
  expect_true("Poweranalyse" %in% st$learning_area)

  few <- make_comp(theta = rep(0, 7), n_items = rep(3, 7))
  expect_equal(nrow(ai_focus_topics(ai_learning_status(few), few)), 0L)
})

test_that("sources fit the area and level, keep every learning function, and are capped", {
  set.seed(1)
  comp <- comp_mixed()
  st <- ai_learning_status(comp)
  focus <- ai_focus_topics(st, comp)
  res <- ai_select_resources(focus, make_lookup())
  for (a in focus$learning_area) {
    ids <- res$id[res$topic == a]
    lk <- make_lookup()
    lk <- lk[lk$id %in% ids, ]
    expect_true(all(lk[[AI_LOOKUP_COLUMNS[[a]]]]))
    lvl <- focus$level[focus$learning_area == a]
    expect_true(all(lk$competence_min <= lvl & lk$competence_max >= lvl))
  }
  # Poweranalyse is "Aufbau" (level 1): all four learning functions, no level 6-7 source
  pw <- res[res$topic == "Poweranalyse", ]
  expect_setequal(pw$learning_function, c("Einstieg", "Vertiefung", "Anwendung", "Nachschlagen"))
  expect_false("s59" %in% pw$id)

  capped <- ai_select_resources(focus, make_lookup(), n_max = 2)
  expect_true(all(table(capped$topic) <= 2))
  expect_null(ai_select_resources(focus, make_lookup()[0, ]))
  expect_null(ai_select_resources(focus, NULL))
})

# ── Prompt and schema ─────────────────────────────────────────────────────────

test_that("the user prompt has levels and words only, no estimates", {
  set.seed(1)
  comp <- comp_mixed()
  st <- ai_learning_status(comp)
  focus <- ai_focus_topics(st, comp)
  res <- ai_select_resources(focus, make_lookup())
  p <- ai_user_prompt(st, focus, res)
  expect_match(p, "^LERNSTAND\nDeskriptivstatistik: Fortgeschritten \\| Evidenz: ")
  expect_match(p, "FOKUSTHEMEN\nGrundlagen der Inferenzstatistik, Poweranalyse, Zusammenhangsmaße\n", fixed = TRUE)
  expect_match(p, "s51 | Quelle 5.1 | Video | Poweranalyse | Einstieg | Intuitiv | Kurz", fixed = TRUE)
  expect_false(grepl("-0.6|1.8|0.4", p))
})

test_that("the system prompt gets the evidence words and a version", {
  sp <- ai_system_prompt()
  expect_false(grepl("[[EVIDENCE_", sp$text, fixed = TRUE))
  expect_match(sp$text, paste(AI_EVIDENCE_WORDS, collapse = ", "), fixed = TRUE)
  expect_match(sp$version, "^[0-9a-f]{8}$")
})

# ── Validation and format ─────────────────────────────────────────────────────

test_that("validation keeps only preselected pairs, each source and topic once", {
  resources <- data.frame(
    id = c("a", "b", "c"), topic = c("Regression", "Regression", "Poweranalyse"),
    stringsAsFactors = FALSE
  )
  focus <- data.frame(learning_area = c("Regression", "Poweranalyse"))
  resp <- list(
    summary = "  Gut.  ",
    recommendations = data.frame(
      topic = factor(c("Regression", "Regression", "Poweranalyse", "Poweranalyse")),
      resource_id = factor(c("a", "b", "a", "c")),
      text = c("eins", "zwei", "drei", "vier")
    )
  )
  v <- ai_validate_response(resp, focus, resources)
  expect_equal(v$summary, "Gut.")
  expect_equal(v$recommendations$resource_id, c("a", "c"))
  expect_type(v$recommendations$topic, "character")

  resp$recommendations$resource_id <- factor(c("c", "c", "a", "b"))
  expect_error(ai_validate_response(resp, focus, resources), "None of the recommendations")
  expect_error(ai_validate_response(list(summary = ""), focus, resources), "no summary")
})

test_that("the format check counts internal terms at word starts and length", {
  ok <- list(summary = "Du kommst gut voran.", recommendations = data.frame(text = "Probier das aus."))
  expect_true(ai_check_format(ok)$passes)

  bad <- ok
  bad$summary <- "Dein Lernstand ist gut, die Evidenz hoch, dein Lernstands-Bild klar."
  expect_equal(ai_check_format(bad)$forbidden_words, 3L)
  # inside a compound it isn't the internal term
  bad$summary <- "Dein Gesamtlernstand ist gut."
  expect_true(ai_check_format(bad)$passes)

  long <- ok
  long$summary <- paste(rep("Wort", AI_MAX_WORDS_SUMMARY * AI_FORMAT_TOLERANCE + 1), collapse = " ")
  expect_true(ai_check_format(long)$too_long)
  expect_false(ai_check_format(long)$passes)
})

test_that("errors are classified so only connection problems block a model", {
  expect_equal(ai_error_kind(http_error(429)), "rate_limit")
  expect_equal(ai_error_kind(http_error(404)), "unavailable")
  expect_equal(ai_error_kind(http_error(503)), "unavailable")
  timeout <- structure(class = c("httr2_failure", "error", "condition"),
    list(message = "Failed to perform HTTP request. Timeout was reached", call = NULL))
  expect_equal(ai_error_kind(timeout), "timeout")
  refused <- structure(class = c("httr2_failure", "error", "condition"),
    list(message = "Failed to connect", call = NULL))
  expect_equal(ai_error_kind(refused), "connection")
  expect_equal(ai_error_kind(simpleError("Response contains no summary.")), "invalid")
  expect_false("invalid" %in% AI_BLOCKING_ERRORS)
  expect_false("rate_limit" %in% AI_BLOCKING_ERRORS)
})

# ── Status (may the student ask?) ─────────────────────────────────────────────

test_that("the feature is off without config or key", {
  local_ai_dir(cfg = NULL)
  expect_equal(ai_feedback_status("u", comp_mixed())$state, "off")

  local_ai_dir()
  withr::local_envvar(TIGER_LLM_KEY = "")
  expect_equal(ai_feedback_status("u", comp_mixed())$state, "off")
})

test_that("no feedback before an area has enough items", {
  local_ai_dir()
  few <- make_comp(theta = rep(0, 7), n_items = rep(3, 7))
  st <- ai_feedback_status("u", few)
  expect_equal(st$state, "no_data")
  expect_match(st$notice, as.character(WEAK_AREA_MIN_ITEMS))
})

test_that("new feedback needs only the cooldown to have passed, also on the same estimate", {
  local_ai_dir()
  now <- as.POSIXct("2026-11-20 12:00:00")
  expect_equal(ai_feedback_status("u", comp_mixed(1000L), now = now)$state, "ready")

  ai_db(function(con) {
    DBI::dbExecute(con, "INSERT INTO feedback (id_user, created_at, ability_computed_at) VALUES (?, ?, ?)",
      params = list("u", as.integer(now) - 3600L, 1000L))
  })
  # only 1 of 4 hours passed, with a newer estimate or not
  for (snapshot in c(1000L, 2000L)) {
    st <- ai_feedback_status("u", comp_mixed(snapshot), now = now)
    expect_equal(st$state, "cooldown")
    expect_equal(st$minutes, 180L)
    expect_equal(st$notice, "Ein neues Feedback ist ab 15:00 Uhr möglich.")
  }
  # 4 hours later: possible again, on the same estimate too (the last one
  # may not have helped)
  expect_equal(ai_feedback_status("u", comp_mixed(1000L), now = now + 3 * 3600)$state, "ready")
  expect_equal(ai_feedback_status("u", comp_mixed(2000L), now = now + 3 * 3600)$state, "ready")
  # other users aren't affected
  expect_equal(ai_feedback_status("v", comp_mixed(1000L), now = now)$state, "ready")
})

test_that("the cooldown is set in minutes and 0 turns it off", {
  now <- as.POSIXct("2026-11-20 12:00:00")
  last_feedback <- function() {
    ai_db(function(con) {
      DBI::dbExecute(con, "INSERT INTO feedback (id_user, created_at) VALUES (?, ?)",
        params = list("u", as.integer(now) - 60L))
    })
  }
  local_ai_dir(cfg = list(enabled = TRUE, models = "m1", cooldown_minutes = 10))
  last_feedback()
  st <- ai_feedback_status("u", comp_mixed(), now = now)
  expect_equal(st$state, "cooldown")
  expect_equal(st$notice, "Ein neues Feedback ist ab 12:09 Uhr möglich.")
  expect_equal(st$minutes, 9L)
  expect_equal(ai_feedback_status("u", comp_mixed(), now = now + 9 * 60)$state, "ready")

  local_ai_dir(cfg = list(enabled = TRUE, models = "m1", cooldown_minutes = 0))
  last_feedback()
  expect_equal(ai_feedback_status("u", comp_mixed(), now = now)$state, "ready")

  expect_equal(AI_CONFIG_DEFAULTS$cooldown_minutes, 240)
})

test_that("the daily limit counts today's API calls of all users", {
  local_ai_dir(cfg = list(enabled = TRUE, models = "m1", daily_limit = 2))
  ai_log_call("a", "m1", "ok", 1)
  expect_equal(ai_feedback_status("u", comp_mixed())$state, "ready")
  ai_log_call("b", "m1", "timeout", 30)
  expect_equal(ai_feedback_status("u", comp_mixed())$state, "daily_limit")
})

# ── The whole run ─────────────────────────────────────────────────────────────

test_that("a good answer from the first model is saved with its call's metadata", {
  local_ai_dir(cfg = list(enabled = TRUE, models = c("m1", "m2"), model_labels = list(m1 = "Modell 1")))
  calls <- mock_models(list(m1 = function(req) fake_answer(req)))
  req <- ai_feedback_prepare("u", comp_mixed())
  expect_true(req$ok)
  out <- wait_for(ai_feedback_run(req, "tok"))

  expect_true(out$ok)
  expect_null(out$notice)
  expect_equal(calls$log, "m1")
  fb <- ai_latest_feedback("u")
  expect_equal(fb$id_feedback, out$id_feedback)
  expect_equal(fb$model, "m1")
  expect_equal(fb$used_fallback, 0L)
  expect_equal(fb$ability_computed_at, 1000L)
  expect_equal(nrow(fb$recommendations), 3L)
  expect_true(all(grepl("^Quelle", fb$recommendations$title)))
  expect_true(all(grepl("^https://example.org/", fb$recommendations$link)))

  call <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT * FROM api_call"))
  expect_equal(nrow(call), 1L)
  expect_equal(call$outcome, "ok")
  expect_equal(call$id_feedback, out$id_feedback)
  expect_equal(call$input_tokens, 2300L)
  expect_equal(call$output_tokens, 410L)
  expect_equal(call$finish_reason, "stop")
  saved <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT * FROM feedback"))
  expect_equal(saved$prompt_version, ai_system_prompt()$version)
  expect_equal(saved$user_prompt, req$user_prompt)
  expect_false(grepl("test-key|^u$", saved$user_prompt))
})

test_that("a timeout falls back to the next model, says so, and blocks the model", {
  local_ai_dir(cfg = list(
    enabled = TRUE, models = c("m1", "m2"),
    model_labels = list(m1 = "Modell 1", m2 = "Modell 2")
  ))
  timeout <- structure(class = c("httr2_failure", "error", "condition"),
    list(message = "Timeout was reached", call = NULL))
  calls <- mock_models(list(m1 = function(req) stop(timeout), m2 = function(req) fake_answer(req)))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_true(out$ok)
  expect_match(out$notice, "Modell 1.*Modell 2")
  expect_equal(ai_latest_feedback("u")$used_fallback, 1L)
  expect_equal(ai_blocked_models(), "m1")

  # the next request (another user) skips m1 straight away
  calls$log <- character(0)
  out2 <- wait_for(ai_feedback_run(ai_feedback_prepare("v", comp_mixed()), "tok"))
  expect_true(out2$ok)
  expect_equal(calls$log, "m2")
})

test_that("an unusable answer tries the next model but doesn't block the first", {
  local_ai_dir()
  mock_models(list(
    m1 = function(req) list(summary = "x", recommendations = data.frame(
      topic = factor("Regression"), resource_id = factor("s11"), text = "y"
    )),
    m2 = function(req) fake_answer(req)
  ))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_true(out$ok)
  expect_equal(ai_latest_feedback("u")$model, "m2")
  expect_length(ai_blocked_models(), 0L)
  outcomes <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT model, outcome FROM api_call ORDER BY id_call"))
  expect_equal(outcomes$outcome, c("invalid", "ok"))
})

test_that("an answer breaking the format rules is only used if nothing better comes", {
  local_ai_dir()
  mock_models(list(
    m1 = function(req) fake_answer(req, summary = "Dein Lernstand ist gut."),
    m2 = function(req) fake_answer(req)
  ))
  wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_equal(ai_latest_feedback("u")$model, "m2")

  local_ai_dir(cfg = list(enabled = TRUE, models = "m1"))
  mock_models(list(m1 = function(req) fake_answer(req, summary = "Dein Lernstand ist gut.")))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_true(out$ok)
  saved <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT model, format_ok FROM feedback"))
  expect_equal(saved$format_ok, 0L)
})

test_that("too many requests waits and asks the same model again, then gives up", {
  local_ai_dir()
  n <- 0L
  mock_models(list(
    m1 = function(req) {
      n <<- n + 1L
      if (n < 3L) stop(http_error(429)) else fake_answer(req)
    },
    m2 = function(req) stop("m2 must not be asked")
  ))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_true(out$ok)
  expect_equal(n, 3L)
  expect_equal(ai_latest_feedback("u")$model, "m1")

  local_ai_dir()
  calls <- mock_models(list(m1 = function(req) stop(http_error(429)), m2 = function(req) fake_answer(req)))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_false(out$ok)
  expect_match(out$notice, "viele Personen")
  expect_equal(calls$log, rep("m1", AI_RATE_LIMIT_TRIES)) # another model wouldn't help
  expect_length(ai_blocked_models(), 0L)
})

test_that("nothing usable from any model: no feedback, a notice", {
  local_ai_dir()
  mock_models(list(m1 = function(req) stop(http_error(503)), m2 = function(req) stop(http_error(404))))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_false(out$ok)
  expect_match(out$notice, "nicht verfügbar")
  expect_null(ai_latest_feedback("u"))
})

test_that("prepare stops early without a source list or fitting sources", {
  local_ai_dir(lookup = NULL)
  expect_false(ai_feedback_prepare("u", comp_mixed())$ok)
  local_ai_dir(lookup = make_lookup()[0, ])
  out <- ai_feedback_prepare("u", comp_mixed())
  expect_false(out$ok)
  expect_match(out$notice, "keine passenden Materialien")
})

test_that("a source removed from the list since shows without title and link", {
  local_ai_dir()
  mock_models(list(m1 = function(req) fake_answer(req)))
  wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  fb <- ai_latest_feedback("u", lookup = make_lookup()[0, ])
  expect_true(all(is.na(fb$recommendations$title)))
  expect_equal(nrow(fb$recommendations), 3L)
})

test_that("the cooldown notice names the time it ends", {
  now <- as.POSIXct("2026-11-20 12:00:00")
  expect_equal(ai_cooldown_notice(now + 3600, now), "Ein neues Feedback ist ab 13:00 Uhr möglich.")
  expect_equal(ai_cooldown_notice(now + 14 * 3600, now), "Ein neues Feedback ist ab morgen 02:00 Uhr möglich.")
  expect_equal(ai_cooldown_notice(now + 50 * 3600, now), "Ein neues Feedback ist ab 22. Nov 2026, 14:00 Uhr möglich.")
})

# ── Rating ────────────────────────────────────────────────────────────────────

local_saved_feedback <- function(env = parent.frame()) {
  local_ai_dir(env = env)
  mock_models(list(m1 = function(req) fake_answer(req)), env = env)
  wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))$id_feedback
}

test_that("the thumb is saved alone, then reasons and comment; one rating per student", {
  id <- local_saved_feedback()
  expect_null(ai_get_rating("u", id))

  expect_true(ai_save_rating("u", id, 0L))
  expect_equal(ai_get_rating("u", id), list(helpful = 0L, reasons = character(0), comment = NA_character_))

  expect_true(ai_save_rating("u", id, 0L,
    reasons = c("too_generic", "link_broken", "good_materials", "nonsense"),
    comment = "  Zu allgemein.  "
  ))
  r <- ai_get_rating("u", id)
  expect_setequal(r$reasons, c("too_generic", "link_broken")) # only reasons for a thumbs-down
  expect_equal(r$comment, "Zu allgemein.")

  # the thumb again alone keeps reasons and comment
  expect_true(ai_save_rating("u", id, 0L))
  expect_equal(ai_get_rating("u", id)$comment, "Zu allgemein.")
  expect_length(ai_get_rating("u", id)$reasons, 2L)

  n <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM rating")$n)
  expect_equal(n, 1L)
})

test_that("switching the thumb clears the reasons and comment of the other one", {
  id <- local_saved_feedback()
  ai_save_rating("u", id, 0L, reasons = "incorrect", comment = "Falsch.")
  expect_true(ai_save_rating("u", id, 1L))
  expect_equal(ai_get_rating("u", id), list(helpful = 1L, reasons = character(0), comment = NA_character_))
  ai_save_rating("u", id, 1L, reasons = c("clear_start", "incorrect"), comment = "")
  r <- ai_get_rating("u", id)
  expect_equal(r$reasons, "clear_start")
  expect_true(is.na(r$comment))
})

test_that("ratings are only saved for the student's own feedback and valid values", {
  id <- local_saved_feedback()
  expect_false(ai_save_rating("v", id, 1L))
  expect_false(ai_save_rating("u", id + 99L, 1L))
  expect_false(ai_save_rating("u", id, 2L))
  expect_false(ai_save_rating("u", "x", 1L))
  expect_false(ai_save_rating("u", id, NULL))
  expect_null(ai_get_rating("u", id))
})

test_that("comments are cut to AI_COMMENT_MAX_CHARS", {
  id <- local_saved_feedback()
  ai_save_rating("u", id, 0L, comment = strrep("a", AI_COMMENT_MAX_CHARS + 50))
  expect_equal(nchar(ai_get_rating("u", id)$comment), AI_COMMENT_MAX_CHARS)
})

test_that("a problem report needs a reason or a comment and replaces an earlier one", {
  id <- local_saved_feedback()
  expect_false(ai_save_report("u", id, 1, reason = NULL, comment = "   "))
  expect_false(ai_save_report("u", id, 9, reason = "link_broken")) # no such position
  expect_false(ai_save_report("v", id, 1, reason = "link_broken")) # someone else's
  expect_true(ai_save_report("u", id, 1, reason = "link_broken"))
  expect_true(ai_save_report("u", id, 1, reason = list("materials_unfit"), comment = "Zu schwer."))
  expect_true(ai_save_report("u", id, 2, reason = "nonsense", comment = "Seite leer."))
  rep <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT position, reason_code, comment FROM recommendation_report ORDER BY position"))
  expect_equal(rep$position, 1:2)
  expect_equal(rep$reason_code, c("materials_unfit", "other"))
  expect_equal(rep$comment, c("Zu schwer.", "Seite leer."))
  expect_equal(sort(ai_reported_positions("u", id)), 1:2)
  expect_length(ai_reported_positions("v", id), 0L)
})

test_that("each prompt version's full text is stored once", {
  local_ai_dir()
  mock_models(list(m1 = function(req) fake_answer(req)))
  for (user in c("u", "v")) wait_for(ai_feedback_run(ai_feedback_prepare(user, comp_mixed()), "tok"))
  p <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT * FROM prompt"))
  sp <- ai_system_prompt()
  expect_equal(nrow(p), 1L)
  expect_equal(p$version, sp$version)
  expect_equal(p$text, sp$text)
  versions <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT DISTINCT prompt_version FROM feedback")$prompt_version)
  expect_equal(versions, sp$version)
  # the version is of the text as sent: a different text, a different version
  expect_false(identical(ai_prompt_version(paste(sp$text, "x")), sp$version))
  expect_match(sp$version, "^[0-9a-f]{8}$")
})
