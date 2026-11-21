# The "Persönliches Feedback" card (R/ai_feedback_card.R) and its server part
# in mod_dashboard_server(). Fixtures: helper-ai.R.

fake_feedback <- function(used_fallback = 0L, title = "Quelle 7.1") {
  list(
    id_feedback = 12L,
    created_at = as.integer(as.POSIXct("2026-11-20 14:03:00")),
    ability_computed_at = 1000L,
    model = "m1",
    used_fallback = used_fallback,
    summary = "Du kommst gut voran <b>wirklich</b>.",
    recommendations = data.frame(
      position = 1:2, topic = c("Regression", "Poweranalyse"),
      resource_id = c("s71", "s51"), text = c("Schau rein.", "Probier es aus."),
      title = c(title, "Quelle 5.1"),
      link = c(if (is.na(title)) NA else "https://example.org/7/1", "https://example.org/5/1"),
      stringsAsFactors = FALSE
    )
  )
}

card_html <- function(...) {
  view <- utils::modifyList(
    list(state = "ready", notice = NULL, running = FALSE, feedback = NULL,
         cfg = list(model_labels = list(m1 = "Modell Eins")), flash = NULL),
    list(...)
  )
  as.character(ai_feedback_card(view, ns = function(x) paste0("dash-", x)))
}

# ── The card ──────────────────────────────────────────────────────────────────

test_that("the card always says it's AI-generated", {
  for (state in c("ready", "cooldown", "off")) {
    expect_match(card_html(state = state, notice = "x"), "KI-generiert", fixed = TRUE)
  }
})

test_that("without feedback it explains the feature and offers the button", {
  html <- card_html()
  expect_match(html, "ohne deinen Namen", fixed = TRUE)
  expect_match(html, "Feedback erstellen", fixed = TRUE)
  expect_match(html, "Shiny.setInputValue(&#39;dash-ai_create&#39;", fixed = TRUE)
})

test_that("when it's not possible, the reason replaces the button", {
  html <- card_html(state = "cooldown", notice = "Ein neues Feedback ist ab 18:42 Uhr möglich.", feedback = fake_feedback())
  expect_match(html, "ab 18:42 Uhr", fixed = TRUE)
  expect_no_match(html, "ai_create", fixed = TRUE)
  expect_match(html, "Du kommst gut voran", fixed = TRUE) # the last one stays
})

test_that("switched off: only the notice, no old feedback, no footer", {
  html <- card_html(state = "off", notice = "Das persönliche Feedback ist zurzeit nicht verfügbar.", feedback = fake_feedback())
  expect_match(html, "zurzeit nicht verfügbar", fixed = TRUE)
  expect_no_match(html, "Du kommst gut voran", fixed = TRUE)
  expect_no_match(html, "ai-fb-footer", fixed = TRUE)
})

test_that("feedback: escaped summary, linked titles with click logging, model and date", {
  html <- card_html(feedback = fake_feedback())
  expect_match(html, "Du kommst gut voran &lt;b&gt;wirklich&lt;/b&gt;.", fixed = TRUE)
  expect_match(html, 'href="https://example.org/7/1"', fixed = TRUE)
  expect_match(html, 'target="_blank"', fixed = TRUE)
  expect_match(html, "Shiny.setInputValue(&#39;dash-ai_link&#39;, {id: 12, pos: 1", fixed = TRUE)
  expect_match(html, "Von einer KI erstellt mit Modell Eins, 20. Nov 2026, 14:03.", fixed = TRUE)
  expect_match(html, "Neues Feedback erstellen", fixed = TRUE)
  expect_no_match(html, "Ersatzmodell", fixed = TRUE)
})

test_that("a fallback model and a removed source are said so", {
  html <- card_html(feedback = fake_feedback(used_fallback = 1L, title = NA))
  expect_match(html, "Ersatzmodell", fixed = TRUE)
  expect_match(html, "nicht mehr verfügbar", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("ai-fb-title", html))), 1L)
})

test_that("while running: busy button, no second click possible", {
  html <- card_html(running = TRUE)
  expect_match(html, "Wird erstellt", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
  expect_no_match(html, "ai_create", fixed = TRUE)
  expect_match(html, "ai-feedback-skeleton", fixed = TRUE)
  expect_match(card_html(running = TRUE, feedback = fake_feedback()), "is-running", fixed = TRUE)
})

test_that("a message from the last click is shown", {
  html <- card_html(flash = list(type = "warning", text = "Gerade nicht verfügbar."))
  expect_match(html, "ai-fb-flash is-warning", fixed = TRUE)
  expect_match(html, "Gerade nicht verfügbar.", fixed = TRUE)
})

# ── Rating and "Problem melden" ───────────────────────────────────────────────

test_that("the rating starts with the two thumbs, no form", {
  html <- card_html(feedback = fake_feedback())
  expect_match(html, "War dieses Feedback hilfreich?", fixed = TRUE)
  expect_match(html, 'data-ai-action="rate" data-input="dash-ai_rate" data-id="12" data-helpful="1"', fixed = TRUE)
  expect_match(html, 'data-helpful="0"', fixed = TRUE)
  expect_no_match(html, "ai-fb-rate-form", fixed = TRUE)
  expect_no_match(html, "is-on", fixed = TRUE)
})

test_that("after a thumbs-down the form shows the negative reasons, ticks and the comment", {
  rating <- list(helpful = 0L, reasons = "too_generic", comment = "Zu <b>vage</b>.")
  html <- card_html(feedback = fake_feedback(), rating = rating, rate_open = TRUE)
  expect_match(html, "ai-fb-rate-form", fixed = TRUE)
  for (label in AI_REASONS_NEGATIVE) expect_match(html, label, fixed = TRUE)
  expect_no_match(html, AI_REASONS_POSITIVE[["good_materials"]], fixed = TRUE)
  expect_match(html, 'value="too_generic" checked', fixed = TRUE)
  expect_match(html, "Zu &lt;b&gt;vage&lt;/b&gt;.", fixed = TRUE)
  expect_match(html, sprintf('maxlength="%d"', AI_COMMENT_MAX_CHARS), fixed = TRUE)
  expect_match(html, "Dein Kommentar wird gespeichert", fixed = TRUE)
  # saves itself: no Senden button, the inputs for ticks and comment instead
  expect_no_match(html, "Senden", fixed = TRUE)
  expect_match(html, 'data-reasons-input="dash-ai_rate_reasons" data-comment-input="dash-ai_rate_comment"', fixed = TRUE)
  expect_match(html, "automatisch gespeichert", fixed = TRUE)
  expect_match(html, 'data-ai-action="rate-close" data-input="dash-ai_rate_close"', fixed = TRUE)
  expect_match(html, 'aria-pressed="true"', fixed = TRUE)
})

test_that("after a thumbs-up the form shows the positive reasons", {
  html <- card_html(feedback = fake_feedback(), rating = list(helpful = 1L, reasons = character(0), comment = NA), rate_open = TRUE)
  for (label in AI_REASONS_POSITIVE) expect_match(html, label, fixed = TRUE)
  expect_no_match(html, AI_REASONS_NEGATIVE[["incorrect"]], fixed = TRUE)
})

test_that("coming back later (form closed): a thank-you and Ändern", {
  html <- card_html(feedback = fake_feedback(), rating = list(helpful = 1L, reasons = character(0), comment = NA), rate_open = FALSE)
  expect_match(html, "Danke für deine Rückmeldung!", fixed = TRUE)
  expect_match(html, 'data-ai-action="rate-edit"', fixed = TRUE)
  expect_no_match(html, "ai-fb-rate-form", fixed = TRUE)
})

test_that("a rating from an earlier session is final: thumb shown, nothing to click", {
  rating <- list(helpful = 0L, reasons = "incorrect", comment = "x")
  html <- card_html(feedback = fake_feedback(), rating = rating, rate_locked = TRUE, rate_open = TRUE)
  expect_match(html, "ai-fb-rate is-locked", fixed = TRUE)
  expect_match(html, "Danke für deine Rückmeldung!", fixed = TRUE)
  expect_no_match(html, "data-ai-action=\"rate", fixed = TRUE) # no rate, rate-edit, rate-close
  expect_no_match(html, "Ändern", fixed = TRUE)
  expect_no_match(html, "ai-fb-rate-form", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("disabled", html))), 2L)
  expect_match(html, 'aria-pressed="true"', fixed = TRUE)
  # without a rating there is nothing to lock
  expect_match(card_html(feedback = fake_feedback(), rate_locked = TRUE), 'data-ai-action="rate"', fixed = TRUE)
})

test_that("each recommendation can be reported once", {
  html <- card_html(feedback = fake_feedback(), reported = 2L)
  expect_equal(lengths(regmatches(html, gregexpr("Problem melden", html))), 1L)
  expect_match(html, "Gemeldet, danke!", fixed = TRUE)
  expect_match(html, 'data-ai-action="report-send" data-input="dash-ai_report" data-id="12" data-pos="1"', fixed = TRUE)
  for (label in AI_REPORT_REASONS) expect_match(html, label, fixed = TRUE)
})

test_that("while a new one is written, no rating and no reports", {
  html <- card_html(feedback = fake_feedback(), running = TRUE)
  expect_no_match(html, "War dieses Feedback hilfreich?", fixed = TRUE)
  expect_no_match(html, "Problem melden", fixed = TRUE)
})

# ── Link clicks ───────────────────────────────────────────────────────────────

test_that("link clicks are logged only for the student's own feedback", {
  local_ai_dir()
  mock_models(list(m1 = function(req) fake_answer(req)))
  out <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))
  expect_true(ai_log_link_click("u", out$id_feedback, 2))
  expect_false(ai_log_link_click("v", out$id_feedback, 2)) # someone else's
  expect_false(ai_log_link_click("u", out$id_feedback, 9)) # no such position
  expect_false(ai_log_link_click("u", "x", 1))
  expect_false(ai_log_link_click("u", NULL, 1))
  clicks <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT * FROM link_click"))
  expect_equal(nrow(clicks), 1L)
  expect_equal(clicks$position, 2L)
})

# ── Server part (mod_dashboard_server) ────────────────────────────────────────

# Runs the event loop and Shiny's reactive flush until `done()` is TRUE
pump_until <- function(session, done, timeout = 5) {
  end <- Sys.time() + timeout
  while (!done() && Sys.time() < end) {
    later::run_now(0.01)
    session$flushReact()
  }
  expect_true(done())
}

dashboard_args <- function() {
  list(
    data_item = make_data_item(),
    credentials = fake_credentials("u"),
    write_trigger = reactiveVal(0L),
    ability_computed_this_session = reactiveVal(FALSE)
  )
}

local_ability <- function(dir, computed_at = 1000L) {
  path <- make_ability_db(
    user = "u", computed_at = computed_at,
    theta = setNames(c(1.2, 1.8, -0.6, 1.7, -1.8, 0.1, 1.9), LEARNING_AREA_LEVELS),
    n_items = setNames(rep(10L, 7), LEARNING_AREA_LEVELS),
    se = setNames(rep(0.4, 7), LEARNING_AREA_LEVELS)
  )
  file.copy(path, file.path(dir, "db_ability.sqlite"), overwrite = TRUE)
}

test_that("dashboard: the button creates a feedback in the background and shows it", {
  dir <- local_ai_dir(cfg = list(enabled = TRUE, models = "m1", model_labels = list(m1 = "Modell Eins")))
  local_ability(dir)
  mock_models(list(m1 = function(req) fake_answer(req, summary = "Das ist dein Feedback.")))

  testServer(mod_dashboard_server, args = dashboard_args(), {
    expect_equal(ai_status()$state, "ready")
    session$setInputs(ai_create = 1)
    pump_until(session, function() ai_task$status() == "success")
    session$flushReact()
    html <- as.character(output$ai_feedback$html)
    expect_match(html, "Das ist dein Feedback.", fixed = TRUE)
    expect_match(html, "Modell Eins", fixed = TRUE)
    # the next one only after the cooldown
    expect_equal(ai_status()$state, "cooldown")
    expect_match(html, "Ein neues Feedback ist ab .* Uhr möglich.")
    expect_no_match(html, "ai_create", fixed = TRUE)
    expect_null(ai_flash())

    session$setInputs(ai_link = list(id = ai_latest()$id_feedback, pos = 1, t = 1))
    n <- ai_db(function(con) DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM link_click")$n)
    expect_equal(n, 1L)
  })
})

test_that("dashboard: the thumb, each tick, the comment and Problem melden are saved", {
  dir <- local_ai_dir(cfg = list(enabled = TRUE, models = "m1"))
  local_ability(dir)
  mock_models(list(m1 = function(req) fake_answer(req)))

  testServer(mod_dashboard_server, args = dashboard_args(), {
    session$setInputs(ai_create = 1)
    pump_until(session, function() ai_task$status() == "success")
    session$flushReact()
    id <- ai_latest()$id_feedback

    session$setInputs(ai_rate = list(id = id, helpful = 0L, t = 1))
    expect_equal(ai_rating()$helpful, 0L)
    expect_true(ai_rate_open())
    expect_match(as.character(output$ai_feedback$html), "ai-fb-rate-form", fixed = TRUE)

    # ticks and comment are saved one by one, each keeping the other, and
    # without re-rendering the card (that would wipe a comment being typed)
    version <- ai_rating_version()
    session$setInputs(ai_rate_reasons = list(id = id, reasons = list("too_generic"), t = 2))
    expect_equal(ai_rating()$reasons, "too_generic")
    session$setInputs(ai_rate_comment = list(id = id, comment = "Zu vage.", t = 3))
    session$setInputs(ai_rate_reasons = list(id = id, reasons = list("too_generic", "incorrect"), t = 4))
    expect_setequal(ai_rating()$reasons, c("too_generic", "incorrect"))
    expect_equal(ai_rating()$comment, "Zu vage.")
    expect_equal(ai_rating_version(), version)
    expect_true(ai_rate_open()) # stays open, nothing to send
    # all ticks cleared: an empty list from the browser
    session$setInputs(ai_rate_reasons = list(id = id, reasons = list(), t = 5))
    expect_length(ai_rating()$reasons, 0L)
    expect_equal(ai_rating()$comment, "Zu vage.")
    # a re-render shows what is saved, not what was there when the form opened
    ai_rating_version(ai_rating_version() + 1L)
    session$flushReact()
    expect_match(as.character(output$ai_feedback$html), "Zu vage.", fixed = TRUE)

    # Einklappen folds the form to the thank-you, Ändern opens it again
    session$setInputs(ai_rate_close = list(id = id, t = 7))
    expect_false(ai_rate_open())
    html <- as.character(output$ai_feedback$html)
    expect_match(html, "Danke für deine Rückmeldung!", fixed = TRUE)
    expect_no_match(html, "ai-fb-rate-form", fixed = TRUE)
    session$setInputs(ai_rate_edit = list(id = id, t = 8))
    expect_match(as.character(output$ai_feedback$html), "Zu vage.", fixed = TRUE)

    session$setInputs(ai_report = list(id = id, pos = 2L, reasons = list("link_broken"), comment = "", t = 6))
    expect_equal(ai_reported(), 2L)
    expect_match(as.character(output$ai_feedback$html), "Gemeldet, danke!", fixed = TRUE)
  })
})

test_that("dashboard: a rating from an earlier session is final, the browser can't change it", {
  dir <- local_ai_dir(cfg = list(enabled = TRUE, models = "m1"))
  local_ability(dir)
  mock_models(list(m1 = function(req) fake_answer(req)))
  fb_id <- wait_for(ai_feedback_run(ai_feedback_prepare("u", comp_mixed()), "tok"))$id_feedback
  ai_save_rating("u", fb_id, 1L, reasons = "clear_start")

  testServer(mod_dashboard_server, args = dashboard_args(), {
    html <- as.character(output$ai_feedback$html)
    expect_match(html, "ai-fb-rate is-locked", fixed = TRUE)
    expect_no_match(html, "Ändern", fixed = TRUE)
    # (fb_id, not id: inside testServer, `id` is the module id)
    # whatever the browser sends, nothing changes
    session$setInputs(ai_rate_edit = list(id = fb_id, t = 1))
    expect_false(ai_rate_open())
    session$setInputs(ai_rate = list(id = fb_id, helpful = 0L, t = 2))
    session$setInputs(ai_rate_reasons = list(id = fb_id, reasons = list("incorrect"), t = 3))
    session$setInputs(ai_rate_comment = list(id = fb_id, comment = "geändert", t = 4))
    expect_equal(ai_get_rating("u", fb_id), list(helpful = 1L, reasons = "clear_start", comment = NA_character_))
  })
})

test_that("dashboard: when no model answers, the card says so and nothing is saved", {
  dir <- local_ai_dir(cfg = list(enabled = TRUE, models = "m1"))
  local_ability(dir)
  mock_models(list(m1 = function(req) stop(http_error(503))))

  testServer(mod_dashboard_server, args = dashboard_args(), {
    session$setInputs(ai_create = 1)
    pump_until(session, function() ai_task$status() == "success")
    session$flushReact()
    expect_match(ai_flash()$text, "nicht verfügbar")
    expect_null(ai_latest())
    expect_match(as.character(output$ai_feedback$html), "ai-fb-flash", fixed = TRUE)
    expect_equal(ai_status()$state, "ready") # can try again
  })
})

test_that("dashboard: a click that isn't allowed shows the reason instead of calling", {
  dir <- local_ai_dir(cfg = list(enabled = TRUE, models = "m1"))
  # 3 items per area: below WEAK_AREA_MIN_ITEMS, no focus topic
  path <- make_ability_db(user = "u", computed_at = 1000L, se = setNames(rep(0.4, 7), LEARNING_AREA_LEVELS))
  file.copy(path, file.path(dir, "db_ability.sqlite"), overwrite = TRUE)
  calls <- mock_models(list(m1 = function(req) fake_answer(req)))

  testServer(mod_dashboard_server, args = dashboard_args(), {
    expect_equal(ai_status()$state, "no_data")
    session$setInputs(ai_create = 1)
    expect_match(ai_flash()$text, "mindestens")
    expect_equal(ai_task$status(), "initial")
    expect_length(calls$log, 0L)
  })
})

test_that("dashboard: with the feature off the card shows only the notice", {
  dir <- local_ai_dir(cfg = NULL)
  local_ability(dir)
  testServer(mod_dashboard_server, args = dashboard_args(), {
    expect_equal(ai_status()$state, "off")
    html <- as.character(output$ai_feedback$html)
    expect_match(html, "zurzeit nicht verfügbar", fixed = TRUE)
    expect_false(file.exists(file.path(dir, "db_feedback.sqlite"))) # not even created
  })
})

# ── The two buttons: "Lernstand aktualisieren" and "Feedback erstellen" ───────

test_that("the feedback says which estimate it's based on, and when that changed", {
  fb <- fake_feedback()
  fb$ability_computed_at <- as.integer(as.POSIXct("2026-11-19 10:00:00"))
  html <- card_html(feedback = fb)
  expect_match(html, "Beruht auf deinem Lernstand vom 19. Nov 2026.", fixed = TRUE)
  expect_no_match(html, "seitdem verändert", fixed = TRUE)
  expect_match(card_html(feedback = fb, estimate_newer = TRUE), "Dein Lernstand hat sich seitdem verändert.", fixed = TRUE)
})

test_that("the button says when it updates the estimate first", {
  expect_match(card_html(will_refresh = TRUE), "Dabei wird zuerst dein Lernstand", fixed = TRUE)
  expect_no_match(card_html(will_refresh = FALSE), "Dabei wird zuerst", fixed = TRUE)
  # no button, no note
  expect_no_match(card_html(will_refresh = TRUE, state = "cooldown", notice = "x"), "Dabei wird zuerst", fixed = TRUE)
})

test_that("'Lernstand aktualisieren' is a quiet button with its reason as tooltip", {
  on <- as.character(ability_refresh_button("dash-refresh_ability", TRUE, FALSE, TRUE))
  expect_match(on, "Lernstand aktualisieren", fixed = TRUE)
  expect_match(on, "btn-link", fixed = TRUE)
  expect_no_match(on, "disabled", fixed = TRUE)
  done <- as.character(ability_refresh_button("x", FALSE, TRUE, TRUE))
  expect_match(done, "disabled", fixed = TRUE)
  expect_match(done, "In dieser Sitzung bereits aktualisiert", fixed = TRUE)
  expect_match(as.character(ability_refresh_button("x", FALSE, FALSE, TRUE)), "Keine neuen Antworten", fixed = TRUE)
  expect_match(as.character(ability_refresh_button("x", FALSE, FALSE, FALSE)), "Noch keine Antworten", fixed = TRUE)
})

test_that("dashboard: 'Feedback erstellen' first updates the estimate with new answers", {
  dir <- local_ai_dir(cfg = list(enabled = TRUE, models = "m1"))
  local_ability(dir, computed_at = as.integer(Sys.time()) - 3600L)
  # answers after that estimate (Regression), not in it yet
  file.copy(
    make_user_db(user = "u", item_ids = 1:3, correct = c(TRUE, FALSE, TRUE)),
    file.path(dir, "db_user.sqlite"),
    overwrite = TRUE
  )
  mock_models(list(m1 = function(req) fake_answer(req)))
  args <- dashboard_args()

  testServer(mod_dashboard_server, args = args, {
    expect_true(can_refresh_ability())
    before <- max(competency()$computed_at)
    expect_match(as.character(output$ai_feedback$html), "Dabei wird zuerst dein Lernstand", fixed = TRUE)

    session$setInputs(ai_create = 1)
    pump_until(session, function() ai_task$status() == "success")
    session$flushReact()

    # the estimate was updated (it counts as the session's one update) ...
    expect_true(ability_computed_this_session())
    expect_false(can_refresh_ability())
    after <- max(competency()$computed_at)
    expect_gt(after, before)
    # ... and the feedback is based on the updated one
    expect_equal(ai_latest()$ability_computed_at, after)
    expect_no_match(as.character(output$ai_feedback$html), "seitdem verändert", fixed = TRUE)
  })
})
