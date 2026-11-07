# ── competency_label ──────────────────────────────────────────────────────────

test_that("competency_label returns 'Keine Daten' when n_unique is 0", {
  cl <- competency_label(theta = 2.0, n_unique = 0L)
  expect_equal(cl$label, "Keine Daten")
  expect_equal(cl$level, 0L)
})

test_that("competency_label returns 'Keine Daten' when theta is NA", {
  cl <- competency_label(theta = NA_real_, n_unique = 5L)
  expect_equal(cl$label, "Keine Daten")
})

test_that("competency_label maps theta to every COMPETENCY_SCALE label", {
  mids <- c(-2, -0.5, 0, 0.75, 1.25, 1.75, 2.5)
  labels <- vapply(mids, function(t) competency_label(t, 5L)$label, character(1))
  expect_equal(labels, COMPETENCY_SCALE$label)
})

test_that("competency_label levels are strictly ordered", {
  lvl <- function(theta) competency_label(theta, n_unique = 5L)$level
  expect_true(all(diff(vapply(c(-3, -0.5, 0, 0.75, 1.25, 1.75, 3), lvl, integer(1))) > 0))
})

test_that("competency_label intervals are left-closed [lower, upper)", {
  inner <- COMPETENCY_SCALE$lower[-1]
  got <- vapply(inner, function(t) competency_label(t, 5L)$label, character(1))
  expect_equal(got, COMPETENCY_SCALE$label[-1])
})

test_that("competency_label covers the full estimate_theta range [-3, 3]", {
  expect_equal(competency_label(-3, 5L)$label, COMPETENCY_SCALE$label[1])
  expect_equal(competency_label(3, 5L)$label, COMPETENCY_SCALE$label[nrow(COMPETENCY_SCALE)])
})

test_that("COMPETENCY_SCALE is ordered and contiguous", {
  sc <- COMPETENCY_SCALE
  expect_true(all(diff(sc$lower) > 0))
  expect_equal(sc$upper[-nrow(sc)], sc$lower[-1])
  expect_equal(sc$lower[1], -Inf)
  expect_equal(sc$upper[nrow(sc)], Inf)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", sc$color_hex)))
})

test_that("competency_scale_bar fills segments up to the level and marks the current one", {
  html <- as.character(competency_scale_bar(competency_label(0.75, 5L)))
  n_seg <- nrow(COMPETENCY_SCALE)
  expect_equal(lengths(regmatches(html, gregexpr("comp-seg", html))), n_seg)
  expect_equal(lengths(regmatches(html, gregexpr("is-filled", html))), 4L)
  expect_equal(lengths(regmatches(html, gregexpr("is-current", html))), 1L)
  expect_match(html, COMPETENCY_SCALE$color_hex[4], fixed = TRUE)
  expect_no_match(html, COMPETENCY_SCALE$color_hex[5], fixed = TRUE)
})

test_that("competency_scale_bar shows an empty scale when there is no data", {
  html <- as.character(competency_scale_bar(competency_label(NA_real_, 0L)))
  expect_match(html, "is-empty", fixed = TRUE)
  expect_no_match(html, "is-filled", fixed = TRUE)
})

test_that("format_date_de uses German month abbreviations regardless of locale", {
  expect_equal(format_date_de(as.Date("2026-08-24")), "24. Aug 2026")
  expect_equal(format_date_de(as.Date("2026-03-05")), "5. Mär 2026")
  expect_equal(format_date_de(as.Date("2026-12-31")), "31. Dez 2026")
})

# ── evidence_label ────────────────────────────────────────────────────────────

test_that("evidence_label maps n_unique to correct tiers", {
  expect_equal(evidence_label(0L)$label, "Keine")
  expect_equal(evidence_label(NA_integer_)$dots, 0L)
  expect_equal(evidence_label(1L)$label, "Niedrig")
  expect_equal(evidence_label(2L)$label, "Niedrig")
  expect_equal(evidence_label(3L)$label, "Mittel")
  expect_equal(evidence_label(7L)$label, "Mittel")
  expect_equal(evidence_label(8L)$label, "Hoch")
  expect_equal(evidence_label(100L)$label, "Hoch")
})

test_that("evidence_label dot count matches tier", {
  expect_equal(evidence_label(0L)$dots, 0L)
  expect_equal(evidence_label(1L)$dots, 1L)
  expect_equal(evidence_label(3L)$dots, 2L)
  expect_equal(evidence_label(8L)$dots, 3L)
})

# ── ability_trajectory_data ───────────────────────────────────────────────────

test_that("ability_trajectory_data drops rows with NA theta", {
  ab <- data.frame(
    computed_at = c(1000L, 1000L, 2000L, 2000L),
    learning_area = rep(c(LEARNING_AREA_LEVELS[1], LEARNING_AREA_LEVELS[2]), 2),
    theta = c(0.5, NA, 0.7, NA),
    n_items = c(5L, 0L, 6L, 0L),
    stringsAsFactors = FALSE
  )
  result <- ability_trajectory_data(ab)
  expect_equal(nrow(result), 2L)
  expect_true(all(!is.na(result$theta)))
})

test_that("ability_trajectory_data adds time and date columns", {
  ab <- data.frame(
    computed_at = c(1700000000L, 1700000000L),
    learning_area = c(LEARNING_AREA_LEVELS[3], LEARNING_AREA_LEVELS[1]),
    theta = c(0.1, 0.2),
    n_items = c(4L, 5L),
    stringsAsFactors = FALSE
  )
  result <- ability_trajectory_data(ab)
  expect_s3_class(result$time, "POSIXct")
  expect_s3_class(result$date, "Date")
  expect_equal(as.numeric(result$time), c(1700000000, 1700000000))
})

test_that("ability_trajectory_data handles an all-NA input", {
  ab <- data.frame(
    computed_at = 1000L,
    learning_area = LEARNING_AREA_LEVELS[1],
    theta = NA_real_,
    n_items = 0L,
    stringsAsFactors = FALSE
  )
  result <- ability_trajectory_data(ab)
  expect_equal(nrow(result), 0L)
})

# ── trajectory small multiples ────────────────────────────────────────────────

traj_hist <- function(times = c(1700000000, 1700086400), areas = LEARNING_AREA_LEVELS[1], theta = 0.3) {
  ab <- expand.grid(computed_at = as.integer(times), learning_area = areas, stringsAsFactors = FALSE)
  ab$theta <- rep_len(theta, nrow(ab))
  ab$n_items <- 3L
  ability_trajectory_data(ab)
}

test_that("competency_band_shapes fills the panel with one contiguous band per level", {
  bands <- competency_band_shapes()
  expect_length(bands, nrow(COMPETENCY_SCALE))
  y0 <- vapply(bands, `[[`, numeric(1), "y0")
  y1 <- vapply(bands, `[[`, numeric(1), "y1")
  expect_equal(y0[1], TRAJECTORY_Y_RANGE[1])
  expect_equal(y1[length(y1)], TRAJECTORY_Y_RANGE[2])
  expect_true(all(TRAJECTORY_Y_RANGE[1] < THETA_RANGE[1], TRAJECTORY_Y_RANGE[2] > THETA_RANGE[2]))
  expect_equal(y0[-1], y1[-length(y1)])
  expect_equal(vapply(bands, `[[`, character(1), "fillcolor"), COMPETENCY_SCALE$color_hex)
})

test_that("trajectory_x_range pads the time range, also for a single day", {
  h <- traj_hist()
  r <- trajectory_x_range(h)
  expect_true(r[1] < min(h$time) && r[2] > max(h$time))
  same_day <- traj_hist(times = c(1700000000, 1700003600))
  r2 <- trajectory_x_range(same_day)
  expect_gte(as.numeric(diff(r2), units = "hours"), 24)
})

# a spec as the browser gets it: through JSON
spec_json <- function(spec) jsonlite::fromJSON(trajectory_spec_json(spec), simplifyVector = FALSE)

test_that("trajectory_panel_spec plots the saved estimates with the competency label on hover", {
  h <- traj_hist(theta = c(-0.5, 1.2))
  f <- spec_json(trajectory_panel_spec(h, trajectory_x_range(h)))
  markers <- Filter(function(tr) identical(tr$mode, "markers"), f$data)[[1]]
  expect_equal(unlist(markers$y), c(-0.5, 1.2))
  line <- Filter(function(tr) identical(tr$mode, "lines"), f$data)[[1]]
  expect_equal(unlist(line$y), c(-0.5, 1.2)) # not smoothed
  txt <- unlist(markers$text)
  expect_match(txt[1], "Basis")
  expect_match(txt[2], "Fortgeschritten")
  expect_match(txt[2], "1.20", fixed = TRUE)
  expect_length(f$layout$shapes, nrow(COMPETENCY_SCALE))
  expect_equal(unlist(markers$x), format(h$time, "%Y-%m-%d %H:%M:%S"))
  expect_true(f$layout$xaxis$automargin) # else the date labels are cut off
})

test_that("trajectory_panel_spec shows how many items each estimate is based on", {
  h <- traj_hist(theta = c(-0.5, 1.2))
  h$n_items <- c(1L, 12L)
  txt <- unlist(spec_json(trajectory_panel_spec(h, trajectory_x_range(h)))$data[[2]]$text)
  expect_match(txt[1], "Basierend auf 1 Aufgabe", fixed = TRUE)
  expect_false(grepl("1 Aufgaben", txt[1], fixed = TRUE))
  expect_match(txt[2], "Basierend auf 12 Aufgaben", fixed = TRUE)
  h$n_items <- NA_integer_
  txt <- unlist(spec_json(trajectory_panel_spec(h, trajectory_x_range(h)))$data[[2]]$text)
  expect_false(any(grepl("Basierend", txt, fixed = TRUE)))
})

test_that("trajectory_panel_spec sends arrays also for a single estimate", {
  h <- traj_hist(times = 1700000000, theta = 0.3)
  json <- trajectory_spec_json(trajectory_panel_spec(h, trajectory_x_range(h)))
  # arrays, not plain values: otherwise %{text} shows placeholders
  expect_match(json, '"text":["', fixed = TRUE)
  expect_match(json, '"y":[0.3]', fixed = TRUE)
  expect_match(json, "Solide", fixed = TRUE)
})

test_that("trajectory_spec_json can't end its script element", {
  json <- trajectory_spec_json(trajectory_panel_spec(traj_hist(), trajectory_x_range(traj_hist())))
  expect_false(grepl("</", json, fixed = TRUE))
  expect_match(spec_json(trajectory_panel_spec(traj_hist(), trajectory_x_range(traj_hist())))$data[[2]]$text[[1]], "</b>", fixed = TRUE)
})

test_that("trajectory_section asks for a second estimate before showing panels", {
  one <- traj_hist(times = 1700000000)
  html <- as.character(trajectory_section(one, shiny::NS("d")))
  expect_match(html, "mindestens zwei", fixed = TRUE)
  expect_no_match(html, "traj-panel", fixed = TRUE)
})

test_that("trajectory_section shows one panel per area, charts only where there is data", {
  h <- traj_hist(areas = LEARNING_AREA_LEVELS[c(1, 3)])
  html <- as.character(trajectory_section(h, shiny::NS("d")))
  expect_equal(lengths(regmatches(html, gregexpr('class="traj-panel"', html))), length(LEARNING_AREA_LEVELS))
  expect_match(html, 'id="d-traj_1"', fixed = TRUE)
  expect_match(html, 'id="d-traj_3"', fixed = TRUE)
  expect_no_match(html, 'id="d-traj_2"', fixed = TRUE)
  expect_equal(
    lengths(regmatches(html, gregexpr("Noch keine Schätzung", html, fixed = TRUE))),
    length(LEARNING_AREA_LEVELS) - 2L
  )
})

test_that("trajectory_section puts the legend in the grid after the panels, plus a top variant", {
  html <- as.character(trajectory_section(traj_hist(), shiny::NS("d")))
  expect_match(html, "traj-legend traj-legend-top", fixed = TRUE)
  last_panel <- max(gregexpr('class="traj-panel"', html, fixed = TRUE)[[1]])
  expect_gt(regexpr("traj-legend-panel", html, fixed = TRUE), last_panel)
  # Highest level first, like the bands in the charts
  legend <- regmatches(html, regexpr("traj-legend-panel.*", html))
  pos <- vapply(COMPETENCY_SCALE$label, function(l) regexpr(l, legend, fixed = TRUE), integer(1))
  expect_true(all(diff(pos) < 0))
})

# ── certainty (label probability → dots) ──────────────────────────────────────

test_that("certainty_label maps probabilities to dots at the 1/3 and 2/3 cutoffs", {
  expect_equal(certainty_label(0.9)$dots, 3L)
  expect_equal(certainty_label(CERTAINTY_HIGH)$dots, 3L)
  expect_equal(certainty_label(0.5)$dots, 2L)
  expect_equal(certainty_label(CERTAINTY_MED)$dots, 2L)
  expect_equal(certainty_label(0.1)$dots, 1L)
})

test_that("label_certainty is P(theta in the assigned label's interval)", {
  cl <- competency_label(0.125, 5L) # Solide [-0.25, 0.5)
  expect_equal(label_certainty(0.125, 0.5, cl), prob_in_interval(0.125, 0.5, -0.25, 0.5))
  expect_true(is.na(label_certainty(0.125, NA_real_, cl)))
  expect_true(is.na(label_certainty(NA_real_, 0.5, competency_label(NA_real_, 0L))))
})

test_that("label_certainty clips the open-ended outer labels to THETA_RANGE", {
  # 4 items all wrong: theta stuck at the lower bound with a huge SE
  cl <- competency_label(-3, 4L)
  expect_equal(cl$label, "Aufbau")
  p <- label_certainty(-3, 2.46, cl)
  expect_equal(p, prob_in_interval(-3, 2.46, -3, -1))
  expect_lt(p, CERTAINTY_MED) # "unsicher", not "wahrscheinlich"
  # with real precision the outer label can still be likely
  expect_gt(label_certainty(-1.5, 0.4, competency_label(-1.5, 25L)), CERTAINTY_HIGH)
})

test_that("certainty_dots uses the probability when an SE is available", {
  cl <- competency_label(0.125, 20L)
  html <- as.character(certainty_dots(0.125, 0.2, 20L, cl)) # P ~ 0.94
  expect_equal(lengths(regmatches(html, gregexpr("is-filled", html))), 3L)
  expect_match(html, "Solide", fixed = TRUE)
  expect_match(html, "etwa 95 %", fixed = TRUE)
})

test_that("certainty_dots falls back to the item count for snapshots without an SE", {
  cl <- competency_label(0.125, 9L)
  html <- as.character(certainty_dots(0.125, NA_real_, 9L, cl))
  expect_equal(lengths(regmatches(html, gregexpr("is-filled", html))), 3L)
  expect_match(html, "Evidenz: Hoch", fixed = TRUE)
})

test_that("format_prob_de rounds to 5 % steps and clamps the extremes", {
  expect_equal(format_prob_de(0.72), "etwa 70 %")
  expect_equal(format_prob_de(0.01), "unter 5 %")
  expect_equal(format_prob_de(0.99), "über 95 %")
})

# ── ai_feedback_card ──────────────────────────────────────────────────────────

test_that("ai_feedback_card shows the placeholder when there is no feedback", {
  html <- as.character(ai_feedback_card(NULL))
  expect_match(html, "Bald verfügbar", fixed = TRUE)
  expect_match(html, "ai-feedback-skeleton", fixed = TRUE)
})

test_that("ai_feedback_card renders Markdown feedback instead of the placeholder", {
  html <- as.character(ai_feedback_card("Du bist **stark** in Regression."))
  expect_match(html, "<strong>stark</strong>", fixed = TRUE)
  expect_no_match(html, "ai-feedback-skeleton", fixed = TRUE)
  expect_no_match(html, "Bald verfügbar", fixed = TRUE)
})

# ── answer tree ───────────────────────────────────────────────────────────────

tree_ud <- function(id_item, bool_correct, skipped = rep(FALSE, length(id_item))) {
  data.frame(
    id_item = id_item, bool_correct = bool_correct, skipped = skipped,
    id_datetime = seq_along(id_item)
  )
}

test_that("answer_tree_stats splits the pool attempt by attempt", {
  pool <- data.frame(id_item = 1:6)
  ud <- tree_ud(
    id_item      = c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L),
    bool_correct = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  tr <- answer_tree_stats(ud, pool, depth = 3L)
  n <- function(node) vapply(node$children, `[[`, integer(1), "n")
  expect_equal(tr$n, 6L)
  # 1st attempt: 1, 4 right; 2, 3 wrong; 5, 6 not worked on
  expect_equal(unname(n(tr)), c(2L, 2L, 2L))
  # right first: 4 regresses to wrong, 1 not repeated
  expect_equal(unname(n(tr$children$correct)), c(0L, 1L, 1L))
  # wrong first: 2 right at the 2nd attempt, 3 wrong again ...
  expect_equal(unname(n(tr$children$wrong)), c(1L, 1L, 0L))
  # ... and right at the 3rd
  expect_equal(unname(n(tr$children$wrong$children$wrong)), c(1L, 0L, 0L))
  # "none" nodes and the last attempt's nodes are leaves
  expect_null(tr$children$none$children)
  expect_null(tr$children$wrong$children$wrong$children$correct$children)
})

test_that("the answer tree goes ANSWER_TREE_DEPTH attempts deep by default", {
  expect_equal(ANSWER_TREE_DEPTH, 4L)
  ud <- tree_ud(id_item = rep(1L, 4), bool_correct = c(FALSE, FALSE, FALSE, TRUE))
  tr <- answer_tree_stats(ud, data.frame(id_item = 1L))
  leaf <- tr$children$wrong$children$wrong$children$wrong$children$correct
  expect_equal(leaf$n, 1L)
  expect_null(leaf$children)
  html <- as.character(answer_tree_card(tr))
  expect_match(html, "--cols:5;", fixed = TRUE)
  expect_match(html, "4. Versuch", fixed = TRUE)
})

test_that("answer tree: grey ring on nodes nothing follows, none on leaves of the last attempt", {
  ud <- tree_ud(id_item = c(1L, 2L), bool_correct = c(TRUE, FALSE))
  html <- as.character(answer_tree_card(answer_tree_stats(ud, data.frame(id_item = 1:3), depth = 1L)))
  # root only (its children are the last attempt) + the "nicht bearbeitet" node
  expect_equal(lengths(regmatches(html, gregexpr("lt-donut", html, fixed = TRUE))), 2L)
  expect_match(html, "--a:0.0%;--b:0.0%;", fixed = TRUE) # all grey
})

test_that("answer_tree_stats ignores rows without a result and items outside the pool", {
  pool <- data.frame(id_item = 1:3)
  ud <- tree_ud(
    id_item      = c(1L, 1L, 2L, 99L),
    bool_correct = c(NA, FALSE, NA, TRUE),
    skipped      = c(TRUE, FALSE, FALSE, FALSE)
  )
  tr <- answer_tree_stats(ud, pool)
  # item 1: skip then wrong -> wrong at the 1st attempt, not repeated; item 2
  # only a row without a result -> not worked on; 99 isn't in the pool
  expect_equal(unname(vapply(tr$children, `[[`, integer(1), "n")), c(0L, 1L, 2L))
  expect_equal(tr$children$wrong$children$none$n, 1L)
})

test_that("answer_tree_card starts with every branch closed and only makes non-empty nodes clickable", {
  pool <- data.frame(id_item = 1:3)
  ud <- tree_ud(id_item = c(1L, 1L, 2L), bool_correct = c(FALSE, TRUE, FALSE))
  html <- as.character(answer_tree_card(answer_tree_stats(ud, pool)))
  # wrong at the 1st attempt and right at the 2nd are clickable, both closed;
  # the right-at-1st node is empty, so not a button
  expect_equal(lengths(regmatches(html, gregexpr("data-node", html, fixed = TRUE))), 2L)
  expect_equal(lengths(regmatches(html, gregexpr('aria-expanded="true"', html, fixed = TRUE))), 0L)
  expect_equal(lengths(regmatches(html, gregexpr('<li class="is-open">', html, fixed = TRUE))), 1L) # root
  expect_match(html, "nicht bearbeitet", fixed = TRUE)
  expect_false(grepl("noch nicht bearbeitet", html, fixed = TRUE))
  expect_match(html, "nicht wiederholt", fixed = TRUE)
})

test_that("answer_tree_stats summarizes the first-wrong items over all later attempts", {
  pool <- data.frame(id_item = 1:4)
  ud <- tree_ud(
    # 1: wrong, never retried; 2: wrong x3, right at the 4th (beyond depth 3);
    # 3: wrong, wrong; 4: right
    id_item      = c(1L, 2L, 2L, 2L, 2L, 3L, 3L, 4L),
    bool_correct = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
  m <- answer_tree_stats(ud, pool)$mistakes
  expect_equal(m, list(n_wrong = 3L, n_retried = 2L, n_solved = 1L, n_open = 2L, n_later_day = 0L))
})

test_that("answer_tree_takeaway puts the tree's message into one sentence", {
  tk <- function(id_item, bool_correct, pool = 1:5) {
    answer_tree_takeaway(answer_tree_stats(tree_ud(id_item, bool_correct), data.frame(id_item = pool)))
  }
  expect_match(tk(integer(0), logical(0)), "Sobald du Aufgaben bearbeitest")
  expect_match(tk(1L, TRUE), "keine Aufgabe beim ersten Versuch falsch")
  expect_equal(
    tk(c(1L, 2L), c(FALSE, FALSE)),
    "Von 2 Aufgaben, die du beim ersten Versuch falsch hattest, hast du noch keine erneut versucht."
  )
  expect_equal(
    tk(c(1L, 1L, 2L, 2L, 3L), c(FALSE, TRUE, FALSE, TRUE, FALSE)),
    "Von 3 Aufgaben, die du beim ersten Versuch falsch hattest, hast du 2 noch einmal versucht (am selben Tag) und alle 2 dann gelöst. 1 wartet noch auf dich."
  )
  expect_equal(
    tk(c(1L, 1L), c(FALSE, FALSE)),
    "Von 1 Aufgabe, die du beim ersten Versuch falsch hattest, hast du 1 noch einmal versucht (am selben Tag), aber noch keine davon gelöst. 1 wartet noch auf dich."
  )
})

test_that("answer_tree_stats backward reads each item from its latest answer", {
  pool <- data.frame(id_item = 1:5)
  ud <- tree_ud(
    # 1: right; 2: wrong, then right (learned); 3: right, then wrong
    # (regressed); 4: wrong, wrong; 5: not worked on
    id_item      = c(1L, 2L, 2L, 3L, 3L, 4L, 4L),
    bool_correct = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  tr <- answer_tree_stats(ud, pool, direction = "backward")
  n <- function(node) unname(vapply(node$children, `[[`, integer(1), "n"))
  expect_equal(tr$direction, "backward")
  # latest answer: 1, 2 right; 3, 4 wrong; 5 not worked on
  expect_equal(n(tr), c(2L, 2L, 1L))
  # before a right latest answer: 2 was wrong, 1 had no earlier attempt
  expect_equal(n(tr$children$correct), c(0L, 1L, 1L))
  # before a wrong latest answer: 3 was right, 4 wrong
  expect_equal(n(tr$children$wrong), c(1L, 1L, 0L))
  expect_equal(tr$latest, list(n_correct = 2L, n_wrong = 2L, n_learned = 1L, n_regressed = 1L))
  expect_null(tr$mistakes)
  expect_equal(
    answer_tree_takeaway(tr),
    "Deine letzte Antwort war bei 2 Aufgaben richtig (1 davon nach einem früheren Fehler) und bei 2 falsch (1 davon hattest du früher schon einmal richtig)."
  )
  tk <- function(id, ok) answer_tree_takeaway(answer_tree_stats(tree_ud(id, ok), pool, direction = "backward"))
  expect_match(tk(integer(0), logical(0)), "Sobald du Aufgaben bearbeitest")
  expect_equal(tk(1L, TRUE), "Deine letzte Antwort war bei 1 Aufgabe richtig und bei keiner falsch.")
  expect_equal(tk(1L, FALSE), "Deine letzte Antwort war bei keiner Aufgabe richtig und bei 1 falsch.")
})

test_that("answer_tree_card with a backward tree adds the toggle and both views", {
  pool <- data.frame(id_item = 1:3)
  ud <- tree_ud(id_item = c(1L, 1L, 2L), bool_correct = c(FALSE, TRUE, FALSE))
  html <- as.character(answer_tree_card(
    answer_tree_stats(ud, pool),
    backward = answer_tree_stats(ud, pool, direction = "backward")
  ))
  expect_match(html, 'class="la-card lt-card" data-view="forward"', fixed = TRUE)
  expect_match(html, 'data-lt-view="forward" aria-pressed="true"', fixed = TRUE)
  expect_match(html, 'data-lt-view="backward" aria-pressed="false"', fixed = TRUE)
  expect_match(html, 'class="lt-view" data-view="backward"', fixed = TRUE)
  expect_match(html, "Vorletzter", fixed = TRUE)
  expect_match(html, "erster Versuch", fixed = TRUE)
  # without a backward tree: no toggle
  expect_false(grepl("data-lt-view", as.character(answer_tree_card(answer_tree_stats(ud, pool))), fixed = TRUE))
})

test_that("answer_tree_stats counts retries on a later day than the first attempt", {
  day <- 86400
  ud <- data.frame(
    id_item = c(1L, 1L, 2L, 2L, 3L, 3L),
    bool_correct = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
    skipped = FALSE,
    # 1 retried the same day, 2 and 3 two days later
    id_datetime = c(10, 20, 30, 2 * day + 30, 40, 2 * day + 40)
  )
  m <- answer_tree_stats(ud, data.frame(id_item = 1:3))$mistakes
  expect_equal(m$n_later_day, 2L)
  expect_match(answer_tree_takeaway(answer_tree_stats(ud, data.frame(id_item = 1:3))), "(2 davon an einem späteren Tag)", fixed = TRUE)
})

# ── Lernverhalten: shared "answered" definition, summary, coverage ───────────

test_that("answered_rows keeps only real answers to items in the pool", {
  ud <- tree_ud(
    id_item      = c(1L, 1L, 2L, 3L, 99L),
    bool_correct = c(NA, TRUE, NA, FALSE, TRUE),
    skipped      = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  a <- answered_rows(ud, data.frame(id_item = 1:3))
  # the skip, the row without a result (item 2) and item 99 are dropped
  expect_equal(a$id_item, c(1L, 3L))
  expect_s3_class(a$date, "Date")
  expect_equal(nrow(answered_rows(data.frame())), 0L)
})

test_that("behaviour_stats reports items, days and the last practice day", {
  day <- 86400
  pool <- data.frame(id_item = 1:4)
  ud <- data.frame(
    id_item = c(1L, 2L, 2L, 3L, 4L),
    bool_correct = c(TRUE, FALSE, TRUE, FALSE, NA),
    skipped = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    id_datetime = c(10, 20, 3 * day, 3 * day + 10, 3 * day + 20)
  )
  st <- behaviour_stats(ud, pool, today = as.Date("1970-01-04"))
  # 3 answered items (the skipped item 4 doesn't count), on 2 days; the last
  # day (Jan 4) has 2 answers (item 2's retry, item 3), 1 right
  expect_equal(st[c("n_items", "n_total", "n_days", "last_n", "last_correct")],
               list(n_items = 3L, n_total = 4L, n_days = 2L, last_n = 2L, last_correct = 1L))
  expect_equal(st$last_date, as.Date("1970-01-04"))
  expect_null(behaviour_stats(data.frame(), pool))
})

test_that("behaviour_tiles shows three tiles, or a sentence before the first answer", {
  st <- list(n_items = 1L, n_total = 4L, n_days = 2L, n_week = 1L,
             last_date = as.Date("2026-10-27"), last_n = 1L, last_correct = 1L)
  html <- as.character(behaviour_tiles(st, today = as.Date("2026-10-28")))
  expect_equal(lengths(regmatches(html, gregexpr('class="la-tile"', html, fixed = TRUE))), 3L)
  expect_match(html, "Aufgabe beantwortet", fixed = TRUE)
  expect_match(html, "27. Okt<", fixed = TRUE)
  expect_match(html, "1 von 1 richtig", fixed = TRUE)
  expect_match(as.character(behaviour_tiles(NULL)), "noch keine Aufgaben beantwortet")
})

test_that("format_date_short adds the year only for other years", {
  expect_equal(format_date_short(as.Date("2026-03-05"), today = as.Date("2026-10-28")), "5. Mär")
  expect_equal(format_date_short(as.Date("2025-12-01"), today = as.Date("2026-10-28")), "1. Dez 2025")
})

test_that("coverage_matrix splits each area x type cell by the latest answer", {
  pool <- data.frame(
    id_item = 1:4,
    learning_area = c("Regression", "Regression", "Regression", "Poweranalyse"),
    type_item = c("content", "content", "coding", "content")
  )
  ud <- tree_ud(
    id_item      = c(1L, 2L, 2L, 3L),
    bool_correct = c(TRUE, TRUE, FALSE, NA),
    skipped      = c(FALSE, FALSE, FALSE, TRUE)
  )
  cov <- coverage_matrix(ud, pool)
  expect_equal(nrow(cov), length(LEARNING_AREA_LEVELS) * length(ITEM_TYPE_LABELS))
  cell <- function(area, type) unlist(cov[cov$learning_area == area & cov$type == type, c("n_total", "n_correct", "n_wrong")])
  # item 2's latest answer is wrong; item 3 was only skipped -> still open
  expect_equal(unname(cell("Regression", "content")), c(2L, 1L, 1L))
  expect_equal(unname(cell("Regression", "coding")), c(1L, 0L, 0L))
  expect_equal(unname(cell("Poweranalyse", "content")), c(1L, 0L, 0L))
  expect_equal(unname(cell("Deskriptivstatistik", "coding")), c(0L, 0L, 0L))
})


test_that("open_items lists items whose latest answer is wrong, newest first", {
  pool <- data.frame(
    id_item = 1:6,
    learning_area = c("Regression", "Regression", "Poweranalyse", "Regression", "Regression", "Regression"),
    type_item = c("content", "coding", "content", "content", "content", "content")
  )
  ud <- tree_ud(
    # 1: wrong, then right -> solved; 2: wrong (earlier); 3: wrong twice
    # (latest answer last); 4: right; 5: only skipped; 6: right, then wrong
    # (regressed -> open again), then skipped (doesn't count)
    id_item      = c(1L, 1L, 2L, 6L, 3L, 4L, 6L, 3L, 5L, 6L),
    bool_correct = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, NA, NA),
    skipped      = c(rep(FALSE, 8), TRUE, TRUE)
  )
  oi <- open_items(ud, pool)
  expect_equal(oi$id_item, c(3L, 6L, 2L))
  expect_equal(oi$type_item, c("content", "content", "coding"))
  # same count as the coverage card's "zuletzt falsch"
  expect_equal(nrow(oi), sum(coverage_matrix(ud, pool)$n_wrong))
  expect_equal(nrow(open_items(tree_ud(1L, TRUE), pool)), 0L)
  # wrong, then right again -> solved
  expect_equal(nrow(open_items(tree_ud(c(1L, 1L, 1L), c(TRUE, FALSE, TRUE)), pool)), 0L)
})

test_that("calendar_month_labels marks the week of each month's 1st", {
  today <- as.Date("2026-11-02") # a Monday
  days <- seq(as.Date("2026-09-14"), as.Date("2026-11-08"), by = "day") # 8 weeks
  # 1 Oct is in the week of 28 Sep, 1 Nov (a Sunday) in the week of 26 Oct;
  # the top row names its month since the next row has no label
  expect_equal(calendar_month_labels(days, today), c("Sep", "", "Okt", "", "", "", "Nov", ""))
  # the next row is labelled: no top label (they'd collide)
  expect_equal(calendar_month_labels(seq(as.Date("2026-09-21"), as.Date("2026-10-11"), by = "day"), today), c("", "Okt", ""))
  # a 1st that is still in the future isn't labelled
  expect_equal(calendar_month_labels(seq(as.Date("2026-11-23"), as.Date("2026-12-06"), by = "day"), as.Date("2026-11-30")), c("Nov", ""))
})

test_that("calendar_card has one row per week and a phone label for the 8th-last week", {
  today <- as.Date("2026-11-02")
  ud <- data.frame(
    id_item = 1:2, bool_correct = c(TRUE, FALSE), skipped = FALSE,
    id_datetime = as.numeric(as.POSIXct(c("2026-08-12 10:00", "2026-11-02 10:00")))
  )
  html <- as.character(calendar_card(ud, today))
  # 10 Aug .. 8 Nov = 13 weeks
  expect_equal(lengths(regmatches(html, gregexpr('class="cal-week"', html))), 13L)
  # the 8th-last week (14 Sep) has no own label -> phone fallback "Sep"
  expect_match(html, 'data-alt="Sep"')
})

test_that("retry_eligible applies the time-out of RETRY_MIN_DAYS calendar days", {
  today <- as.Date("2026-10-28")
  oi <- data.frame(id_item = 1:3, last_date = today - c(0L, 1L, 5L))
  expect_equal(retry_eligible(oi, today)$id_item, which(c(0L, 1L, 5L) >= RETRY_MIN_DAYS))
})

test_that("open_items_panel offers the retry only for mistakes past their time-out", {
  today <- as.Date("2026-10-28")
  oi <- function(days_ago) data.frame(
    id_item = seq_along(days_ago), learning_area = rep("Regression", length(days_ago)),
    type_item = rep("content", length(days_ago)), last_date = today - days_ago
  )
  ns <- shiny::NS("dashboard_1")
  # 12 old mistakes -> capped button
  html <- as.character(open_items_panel(oi(rep(3L, 12)), ns, today))
  expect_match(html, "dashboard_1-retry_open", fixed = TRUE)
  expect_match(html, sprintf("%d davon nochmal üben", RETRY_MAX_ITEMS), fixed = TRUE)
  # 2 old + 1 from today -> "Alle 2" plus the waiting note
  html <- as.character(open_items_panel(oi(c(2L, 4L, 0L)), ns, today))
  expect_match(html, "Alle 2 nochmal üben", fixed = TRUE)
  expect_match(html, "1 von heute erst ab morgen", fixed = TRUE)
  # only mistakes from today -> no button at all
  html <- as.character(open_items_panel(oi(c(0L, 0L)), ns, today))
  expect_false(grepl("retry_open", html, fixed = TRUE))
  expect_match(html, "Alle von heute", fixed = TRUE)
  # no item list or solution lookup
  expect_false(grepl("oi-item", html, fixed = TRUE))
  expect_match(as.character(open_items_panel(oi(integer(0)), ns, today)), "überall richtig")
})

test_that("open_items_panel shows the open count big and the fixed mistakes", {
  today <- as.Date("2026-10-28")
  oi <- data.frame(id_item = 1:3, learning_area = "Regression", type_item = "content", last_date = today - 3L)
  ns <- shiny::NS("d")
  html <- as.character(open_items_panel(oi, ns, today, fixed = 2L))
  expect_match(html, '<span class="oi-big-val">3</span>', fixed = TRUE)
  expect_match(html, "zuletzt falsch", fixed = TRUE)
  expect_match(html, "Schon ausgebessert: 2 Aufgaben", fixed = TRUE)
  expect_match(as.character(open_items_panel(oi, ns, today, fixed = 1L)), "1 Aufgabe nach", fixed = TRUE)
  expect_false(grepl("ausgebessert", as.character(open_items_panel(oi, ns, today)), fixed = TRUE))
  expect_match(as.character(open_items_panel(oi[0, ], ns, today)), "oi-hero is-clear", fixed = TRUE)
})

test_that("fixed_mistakes counts items answered right after an earlier mistake", {
  pool <- data.frame(id_item = 1:4)
  ud <- tree_ud(
    id_item = c(1L, 1L, 2L, 2L, 3L, 4L, 4L),
    bool_correct = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
  )
  # only item 1: wrong, then right (2 regressed, 3 right at once, 4 still wrong)
  expect_equal(fixed_mistakes(ud, pool), 1L)
  expect_equal(fixed_mistakes(ud[0, ], pool), 0L)
})

test_that("the detail section labels the interval as a confidence interval and explains it in four steps", {
  html <- as.character(detail_section(NULL))
  expect_match(html, "Konfidenzintervall", fixed = TRUE)
  # the explainer is closed by default, opened by the button in the title
  expect_match(html, 'data-explain="closed"', fixed = TRUE)
  expect_match(html, 'data-explain-toggle="" aria-expanded="false" aria-controls="la-explainer"', fixed = TRUE)
  expect_false(grepl("Wahrscheinlichkeit liegt", html, fixed = TRUE))
  ex <- as.character(explainer_card())
  expect_match(ex, '^<div id="la-explainer" class="la-card la-explainer">')
  expect_equal(lengths(regmatches(ex, gregexpr('class="la-step"', ex))), 4L)
  expect_match(ex, "gemessen an der Schwierigkeit", fixed = TRUE)
})

# ── latest_ability / time windows ─────────────────────────────────────────────

test_that("latest_ability takes each area's latest row, not the latest batch", {
  ab <- data.frame(
    computed_at = c(100L, 100L, 200L),
    learning_area = LEARNING_AREA_LEVELS[c(1, 2, 2)],
    theta = c(0.5, -1, 1.5), se = c(0.4, 0.6, 0.3), n_items = c(4L, 2L, 6L)
  )
  comp <- latest_ability(ab)
  expect_equal(nrow(comp), length(LEARNING_AREA_LEVELS))
  expect_equal(comp$theta[1:2], c(0.5, 1.5)) # area 1 kept from the older batch
  expect_equal(comp$n_items[1:3], c(4L, 6L, 0L))
  expect_true(is.na(comp$theta[3]))
  expect_equal(comp$computed_at[1], 200L)
  expect_null(latest_ability(data.frame()))
})

test_that("trajectory_x_range with a window covers the last days up to now", {
  h <- traj_hist()
  now <- as.POSIXct(1700086400, origin = "1970-01-01")
  r <- trajectory_x_range(h, days = 14, now = now)
  expect_equal(r[1], now - 14 * 86400)
  expect_true(r[2] > now)
  expect_equal(trajectory_x_range(h, NA), trajectory_x_range(h))
})

test_that("trajectory_panel_spec has each window's layout and notes an area without estimates in it", {
  h <- traj_hist()
  far <- trajectory_x_range(h, days = 14, now = max(h$time) + 60 * 86400)
  f <- spec_json(trajectory_panel_spec(h, list(
    all = trajectory_window_axis(trajectory_x_range(h)), "14" = trajectory_window_axis(far)
  )))
  expect_named(f$windows, c("all", "14"))
  expect_length(f$windows$all$annotations, 0L)
  expect_match(f$windows[["14"]]$annotations[[1]]$text, "nicht geübt", fixed = TRUE)
  expect_equal(unlist(f$windows[["14"]]$range), format(far, "%Y-%m-%d %H:%M:%S"))
  # the first window is the default layout
  expect_equal(f$layout$xaxis$range, f$windows$all$range)
})

test_that("trajectory_section shows the time-window switch with the panels", {
  html <- as.character(trajectory_section(traj_hist(), shiny::NS("d")))
  for (lbl in names(TRAJECTORY_WINDOWS)) expect_match(html, lbl, fixed = TRUE)
  # one spec per panel with data, with a layout for every window
  specs <- regmatches(html, gregexpr('(?<=<script type="application/json" class="traj-spec">).*?(?=</script>)', html, perl = TRUE))[[1]]
  expect_length(specs, 1L)
  expect_named(jsonlite::fromJSON(specs[1], simplifyVector = FALSE)$windows, c("all", "28", "14"))
  expect_match(html, 'id="d-traj_1" class="traj-plot"', fixed = TRUE)
  expect_match(html, 'data-traj-window="all" aria-pressed="true"', fixed = TRUE)
})

test_that("trajectory_ticks spreads 4 day labels over the range", {
  now <- as.POSIXct("2026-11-04 12:00")
  for (days in c(14, 28, 180)) {
    t <- trajectory_ticks(c(now - days * 86400, now))
    expect_length(t$vals, 4L)
    expect_match(t$text, "^\\d{2}\\.\\d{2}\\.$")
    expect_false(anyDuplicated(t$vals) > 0)
  }
  # a one-day range can't hold 4 distinct days
  short <- trajectory_ticks(c(now - 12 * 3600, now + 12 * 3600))
  expect_lte(length(short$vals), 2L)
  f <- spec_json(trajectory_panel_spec(traj_hist(), c(now - 14 * 86400, now)))
  expect_equal(f$layout$xaxis$tickmode, "array")
  expect_length(f$layout$xaxis$ticktext, 4L)
})

test_that("answer_tree_by_area restricts each area's trees to its items", {
  pool <- data.frame(id_item = 1:3, learning_area = LEARNING_AREA_LEVELS[c(1, 1, 3)])
  ud <- tree_ud(id_item = c(1L, 3L, 3L), bool_correct = c(TRUE, FALSE, TRUE))
  ba <- answer_tree_by_area(ud, pool)
  expect_equal(names(ba), LEARNING_AREA_LEVELS[c(1, 3)])
  expect_equal(ba[[1]]$forward$n, 2L)
  expect_equal(ba[[2]]$forward$children$wrong$n, 1L)
  expect_equal(ba[[2]]$backward$direction, "backward")
})

test_that("answer_tree_card with by_area adds the area chips and hides all but the default view", {
  pool <- data.frame(id_item = 1:3, learning_area = LEARNING_AREA_LEVELS[c(1, 1, 3)])
  ud <- tree_ud(id_item = c(1L, 3L), bool_correct = c(TRUE, FALSE))
  html <- as.character(answer_tree_card(
    answer_tree_stats(ud, pool),
    backward = answer_tree_stats(ud, pool, direction = "backward"),
    by_area = answer_tree_by_area(ud, pool)
  ))
  expect_match(html, 'data-lt-area="all" aria-pressed="true">Alle</button>', fixed = TRUE)
  expect_match(html, 'data-lt-area="3" aria-pressed="false">Inferenz</button>', fixed = TRUE)
  expect_false(grepl("lt-area-chip[^>]*title=", html)) # no native hover label
  expect_equal(lengths(regmatches(html, gregexpr("lt-area-chip", html, fixed = TRUE))), 3L) # Alle + 2 areas
  views <- regmatches(html, gregexpr('<div class="lt-view"[^>]*>', html))[[1]]
  expect_length(views, 6L) # (all + 2 areas) x 2 directions
  expect_equal(sum(!grepl("hidden", views)), 1L)
  expect_match(views[!grepl("hidden", views)], 'data-view="forward" data-area="all"', fixed = TRUE)
})

test_that("trajectory panel titles have no native hover tooltip", {
  html <- as.character(trajectory_section(traj_hist(), shiny::NS("d")))
  expect_false(grepl('class="traj-panel-title" title=', html, fixed = TRUE))
})

test_that("the Lernstand card uses the app tooltip, not native titles", {
  cl <- competency_label(0.3, 12L)
  dots <- as.character(certainty_dots(0.3, 0.4, 12L, cl))
  bar <- as.character(competency_scale_bar(cl))
  expect_match(dots, 'data-tip="[^"]*Wahrscheinlichkeit')
  expect_match(bar, 'data-tip="Kompetenz: ')
  expect_false(grepl(" title=", dots, fixed = TRUE))
  expect_false(grepl(" title=", bar, fixed = TRUE))
})

test_that("the trajectory has an (i) explaining its points next to the time switch", {
  html <- as.character(trajectory_section(traj_hist(), shiny::NS("d")))
  expect_match(html, '<div class="traj-controls">', fixed = TRUE)
  expect_match(html, 'class="la-info" data-tip="Jeder Punkt ist eine gespeicherte Schätzung', fixed = TRUE)
})

test_that("the explainer's button looks like the (i) hints, right after the card title", {
  html <- as.character(detail_section(NULL))
  expect_match(html, '<span>Wie sicher ist die Einschätzung\\?</span>\\s*<button type="button" class="la-info la-info-text ivl-explain-btn"')
})

test_that("latest_ability reads n_provisional, NA for snapshots without it", {
  ab <- data.frame(computed_at = 100L, learning_area = LEARNING_AREA_LEVELS[1],
                   theta = 0.5, se = 0.4, n_items = 4L)
  expect_true(is.na(latest_ability(ab)$n_provisional[1]))
  ab$n_provisional <- 3L
  expect_equal(latest_ability(ab)$n_provisional[1], 3L)
})

test_that("interval_sentence names the items with a preliminary difficulty", {
  expect_match(interval_sentence(0.2, 0.4, 12L, 4L), "12 Aufgaben, davon 4 mit vorläufiger Schwierigkeit", fixed = TRUE)
  expect_no_match(interval_sentence(0.2, 0.4, 12L, 0L), "vorläufig")
  expect_no_match(interval_sentence(0.2, 0.4, 12L), "vorläufig")
  expect_no_match(interval_sentence(0.2, NA, 12L, NA_integer_), "vorläufig")
})
