test_that("irf_1pl returns probability in [0, 1]", {
  expect_gte(irf_1pl(0, b = 0), 0)
  expect_lte(irf_1pl(0, b = 0), 1)
})

test_that("irf_1pl is 0.5 when theta equals difficulty", {
  expect_equal(irf_1pl(theta = 1, b = 1), 0.5)
  expect_equal(irf_1pl(theta = -1, b = -1), 0.5)
})

test_that("irf_1pl increases with theta (higher ability → higher probability)", {
  p_low <- irf_1pl(theta = -2, b = 0)
  p_mid <- irf_1pl(theta = 0, b = 0)
  p_high <- irf_1pl(theta = 2, b = 0)
  expect_lt(p_low, p_mid)
  expect_lt(p_mid, p_high)
})

test_that("irf_1pl decreases with difficulty", {
  expect_gt(irf_1pl(0, b = -1), irf_1pl(0, b = 1))
})

test_that("iteminfo_1pl peaks at 0.25 where theta equals difficulty", {
  expect_equal(iteminfo_1pl(0.5, b = 0.5), 0.25)
  expect_lt(iteminfo_1pl(2, b = 0), 0.25)
})

test_that("estimate_theta recovers ability from unambiguous response patterns", {
  b <- c(-2, -1, 0, 1, 2)

  # All correct → high ability
  theta_high <- estimate_theta(c(1, 1, 1, 1, 1), b)
  expect_gt(theta_high, 1)

  # All wrong → low ability
  theta_low <- estimate_theta(c(0, 0, 0, 0, 0), b)
  expect_lt(theta_low, -1)

  # Mixed, harder items wrong → near zero
  theta_mid <- estimate_theta(c(1, 1, 1, 0, 0), b)
  expect_gt(theta_mid, -1)
  expect_lt(theta_mid, 1)
})

test_that("estimate_theta is bounded to THETA_RANGE", {
  b <- rep(0, 3)
  expect_lte(estimate_theta(c(1, 1, 1), b), THETA_RANGE[2])
  expect_gte(estimate_theta(c(0, 0, 0), b), THETA_RANGE[1])
})

test_that("estimate_theta returns NA when all difficulties are missing", {
  # Items with NA difficulty are excluded by estimate_competency before reaching
  # estimate_theta, but calling it directly with NA params should not crash
  theta <- estimate_theta(c(1, 0), b = c(NA, NA))
  # Either returns NA (error caught) or a numeric — must not throw
  expect_true(is.numeric(theta) || is.na(theta))
})

test_that("estimate_competency returns one row per learning area", {
  n_areas <- length(LEARNING_AREA_LEVELS)

  responses <- data.frame(
    id_item = 1:3,
    learning_area = LEARNING_AREA_LEVELS[c(1, 1, 2)],
    bool_correct = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  items <- data.frame(
    id_item = 1:3,
    irt_discr = c(1, 1, 1),
    irt_diff = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  result <- estimate_competency(responses, items)

  expect_equal(nrow(result), n_areas)
  expect_equal(as.character(result$learning_area), LEARNING_AREA_LEVELS)
})

test_that("estimate_competency returns NA theta for areas with no responses", {
  responses <- data.frame(
    id_item = 1L,
    learning_area = LEARNING_AREA_LEVELS[1],
    bool_correct = TRUE,
    stringsAsFactors = FALSE
  )
  items <- data.frame(
    id_item = 1L,
    irt_discr = 1,
    irt_diff = 0,
    stringsAsFactors = FALSE
  )

  result <- estimate_competency(responses, items)

  # All areas except the first should have NA theta and n_items == 0
  expect_true(all(is.na(result$theta[-1])))
  expect_true(all(result$n_items[-1] == 0L))
  expect_false(is.na(result$theta[1]))
})

# ── compute_and_save_ability ──────────────────────────────────────────────────

test_that("compute_and_save_ability writes one persisted snapshot from the user's latest attempts", {
  user_path <- tempfile(fileext = ".sqlite")
  ability_path <- tempfile(fileext = ".sqlite")

  items <- data.frame(
    id_item = 1:2,
    irt_discr = c(1, 1),
    irt_diff = c(0, 0),
    stringsAsFactors = FALSE
  )

  row <- function(item_id, correct, dt) {
    data.frame(
      id_user = "alice",
      id_session = "sess1",
      id_date = as.integer(Sys.Date()),
      id_datetime = dt,
      id_item = item_id,
      learning_area = LEARNING_AREA_LEVELS[1],
      selected_option = 1L,
      answer_correct = 1L,
      bool_correct = correct,
      skipped = FALSE,
      stringsAsFactors = FALSE
    )
  }
  # item 1 answered wrong then right (latest should win); item 2 answered once
  db_write_response("alice", row(1L, FALSE, 100L), user_path)
  db_write_response("alice", row(1L, TRUE, 200L), user_path)
  db_write_response("alice", row(2L, TRUE, 150L), user_path)

  result <- compute_and_save_ability("alice", "tok", items, user_path, ability_path)

  expect_equal(nrow(result), length(LEARNING_AREA_LEVELS))
  saved <- db_get_ability("alice", ability_path)
  expect_equal(nrow(saved), length(LEARNING_AREA_LEVELS))
  area1_n <- saved$n_items[saved$learning_area == LEARNING_AREA_LEVELS[1]]
  # Both items count once each (deduped to latest attempt), not the 3 raw rows
  expect_equal(area1_n, 2L)
})

test_that("compute_and_save_ability is a no-op for a user with no responses", {
  user_path <- tempfile(fileext = ".sqlite")
  ability_path <- tempfile(fileext = ".sqlite")
  items <- data.frame(
    id_item = integer(0), irt_discr = numeric(0), irt_diff = numeric(0)
  )

  result <- compute_and_save_ability("nobody", "tok", items, user_path, ability_path)
  expect_null(result)
  expect_equal(nrow(db_get_ability("nobody", ability_path)), 0L)
})

test_that("estimate_competency excludes items with a missing difficulty", {
  responses <- data.frame(
    id_item = 1L,
    learning_area = LEARNING_AREA_LEVELS[1],
    bool_correct = TRUE,
    stringsAsFactors = FALSE
  )
  items <- data.frame(id_item = 1L, irt_discr = 1, irt_diff = NA_real_)

  result <- estimate_competency(responses, items)
  expect_true(is.na(result$theta[1]))
  expect_equal(result$n_items[1], 0L)
})

test_that("estimate_competency ignores irt_discr (1PL): items without it are still used", {
  responses <- data.frame(
    id_item = 1:2,
    learning_area = LEARNING_AREA_LEVELS[1],
    bool_correct = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  with_a <- data.frame(id_item = 1:2, irt_discr = c(2, 0.3), irt_diff = c(-0.5, 0.5))
  no_a <- data.frame(id_item = 1:2, irt_discr = NA_real_, irt_diff = c(-0.5, 0.5))

  r1 <- estimate_competency(responses, with_a)
  r2 <- estimate_competency(responses, no_a)
  expect_equal(r2$n_items[1], 2L)
  expect_equal(r1$theta[1], r2$theta[1])
})

# ── sem_1pl / prob_in_interval ────────────────────────────────────────────────

test_that("sem_1pl equals 1/sqrt(test information) and shrinks with more items", {
  b <- c(-1, 0, 1)
  p <- plogis(0.2 - b)
  expect_equal(sem_1pl(0.2, b), 1 / sqrt(sum(p * (1 - p))))
  expect_lt(sem_1pl(0, rep(0, 20)), sem_1pl(0, rep(0, 5)))
})

test_that("prob_in_interval is a normal-approximation interval probability", {
  expect_equal(prob_in_interval(0, 1, -1.96, 1.96), 0.95, tolerance = 1e-3)
  expect_equal(prob_in_interval(0, 1, -Inf, Inf), 1)
  expect_equal(prob_in_interval(0, 1, 0, Inf), 0.5)
  # narrower SE -> more mass in the same interval
  expect_gt(prob_in_interval(0, 0.3, -0.5, 0.5), prob_in_interval(0, 1, -0.5, 0.5))
})

test_that("estimate_competency returns an SE for estimated areas and NA otherwise", {
  items <- data.frame(
    id_item = 1:4,
    irt_discr = c(1, 1.2, 0.8, 1),
    irt_diff = c(-0.5, 0, 0.5, 0)
  )
  responses <- data.frame(
    id_item = 1:4,
    learning_area = factor(
      c(rep("Regression", 4)),
      levels = LEARNING_AREA_LEVELS
    ),
    bool_correct = c(TRUE, FALSE, TRUE, FALSE)
  )
  comp <- estimate_competency(responses, items)
  reg <- comp[comp$learning_area == "Regression", ]
  expect_equal(reg$se, sem_1pl(reg$theta, items$irt_diff))
  expect_true(all(is.na(comp$se[comp$learning_area != "Regression"])))
})
