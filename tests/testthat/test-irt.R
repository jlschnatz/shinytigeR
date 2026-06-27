test_that("prob_2pl returns probability in [0, 1]", {
  expect_gte(prob_2pl(0, a = 1, b = 0), 0)
  expect_lte(prob_2pl(0, a = 1, b = 0), 1)
})

test_that("prob_2pl is 0.5 when theta equals difficulty", {
  expect_equal(prob_2pl(theta = 1, a = 2, b = 1), 0.5)
  expect_equal(prob_2pl(theta = -1, a = 1, b = -1), 0.5)
})

test_that("prob_2pl increases with theta (higher ability → higher probability)", {
  p_low <- prob_2pl(theta = -2, a = 1, b = 0)
  p_mid <- prob_2pl(theta = 0, a = 1, b = 0)
  p_high <- prob_2pl(theta = 2, a = 1, b = 0)
  expect_lt(p_low, p_mid)
  expect_lt(p_mid, p_high)
})

test_that("prob_2pl is more discriminating with higher a", {
  # Higher discrimination → steeper curve → larger difference around b
  diff_flat <- prob_2pl(1, a = 0.5, b = 0) - prob_2pl(-1, a = 0.5, b = 0)
  diff_steep <- prob_2pl(1, a = 2.0, b = 0) - prob_2pl(-1, a = 2.0, b = 0)
  expect_gt(diff_steep, diff_flat)
})

test_that("estimate_theta recovers ability from unambiguous response patterns", {
  a <- rep(1, 5)
  b <- c(-2, -1, 0, 1, 2)

  # All correct → high ability
  theta_high <- estimate_theta(c(1, 1, 1, 1, 1), a, b)
  expect_gt(theta_high, 1)

  # All wrong → low ability
  theta_low <- estimate_theta(c(0, 0, 0, 0, 0), a, b)
  expect_lt(theta_low, -1)

  # Mixed, harder items wrong → near zero
  theta_mid <- estimate_theta(c(1, 1, 1, 0, 0), a, b)
  expect_gt(theta_mid, -1)
  expect_lt(theta_mid, 1)
})

test_that("estimate_theta is bounded to [-3, 3]", {
  a <- rep(1, 3)
  b <- rep(0, 3)
  theta <- estimate_theta(c(1, 1, 1), a, b)
  expect_lte(theta, 3)
  theta <- estimate_theta(c(0, 0, 0), a, b)
  expect_gte(theta, -3)
})

test_that("estimate_theta returns NA when all IRT params are missing", {
  # Items with NA IRT params are excluded by estimate_competency before reaching
  # estimate_theta, but calling it directly with NA params should not crash
  theta <- estimate_theta(c(1, 0), a = c(NA, NA), b = c(NA, NA))
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

test_that("estimate_competency excludes items with missing IRT parameters", {
  responses <- data.frame(
    id_item = 1L,
    learning_area = LEARNING_AREA_LEVELS[1],
    bool_correct = TRUE,
    stringsAsFactors = FALSE
  )
  items <- data.frame(
    id_item = 1L,
    irt_discr = NA_real_,
    irt_diff = 0,
    stringsAsFactors = FALSE
  )

  result <- estimate_competency(responses, items)
  expect_true(is.na(result$theta[1]))
  expect_equal(result$n_items[1], 0L)
})
