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

test_that("competency_label maps theta correctly to all four levels", {
  expect_equal(competency_label(1.5, 5L)$label, "Stark")
  expect_equal(competency_label(0.5, 5L)$label, "Gut entwickelt")
  expect_equal(competency_label(-0.3, 5L)$label, "Entwickelt sich")
  expect_equal(competency_label(-1.0, 5L)$label, "Übungsbedarf")
})

test_that("competency_label levels are strictly ordered", {
  lvl <- function(theta) competency_label(theta, n_unique = 5L)$level
  expect_gt(lvl(1.5), lvl(0.5))
  expect_gt(lvl(0.5), lvl(-0.3))
  expect_gt(lvl(-0.3), lvl(-1.0))
})

test_that("competency_label thresholds are exact (boundary values)", {
  # theta == 1.0 → NOT "Stark" (strict >)
  expect_equal(competency_label(1.0, 5L)$label, "Gut entwickelt")
  # theta == 0.0 → NOT "Gut entwickelt" (strict >)
  expect_equal(competency_label(0.0, 5L)$label, "Entwickelt sich")
  # theta == -0.5 → NOT "Entwickelt sich" (strict >)
  expect_equal(competency_label(-0.5, 5L)$label, "Übungsbedarf")
})

# ── evidence_label ────────────────────────────────────────────────────────────

test_that("evidence_label maps n_unique to correct tiers", {
  expect_equal(evidence_label(0L)$label, "–")
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

# ── rolling_mean_k ────────────────────────────────────────────────────────────

test_that("rolling_mean_k returns same length as input", {
  x <- c(1, 0, 1, 1, 0)
  expect_length(rolling_mean_k(x, k = 3L), length(x))
})

test_that("rolling_mean_k with k=1 returns the input unchanged", {
  x <- c(0.2, 0.8, 0.5)
  expect_equal(rolling_mean_k(x, k = 1L), x)
})

test_that("rolling_mean_k uses expanding window at the start", {
  # First value: window of 1 → mean of x[1] = 1
  # Second value: window of 2 → mean of x[1:2] = 0.5
  x <- c(1, 0, 0, 0, 0)
  result <- rolling_mean_k(x, k = 10L)
  expect_equal(result[1], 1.0)
  expect_equal(result[2], 0.5)
})

test_that("rolling_mean_k converges to mean over full window once k items seen", {
  x <- c(1, 0, 1, 0, 1, 0) # alternating, mean = 0.5
  result <- rolling_mean_k(x, k = 4L)
  # From position 4 onward the window is full
  expect_equal(result[4], mean(x[1:4]))
  expect_equal(result[6], mean(x[3:6]))
})

test_that("rolling_mean_k handles length-1 input", {
  expect_equal(rolling_mean_k(0.7, k = 5L), 0.7)
})

# ── recommend_next ────────────────────────────────────────────────────────────

make_comp <- function(thetas = rep(NA_real_, length(LEARNING_AREA_LEVELS))) {
  data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = thetas,
    n_items = rep(0L, length(LEARNING_AREA_LEVELS)),
    stringsAsFactors = FALSE
  )
}

test_that("recommend_next returns empty list when all areas are well-covered", {
  comp <- make_comp(thetas = rep(1.0, length(LEARNING_AREA_LEVELS)))
  n_unique <- setNames(
    rep(10L, length(LEARNING_AREA_LEVELS)),
    LEARNING_AREA_LEVELS
  )
  recs <- recommend_next(comp, n_unique)
  expect_length(recs, 0L)
})

test_that("recommend_next puts no-data areas first", {
  comp <- make_comp()
  n_unique <- setNames(
    rep(0L, length(LEARNING_AREA_LEVELS)),
    LEARNING_AREA_LEVELS
  )
  # Give one area enough data and a good theta so it doesn't appear
  n_unique[["Regression"]] <- 10L
  comp$theta[comp$learning_area == "Regression"] <- 1.5

  recs <- recommend_next(comp, n_unique)
  # All recommendations should be for no-data areas (priority 10)
  priorities <- vapply(recs, `[[`, integer(1), "priority")
  expect_true(all(priorities == 10L))
  areas <- vapply(recs, `[[`, character(1), "area")
  expect_false("Regression" %in% areas)
})

test_that("recommend_next puts low-evidence areas before low-theta areas", {
  comp <- make_comp(thetas = rep(0.0, length(LEARNING_AREA_LEVELS)))
  n_unique <- setNames(
    rep(10L, length(LEARNING_AREA_LEVELS)),
    LEARNING_AREA_LEVELS
  )

  # One area has very low theta → should recommend
  comp$theta[comp$learning_area == "Regression"] <- -2.0
  # One area has low evidence
  n_unique[["Poweranalyse"]] <- 2L

  recs <- recommend_next(comp, n_unique)
  areas <- vapply(recs, `[[`, character(1), "area")

  power_pos <- which(areas == "Poweranalyse")
  regression_pos <- which(areas == "Regression")

  # Low evidence (priority 20) should rank above low theta (priority ~50)
  expect_lt(power_pos, regression_pos)
})

test_that("recommend_next preserves ordering by priority", {
  comp <- make_comp()
  n_unique <- setNames(
    rep(0L, length(LEARNING_AREA_LEVELS)),
    LEARNING_AREA_LEVELS
  )

  recs <- recommend_next(comp, n_unique)
  priorities <- vapply(recs, `[[`, integer(1), "priority")
  expect_equal(priorities, sort(priorities))
})
