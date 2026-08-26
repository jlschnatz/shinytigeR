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

test_that("ability_trajectory_data adds a Date column and short, ordered area labels", {
  ab <- data.frame(
    computed_at = c(1700000000L, 1700000000L),
    learning_area = c(LEARNING_AREA_LEVELS[3], LEARNING_AREA_LEVELS[1]),
    theta = c(0.1, 0.2),
    n_items = c(4L, 5L),
    stringsAsFactors = FALSE
  )
  result <- ability_trajectory_data(ab)
  expect_s3_class(result$date, "Date")
  expect_s3_class(result$area_short, "factor")
  # Factor levels follow LEARNING_AREA_LEVELS canonical order, not input order
  expect_equal(
    levels(result$area_short)[1:3],
    names(LEARNING_AREA_LABELS)[match(LEARNING_AREA_LEVELS[1:3], LEARNING_AREA_LABELS)]
  )
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
