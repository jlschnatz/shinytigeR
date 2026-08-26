# ── Math rendering ────────────────────────────────────────────────────────────

test_that("protect_math_delimiters converts inline math to MathJax delimiters", {
  result <- protect_math_delimiters("This is $x^2$ inline")
  expect_match(result, "\\\\(", fixed = TRUE)
  expect_match(result, "\\\\)", fixed = TRUE)
  expect_false(grepl("$", result, fixed = TRUE))
})

test_that("protect_math_delimiters converts display math to MathJax delimiters", {
  result <- protect_math_delimiters("$$x = \\frac{a}{b}$$")
  expect_match(result, "\\\\[", fixed = TRUE)
  expect_match(result, "\\\\]", fixed = TRUE)
})

test_that("protect_math_delimiters escapes * inside math so markdown ignores it", {
  # Without protection, $a * b$ would become <em> in markdown
  result <- protect_math_delimiters("$a * b$")
  # The * should be escaped as \* before the $ conversion
  # The final string should not contain bare * inside the math region
  expect_false(grepl("\\(a \\* b\\)", result, fixed = TRUE))
  # But it should contain the escaped version inside \(...\)
  expect_match(result, "\\\\(", fixed = TRUE)
})

test_that("protect_math_delimiters escapes _ inside math", {
  result <- protect_math_delimiters("$x_i$")
  expect_match(result, "\\\\(", fixed = TRUE)
  # No bare underscore that would trigger markdown emphasis
  expect_false(grepl("\\(x_i\\)", result, fixed = TRUE))
})

test_that("protect_math_delimiters leaves inline code spans untouched", {
  # income$salary should not be treated as math
  result <- protect_math_delimiters("`income$salary`")
  expect_match(result, "income$salary", fixed = TRUE)
  # Should not have introduced MathJax delimiters
  expect_false(grepl("\\\\(", result, fixed = TRUE))
})

test_that("protect_math_delimiters handles text with no math unchanged (no $ signs)", {
  input <- "Just plain text with no math."
  result <- protect_math_delimiters(input)
  expect_equal(result, input)
})

test_that("render_md returns empty string for NA, NULL, or blank input", {
  expect_equal(render_md(NA_character_), "")
  expect_equal(render_md(NULL), "")
  expect_equal(render_md("   "), "")
  expect_equal(render_md(""), "")
})

test_that("render_md wraps output in HTML paragraph tags for normal text", {
  result <- render_md("Hello world")
  expect_match(result, "<p>")
})

test_that("render_md handles bold markdown", {
  result <- render_md("**bold**")
  expect_match(result, "<strong>")
})

# ── Answer option helpers ─────────────────────────────────────────────────────

make_item <- function(n_options = 4, correct = 1L, type = "text") {
  item <- list(answer_correct = correct, type_answer = type)
  for (i in 1:6) {
    key <- paste0("answeroption_0", i)
    item[[key]] <- if (i <= n_options) paste0("Option ", i) else NA_character_
    key2 <- paste0("if_answeroption_0", i)
    item[[key2]] <- if (i <= n_options) {
      paste0("Feedback ", i)
    } else {
      NA_character_
    }
  }
  item
}

test_that("get_answeroptions returns exactly the non-NA options", {
  item <- make_item(n_options = 3)
  opts <- get_answeroptions(item)
  expect_length(opts, 3)
  expect_equal(unname(opts), c("Option 1", "Option 2", "Option 3"))
})

test_that("get_feedbackoptions matches answer option count", {
  item <- make_item(n_options = 4)
  expect_length(get_feedbackoptions(item), 4)
})

test_that("evaluate_answer returns 'correct' for the correct option", {
  item <- make_item(n_options = 4, correct = 2L)
  expect_equal(evaluate_answer(item, 2L), "correct")
})

test_that("evaluate_answer returns 'skip' for the last option", {
  item <- make_item(n_options = 4, correct = 1L)
  # Last option (index 4) is always skip
  expect_equal(evaluate_answer(item, 4L), "skip")
})

test_that("evaluate_answer returns 'incorrect' for wrong non-skip options", {
  item <- make_item(n_options = 4, correct = 1L)
  expect_equal(evaluate_answer(item, 2L), "incorrect")
  expect_equal(evaluate_answer(item, 3L), "incorrect")
})

# ── build_response_row ────────────────────────────────────────────────────────

test_that("build_response_row produces correct columns and types", {
  item <- make_item(n_options = 4, correct = 1L)
  item$id_item <- 42L
  item$learning_area <- "Regression"

  row <- build_response_row(
    item,
    answer_idx = 1L,
    user_id = "user1",
    session_token = "tok"
  )

  expect_s3_class(row, "data.frame")
  expect_equal(nrow(row), 1L)
  expect_equal(row$id_item, 42L)
  expect_equal(row$id_user, "user1")
  expect_true(isTRUE(row$bool_correct))
  expect_false(row$skipped)
})

test_that("build_response_row marks skipped correctly", {
  item <- make_item(n_options = 4, correct = 1L)
  item$id_item <- 1L
  item$learning_area <- "Regression"

  row <- build_response_row(
    item,
    answer_idx = 4L,
    user_id = "u",
    session_token = "s"
  )
  expect_true(row$skipped)
  expect_true(is.na(row$bool_correct))
})

test_that("build_response_row marks incorrect correctly", {
  item <- make_item(n_options = 4, correct = 1L)
  item$id_item <- 1L
  item$learning_area <- "Regression"

  row <- build_response_row(
    item,
    answer_idx = 2L,
    user_id = "u",
    session_token = "s"
  )
  expect_false(row$skipped)
  expect_false(isTRUE(row$bool_correct))
})

# ── safe_sample ───────────────────────────────────────────────────────────────

test_that("safe_sample returns at most size elements", {
  expect_length(safe_sample(1:5, size = 3), 3)
})

test_that("safe_sample returns all elements when size > length(x)", {
  result <- safe_sample(1:3, size = 10)
  expect_length(result, 3)
  expect_setequal(result, 1:3)
})

test_that("safe_sample handles empty input without error", {
  expect_length(safe_sample(integer(0)), 0)
})

# ── latest_attempts ───────────────────────────────────────────────────────────

test_that("latest_attempts keeps the most recent row per item, not the first", {
  ud <- data.frame(
    id_item = c(1L, 1L, 2L),
    id_datetime = c(100L, 200L, 150L),
    bool_correct = c(FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- latest_attempts(ud)
  expect_equal(nrow(result), 2L)
  expect_equal(result$bool_correct[result$id_item == 1L], TRUE)
  expect_equal(result$id_datetime[result$id_item == 1L], 200L)
})

test_that("latest_attempts handles an empty data.frame", {
  ud <- data.frame(id_item = integer(0), id_datetime = integer(0))
  result <- latest_attempts(ud)
  expect_equal(nrow(result), 0L)
})

test_that("latest_attempts is a no-op when each item was answered once", {
  ud <- data.frame(
    id_item = 1:3,
    id_datetime = c(100L, 200L, 300L),
    stringsAsFactors = FALSE
  )
  result <- latest_attempts(ud)
  expect_equal(nrow(result), 3L)
  expect_setequal(result$id_item, 1:3)
})

# ── build_ability_rows ────────────────────────────────────────────────────────

test_that("build_ability_rows produces one row per learning area with a shared computed_at", {
  comp <- data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = c(1.2, NA, 0, -0.5, NA, 0.3, 1.0),
    n_items = c(5L, 0L, 2L, 3L, 0L, 4L, 6L),
    stringsAsFactors = FALSE
  )
  rows <- build_ability_rows(comp, user_id = "alice", session_token = "tok1")

  expect_equal(nrow(rows), length(LEARNING_AREA_LEVELS))
  expect_true(all(rows$id_user == "alice"))
  expect_true(all(rows$id_session == "tok1"))
  expect_length(unique(rows$computed_at), 1L)
  expect_equal(rows$theta, comp$theta)
})

# ── is_img_path ───────────────────────────────────────────────────────────────

test_that("is_img_path detects common image extensions", {
  expect_true(is_img_path("img_item/foo.png"))
  expect_true(is_img_path("bar.JPG"))
  expect_true(is_img_path("baz.svg"))
  expect_false(is_img_path("answer text"))
  expect_false(is_img_path(NA_character_))
})
