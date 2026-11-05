# app.css uses the type scale (--fs-* / --fw-* tokens in :root) for every
# font size and weight, so each level of the hierarchy looks the same
# everywhere. A plain value like 0.82rem would quietly start a new level.

css_path <- function() {
  p <- test_path("..", "..", "inst", "app", "www", "css", "app.css")
  if (!file.exists(p)) p <- system.file("app", "www", "css", "app.css", package = "shinytigeR")
  p
}

css_without_root_and_comments <- function() {
  css <- paste(readLines(css_path(), warn = FALSE), collapse = "\n")
  css <- gsub("/\\*.*?\\*/", "", css, perl = TRUE)
  # the token definitions themselves
  sub(":root\\s*\\{[^}]*--fs-page[^}]*\\}", "", css, perl = TRUE)
}

test_that("app.css sets font sizes only via the type-scale tokens", {
  css <- css_without_root_and_comments()
  literal <- regmatches(css, gregexpr("font-size:\\s*[0-9.]+(rem|px)", css, perl = TRUE))[[1]]
  expect_identical(literal, character(0))
})

test_that("app.css sets font weights only via the tokens (icon fonts excepted)", {
  css <- css_without_root_and_comments()
  literal <- regmatches(css, gregexpr("font-weight:\\s*[0-9]+", css, perl = TRUE))[[1]]
  # 900 selects Font Awesome's solid icon style, it isn't a text weight
  expect_identical(setdiff(literal, "font-weight: 900"), character(0))
})

test_that("every token used in app.css is defined", {
  css <- paste(readLines(css_path(), warn = FALSE), collapse = "\n")
  used <- unique(regmatches(css, gregexpr("var\\(--f[sw]-[a-z-]+\\)", css))[[1]])
  used <- sub("var\\((.*)\\)", "\\1", used)
  defined <- regmatches(css, gregexpr("--f[sw]-[a-z-]+(?=:)", css, perl = TRUE))[[1]]
  expect_true(all(used %in% defined), info = paste(setdiff(used, defined), collapse = ", "))
})
