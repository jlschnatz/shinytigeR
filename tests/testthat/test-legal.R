test_that("every legal section has a title and a body", {
  s <- legal_sections()
  expect_gt(length(s), 0)
  for (x in s) {
    expect_true(nzchar(x$title))
    expect_true(nzchar(x$body))
  }
})

test_that("the legal page shows every section, links open in a new tab", {
  html <- as.character(legal_page())
  for (x in legal_sections()) expect_match(html, x$title, fixed = TRUE)
  expect_match(html, CONTACT_EMAIL, fixed = TRUE)
  # a link in the same tab would leave tigeR and end the Shiny session
  links <- regmatches(html, gregexpr("<a [^>]*>", html))[[1]]
  expect_true(all(grepl('target="_blank"', links)))
})

test_that("the footer links to the legal page on every page", {
  footer <- as.character(app_footer())
  expect_match(footer, 'id="show_legal"', fixed = TRUE)
  ui <- as.character(app_ui())
  expect_match(ui, 'data-value="legal_panel"', fixed = TRUE)
  expect_match(ui, 'class="app-footer"', fixed = TRUE)
})
