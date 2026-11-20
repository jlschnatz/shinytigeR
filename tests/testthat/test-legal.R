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

test_that("the privacy page covers the AI feedback like the FAQ", {
  s <- legal_sections()
  titles <- vapply(s, `[[`, character(1), "title")
  ai <- s[[which(titles == "Persönliches Feedback (KI)")]]$body
  # what goes to the AI, and what doesn't (same as the FAQ answer)
  expect_match(ai, "Einstufung pro Themenbereich", fixed = TRUE)
  expect_match(ai, "Benutzername, deine Antworten auf Aufgaben und deine Kommentare werden nicht übermittelt", fixed = TRUE)
  # what tigeR stores
  for (x in c("Bewertung", "Kommentar", "Meldungen", "geöffnet")) expect_match(ai, x, fixed = TRUE)
  stored <- s[[which(titles == "Welche Daten tigeR verarbeitet")]]$body
  expect_match(stored, "Persönliches Feedback (KI)", fixed = TRUE)
})
