test_that("every FAQ entry has a known topic, a question and an answer", {
  entries <- faq_entries()
  expect_gt(length(entries), 0L)
  for (e in entries) {
    expect_true(e$topic %in% FAQ_TOPICS, info = e$q)
    expect_true(nzchar(e$q) && nzchar(e$a), info = e$q)
  }
  # questions are unique, so no entry is shown twice
  qs <- vapply(entries, `[[`, character(1), "q")
  expect_equal(anyDuplicated(qs), 0L)
})

test_that("faq_section renders one closed accordion per topic, in topic order", {
  html <- as.character(tagList(faq_section()))
  topics <- regmatches(html, gregexpr('<h5 class="faq-topic-title">[^<]+', html))[[1]]
  expect_equal(sub(".*>", "", topics), FAQ_TOPICS)
  # all panels closed by default
  expect_false(grepl('class="accordion-collapse collapse show"', html, fixed = TRUE))
  # the contact address is filled in and the Markdown is rendered
  expect_match(html, CONTACT_EMAIL, fixed = TRUE)
  expect_match(html, "<strong>Starten</strong>", fixed = TRUE)
  # the former footer links are answers now
  expect_match(html, 'href="https://pandar.netlify.app/daten/datensaetze/"', fixed = TRUE)
  expect_match(html, 'href="https://meikesteinhilber.github.io/otter/"', fixed = TRUE)
  # every link opens in a new tab (same tab would end the Shiny session)
  n_links <- lengths(regmatches(html, gregexpr("<a [^>]*href=", html)))
  n_blank <- lengths(regmatches(html, gregexpr('<a target="_blank" rel="noopener" href=', html, fixed = TRUE)))
  expect_gt(n_links, 0L)
  expect_equal(n_blank, n_links)
})

test_that("the access answer matches the registration rules", {
  a <- Filter(function(e) e$q == "Wie bekomme ich einen Zugang?", faq_entries())[[1]]$a
  # keep in sync with REG_USERNAME_PATTERN and REG_PW_MIN_LENGTH
  expect_identical(REG_USERNAME_PATTERN, "^[a-zA-Z0-9_.-]{3,30}$")
  expect_match(a, "3 bis 30 Zeichen", fixed = TRUE)
  expect_match(a, sprintf("mindestens %d Zeichen", REG_PW_MIN_LENGTH), fixed = TRUE)
})
