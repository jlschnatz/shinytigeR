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

test_that("the FAQ explains which items count and preliminary difficulties", {
  e <- faq_entries()
  qs <- vapply(e, `[[`, character(1), "q")
  expect_true("Welche Aufgaben zählen für meinen Lernstand?" %in% qs)
  diff <- e[[which(qs == "Was bedeutet die „Schwierigkeit“ einer Aufgabe?")]]$a
  expect_match(diff, "vorläufige Schätzung", fixed = TRUE)
  expect_match(diff, "KI-Sprachmodell", fixed = TRUE)
})

test_that("the difficulty answer states the current number of preliminary items", {
  diff_answer <- function(...) {
    e <- faq_entries(...)
    e[[which(vapply(e, `[[`, character(1), "q") == "Was bedeutet die „Schwierigkeit“ einer Aufgabe?")]]$a
  }
  expect_match(diff_answer(provisional = c(n = 49L, total = 165L)), "bei 49 von 165 Aufgaben", fixed = TRUE)
  expect_match(diff_answer(provisional = c(n = 0L, total = 165L)), "aller Aufgaben aus Antworten berechnet", fixed = TRUE)
  expect_no_match(diff_answer(), "Zurzeit")
})

test_that("provisional_item_count reads the pool's irt_provisional column", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "db_item.sqlite")
  db_with(path, function(con) {
    DBI::dbWriteTable(con, "item_db", data.frame(
      id_item = 1:3, learning_area = LEARNING_AREA_LEVELS[1], stimulus_image = NA_character_,
      irt_diff = c(0, 1, 2), irt_provisional = c(1L, 0L, NA)
    ))
  })
  expect_equal(provisional_item_count(path), c(n = 1L, total = 3L))

  # no column, or no file: NULL, and no empty DB is created
  db_with(path, function(con) DBI::dbExecute(con, "ALTER TABLE item_db DROP COLUMN irt_provisional"))
  expect_null(provisional_item_count(path))
  missing <- file.path(dir, "nope.sqlite")
  expect_null(provisional_item_count(missing))
  expect_false(file.exists(missing))
})
