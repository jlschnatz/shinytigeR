# ── The lookup table (inst/extdata/lookup_exam.csv) ───────────────────────────

lookup_file <- function() {
  # tests run against the source tree (load_all); the same file is installed
  # with the package
  testthat::test_path("..", "..", "inst", "extdata", EXAM_LOOKUP_FILE)
}

test_that("the exam lookup table has known areas and types and complete rows", {
  raw <- utils::read.csv(lookup_file(), stringsAsFactors = FALSE, na.strings = "",
                         encoding = "UTF-8", colClasses = "character")
  expect_named(raw, c("learning_area", "type", "text", "url"))
  # a misspelt area would silently never be recommended
  expect_true(all(raw$learning_area %in% LEARNING_AREA_LEVELS))
  expect_true(all(raw$type %in% c("lehrbuch", "pandar")))
  expect_false(anyNA(raw$text))
  pandar <- raw[raw$type == "pandar", ]
  expect_true(all(startsWith(pandar$url, "https://pandar.netlify.app/")))
  expect_true(all(is.na(raw$url[raw$type == "lehrbuch"])))
  expect_false(anyDuplicated(raw[c("learning_area", "type", "text")]) > 0)
})

test_that("every learning area has textbook sections and a pandaR article", {
  lk <- read_exam_lookup(lookup_file())
  for (type in c("lehrbuch", "pandar")) {
    expect_setequal(unique(lk$learning_area[lk$type == type]), LEARNING_AREA_LEVELS)
  }
})

# ── rank_weak_areas ───────────────────────────────────────────────────────────

comp_fixture <- function(theta, n_items) {
  data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = theta, se = 0.5, n_items = n_items
  )
}

test_that("rank_weak_areas sorts by estimate, lowest first, and needs enough items", {
  comp <- comp_fixture(
    theta = c(0.5, -1.2, NA, -0.3, -2, 1, -1.2),
    n_items = c(10L, 6L, 0L, 5L, 4L, 8L, 7L)
  )
  # area 5 (theta -2) has only 4 items; area 3 has no estimate;
  # areas 2 and 7 tie at -1.2 and keep their order
  expect_equal(
    rank_weak_areas(comp, min_items = 5L),
    LEARNING_AREA_LEVELS[c(2, 7, 4, 1, 6)]
  )
  expect_equal(rank_weak_areas(comp, min_items = 1L)[1], LEARNING_AREA_LEVELS[5])
  expect_length(rank_weak_areas(NULL), 0L)
  expect_length(rank_weak_areas(comp_fixture(NA_real_, 0L)), 0L)
})

# ── exam_literature_card ──────────────────────────────────────────────────────

test_that("exam_literature_card shows the weakest area's sections and pandaR links", {
  comp <- comp_fixture(theta = c(0.5, 0, 0, 0, 0, 0, -1.5), n_items = 6L)
  html <- as.character(exam_literature_card(comp, read_exam_lookup(lookup_file())))
  expect_match(html, "Am meisten Nachholbedarf hast du gerade in dem Thema\\s*<b>Regression</b>")
  expect_match(html, "17.1 – 17.8, 19.1 – 19.8, 19.13", fixed = TRUE)
  # the level's coloured segment, as in the strip of "Dein Lernstand"
  expect_match(html, sprintf('<span class="exam-lit-level">(<span class="comp-seg is-filled is-current exam-lit-seg" style="--seg:%s;" aria-hidden="true"></span>Aufbau).</span>', COMPETENCY_SCALE$color_hex[1]), fixed = TRUE)
  expect_match(html, 'href="https://pandar.netlify.app/lehre/statistik-i/einfache-reg" target="_blank" rel="noopener"', fixed = TRUE)
  expect_match(html, "Multiple Regression", fixed = TRUE)
  expect_match(html, "Eid, M., Gollwitzer, M. &amp; Schmitt, M. (2017)", fixed = TRUE)
  # the online version in the university library
  expect_match(html, sprintf('href="%s" target="_blank" rel="noopener">E-Book in der UB', EXAM_BOOK_URL), fixed = TRUE)
  # only the weakest area
  expect_no_match(html, "Deskriptivstatistik")
})

test_that("exam_literature_card leaves out a line the area has no entry for", {
  comp <- comp_fixture(theta = c(0, 0, 0, 0, -2, 0, 0), n_items = 6L)
  lk <- data.frame(learning_area = "Poweranalyse", type = "pandar",
                   text = "Simulation und Poweranalyse", url = "https://pandar.netlify.app/x")
  html <- as.character(exam_literature_card(comp, lk))
  expect_no_match(html, "Lehrbuch</span>")
  expect_no_match(html, "Eid, M.")
  expect_no_match(html, "E-Book in der UB")
  expect_match(html, "pandaR</span>", fixed = TRUE)
})

test_that("exam_literature_card asks for more practice until an area has enough items", {
  comp <- comp_fixture(theta = c(-1, rep(NA, 6)), n_items = c(4L, rep(0L, 6)))
  html <- as.character(exam_literature_card(comp, read_exam_lookup(lookup_file()), min_items = 5L))
  expect_match(html, "mindestens 5 Aufgaben", fixed = TRUE)
  expect_no_match(html, "Am meisten Nachholbedarf")
  expect_match(as.character(exam_literature_card(NULL, read_exam_lookup(lookup_file()))), "mindestens", fixed = TRUE)
})

