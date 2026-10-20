# ── dev/migrate_drop_mc_skip.R ────────────────────────────────────────────────

load_migrate_drop_mc_skip <- function() {
  env <- new.env()
  assign(".migrate_drop_mc_skip_sourced_only", TRUE, envir = env)
  sys.source(
    testthat::test_path("..", "..", "dev", "migrate_drop_mc_skip.R"),
    envir = env
  )
  env$migrate_drop_mc_skip
}

# item_db with only the columns the migration touches; `opts` is a list of
# length-6 character vectors (NA = empty option)
make_item_db <- function(ids, modes, correct, opts) {
  path <- tempfile(fileext = ".sqlite")
  df <- data.frame(id_item = ids, answer_mode = modes, answer_correct = correct)
  for (k in 1:6) {
    vals <- vapply(opts, `[`, character(1), k)
    df[[sprintf("answeroption_%02d", k)]] <- vals
    df[[sprintf("if_answeroption_%02d", k)]] <- ifelse(is.na(vals), NA, paste("fb", k))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "item_db", df)
  DBI::dbDisconnect(con)
  path
}

read_item_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, "item_db")
}

test_that("migrate_drop_mc_skip removes only trailing MC skip options, idempotently", {
  migrate <- load_migrate_drop_mc_skip()
  skip_txt <- "Frage überspringen."
  path <- make_item_db(
    ids = 1:5,
    modes = c("mc", "mc", "mc", "mc", "num"),
    correct = c("2", "1", "1", "1", "1"),
    opts = list(
      c("A", "B", "C", skip_txt, NA, NA), # text skip at position 4
      c("a.png", "b.png", NA, NA, NA, "www/skip.png"), # image skip at 6, with a gap
      c("A", "B", "C", NA, NA, NA), # no skip
      c(skip_txt, "B", "C", NA, NA, NA), # skip text, but not the last option
      c("5", "4.5", "6", NA, NA, NA) # numeric item
    )
  )
  before <- read_item_db(path)

  expect_message(n <- migrate(path), "2 MC item")
  expect_equal(n, 2L)
  after <- read_item_db(path)

  expect_true(is.na(after$answeroption_04[1]))
  expect_true(is.na(after$if_answeroption_04[1]))
  expect_equal(after$answeroption_03[1], "C")
  expect_true(is.na(after$answeroption_06[2]))
  expect_true(is.na(after$if_answeroption_06[2]))
  expect_equal(after$answeroption_02[2], "b.png")
  expect_equal(after[3:5, ], before[3:5, ], ignore_attr = TRUE)

  # Second run finds nothing and changes nothing
  expect_message(n2 <- migrate(path), "nothing to do")
  expect_equal(n2, 0L)
  expect_equal(read_item_db(path), after)
})

test_that("migrate_drop_mc_skip aborts without changes if a skip option is marked correct", {
  migrate <- load_migrate_drop_mc_skip()
  path <- make_item_db(
    ids = 1:2,
    modes = c("mc", "mc"),
    correct = c("3", "1"),
    opts = list(
      c("A", "B", "Frage überspringen.", NA, NA, NA),
      c("A", "B", "Frage überspringen.", NA, NA, NA)
    )
  )
  before <- read_item_db(path)
  expect_error(migrate(path), "item\\(s\\) 1")
  expect_equal(read_item_db(path), before)
})
