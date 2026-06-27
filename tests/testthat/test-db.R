# All DB tests use temporary in-memory or temp-file SQLite databases so they
# never touch the real db_user.sqlite or db_item.sqlite.

make_temp_db <- function() {
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbDisconnect(con)
  path
}

make_item_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(
    con,
    "item_db",
    data.frame(
      id_item = 1:3,
      learning_area = c("Regression", "Regression", "Poweranalyse"),
      type_item = c("content", "coding", "content"),
      bloom_taxonomy = c("knowledge", "application", "comprehension"),
      stimulus_text = c("Text 1", NA, "Text 3"),
      stimulus_image = c(NA, "www/img_item/foo.png", NA),
      answeroption_01 = c("A", "A", "A"),
      answeroption_02 = c("B", "B", "B"),
      answeroption_03 = NA,
      answeroption_04 = NA,
      answeroption_05 = NA,
      answeroption_06 = NA,
      if_answeroption_01 = c("F1", "F1", "F1"),
      if_answeroption_02 = c("F2", "F2", "F2"),
      if_answeroption_03 = NA,
      if_answeroption_04 = NA,
      if_answeroption_05 = NA,
      if_answeroption_06 = NA,
      answer_correct = c(1L, 1L, 2L),
      type_answer = "text",
      irt_discr = c(1.0, 1.2, 0.8),
      irt_diff = c(0.0, -0.5, 0.5),
      stringsAsFactors = FALSE
    )
  )
  path
}

make_response_row <- function(
  user_id = "test_user",
  item_id = 1L,
  learning_area = "Regression",
  bool_correct = TRUE,
  skipped = FALSE
) {
  data.frame(
    id_user = user_id,
    id_session = "sess1",
    id_date = as.integer(Sys.Date()),
    id_datetime = as.integer(Sys.time()),
    id_item = item_id,
    learning_area = learning_area,
    selected_option = 1L,
    answer_correct = 1L,
    bool_correct = bool_correct,
    skipped = skipped,
    stringsAsFactors = FALSE
  )
}

# ── db_with ───────────────────────────────────────────────────────────────────

test_that("db_with opens a connection, runs fn, and closes it", {
  path <- make_temp_db()
  result <- db_with(path, function(con) {
    expect_true(DBI::dbIsValid(con))
    42L
  })
  expect_equal(result, 42L)
})

test_that("db_with closes the connection even if fn errors", {
  path <- make_temp_db()
  con_ref <- NULL
  expect_error(
    db_with(path, function(con) {
      con_ref <<- con
      stop("intentional error")
    })
  )
  expect_false(DBI::dbIsValid(con_ref))
})

# ── db_get_items ──────────────────────────────────────────────────────────────

test_that("db_get_items returns a data.frame with expected columns", {
  path <- make_item_db(make_temp_db())
  df <- db_get_items(path)

  expect_s3_class(df, "data.frame")
  expect_true(all(
    c("id_item", "learning_area", "type_item", "irt_discr", "irt_diff") %in%
      names(df)
  ))
})

test_that("db_get_items rewrites www/ image paths to img_item/", {
  path <- make_item_db(make_temp_db())
  df <- db_get_items(path)

  img_cols <- grep("^(stimulus_image|answeroption_)", names(df), value = TRUE)
  all_paths <- unlist(df[img_cols])
  all_paths <- all_paths[!is.na(all_paths) & nzchar(all_paths)]

  expect_false(any(startsWith(all_paths, "www/")))
  expect_true(any(startsWith(all_paths, "img_item/")))
})

test_that("db_get_items sets learning_area as an ordered factor", {
  path <- make_item_db(make_temp_db())
  df <- db_get_items(path)
  expect_s3_class(df$learning_area, "factor")
  expect_equal(levels(df$learning_area), LEARNING_AREA_LEVELS)
})

# ── db_user_exists / db_get_userdata / db_write_response ──────────────────────

test_that("db_user_exists returns FALSE for new user", {
  path <- make_temp_db()
  expect_false(db_user_exists("alice", path))
})

test_that("db_write_response creates user table and writes a row", {
  path <- make_temp_db()
  row <- make_response_row("alice")

  db_write_response("alice", row, path)

  expect_true(db_user_exists("alice", path))
  ud <- db_get_userdata("alice", path)
  expect_equal(nrow(ud), 1L)
  expect_equal(ud$id_item, 1L)
})

test_that("db_write_response appends rows for subsequent answers", {
  path <- make_temp_db()

  db_write_response("bob", make_response_row("bob", item_id = 1L), path)
  db_write_response("bob", make_response_row("bob", item_id = 2L), path)

  ud <- db_get_userdata("bob", path)
  expect_equal(nrow(ud), 2L)
  expect_setequal(ud$id_item, c(1L, 2L))
})

test_that("db_get_userdata returns empty data.frame for unknown user", {
  path <- make_temp_db()
  ud <- db_get_userdata("nobody", path)
  expect_s3_class(ud, "data.frame")
  expect_equal(nrow(ud), 0L)
})

test_that("db_write_response keeps data from different users separate", {
  path <- make_temp_db()

  db_write_response("alice", make_response_row("alice", item_id = 10L), path)
  db_write_response("bob", make_response_row("bob", item_id = 20L), path)

  expect_equal(db_get_userdata("alice", path)$id_item, 10L)
  expect_equal(db_get_userdata("bob", path)$id_item, 20L)
})

# ── DB path env var ───────────────────────────────────────────────────────────

test_that("DB_ITEMS/USERS/CREDS use TIGER_DB_DIR env var", {
  withr::with_envvar(c(TIGER_DB_DIR = "/custom/path"), {
    expect_equal(DB_ITEMS(), "/custom/path/db_item.sqlite")
    expect_equal(DB_USERS(), "/custom/path/db_user.sqlite")
    expect_equal(DB_CREDS(), "/custom/path/db_credentials.sqlite")
  })
})

test_that("DB path functions default to current directory when env var is unset", {
  withr::with_envvar(c(TIGER_DB_DIR = ""), {
    expect_match(DB_ITEMS(), "db_item.sqlite$")
  })
})
