library(shiny)

# ── mod_selector_server ───────────────────────────────────────────────────────

test_that("selector: filtered_items respects area filter", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials()
      ),
      {
        # Select only Regression area, both types
        session$setInputs(
          areas = "Regression",
          types = c("content", "coding"),
          n_items = 10L,
          only_new = FALSE
        )
        # 3 items belong to Regression
        expect_equal(nrow(filtered_items()), 3L)
      }
    )
  })
})

test_that("selector: filtered_items respects type filter", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials()
      ),
      {
        session$setInputs(
          areas = LEARNING_AREA_LEVELS,
          types = "content", # only content, not coding
          n_items = 10L,
          only_new = FALSE
        )
        # 4 of 6 items are type "content"
        expect_equal(nrow(filtered_items()), 4L)
        expect_true(all(filtered_items()$type_item == "content"))
      }
    )
  })
})

test_that("selector: submit sets practice_ids to a sampled integer vector", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials()
      ),
      {
        session$setInputs(
          areas = LEARNING_AREA_LEVELS,
          types = c("content", "coding"),
          n_items = 3L,
          only_new = FALSE
        )
        session$setInputs(submit = 1L)

        ids <- practice_ids()
        expect_false(is.null(ids))
        expect_type(ids, "integer")
        expect_length(ids, 3L)
        expect_true(all(ids %in% data_item$id_item))
      }
    )
  })
})

test_that("selector: submit respects n_items cap when fewer items available", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials()
      ),
      {
        # Request more items than exist in the filtered set
        session$setInputs(
          areas = "Regression", # only 3 items
          types = c("content", "coding"),
          n_items = 50L,
          only_new = FALSE
        )
        session$setInputs(submit = 1L)
        expect_length(practice_ids(), 3L)
      }
    )
  })
})

test_that("selector: only_new excludes already-answered items", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)

  # Pre-populate DB: items 1 and 2 already answered by testuser
  db_path <- make_user_db("testuser", item_ids = 1:2)

  withr::with_envvar(list(TIGER_DB_DIR = dirname(db_path)), {
    # Rename file to match expected DB_USERS() name
    file.rename(db_path, DB_USERS())

    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials()
      ),
      {
        session$setInputs(
          areas = "Regression", # items 1, 2, 3
          types = c("content", "coding"),
          n_items = 10L,
          only_new = TRUE
        )
        # Items 1 and 2 already answered → only item 3 remains
        expect_equal(nrow(filtered_items()), 1L)
        expect_equal(filtered_items()$id_item, 3L)
      }
    )
  })
})

# ── mod_practice_server ───────────────────────────────────────────────────────

test_that("practice: starts at position 1", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(1L, 2L, 3L))
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        expect_equal(state$pos, 1L)
        expect_false(state$checked)
      }
    )
  })
})

test_that("practice: current_item matches the item at state$pos", {
  data_item <- make_data_item()
  ids <- c(3L, 1L, 2L)
  practice_ids <- reactiveVal(ids)
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        expect_equal(current_item()$id_item, ids[1])
      }
    )
  })
})

test_that("practice: checking an answer sets state$checked and writes to DB", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(1L, 2L))
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(answer = "1", check = 1L)

        expect_true(state$checked)
        expect_equal(state$answer_id, 1L)

        # write_trigger should have incremented
        expect_equal(write_trigger(), 1L)

        # A row should exist in the user DB
        ud <- db_get_userdata("testuser", DB_USERS())
        expect_equal(nrow(ud), 1L)
        expect_equal(as.logical(ud$bool_correct), TRUE)
      }
    )
  })
})

test_that("practice: next_item advances position", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(1L, 2L, 3L))
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(answer = "1", check = 1L)
        session$setInputs(next_item = 1L)

        expect_equal(state$pos, 2L)
        expect_false(state$checked) # reset for next item
      }
    )
  })
})

test_that("practice: finishing all items resets practice_ids to NULL", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(1L)) # single item
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(answer = "1", check = 1L)
        session$setInputs(next_item = 1L)

        expect_null(practice_ids())
      }
    )
  })
})

test_that("practice: back button resets practice_ids to NULL", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(1L, 2L))
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(back = 1L)
        expect_null(practice_ids())
      }
    )
  })
})

test_that("practice: skip answer (last option) records skipped = TRUE", {
  data_item <- make_data_item() # 3 options per item; option 3 = skip
  practice_ids <- reactiveVal(c(1L))
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(answer = "3", check = 1L) # option 3 = skip

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_equal(as.logical(ud$skipped), TRUE)
        expect_true(is.na(ud$bool_correct))
      }
    )
  })
})

test_that("practice: new practice_ids resets position to 1", {
  # When practice_ids changes (new practice session started), the observer
  # inside mod_practice resets state$pos to 1. testServer flushes reactives
  # after setInputs; we trigger a harmless input change to force the flush.
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(2L, 3L))
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(answer = "1", check = 1L)
        session$setInputs(next_item = 1L)
        expect_equal(state$pos, 2L)

        # Change practice_ids, then flush via setInputs so the observer fires
        practice_ids(c(5L, 6L))
        session$setInputs(answer = NULL) # flush; triggers observer reset
        expect_equal(state$pos, 1L)
      }
    )
  })
})

# ── mod_dashboard_server ──────────────────────────────────────────────────────

test_that("dashboard: user_exists is FALSE with no DB data", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        expect_false(isTRUE(user_exists()))
      }
    )
  })
})

test_that("dashboard: user_exists becomes TRUE after a response is written", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        expect_false(isTRUE(user_exists()))

        # Simulate a DB write from the practice module
        row <- build_response_row(
          as.list(make_data_item()[1, ]),
          answer_idx = 1L,
          user_id = "testuser",
          session_token = "s"
        )
        db_write_response("testuser", row, DB_USERS())
        write_trigger(1L) # trigger dashboard invalidation

        expect_true(isTRUE(user_exists()))
      }
    )
  })
})

test_that("dashboard: first_attempts de-duplicates repeated items", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    # Write item 1 twice (one repeat)
    db_path <- make_user_db(
      "testuser",
      item_ids = c(1L, 1L, 2L),
      correct = c(TRUE, FALSE, TRUE)
    )
    file.copy(db_path, DB_USERS())

    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        ud <- user_data()
        fa <- first_attempts()

        expect_equal(nrow(ud), 3L) # all attempts
        expect_equal(nrow(fa), 2L) # only first per item
        # First attempt at item 1 was correct
        expect_true(fa$bool_correct[fa$id_item == 1L])
      }
    )
  })
})

test_that("dashboard: competency is NULL with no user data", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        expect_null(competency())
      }
    )
  })
})

test_that("dashboard: write_trigger invalidates user_data cache", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        expect_equal(nrow(user_data()), 0L)

        # Write a row to DB without going through the practice module
        row <- build_response_row(
          as.list(make_data_item()[1, ]),
          answer_idx = 1L,
          user_id = "testuser",
          session_token = "s"
        )
        db_write_response("testuser", row, DB_USERS())

        # Dashboard has NOT re-read yet — write_trigger not incremented
        expect_equal(nrow(user_data()), 0L)

        # Now increment write_trigger (as practice module would)
        write_trigger(1L)
        expect_equal(nrow(user_data()), 1L)
      }
    )
  })
})
