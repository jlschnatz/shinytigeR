library(shiny)

# ── mod_selector_server ───────────────────────────────────────────────────────

test_that("selector: filtered_items respects area filter", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        inspect_id = reactiveVal(NULL),
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        # Select only Regression area, both types
        do.call(session$setInputs, selector_cell_inputs(area_vals = "Regression"))
        # 3 items belong to Regression
        expect_equal(nrow(filtered_items()), 3L)
      }
    )
  })
})

test_that("selector: filtered_items respects type filter", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        inspect_id = reactiveVal(NULL),
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        # Only content type across all areas
        do.call(session$setInputs, selector_cell_inputs(type_vals = "content"))
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
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        inspect_id = reactiveVal(NULL),
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        do.call(session$setInputs, selector_cell_inputs())
        n_items(3L)
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
  write_trigger <- reactiveVal(0L)

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        inspect_id = reactiveVal(NULL),
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        # Request more items than exist in the filtered set (only 3 in Regression)
        do.call(
          session$setInputs,
          selector_cell_inputs(area_vals = "Regression")
        )
        n_items(50L)
        session$setInputs(submit = 1L)
        expect_length(practice_ids(), 3L)
      }
    )
  })
})

test_that("selector: only_new excludes already-answered items", {
  data_item <- make_data_item()
  practice_ids <- reactiveVal(NULL)
  write_trigger <- reactiveVal(0L)

  # Pre-populate DB: items 1 and 2 already answered by testuser
  db_path <- make_user_db("testuser", item_ids = 1:2)

  withr::with_envvar(list(TIGER_DB_DIR = dirname(db_path)), {
    file.rename(db_path, DB_USERS())

    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        inspect_id = reactiveVal(NULL),
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        # Regression area (items 1, 2, 3); items 1 & 2 already answered
        do.call(
          session$setInputs,
          selector_cell_inputs(area_vals = "Regression", only_new = TRUE)
        )
        # Only item 3 remains
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

# ── mod_inspect_server ────────────────────────────────────────────────────────

test_that("inspect: skip option is excluded before and after reveal", {
  data_item <- make_data_item() # 3 options per item; option 3 = skip
  inspect_id <- reactiveVal(NULL)

  testServer(
    mod_inspect_server,
    args = list(data_item = data_item, inspect_id = inspect_id),
    {
      inspect_id(1L)
      session$flushReact()
      html_before <- output$item_answers$html
      expect_false(grepl("berspringen", html_before))
      expect_true(grepl("Richtig", html_before))
      expect_true(grepl("Falsch", html_before))

      session$setInputs(reveal = 1L)
      html_after <- output$item_answers$html
      expect_false(grepl("berspringen", html_after))
      expect_true(grepl("Super!", html_after)) # feedback for the correct option
    }
  )
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
        write_trigger = write_trigger,
        ability_computed_this_session = reactiveVal(FALSE)
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
        write_trigger = write_trigger,
        ability_computed_this_session = reactiveVal(FALSE)
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
        write_trigger = write_trigger,
        ability_computed_this_session = reactiveVal(FALSE)
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
        write_trigger = write_trigger,
        ability_computed_this_session = reactiveVal(FALSE)
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
        write_trigger = write_trigger,
        ability_computed_this_session = reactiveVal(FALSE)
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

test_that("dashboard: competency reads from the persisted ability snapshot", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    theta <- setNames(rep(NA_real_, length(LEARNING_AREA_LEVELS)), LEARNING_AREA_LEVELS)
    theta["Regression"] <- 1.5
    ability_path <- make_ability_db("testuser", theta = theta)
    file.copy(ability_path, DB_ABILITY())

    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger,
        ability_computed_this_session = reactiveVal(FALSE)
      ),
      {
        comp <- competency()
        expect_false(is.null(comp))
        expect_equal(comp$theta[comp$learning_area == "Regression"], 1.5)
      }
    )
  })
})

test_that("dashboard: refresh button is blocked once already computed this session", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    user_path <- make_user_db("testuser", item_ids = 1:2, areas = c("Regression", "Regression"))
    file.copy(user_path, DB_USERS())

    ability_flag <- reactiveVal(TRUE) # already computed this session
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger,
        ability_computed_this_session = ability_flag
      ),
      {
        expect_false(can_refresh_ability())
      }
    )
  })
})

test_that("dashboard: refresh button computes and saves ability, then latches the session flag", {
  data_item <- make_data_item()
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    user_path <- make_user_db("testuser", item_ids = 1:2, areas = c("Regression", "Regression"))
    file.copy(user_path, DB_USERS())

    ability_flag <- reactiveVal(FALSE)
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = write_trigger,
        ability_computed_this_session = ability_flag
      ),
      {
        expect_true(can_refresh_ability())
        expect_null(competency())

        session$setInputs(refresh_ability = 1L)

        expect_true(isTRUE(ability_flag()))
        expect_false(is.null(competency()))
        expect_false(can_refresh_ability())
      }
    )
  })
})
