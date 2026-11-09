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

test_that("selector: only_new still offers items that were only skipped", {
  data_item <- make_data_item()
  # item 1 answered, item 2 only skipped -> 2 and 3 count as new (same as the
  # dashboard's "noch offen")
  db_path <- make_user_db("testuser", item_ids = 1:2)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "UPDATE testuser SET skipped = 1, bool_correct = NULL WHERE id_item = 2")
  DBI::dbDisconnect(con)

  withr::with_envvar(list(TIGER_DB_DIR = dirname(db_path)), {
    file.rename(db_path, DB_USERS())

    testServer(
      mod_selector_server,
      args = list(
        data_item = data_item,
        practice_ids = reactiveVal(NULL),
        inspect_id = reactiveVal(NULL),
        credentials = fake_credentials(),
        write_trigger = reactiveVal(0L)
      ),
      {
        do.call(
          session$setInputs,
          selector_cell_inputs(area_vals = "Regression", only_new = TRUE)
        )
        expect_setequal(filtered_items()$id_item, c(2L, 3L))
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
        session$setInputs(`answer_mc-answer` = "1", check = 1L)

        expect_true(state$checked)
        expect_equal(state$result$answer_idx, 1L)

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

test_that("practice: outputs are revealed by item, with the math rendered on the server", {
  # www/js/practice.js reveals a new item's .reveal-output outputs together,
  # by their data-reveal-key; the formulas arrive as finished KaTeX HTML
  # (render_math()), also in the feedback after the check.
  data_item <- make_data_item()
  data_item$if_answeroption_01 <- "Richtig, $B_1 = b_1 \\cdot \\frac{s_x}{s_y}$."

  ui <- as.character(mod_practice_ui("p"))
  for (out in c("p-item_stimulus", "p-feedback", "p-answer_mc-answer_ui")) {
    expect_match(ui, sprintf('class="[^"]*reveal-output[^"]*" id="%s"', out))
  }

  withr::with_envvar(list(TIGER_DB_DIR = tempdir()), {
    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = reactiveVal(1L),
        credentials = fake_credentials(),
        write_trigger = reactiveVal(0L)
      ),
      {
        key <- sprintf('data-reveal-key="%s"', data_item$id_item[1])
        expect_match(output$item_stimulus$html, key, fixed = TRUE)
        expect_match(output$`answer_mc-answer_ui`$html, key, fixed = TRUE)

        options_before <- output$`answer_mc-answer_ui`$html
        session$setInputs(`answer_mc-answer` = "1", check = 1L)
        html <- output$feedback$html
        # the options aren't re-rendered on the check; the result is marked
        # on them in the browser
        expect_identical(output$`answer_mc-answer_ui`$html, options_before)
        expect_match(html, "feedback-card", fixed = TRUE)
        expect_match(html, key, fixed = TRUE)
        expect_match(html, 'class="katex"', fixed = TRUE)
        expect_no_match(html, "\\(B_1", fixed = TRUE)
        expect_no_match(html, "katex-error", fixed = TRUE)
        # the options only show the coloring, the card is the feedback output's
        expect_false(grepl("feedback-card", output$`answer_mc-answer_ui`$html, fixed = TRUE))
      }
    )
  })
})

test_that("practice_feedback_card covers MC text, skip, numeric and unmatched", {
  mc <- make_data_item()[1, ]
  mc$if_answeroption_01 <- "Gut gemacht."
  html <- function(x) as.character(x)
  expect_match(html(practice_feedback_card(mc, list(category = "correct", answer_idx = 1L))), "Gut gemacht.")
  expect_match(html(practice_feedback_card(mc, list(category = "skip", answer_idx = NA_integer_))), "Übersprungen")
  # an MC item without feedback texts: no card
  mc[grep("^if_answeroption_", names(mc))] <- NA
  expect_null(practice_feedback_card(mc, list(category = "correct", answer_idx = 1L)))
  num <- make_numeric_item(90001L, correct_value = 5, distractors = c(4.5))
  expect_match(html(practice_feedback_card(num, list(category = "unmatched", answer_idx = NA_integer_))), "Antwort nicht erkannt")
  # numeric: the header even without feedback text
  num$if_answeroption_01 <- NA
  expect_match(html(practice_feedback_card(num, list(category = "correct", answer_idx = 1L))), "Richtig!")
  expect_null(practice_feedback_card(mc, NULL))
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
        session$setInputs(`answer_mc-answer` = "1", check = 1L)
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
        session$setInputs(`answer_mc-answer` = "1", check = 1L)
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

test_that("practice: MC item — skip button records skipped = TRUE", {
  data_item <- make_data_item()
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
        # No option selected — skipping doesn't need one
        session$setInputs(skip = 1L)

        expect_true(state$checked)
        expect_equal(state$result$category, "skip")
        expect_equal(write_trigger(), 1L)

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_equal(nrow(ud), 1L)
        expect_true(as.logical(ud$skipped))
        expect_true(is.na(ud$selected_option))
        expect_true(is.na(ud$bool_correct))
      }
    )
  })
})

test_that("practice: MC item — last option is an ordinary answer, not a skip", {
  data_item <- make_data_item() # 3 options, option 1 correct
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
        session$setInputs(`answer_mc-answer` = "3", check = 1L)

        expect_equal(state$result$category, "incorrect")
        ud <- db_get_userdata("testuser", DB_USERS())
        expect_false(as.logical(ud$skipped))
        expect_false(as.logical(ud$bool_correct))
        expect_equal(ud$selected_option, 3L)
      }
    )
  })
})

test_that("practice: skip button is ignored once the item is checked", {
  data_item <- make_data_item()
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
        session$setInputs(`answer_mc-answer` = "1", check = 1L)
        session$setInputs(skip = 1L)

        expect_equal(state$result$category, "correct")
        ud <- db_get_userdata("testuser", DB_USERS())
        expect_equal(nrow(ud), 1L) # no second (skip) row
      }
    )
  })
})

test_that("practice: legacy user table with an old MC skip row keeps working", {
  # Old accounts have per-user tables from before the skip button: no
  # typed_value column, answer_correct INTEGER, and MC skips recorded as
  # selected_option = <index of the former skip option> with skipped = 1.
  data_item <- make_data_item()
  practice_ids <- reactiveVal(c(3L))
  write_trigger <- reactiveVal(0L)
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_USERS())
    DBI::dbExecute(con, paste(
      "CREATE TABLE testuser (id_user TEXT, id_session TEXT, id_date INTEGER,",
      "id_datetime INTEGER, id_item INTEGER, learning_area TEXT,",
      "selected_option INTEGER, answer_correct INTEGER, bool_correct INTEGER,",
      "skipped INTEGER)"
    ))
    DBI::dbExecute(con, paste(
      "INSERT INTO testuser VALUES",
      "('testuser', 's', 1, 1, 1, 'Regression', 1, 1, 1, 0),",
      "('testuser', 's', 1, 2, 2, 'Regression', 3, 1, NULL, 1)"
    ))
    DBI::dbDisconnect(con)

    testServer(
      mod_practice_server,
      args = list(
        data_item = data_item,
        practice_ids = practice_ids,
        credentials = fake_credentials(),
        write_trigger = write_trigger
      ),
      {
        session$setInputs(skip = 1L) # new-style skip on item 3
      }
    )

    ud <- db_get_userdata("testuser", DB_USERS())
    expect_equal(nrow(ud), 3L)
    # Old and new skips are both counted via `skipped` (as the dashboard does)
    expect_equal(as.logical(ud$skipped), c(FALSE, TRUE, TRUE))
    expect_equal(sum(ud$skipped == TRUE, na.rm = TRUE), 2L)
    expect_equal(ud$selected_option, c(1L, 3L, NA))
    # ...and both are excluded from IRT scoring
    comp <- estimate_competency(ud, data_item)
    expect_equal(comp$n_items[comp$learning_area == "Regression"], 1L)
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
        session$setInputs(`answer_mc-answer` = "1", check = 1L)
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

# ── mod_practice_server: numeric items ──────────────────────────────────────────

test_that("practice: numeric item — exact match is scored correct and stores typed_value", {
  data_item <- make_numeric_item(id_item = 200L, correct_value = 5, distractors = c(4.5, 6, 20))
  practice_ids <- reactiveVal(c(200L))
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
        session$setInputs(`answer_num-num_value` = "5", check = 1L)

        expect_true(state$checked)
        expect_equal(state$result$category, "correct")

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_equal(nrow(ud), 1L)
        expect_equal(as.logical(ud$bool_correct), TRUE)
        expect_equal(ud$typed_value, 5)
      }
    )
  })
})

test_that("practice: numeric item — comma decimal is parsed and matched", {
  data_item <- make_numeric_item(id_item = 200L, correct_value = 5, distractors = c(4.5, 6, 20))
  practice_ids <- reactiveVal(c(200L))
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
        session$setInputs(`answer_num-num_value` = "4,5", check = 1L)

        expect_equal(state$result$category, "incorrect")
        expect_equal(state$result$answer_idx, 2L)

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_equal(ud$typed_value, 4.5)
        expect_false(isTRUE(as.logical(ud$bool_correct)))
      }
    )
  })
})

test_that("practice: numeric item — an unmatched value is stored as wrong, not skipped", {
  data_item <- make_numeric_item(id_item = 200L, correct_value = 5, distractors = c(4.5, 6, 20))
  practice_ids <- reactiveVal(c(200L))
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
        session$setInputs(`answer_num-num_value` = "999", check = 1L)

        expect_equal(state$result$category, "unmatched")

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_false(as.logical(ud$bool_correct))
        expect_false(as.logical(ud$skipped))
        expect_equal(ud$typed_value, 999)
        # and the feedback card says it's wrong
        expect_match(output$feedback$html, "Deine Antwort ist leider falsch.", fixed = TRUE)
      }
    )
  })
})

test_that("practice: numeric item — second correct option within its range is scored correct", {
  data_item <- make_numeric_item(
    id_item = 200L, correct_value = 6.67, distractors = c(5, 20),
    lower = c(6.66, NA, NA), upper = c(6.67, NA, NA),
    answer_correct = "1;2"
  )
  practice_ids <- reactiveVal(c(200L))
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
        session$setInputs(`answer_num-num_value` = "5", check = 1L)

        expect_equal(state$result$category, "correct")
        expect_equal(state$result$answer_idx, 2L)

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_true(as.logical(ud$bool_correct))
        expect_equal(ud$selected_option, 2L)
        expect_equal(ud$answer_correct, "1;2")
      }
    )
  })
})

test_that("practice: numeric item — skip button records skipped = TRUE", {
  data_item <- make_numeric_item(id_item = 200L, correct_value = 5, distractors = c(4.5, 6, 20))
  practice_ids <- reactiveVal(c(200L))
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
        session$setInputs(skip = 1L)

        expect_true(state$checked)
        expect_equal(state$result$category, "skip")

        ud <- db_get_userdata("testuser", DB_USERS())
        expect_true(as.logical(ud$skipped))
        expect_true(is.na(ud$bool_correct))
        expect_true(is.na(ud$typed_value))
      }
    )
  })
})

# ── mod_inspect_server ────────────────────────────────────────────────────────

test_that("inspect: MC item shows every option, feedback only after reveal", {
  data_item <- make_data_item() # 3 options, option 1 correct
  inspect_id <- reactiveVal(NULL)

  testServer(
    mod_inspect_server,
    args = list(data_item = data_item, inspect_id = inspect_id),
    {
      inspect_id(1L)
      session$flushReact()
      html_before <- output$item_answers$html
      expect_true(grepl("Richtig", html_before))
      expect_true(grepl("Falsch", html_before))
      expect_true(grepl("Auch falsch", html_before)) # last option is not dropped
      expect_false(grepl("Super!", html_before))

      session$setInputs(reveal = 1L)
      html_after <- output$item_answers$html
      expect_true(grepl("Super!", html_after)) # feedback for the correct option
      expect_true(grepl("Ebenfalls falsch.", html_after, fixed = TRUE))
    }
  )
})

test_that("inspect: numeric item hides the answer before reveal and shows only correct-value feedback after", {
  data_item <- make_numeric_item(id_item = 200L, correct_value = 5, distractors = c(4.5, 6, 20))
  inspect_id <- reactiveVal(NULL)

  testServer(
    mod_inspect_server,
    args = list(data_item = data_item, inspect_id = inspect_id),
    {
      inspect_id(200L)
      session$flushReact()
      html_before <- output$item_answers$html
      expect_false(grepl("Feedback 1", html_before))
      expect_false(grepl("Richtige Antwort", html_before))

      session$setInputs(reveal = 1L)
      html_after <- output$item_answers$html
      expect_true(grepl("Richtige Antwort", html_after))
      expect_true(grepl("Feedback 1", html_after)) # correct distractor's own feedback
      # Distractor feedback for the other (incorrect) values must stay hidden
      expect_false(grepl("Feedback 2", html_after))
      expect_false(grepl("Feedback 3", html_after))
      expect_false(grepl("Feedback 4", html_after))
    }
  )
})

test_that("inspect: numeric item with several correct options lists each, with its range", {
  data_item <- make_numeric_item(
    id_item = 200L, correct_value = 6.67, distractors = c(5, 20),
    lower = c(6.66, NA, NA), upper = c(6.67, NA, NA),
    answer_correct = "1;2"
  )
  inspect_id <- reactiveVal(NULL)

  testServer(
    mod_inspect_server,
    args = list(data_item = data_item, inspect_id = inspect_id),
    {
      inspect_id(200L)
      session$flushReact()
      session$setInputs(reveal = 1L)
      html <- output$item_answers$html
      expect_true(grepl("Richtige Antworten", html))
      expect_true(grepl("6.67 (akzeptiert: 6.66 bis 6.67)", html, fixed = TRUE))
      expect_true(grepl("Feedback 1", html))
      expect_true(grepl("Feedback 2", html))
      expect_false(grepl("Feedback 3", html)) # the incorrect option stays hidden
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

test_that("dashboard: 'nochmal üben' starts a queue of the open (wrong, unsolved) items", {
  data_item <- make_data_item()
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)
  # items 1-4 answered: 1 right, 2-4 wrong -> 2-4 are open. 2 and 3 were
  # answered three days ago, 4 just now (still in its time-out)
  ids <- data_item$id_item[1:4]
  path <- make_user_db(item_ids = ids, correct = c(TRUE, FALSE, FALSE, FALSE))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, sprintf(
    "UPDATE testuser SET id_datetime = id_datetime - 3 * 86400 WHERE id_item IN (%d, %d, %d)",
    ids[1], ids[2], ids[3]
  ))
  DBI::dbDisconnect(con)
  file.copy(path, file.path(db_dir, "db_user.sqlite"))
  queued <- NULL

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = reactiveVal(0L),
        ability_computed_this_session = reactiveVal(FALSE),
        retry_items = function(ids) queued <<- ids
      ),
      {
        session$setInputs(retry_open = 1)
        expect_setequal(queued, ids[2:3])
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

test_that("dashboard: user_data keeps every attempt, also repeats", {
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

        expect_equal(nrow(ud), 3L) # all attempts, the repeat of item 1 too
        expect_equal(ud$bool_correct[ud$id_item == 1L], c(TRUE, FALSE))
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

test_that("dashboard: renders a trajectory panel for each area with saved estimates", {
  data_item <- make_data_item()
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    user_path <- make_user_db("testuser", item_ids = 1:2, areas = c("Regression", "Regression"))
    file.copy(user_path, DB_USERS())
    theta <- setNames(rep(NA_real_, length(LEARNING_AREA_LEVELS)), LEARNING_AREA_LEVELS)
    theta["Regression"] <- 0.4
    ability_path <- make_ability_db("testuser", computed_at = 1700000000L, theta = theta)
    con <- DBI::dbConnect(RSQLite::SQLite(), ability_path)
    theta["Regression"] <- 0.9
    DBI::dbAppendTable(con, "testuser", data.frame(
      id_user = "testuser", id_session = "sess", computed_at = 1700086400L,
      learning_area = LEARNING_AREA_LEVELS, theta = unname(theta), n_items = 3L
    ))
    DBI::dbDisconnect(con)
    file.copy(ability_path, DB_ABILITY())

    testServer(
      mod_dashboard_server,
      args = list(
        data_item = data_item,
        credentials = fake_credentials(),
        write_trigger = reactiveVal(0L),
        ability_computed_this_session = reactiveVal(FALSE)
      ),
      {
        k <- match("Regression", LEARNING_AREA_LEVELS)
        html <- as.character(output$dash_content$html)
        expect_match(html, sprintf('id="%s" class="traj-plot"', session$ns(paste0("traj_", k))), fixed = TRUE)
        # the panel's spec follows its plot div
        spec <- regmatches(html, regexpr(sprintf(
          '(?<=id="%s" class="traj-plot"></div>\\s{0,20}<script type="application/json" class="traj-spec">).*?(?=</script>)',
          session$ns(paste0("traj_", k))
        ), html, perl = TRUE))
        fig <- jsonlite::fromJSON(spec, simplifyVector = FALSE)
        ys <- unlist(lapply(fig$data, function(tr) if (identical(tr$mode, "markers")) tr$y))
        expect_equal(ys, c(0.4, 0.9))
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

test_that("home: both start buttons call their navigation callbacks", {
  went <- character(0)
  testServer(
    mod_home_server,
    args = list(
      data_item = make_data_item(),
      credentials = fake_credentials(),
      go_train = function() went <<- c(went, "train"),
      go_progress = function() went <<- c(went, "progress")
    ),
    {
      session$setInputs(start = 1L)
      session$setInputs(progress = 1L)
      expect_equal(went, c("train", "progress"))
    }
  )
  html <- as.character(mod_home_ui("h"))
  expect_match(html, "Fortschritt ansehen", fixed = TRUE)
  expect_match(html, "btn-outline-primary", fixed = TRUE)
})
