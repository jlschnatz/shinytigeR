# App-level integration tests via testServer(app_server, ...). Namespaced
# module inputs are addressed as "<module_id>-<input_id>" (Shiny's standard
# nested-module testServer convention) — e.g. "dashboard_1-refresh_ability".

make_server_test_dbs <- function(db_dir) {
  creds_path <- file.path(db_dir, "db_credentials.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), creds_path)
  DBI::dbWriteTable(con, "credentials_db", data.frame(
    user_name = "test",
    password_hashed = sodium::password_store("test123"),
    permissions = "standard",
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con)

  item_path <- file.path(db_dir, "db_item.sqlite")
  con2 <- DBI::dbConnect(RSQLite::SQLite(), item_path)
  DBI::dbWriteTable(con2, "item_db", data.frame(
    id_item = 1:2,
    learning_area = LEARNING_AREA_LEVELS[1],
    type_item = "content",
    bloom_taxonomy = "knowledge",
    stimulus_text = "Q",
    stimulus_image = NA_character_,
    answeroption_01 = "A",
    answeroption_02 = "B",
    answeroption_03 = NA_character_,
    answeroption_04 = NA_character_,
    answeroption_05 = NA_character_,
    answeroption_06 = NA_character_,
    if_answeroption_01 = NA_character_,
    if_answeroption_02 = NA_character_,
    if_answeroption_03 = NA_character_,
    if_answeroption_04 = NA_character_,
    if_answeroption_05 = NA_character_,
    if_answeroption_06 = NA_character_,
    answer_correct = 1L,
    type_answer = "text",
    irt_discr = 1.0,
    irt_diff = 0.0,
    stringsAsFactors = FALSE
  ))
  DBI::dbDisconnect(con2)
}

response_row <- function(item_id, dt) {
  data.frame(
    id_user = "test",
    id_session = "sess",
    id_date = as.integer(Sys.Date()),
    id_datetime = dt,
    id_item = item_id,
    learning_area = LEARNING_AREA_LEVELS[1],
    selected_option = 1L,
    answer_correct = 1L,
    bool_correct = TRUE,
    skipped = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("login's silent ability catch-up does not block the dashboard button for new in-session practice", {
  # Regression test for a real bug: the login-time auto-catch-up (meant only
  # to clear out STALE data from a prior session) was also latching
  # ability_computed_this_session, which then blocked the refresh button for
  # data answered THIS session — "Fähigkeitsverlauf aktualisieren" incorrectly
  # reported "up to date" right after practicing. See the "Bug, hit and fixed
  # once" callout in CLAUDE.md's R/mod_dashboard.R section.
  db_dir <- tempfile("tiger_test")
  dir.create(db_dir)

  withr::with_envvar(list(TIGER_DB_DIR = db_dir), {
    make_server_test_dbs(db_dir)

    # Stale, unprocessed practice from "a prior session" — no ability
    # snapshot exists yet, so login's auto-catch-up will fire for it.
    db_write_response("test", response_row(1L, dt = 1000L), DB_USERS())

    shiny::testServer(app_server, {
      session$setInputs(
        "login-user_name" = "test",
        "login-password" = "test123",
        "login-button" = 1
      )
      session$flushReact()

      # Auto-catch-up fired: one batch, item 1 only.
      snapshot <- db_get_ability("test")
      expect_equal(nrow(snapshot), length(LEARNING_AREA_LEVELS))
      area1 <- LEARNING_AREA_LEVELS[1]
      expect_equal(snapshot$n_items[snapshot$learning_area == area1], 1L)

      # New practice THIS session, driven through the real selector + practice
      # UI (not a direct db_write_response()) so write_trigger actually fires
      # the same way it did when the bug was reported — a direct DB write
      # would bypass write_trigger and mask the very cache-invalidation path
      # the bug lived in.
      session$setInputs("selector_1-cell_1_1" = TRUE)
      session$setInputs("selector_1-submit" = 1L)
      session$flushReact()

      click <- 1L
      for (i in 1:2) {
        session$setInputs("practice_1-answer" = "1")
        session$setInputs("practice_1-check" = click)
        session$flushReact()
        click <- click + 1L
        session$setInputs("practice_1-next_item" = click)
        session$flushReact()
        click <- click + 1L
      }

      # New practice must still be refreshable, not blocked by the silent
      # catch-up that ran at login.
      session$setInputs("dashboard_1-refresh_ability" = 1)
      session$flushReact()

      updated <- db_get_ability("test")
      latest_batch <- updated[updated$computed_at == max(updated$computed_at), ]
      # Both items now feed the estimate — proves the button actually fired.
      expect_equal(latest_batch$n_items[latest_batch$learning_area == area1], 2L)
    })
  })
})
