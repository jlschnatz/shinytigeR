# Registration DB tests use temporary SQLite files — never the real credentials DB.

make_creds_db <- function() {
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(
    con,
    "credentials_db",
    data.frame(
      user_name = "existing_user",
      password_hashed = sodium::password_store("existingpass"),
      stringsAsFactors = FALSE
    )
  )
  path
}

# ── db_username_exists ────────────────────────────────────────────────────────

test_that("db_username_exists returns TRUE for a user that exists", {
  path <- make_creds_db()
  expect_true(db_username_exists("existing_user", path))
})

test_that("db_username_exists returns FALSE for a user that does not exist", {
  path <- make_creds_db()
  expect_false(db_username_exists("new_user", path))
})

test_that("db_username_exists is case-sensitive", {
  path <- make_creds_db()
  expect_false(db_username_exists("Existing_User", path))
  expect_false(db_username_exists("EXISTING_USER", path))
})

# ── db_register_user — happy path ─────────────────────────────────────────────

test_that("db_register_user inserts a new user", {
  path <- make_creds_db()
  db_register_user("new_user", "securepassword1", path)
  expect_true(db_username_exists("new_user", path))
})

test_that("db_register_user stores a sodium hash, not the plaintext password", {
  path <- make_creds_db()
  db_register_user("new_user", "securepassword1", path)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  row <- DBI::dbGetQuery(
    con,
    "SELECT password_hashed FROM credentials_db WHERE user_name = 'new_user'"
  )
  expect_false(grepl("securepassword1", row$password_hashed))
  expect_true(sodium::password_verify(row$password_hashed, "securepassword1"))
})

test_that("db_register_user does not disturb existing users", {
  path <- make_creds_db()
  db_register_user("new_user", "securepassword1", path)

  expect_true(db_username_exists("existing_user", path))
  expect_true(db_username_exists("new_user", path))
})

test_that("db_register_user allows multiple distinct users", {
  path <- make_creds_db()
  db_register_user("alice", "password_alice1", path)
  db_register_user("bob",   "password_bob123", path)

  expect_true(db_username_exists("alice", path))
  expect_true(db_username_exists("bob", path))
})

# ── db_register_user — duplicate prevention ───────────────────────────────────

test_that("db_register_user errors with 'username_taken' for an existing username", {
  path <- make_creds_db()
  expect_error(
    db_register_user("existing_user", "newpassword1", path),
    regexp = "username_taken",
    fixed = TRUE
  )
})

test_that("db_register_user does not overwrite the password of an existing user", {
  path <- make_creds_db()
  tryCatch(
    db_register_user("existing_user", "attackerpassword", path),
    error = function(e) NULL
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  row <- DBI::dbGetQuery(
    con,
    "SELECT password_hashed FROM credentials_db WHERE user_name = 'existing_user'"
  )
  # The original password must still verify; the attacker's must not
  expect_true(sodium::password_verify(row$password_hashed, "existingpass"))
  expect_false(sodium::password_verify(row$password_hashed, "attackerpassword"))
})

test_that("db_register_user duplicate leaves row count unchanged", {
  path <- make_creds_db()
  tryCatch(
    db_register_user("existing_user", "anotherpass1", path),
    error = function(e) NULL
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  n <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM credentials_db WHERE user_name = 'existing_user'"
  )$n
  expect_equal(n, 1L)
})

# ── Input validation (constants) ──────────────────────────────────────────────

test_that("REG_USERNAME_PATTERN accepts valid usernames", {
  valid <- c("alice", "max.mustermann", "user_123", "a-b", "ABC", "a.b-c_D9")
  for (u in valid) {
    expect_true(
      grepl(REG_USERNAME_PATTERN, u),
      info = paste("should be valid:", u)
    )
  }
})

test_that("REG_USERNAME_PATTERN rejects invalid usernames", {
  invalid <- c(
    "ab",              # too short (< 3)
    "",                # empty
    "user name",       # space
    "user@domain",     # @ not allowed
    "user/path",       # slash not allowed
    "'; DROP TABLE--", # SQL injection attempt
    paste(rep("a", 31), collapse = "") # too long (> 30)
  )
  for (u in invalid) {
    expect_false(
      grepl(REG_USERNAME_PATTERN, u),
      info = paste("should be invalid:", u)
    )
  }
})

test_that("REG_PW_MIN_LENGTH is at least 8", {
  expect_gte(REG_PW_MIN_LENGTH, 8L)
})

test_that("REG_MAX_ATTEMPTS is a positive integer", {
  expect_true(is.integer(REG_MAX_ATTEMPTS))
  expect_gt(REG_MAX_ATTEMPTS, 0L)
})

# ── Semester code env var ─────────────────────────────────────────────────────

test_that("TIGER_REG_CODE env var is readable and returns empty string when unset", {
  withr::with_envvar(c(TIGER_REG_CODE = ""), {
    expect_equal(Sys.getenv("TIGER_REG_CODE"), "")
    expect_false(nzchar(Sys.getenv("TIGER_REG_CODE")))
  })
})

test_that("TIGER_REG_CODE env var can be set for testing", {
  withr::with_envvar(c(TIGER_REG_CODE = "tiger2025"), {
    expect_equal(Sys.getenv("TIGER_REG_CODE"), "tiger2025")
  })
})
