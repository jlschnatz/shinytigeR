# Shared fixtures for testServer() module tests

# Minimal item data.frame with IRT params, two areas, two types
make_data_item <- function() {
  data.frame(
    id_item = 1:6,
    learning_area = factor(
      c(rep("Regression", 3), rep("Poweranalyse", 3)),
      levels = LEARNING_AREA_LEVELS
    ),
    type_item = rep(c("content", "coding", "content"), 2),
    bloom_taxonomy = rep(c("knowledge", "application", "comprehension"), 2),
    stimulus_text = paste0("Frage ", 1:6),
    stimulus_image = NA_character_,
    answeroption_01 = "Richtig",
    answeroption_02 = "Falsch",
    answeroption_03 = "Überspringen",
    answeroption_04 = NA_character_,
    answeroption_05 = NA_character_,
    answeroption_06 = NA_character_,
    if_answeroption_01 = "Super!",
    if_answeroption_02 = "Leider falsch.",
    if_answeroption_03 = "Übersprungen.",
    if_answeroption_04 = NA_character_,
    if_answeroption_05 = NA_character_,
    if_answeroption_06 = NA_character_,
    answer_correct = 1L,
    type_answer = "text",
    irt_discr = 1.0,
    irt_diff = 0.0,
    stringsAsFactors = FALSE
  )
}

# Fake credentials reactive — returns a logged-in user
fake_credentials <- function(user = "testuser") {
  reactive(list(
    user_auth = TRUE,
    info = data.frame(user_name = user, stringsAsFactors = FALSE)
  ))
}

# Build a temp SQLite user DB pre-populated with response rows
make_user_db <- function(
  user = "testuser",
  item_ids = 1:3,
  n_items = length(item_ids),
  correct = rep(TRUE, n_items),
  areas = rep("Regression", n_items)
) {
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- data.frame(
    id_user = user,
    id_session = "sess",
    id_date = as.integer(Sys.Date()),
    id_datetime = as.integer(Sys.time()) + seq_len(n_items),
    id_item = as.integer(item_ids),
    learning_area = areas,
    selected_option = ifelse(correct, 1L, 2L),
    answer_correct = 1L,
    bool_correct = as.integer(correct),
    skipped = 0L,
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, user, rows)
  path
}
