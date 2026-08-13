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
    answer_mode = "mc",
    stringsAsFactors = FALSE
  )
}

# Single numeric-item row (answer_mode = "num") — distractor values live in
# answeroption_0X same as MC, but as numeric strings and with no trailing
# "Überspringen" slot (numeric items skip via a dedicated button, not a last
# radio option). Correct value is answeroption_01 (answer_correct = 1L).
make_numeric_item <- function(id_item = 200L, correct_value = 5, distractors = c(4.5, 6, 20)) {
  values <- c(correct_value, distractors)
  n <- length(values)
  opts <- rep(NA_character_, 6)
  fbs <- rep(NA_character_, 6)
  opts[seq_len(n)] <- as.character(values)
  fbs[seq_len(n)] <- paste0("Feedback ", seq_len(n))
  data.frame(
    id_item = id_item,
    learning_area = factor("Deskriptivstatistik", levels = LEARNING_AREA_LEVELS),
    type_item = "content",
    bloom_taxonomy = "application",
    stimulus_text = "Berechne den Mittelwert.",
    stimulus_image = NA_character_,
    answeroption_01 = opts[1],
    answeroption_02 = opts[2],
    answeroption_03 = opts[3],
    answeroption_04 = opts[4],
    answeroption_05 = opts[5],
    answeroption_06 = opts[6],
    if_answeroption_01 = fbs[1],
    if_answeroption_02 = fbs[2],
    if_answeroption_03 = fbs[3],
    if_answeroption_04 = fbs[4],
    if_answeroption_05 = fbs[5],
    if_answeroption_06 = fbs[6],
    answer_correct = 1L,
    type_answer = NA_character_,
    irt_discr = 1.0,
    irt_diff = 0.0,
    answer_mode = "num",
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

# Build a named list of cell_i_j inputs for mod_selector_server.
# Pass area_vals / type_vals to select specific subsets; NULL means all.
# n_items is no longer a bindable input (stepper/presets drive an internal
# reactiveVal) — set it directly in the test body via `n_items(<value>)`,
# which testServer's expr can call since it runs in the module's environment.
selector_cell_inputs <- function(
  area_vals  = unname(LEARNING_AREA_LABELS),
  type_vals  = unname(ITEM_TYPE_LABELS),
  only_new   = FALSE
) {
  all_areas <- unname(LEARNING_AREA_LABELS)
  all_types <- unname(ITEM_TYPE_LABELS)
  inputs <- list(only_new = only_new)
  for (i in seq_along(all_types)) {
    for (j in seq_along(all_areas)) {
      key <- paste0("cell_", i, "_", j)
      inputs[[key]] <- all_types[i] %in% type_vals && all_areas[j] %in% area_vals
    }
  }
  inputs
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
