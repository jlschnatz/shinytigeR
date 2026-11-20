# Fixtures for the AI feedback tests (test-ai-feedback.R, test-ai-feedback-card.R).
# The model is never called: mock_models() replaces ai_query_model().


# A snapshot shaped like latest_ability() (all 7 areas, the dashboard's input)
make_comp <- function(theta, se = rep(0.4, 7), n_items = rep(10L, 7), computed_at = 1000L) {
  data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = theta, se = se, n_items = as.integer(n_items),
    n_provisional = 0L, computed_at = computed_at,
    stringsAsFactors = FALSE
  )
}

# Made-up sources: for every area one per learning function, covering all
# levels, plus one only for levels 6-7
make_lookup <- function() {
  fns <- c("Einstieg", "Vertiefung", "Anwendung", "Nachschlagen")
  rows <- list()
  for (k in seq_along(AI_LOOKUP_COLUMNS)) {
    for (j in seq_along(fns)) {
      rows[[length(rows) + 1L]] <- data.frame(
        id = sprintf("s%d%d", k, j), display_name = sprintf("Quelle %d.%d", k, j),
        link = sprintf("https://example.org/%d/%d", k, j), resource_type = "Video",
        competence_min = 1, competence_max = if (j == 4L) 2 else 7,
        learning_function = fns[j], difficulty = "Intuitiv", effort = "Kurz",
        stringsAsFactors = FALSE
      )
    }
    rows[[length(rows) + 1L]] <- data.frame(
      id = sprintf("s%d9", k), display_name = sprintf("Profi %d", k),
      link = sprintf("https://example.org/%d/9", k), resource_type = "Lehrbuch",
      competence_min = 6, competence_max = 7, learning_function = "Anwendung",
      difficulty = "Formal", effort = "Lang", stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, rows)
  area <- as.integer(substr(df$id, 2, 2))
  for (k in seq_along(AI_LOOKUP_COLUMNS)) df[[AI_LOOKUP_COLUMNS[[k]]]] <- area == k
  df
}

# A DB folder with config, source list and key; returns its path
local_ai_dir <- function(cfg = list(enabled = TRUE, models = c("m1", "m2")),
                         lookup = make_lookup(), env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(TIGER_DB_DIR = dir, TIGER_LLM_KEY = "test-key", .local_envir = env)
  if (!is.null(cfg)) jsonlite::write_json(cfg, file.path(dir, AI_CONFIG_FILE), auto_unbox = TRUE)
  if (!is.null(lookup)) utils::write.csv(lookup, file.path(dir, AI_LOOKUP_FILE), row.names = FALSE)
  dir
}

# Runs the event loop until the promise settles; its value, or its error
wait_for <- function(p, timeout = 5) {
  state <- new.env()
  promises::then(p,
    onFulfilled = function(v) state$value <- list(v),
    onRejected = function(e) state$error <- e
  )
  end <- Sys.time() + timeout
  while (is.null(state$value) && is.null(state$error) && Sys.time() < end) later::run_now(0.01)
  if (!is.null(state$error)) stop(state$error)
  if (is.null(state$value)) stop("promise did not settle")
  state$value[[1]]
}

http_error <- function(status) {
  structure(
    class = c(paste0("httr2_http_", status), "httr2_http", "httr2_error", "error", "condition"),
    list(message = sprintf("HTTP %d.", status), call = NULL)
  )
}

# An answer as ellmer returns it: enum fields as factors
fake_answer <- function(req, summary = "Du bist auf einem guten Weg.", text = "Schau dir das an.") {
  focus <- req$focus$learning_area
  ids <- vapply(focus, function(a) req$resources$id[req$resources$topic == a][1], character(1))
  list(
    summary = summary,
    recommendations = data.frame(
      topic = factor(focus), resource_id = factor(unname(ids)),
      text = rep(text, length(focus)), stringsAsFactors = FALSE
    )
  )
}

fake_meta <- list(
  input_tokens = 2300, output_tokens = 410, cached_tokens = 0,
  reasoning_tokens = NA, finish_reason = "stop", response_model = "m", response_id = "x1"
)

# Mocks the model: `behaviour` is a named list, model -> function(req) that
# returns the answer data or calls stop() with a condition
mock_models <- function(behaviour, env = parent.frame()) {
  calls <- new.env()
  calls$log <- character(0)
  testthat::local_mocked_bindings(
    ai_query_model = function(model, req) {
      calls$log <- c(calls$log, model)
      res <- tryCatch(behaviour[[model]](req), error = function(e) e)
      if (inherits(res, "error")) promises::promise_reject(res)
      else promises::promise_resolve(list(data = res, meta = fake_meta))
    },
    ai_delay = function(seconds) promises::promise_resolve(TRUE),
    .env = env
  )
  calls
}

comp_mixed <- function(computed_at = 1000L) {
  make_comp(theta = c(1.2, 1.8, -0.6, 1.7, -1.8, 0.1, 1.9), computed_at = computed_at)
}

