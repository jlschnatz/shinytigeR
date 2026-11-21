# Generates AI feedback for a local user and prints it, with the API call's
# metadata: for trying models, prompt changes and the source list before the
# dashboard card exists (and without clicking through the app afterwards).
#
#   rv run dev/try_ai_feedback.R [user] [--force]
#
# user     a user with a saved ability estimate in db_ability.sqlite
#          (default: tiger_demo, see dev/add_showcase_user.R)
# --force  ignore the cooldown and the daily limit
#
# Uses the DB folder (TIGER_DB_DIR, default ../db) like the app: needs
# ai_config.json (copy dev/ai_config.example.json), ai_system_prompt.txt
# (kept outside the repo), ai_lookup.csv (dev/export_ai_lookup.R) and the key
# (TIGER_LLM_KEY or llm_key.txt).
# The feedback is saved to db_feedback.sqlite like a real one.

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args
args <- setdiff(args, "--force")
user <- if (length(args) >= 1L) args[1] else "tiger_demo"

suppressWarnings(pkgload::load_all(quiet = TRUE))
dir <- .db_dir()
dir <- normalizePath(dir, mustWork = FALSE)
Sys.setenv(TIGER_DB_DIR = dir)

# What's missing?
cfg <- ai_config()
problems <- c(
  if (!file.exists(file.path(dir, AI_CONFIG_FILE))) {
    sprintf("%s missing: cp dev/ai_config.example.json %s", AI_CONFIG_FILE, file.path(dir, AI_CONFIG_FILE))
  } else if (!cfg$enabled) {
    sprintf("%s: \"enabled\" is false or no models listed", AI_CONFIG_FILE)
  },
  if (!nzchar(ai_api_key())) sprintf("no key: set %s or create %s", AI_KEY_ENV, file.path(dir, AI_KEY_FILE)),
  local({
    p <- ai_prompt_problems(ai_system_prompt_path(dir))
    if (length(p) > 0L) paste(AI_SYSTEM_PROMPT_FILE, "-", p, "(it's kept outside the repo)")
  }),
  local({
    p <- ai_lookup_problems(read_ai_lookup())
    if (length(p) > 0L) paste(AI_LOOKUP_FILE, "-", p, "(dev/export_ai_lookup.R)")
  })
)
if (length(problems) > 0L) {
  cat("Can't run:\n", paste0("  - ", problems, "\n"), sep = "")
  quit(status = 1L)
}

comp <- latest_ability(db_get_ability(user))
if (is.null(comp)) {
  cat("No ability estimate for", user, "in db_ability.sqlite.\n")
  quit(status = 1L)
}

cat("User:", user, "| DB folder:", dir, "| models:", paste(cfg$models, collapse = ", "), "\n")
if (!force) {
  st <- ai_feedback_status(user, comp, cfg)
  if (st$state != "ready") {
    cat("Not ready (", st$state, "): ", st$notice, "\nRun with --force to try anyway.\n", sep = "")
    quit(status = 1L)
  }
}

req <- ai_feedback_prepare(user, comp, cfg, check_status = FALSE)
if (!req$ok) {
  cat("Not prepared:", req$notice, "\n")
  quit(status = 1L)
}
cat("\n----- sent to the model -----\n", req$user_prompt, "\n", sep = "")

# Run the event loop until the async call is done
state <- new.env()
t0 <- Sys.time()
promises::then(ai_feedback_run(req, "dev"),
  onFulfilled = function(out) state$out <- out,
  onRejected = function(e) state$out <- list(ok = FALSE, notice = conditionMessage(e))
)
while (is.null(state$out)) later::run_now(0.05)
out <- state$out
cat(sprintf("\n----- result after %.1f s -----\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
if (!is.null(out$notice)) cat("Notice:", out$notice, "\n")

if (isTRUE(out$ok)) {
  fb <- ai_latest_feedback(user)
  cat("\n", paste(strwrap(fb$summary, 90), collapse = "\n"), "\n", sep = "")
  for (i in seq_len(nrow(fb$recommendations))) {
    r <- fb$recommendations[i, ]
    cat(sprintf("\n[%d] %s: %s\n    %s\n", i, r$topic, r$title, r$link))
    cat(paste0("    ", strwrap(r$text, 86), collapse = "\n"), "\n")
  }
  cat(sprintf("\nModel: %s | words: summary %d\n", ai_model_label(fb$model, cfg), count_words(fb$summary)))
}

cat("\n----- API calls of this run -----\n")
print(ai_db(function(con) {
  DBI::dbGetQuery(con, "
    SELECT model, outcome, duration_seconds AS secs, input_tokens AS input,
           output_tokens AS output, reasoning_tokens AS reasoning,
           finish_reason AS finish, substr(message, 1, 70) AS message
    FROM api_call WHERE id_user = ? AND created_at >= ? ORDER BY id_call",
    params = list(user, as.integer(t0) - 1L)
  )
}), row.names = FALSE)
