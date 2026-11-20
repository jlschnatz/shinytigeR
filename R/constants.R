LEARNING_AREA_LEVELS <- c(
  "Deskriptivstatistik",
  "Wahrscheinlichkeit",
  "Grundlagen der Inferenzstatistik",
  "Gruppenvergleiche",
  "Poweranalyse",
  "Zusammenhangsmaße",
  "Regression"
)

LEARNING_AREA_LABELS <- c(
  "Deskriptiv" = "Deskriptivstatistik",
  "Wahrscheinl." = "Wahrscheinlichkeit",
  "Inferenz" = "Grundlagen der Inferenzstatistik",
  "Gruppen" = "Gruppenvergleiche",
  "Power" = "Poweranalyse",
  "Zusammenhang" = "Zusammenhangsmaße",
  "Regression" = "Regression"
)

ITEM_TYPE_LABELS <- c(
  "Inhaltlich" = "content",
  "R-Code" = "coding"
)

PRIMARY_COLOR <- "#006290"

ANSWER_COLORS <- list(
  correct = "#00618f",
  incorrect = "#D81B60",
  skip = "#FFA000"
)

# Line and marker color of the "Fähigkeitsverlauf" panels: a dark neutral that
# stays readable on every COMPETENCY_SCALE background band (the primary blue
# would vanish on the blue bands at the top of the scale).
TRAJECTORY_COLOR <- "#2b3a4a"

# Time windows of the "Fähigkeitsverlauf" switch: days back from today (NA =
# the whole history). The names are the button labels; the first is the default.
TRAJECTORY_WINDOWS <- c("Gesamt" = NA, "4 Wochen" = 28, "2 Wochen" = 14)

# Feedback shown after the "Aufgabe überspringen" button (MC and numeric items
# alike). Skipping is an app action, not an answer option stored in the item DB.
SKIP_FEEDBACK <- "Alles klar! Du hast die Aufgabe übersprungen."

# The DB folder: the databases, reg_code.txt and the item images (img_item/).
# On the server TIGER_DB_DIR=/opt/shinyapp (the mounted shinytigeR-DB folder);
# locally it's the folder next to the repo, ../db (dev/run.R sets it). Same
# layout in both places, so the app finds its files the same way.
DB_DIR_DEFAULT <- "../db"
.db_dir <- function() {
  d <- Sys.getenv("TIGER_DB_DIR", unset = DB_DIR_DEFAULT)
  if (!nzchar(d)) d <- DB_DIR_DEFAULT
  normalizePath(d, mustWork = FALSE)
}

DB_ITEMS <- function() file.path(.db_dir(), "db_item.sqlite")
DB_USERS <- function() file.path(.db_dir(), "db_user.sqlite")
DB_CREDS <- function() file.path(.db_dir(), "db_credentials.sqlite")
DB_ABILITY <- function() file.path(.db_dir(), "db_ability.sqlite")
# Item images live next to the item DB, not in the package: items and their
# images are uploaded together (SFTP), without a new build
ITEM_IMG_DIR <- function() file.path(.db_dir(), "img_item")

CONTACT_EMAIL <- "tiger@psych.uni-frankfurt.de"

# Competency scale: maps an ability estimate (theta) to a label shown in the
# dashboard's "Dein Lernstand" card. Rows must be ordered and contiguous
# (each `upper` equals the next row's `lower`); intervals are left-closed,
# [lower, upper), so theta == -1 is "Basis", not "Aufbau". Only `lower` is
# used for the lookup (findInterval); `upper` is kept for readability and
# checked for contiguity in test-dashboard-helpers.R. Text color on each pill
# is derived from `color_hex` for contrast, so a color can be changed here
# without touching the UI code.
COMPETENCY_SCALE <- data.frame(
  label = c("Aufbau", "Basis", "Solide", "Kompetent", "Fortgeschritten", "Versiert", "Souverän"),
  lower = c(-Inf, -1, -0.25, 0.5, 1, 1.5, 2),
  upper = c(-1, -0.25, 0.5, 1, 1.5, 2, Inf),
  color_hex = c("#D73027", "#F46D43", "#FDAE61", "#FEE090", "#ABD9E9", "#74ADD1", "#4575B4"),
  stringsAsFactors = FALSE
)

# Range of the ability estimate (estimate_theta() is bounded to it). Also the
# effective ends of COMPETENCY_SCALE when computing label certainty: the model
# can't distinguish abilities beyond these bounds, so the open-ended outer
# labels are evaluated as [-3, -1) and [2, 3] rather than out to +/- Inf.
THETA_RANGE <- c(-3, 3)

# Certainty of a competency label = P(true theta lies in the label's
# interval), see prob_in_interval() in R/irt.R. Cutoffs follow the IPCC's
# calibrated probability language: >= 2/3 "likely", 1/3-2/3 "about as likely
# as not", < 1/3 "unlikely".
CERTAINTY_HIGH    <- 2 / 3
CERTAINTY_MED     <- 1 / 3

# Coverage of the interval bar in "Dein Lernstand im Detail": the range the
# true ability lies in with this probability (normal approximation)
ESTIMATE_INTERVAL_LEVEL <- 0.8

# "Fehler nochmal üben" on the dashboard: at most this many open (wrong, not
# yet solved) items per retry queue, drawn at random. A mistake only becomes
# retryable RETRY_MIN_DAYS calendar days after its latest answer — a time-out
# against redoing the same items over and over within minutes (spacing helps
# retention, too). Random draws in the selector aren't affected.
RETRY_MAX_ITEMS <- 10L
RETRY_MIN_DAYS <- 1L

# "Zum Nacharbeiten" on the dashboard (R/exam_literature.R): the lookup table
# in inst/extdata, and how many items an area's estimate needs before the area
# can count as the weakest one (with fewer it looks weak by chance)
EXAM_LOOKUP_FILE <- "lookup_exam.csv"
# The textbook the sections in the lookup table refer to (shown below them)
EXAM_BOOK <- "Eid, M., Gollwitzer, M. & Schmitt, M. (2017). Statistik und Forschungsmethoden (5. Aufl.). Beltz."
# Its online version in the university library's catalogue (access with the
# university login), linked next to the sections
EXAM_BOOK_URL <- "https://ubffm.hds.hebis.de/Record/HEB366849158"
WEAK_AREA_MIN_ITEMS <- 5L
# Its sentence depends on the weakest area's level (index in COMPETENCY_SCALE):
# up to the first value "Nachholbedarf", up to the second "Wiederholen",
# above it "Vertiefen" (so a student who is good everywhere isn't told about
# a backlog). Levels 1-2 Aufbau/Basis, 3-4 Solide/Kompetent, 5-7 the rest
WEAK_AREA_TIERS <- c(2L, 4L)

# ── AI feedback ("Persönliches Feedback", R/ai_feedback.R) ───────────────────
# Settings that change at runtime live in the DB folder, so they can be
# changed by SFTP without a rebuild: the config (AI_CONFIG_FILE, read on every
# request; missing or broken = feature off), the source list (AI_LOOKUP_FILE,
# not in the package because the repo is public) and the API key
# (TIGER_LLM_KEY, else AI_KEY_FILE, via read_secret()).
DB_FEEDBACK <- function() file.path(.db_dir(), "db_feedback.sqlite")
AI_CONFIG_FILE <- "ai_config.json"
AI_LOOKUP_FILE <- "ai_lookup.csv"
AI_KEY_ENV <- "TIGER_LLM_KEY"
AI_KEY_FILE <- "llm_key.txt"
# The system prompt (German), shipped with the package
AI_SYSTEM_PROMPT_FILE <- "ai_system_prompt.txt"

# Defaults for the optional keys of AI_CONFIG_FILE
AI_CONFIG_DEFAULTS <- list(
  enabled = FALSE,
  base_url = "https://litellm.s.studiumdigitale.uni-frankfurt.de/v1/",
  models = character(0),
  model_labels = list(),
  model_args = list(),
  cooldown_minutes = 240,
  daily_limit = 1000L, # API calls per day, all users together
  timeout_seconds = 30, # per call
  max_tokens = 3000L, # incl. a reasoning model's thinking
  reasoning_effort = "low" # sent to every model without an entry in model_args
)

# Recommendations are given for the first AI_FOCUS_TOPICS areas of
# rank_weak_areas() (the "Zum Nacharbeiten" ranking), choosing from at most
# AI_MAX_RESOURCES_PER_TOPIC sources per area
AI_FOCUS_TOPICS <- 3L
AI_MAX_RESOURCES_PER_TOPIC <- 10L

# The certainty dots (1-3, certainty_dot_count()) as words for the model.
# They replace [[EVIDENCE_1]] ... [[EVIDENCE_3]] in the system prompt.
AI_EVIDENCE_WORDS <- c("niedrig", "mittel", "hoch")

# Column of AI_LOOKUP_FILE that marks a source as fitting a learning area
AI_LOOKUP_COLUMNS <- c(
  "Deskriptivstatistik" = "descriptive",
  "Wahrscheinlichkeit" = "probability",
  "Grundlagen der Inferenzstatistik" = "inference",
  "Gruppenvergleiche" = "group_comparisons",
  "Poweranalyse" = "power_analysis",
  "Zusammenhangsmaße" = "association",
  "Regression" = "regression"
)

# Format rules from the system prompt, checked after the answer arrives. An
# answer that breaks them is only used if no other model gives a clean one.
# AI_FORBIDDEN_WORDS are word stems: they match at the start of a word.
AI_MAX_WORDS_SUMMARY <- 120L
AI_MAX_WORDS_RECOMMENDATION <- 40L
AI_FORMAT_TOLERANCE <- 1.25
AI_FORBIDDEN_WORDS <- c(
  "Evidenz", "Kompetenzstufe", "Lernstand", "Fokusthem",
  "Schwerpunkt", "Ressource", "Lernfunktion"
)

# Error handling. "Too many requests" (HTTP 429) is about the key, not the
# model: wait and ask the same model again, at most AI_RATE_LIMIT_TRIES times.
# A model that failed with one of AI_BLOCKING_ERRORS in the last
# AI_BLOCK_MINUTES is skipped (for everyone); a bad answer ("invalid")
# doesn't block it.
AI_RATE_LIMIT_TRIES <- 3L
AI_RATE_LIMIT_WAIT_S <- 5
AI_BLOCK_MINUTES <- 10
AI_BLOCKING_ERRORS <- c("timeout", "connection", "unavailable")

# Evidence strength thresholds (unique items per area) — fallback for saved
# snapshots from before the standard error was stored
EVIDENCE_HIGH     <-  8L
EVIDENCE_MED      <-  3L

# Self-registration validation
REG_USERNAME_PATTERN <- "^[a-zA-Z0-9_.-]{3,30}$"
REG_PW_MIN_LENGTH    <- 8L
REG_MAX_ATTEMPTS     <- 3L
REG_ATTEMPT_DELAY_S  <- 1.0
# Semester code fallback when TIGER_REG_CODE isn't set: this file in the DB
# folder (see read_secret())
REG_CODE_FILE        <- "reg_code.txt"
