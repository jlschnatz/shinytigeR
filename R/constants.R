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

PRIMARY_COLOR <- "#285f8a"

ANSWER_COLORS <- list(
  correct = "#00618f",
  incorrect = "#D81B60",
  skip = "#FFA000"
)

# Line and marker color of the "Fähigkeitsverlauf" panels: a dark neutral that
# stays readable on every COMPETENCY_SCALE background band (the primary blue
# would vanish on the blue bands at the top of the scale).
TRAJECTORY_COLOR <- "#2b3a4a"

# Feedback shown after the "Aufgabe überspringen" button (MC and numeric items
# alike). Skipping is an app action, not an answer option stored in the item DB.
SKIP_FEEDBACK <- "Alles klar! Du hast die Aufgabe übersprungen."

.db_dir <- function() {
  d <- Sys.getenv("TIGER_DB_DIR", unset = ".")
  normalizePath(d, mustWork = FALSE)
}

DB_ITEMS <- function() file.path(.db_dir(), "db_item.sqlite")
DB_USERS <- function() file.path(.db_dir(), "db_user.sqlite")
DB_CREDS <- function() file.path(.db_dir(), "db_credentials.sqlite")
DB_ABILITY <- function() file.path(.db_dir(), "db_ability.sqlite")

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

# Evidence strength thresholds (unique items per area) — fallback for saved
# snapshots from before the standard error was stored
EVIDENCE_HIGH     <-  8L
EVIDENCE_MED      <-  3L

# Self-registration validation
REG_USERNAME_PATTERN <- "^[a-zA-Z0-9_.-]{3,30}$"
REG_PW_MIN_LENGTH    <- 8L
REG_MAX_ATTEMPTS     <- 3L
REG_ATTEMPT_DELAY_S  <- 1.0
