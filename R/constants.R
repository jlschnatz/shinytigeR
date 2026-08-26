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

.db_dir <- function() {
  d <- Sys.getenv("TIGER_DB_DIR", unset = ".")
  normalizePath(d, mustWork = FALSE)
}

DB_ITEMS <- function() file.path(.db_dir(), "db_item.sqlite")
DB_USERS <- function() file.path(.db_dir(), "db_user.sqlite")
DB_CREDS <- function() file.path(.db_dir(), "db_credentials.sqlite")
DB_ABILITY <- function() file.path(.db_dir(), "db_ability.sqlite")

CONTACT_EMAIL <- "tiger@psych.uni-frankfurt.de"

# IRT competency thresholds (theta)
IRT_THETA_HIGH    <-  1.0
IRT_THETA_MED     <-  0.0
IRT_THETA_LOW     <- -0.5

# Evidence strength thresholds (unique items per area)
EVIDENCE_HIGH     <-  8L
EVIDENCE_MED      <-  3L

# Self-registration validation
REG_USERNAME_PATTERN <- "^[a-zA-Z0-9_.-]{3,30}$"
REG_PW_MIN_LENGTH    <- 8L
REG_MAX_ATTEMPTS     <- 3L
REG_ATTEMPT_DELAY_S  <- 1.0
