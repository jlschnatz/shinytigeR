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

CONTACT_EMAIL <- "tiger@psych.uni-frankfurt.de"
