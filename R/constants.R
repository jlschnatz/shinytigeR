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

# Categorical palette for the interactive ability-trajectory chart (one color
# per learning area, matching LEARNING_AREA_LEVELS order). Built from the
# official 5-color Goethe University palette (blue/yellow/magenta/green/
# orange) — but NOT via colorRampPalette() across all 5: interpolating
# between non-adjacent hues (blue<->yellow, magenta<->green, ...) produces
# muddy near-identical browns/olives regardless of color space (tried both
# sRGB and Lab), because those pairs are colour-opponent. There are only 5
# official hues for 7 areas, so slots 6-7 are lighter TINTS (same hue, higher
# value/lower saturation via HSV) of two of the five, not new hues and not
# blends. Blue and magenta were tinted specifically because they're the most
# hue-isolated of the five (each ~130 degrees from its nearest neighbor,
# vs. orange/yellow/green clustering within ~22 degrees of each other) — a
# tint of an already-isolated hue is least likely to drift into that
# crowded neighborhood. Kept viable at 7 series (beyond the CVD-safe cap of
# ~3 for charts where any two lines can be adjacent) because the chart also
# ships hover tooltips and a clickable legend as secondary encoding.
LEARNING_AREA_COLORS <- c(
  "#00618F", # blue          (official)
  "#E3BA0F", # yellow        (official)
  "#AD3B76", # magenta       (official)
  "#737C45", # green         (official)
  "#C96215", # orange        (official)
  "#70BDE0", # light blue    (tint of blue)
  "#E08BB7" # light magenta (tint of magenta)
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
