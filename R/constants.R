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

# Evidence strength thresholds (unique items per area) — fallback for saved
# snapshots from before the standard error was stored
EVIDENCE_HIGH     <-  8L
EVIDENCE_MED      <-  3L

# Self-registration validation
REG_USERNAME_PATTERN <- "^[a-zA-Z0-9_.-]{3,30}$"
REG_PW_MIN_LENGTH    <- 8L
REG_MAX_ATTEMPTS     <- 3L
REG_ATTEMPT_DELAY_S  <- 1.0
