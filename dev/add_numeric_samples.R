# Adds a small set of numeric-item (answer_mode = "num") samples to whatever
# db_item.sqlite TIGER_DB_DIR points at (default: project root). Dev/local
# testing only — never run this against the production DB, since it inserts
# fake practice items into the real item pool. For the production-safe
# schema migration alone (answer_mode column + backfill, no sample items),
# use dev/migrate_answer_mode.R and dev/migrate_numeric_bounds.R directly —
# this script sources both to reuse their functions rather than duplicating
# that logic.
#
# Idempotent — safe to re-run. Existing rows with the sample ids (9000x) are
# deleted and re-inserted, so edits to the samples below take effect on the
# next run. Only these fake sample ids are ever touched.
#
# Why a script and not a one-off SQL edit: dev/make_sample_items.R rebuilds
# dev/db_item_sample.sqlite from scratch out of the full db_item.sqlite —
# any numeric item hand-patched only into the sample file would be silently
# wiped out the next time that script runs. Running this script against the
# full pool (and re-running dev/make_sample_items.R afterwards, or running
# this script directly against dev/db_item_sample.sqlite too) is the durable
# way to keep a numeric item available for local testing.
#
# Usage, from the project root:
#
#   rv run dev/add_numeric_samples.R                          # local db_item.sqlite
#   TIGER_DB_DIR=dev rv run dev/add_numeric_samples.R          # dev sample pool directly
#     (TIGER_DB_DIR must point at a directory containing a file literally
#     named db_item.sqlite — for the sample pool, copy/symlink
#     dev/db_item_sample.sqlite to dev/db_item.sqlite first, or run against
#     a temp dir the way SETUP.md's manual-testing instructions do)

.migrate_answer_mode_sourced_only <- TRUE
source("dev/migrate_answer_mode.R") # defines migrate_answer_mode(), doesn't auto-run it
.migrate_numeric_bounds_sourced_only <- TRUE
source("dev/migrate_numeric_bounds.R") # defines migrate_numeric_bounds(), doesn't auto-run it

path <- file.path(Sys.getenv("TIGER_DB_DIR", unset = "."), "db_item.sqlite")
migrate_answer_mode(path)
migrate_numeric_bounds(path)

con <- DBI::dbConnect(RSQLite::SQLite(), path)
on.exit(DBI::dbDisconnect(con), add = TRUE)

# ── Sample numeric items ─────────────────────────────────────────────────────
# One per a few different learning areas, so the numeric path is reachable
# from more than one selector cell. IDs are far outside the real pool's
# range to avoid ever colliding with a real item.
samples <- data.frame(
  id_item = c(90001L, 90002L, 90003L),
  learning_area = c("Deskriptivstatistik", "Wahrscheinlichkeit", "Regression"),
  type_item = "content",
  bloom_taxonomy = c("application", "application", "knowledge"),
  theo_diff = c("easy", "medium", "medium"),
  stimulus_text = c(
    "Berechne den Mittelwert (arithmetisches Mittel) der folgenden Werte: 2, 4, 6, 8. Gib deine Antwort als Zahl ein.",
    "Eine faire Münze wird zweimal geworfen. Mit welcher Wahrscheinlichkeit (als Dezimalzahl, z. B. 0.25) fällt sie beide Male auf Kopf?",
    "Eine Regressionsgerade hat die Gleichung y = 2x + 3. Welchen y-Wert sagt das Modell für x = 4 voraus?"
  ),
  stimulus_image = NA_character_,
  answeroption_01 = c("5", "0.25", "11"),
  answeroption_02 = c("4.5", "0.5", "9"),
  answeroption_03 = c("6", "0.75", "8"),
  answeroption_04 = c("20", "1", "24"),
  answeroption_05 = NA_character_,
  answeroption_06 = NA_character_,
  answer_correct = "1",
  type_stimulus = "text",
  type_answer = "text",
  if_answeroption_01 = c(
    "Richtig! Der Mittelwert ist die Summe aller Werte geteilt durch ihre Anzahl: (2+4+6+8)/4 = 5.",
    "Richtig! P(Kopf) * P(Kopf) = 0.5 * 0.5 = 0.25, da die Würfe unabhängig sind.",
    "Richtig! y = 2*4 + 3 = 11."
  ),
  if_answeroption_02 = c(
    "Das sieht nach einem Rechenfehler bei Summe oder Division aus - prüfe (2+4+6+8=20) und teile noch einmal durch 4.",
    "Das ist die Wahrscheinlichkeit für genau einen Kopf-Wurf, nicht für beide.",
    "Das sieht nach einem Vorzeichen- oder Rechenfehler aus - prüfe 2*4+3 noch einmal."
  ),
  if_answeroption_03 = c(
    "Das ist der Median, nicht der Mittelwert.",
    "Das wäre die Wahrscheinlichkeit für mindestens einen Kopf-Wurf.",
    "Das ist nur der Steigungsanteil (2*4=8) ohne den Achsenabschnitt (+3)."
  ),
  if_answeroption_04 = c(
    "Das ist die Summe aller Werte, nicht der Mittelwert - noch durch die Anzahl (4) teilen.",
    "Das entspricht Sicherheit (100%) - hier sind aber zwei unabhängige Ereignisse gefragt.",
    "Das wäre y = (2+3)*4 - nicht die richtige Formel."
  ),
  if_answeroption_05 = NA_character_,
  if_answeroption_06 = NA_character_,
  ia_diff = NA_real_,
  ia_discr = NA_real_,
  irt_discr = c(1.0, 1.2, 1.1),
  irt_discr_se = NA_real_,
  irt_b0 = NA_real_,
  irt_b0_se = NA_real_,
  irt_diff = c(0.0, 0.3, -0.2),
  irt_diff_se = NA_real_,
  answer_mode = "num",
  stringsAsFactors = FALSE
)

# 90001-90003 have exact results, so no ranges: every option matches its own
# value exactly (all bound columns NA — see "Numeric item rules" in CLAUDE.md).
for (b in c(sprintf("lower_answeroption_%02d", 1:6), sprintf("upper_answeroption_%02d", 1:6))) {
  samples[[b]] <- NA_real_
}

# 90004 exercises the two newer rules: rounded results accepted via ranges
# (asymmetric, covering both rounding and truncating), and two correct
# options (sample variance with n - 1 AND population variance with n), each
# with its own feedback. Values 2, 4, 6, 8: SS = 20, s² = 6.667, σ² = 5,
# s = 2.582, σ = 2.236.
variance <- samples[1, ]
variance$id_item <- 90004L
variance$theo_diff <- "medium"
variance$stimulus_text <- "Berechne die Varianz der folgenden Werte: 2, 4, 6, 8. Runde auf zwei Nachkommastellen."
variance$answeroption_01 <- "6.67"
variance$answeroption_02 <- "5"
variance$answeroption_03 <- "20"
variance$answeroption_04 <- "2.58"
variance$answeroption_05 <- "2.24"
variance$answer_correct <- "1;2"
variance$if_answeroption_01 <- "Richtig! Das ist die Stichprobenvarianz: Quadratsumme 20 geteilt durch n - 1 = 3."
variance$if_answeroption_02 <- "Richtig! Das ist die Populationsvarianz: Quadratsumme 20 geteilt durch n = 4."
variance$if_answeroption_03 <- "Das ist die Quadratsumme - du hast noch nicht durch n bzw. n - 1 geteilt."
variance$if_answeroption_04 <- "Das ist die Standardabweichung (mit n - 1), nicht die Varianz - die Wurzel ist hier ein Schritt zu viel."
variance$if_answeroption_05 <- "Das ist die Standardabweichung (mit n), nicht die Varianz - die Wurzel ist hier ein Schritt zu viel."
variance$lower_answeroption_01 <- 6.66
variance$upper_answeroption_01 <- 6.67
variance$lower_answeroption_04 <- 2.58
variance$upper_answeroption_04 <- 2.59
variance$lower_answeroption_05 <- 2.23
variance$upper_answeroption_05 <- 2.24
variance$irt_discr <- 1.0
variance$irt_diff <- 0.2
samples <- rbind(samples, variance)

# Delete + re-insert (only these fake ids), so edits above take effect on re-run.
invisible(DBI::dbWithTransaction(con, {
  DBI::dbExecute(
    con,
    sprintf("DELETE FROM item_db WHERE id_item IN (%s)", paste(samples$id_item, collapse = ", "))
  )
  DBI::dbAppendTable(con, "item_db", samples)
}))
message(
  "Wrote ", nrow(samples), " numeric sample item(s): ",
  paste(samples$id_item, collapse = ", ")
)
