# Adds a small set of numeric-item (answer_mode = "num") samples to whatever
# db_item.sqlite TIGER_DB_DIR points at (default: project root). Dev/local
# testing only — never run this against the production DB, since it inserts
# fake practice items into the real item pool. For the production-safe
# schema migration alone (answer_mode column + backfill, no sample items),
# use dev/migrate_answer_mode.R directly — this script sources it to reuse
# migrate_answer_mode() rather than duplicating that logic.
#
# Idempotent — safe to re-run. Existing rows with the same id_item are left
# untouched (not re-inserted), so running this twice is a no-op the second
# time.
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

path <- file.path(Sys.getenv("TIGER_DB_DIR", unset = "."), "db_item.sqlite")
migrate_answer_mode(path)

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

existing <- DBI::dbGetQuery(con, "SELECT id_item FROM item_db")$id_item
new_rows <- samples[!samples$id_item %in% existing, ]

if (nrow(new_rows) == 0L) {
  message("Numeric sample items already present (ids ", paste(samples$id_item, collapse = ", "), ") — nothing to do.")
} else {
  DBI::dbAppendTable(con, "item_db", new_rows)
  message(
    "Added ", nrow(new_rows), " numeric sample item(s): ",
    paste(new_rows$id_item, collapse = ", ")
  )
}
