# Extracts a small, committable sample of the item pool from the full
# db_item.sqlite into dev/db_item_sample.sqlite.
#
# Why a sample: the repository is public, and the full pool is the assessment
# content of a module students are graded in. The sample is large enough to
# develop and test against (every learning area, both item types, at least one
# image item) and small enough that publishing it costs nothing.
#
# Maintainers run this when the pool changes; contributors never need to.
# dev/seed_db.R copies the result into place, so a fresh clone can run the app.
#
# Usage, from the project root:
#
#   rv run dev/make_sample_items.R

PER_AREA <- 2L
SRC <- file.path(Sys.getenv("TIGER_DB_DIR", unset = "."), "db_item.sqlite")
OUT <- file.path("dev", "db_item_sample.sqlite")

if (!file.exists(SRC)) {
  stop("Full item pool not found at ", SRC, call. = FALSE)
}

set.seed(42) # reproducible sample, so re-running produces no spurious diff

con <- DBI::dbConnect(RSQLite::SQLite(), SRC)
on.exit(DBI::dbDisconnect(con), add = TRUE)

schema <- DBI::dbGetQuery(
  con,
  "SELECT sql FROM sqlite_master WHERE type='table' AND name='item_db'"
)$sql[1]
items <- DBI::dbReadTable(con, "item_db")
DBI::dbDisconnect(con)
on.exit(NULL)

# ── Choose the sample ───────────────────────────────────────────────────────
# Per learning area, prefer a spread across item types so both the "Inhaltlich"
# and "R-Code" paths are exercised in development.
pick_area <- function(df) {
  # Prefer items that carry IRT parameters: estimate_competency() silently drops
  # items without them, so a sample of unparameterised items would leave the
  # competency dashboard blank and look like a bug to a new contributor.
  # Only 32 of 116 items are parameterised, and three areas have none at all —
  # for those areas this falls back to whatever exists.
  df <- df[order(is.na(df$irt_discr) | is.na(df$irt_diff)), ]
  by_type <- split(df, df$type_item)
  picked <- unlist(
    lapply(by_type, function(d) d$id_item[seq_len(min(1L, nrow(d)))]),
    use.names = FALSE
  )
  remaining <- setdiff(df$id_item, picked)
  need <- PER_AREA - length(picked)
  if (need > 0L && length(remaining) > 0L) {
    picked <- c(picked, remaining[seq_len(min(need, length(remaining)))])
  }
  head(picked, PER_AREA)
}

ids <- unlist(
  lapply(split(items, items$learning_area), pick_area),
  use.names = FALSE
)

# Make sure at least one image-bearing item is present, so the img_item
# resource path and the image-answer rendering path are covered.
has_img <- function(id) {
  row <- items[items$id_item == id, ]
  (!is.na(row$stimulus_image) & nzchar(row$stimulus_image)) |
    identical(row$type_answer, "image")
}
if (!any(vapply(ids, has_img, logical(1)))) {
  img_candidates <- items$id_item[
    (!is.na(items$stimulus_image) & nzchar(items$stimulus_image)) |
      items$type_answer == "image"
  ]
  if (length(img_candidates) > 0L) ids <- c(ids, img_candidates[1])
}

sample_items <- items[items$id_item %in% ids, ]
sample_items <- sample_items[order(sample_items$id_item), ]

# ── Write ───────────────────────────────────────────────────────────────────
if (file.exists(OUT)) invisible(file.remove(OUT))
out <- DBI::dbConnect(RSQLite::SQLite(), OUT)
invisible(DBI::dbExecute(out, schema))
invisible(DBI::dbAppendTable(out, "item_db", sample_items))
DBI::dbDisconnect(out)

message(
  "Wrote ", OUT, " — ", nrow(sample_items), " items\n",
  paste0(
    "  ",
    names(table(sample_items$learning_area)),
    ": ",
    as.integer(table(sample_items$learning_area)),
    collapse = "\n"
  ),
  "\n  types: ",
  paste(names(table(sample_items$type_item)), as.integer(table(sample_items$type_item)),
    sep = "=", collapse = ", "
  )
)
