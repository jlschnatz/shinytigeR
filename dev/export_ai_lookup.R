# Exports the AI feedback's source list from the Excel file (sheet "Sources")
# to ai_lookup.csv in the DB folder, where the app reads it (AI_LOOKUP_FILE).
# The list isn't in the repo: the repo is public.
#
# Needs readxl, which isn't in rproject.toml (only this script uses it):
# install it in a personal library and run with plain Rscript, not rv run:
#
#   Rscript dev/export_ai_lookup.R <lookup.xlsx> [DB folder]
#
# DB folder: default TIGER_DB_DIR, else ../db. Stops without writing if the
# list has problems (ai_lookup_problems(), the same check as
# dev/check_deploy_dbs.R).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) stop("Usage: Rscript dev/export_ai_lookup.R <lookup.xlsx> [DB folder]")
xlsx <- args[1]
db_dir <- if (length(args) >= 2L) args[2] else Sys.getenv("TIGER_DB_DIR", "../db")
if (!nzchar(db_dir)) db_dir <- "../db"

pkgload::load_all(quiet = TRUE)

src <- as.data.frame(readxl::read_excel(xlsx, sheet = "Sources"))
tmp <- tempfile(fileext = ".csv")
utils::write.csv(src, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")

problems <- ai_lookup_problems(read_ai_lookup(tmp))
if (length(problems) > 0L) {
  cat("Not exported, the list has problems:\n", paste0("  - ", problems, "\n"), sep = "")
  quit(status = 1L)
}

out <- file.path(db_dir, AI_LOOKUP_FILE)
file.copy(tmp, out, overwrite = TRUE)
cat(sprintf("%d sources written to %s\n", nrow(src), normalizePath(out)))
