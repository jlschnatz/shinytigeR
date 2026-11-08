# Deterministic literature recommendation in the dashboard (part 1 of the
# kiwi feedback; part 2, the AI feedback, is the card to its right): for the
# student's weakest learning area, the exam-relevant textbook sections and the
# matching pandaR articles. Not AI-generated: a fixed lookup table
# (inst/extdata/lookup_exam.csv), filtered by area.

# The lookup table: one row per entry, in the order shown. Columns:
# learning_area (full name, as in LEARNING_AREA_LEVELS), type ("lehrbuch": the
# exam-relevant sections of the textbook EXAM_BOOK; "pandar": an article),
# text (the sections, e.g. "17.1 – 17.8, 19.13", or the article's title), url
# (pandaR only). A CSV in the package rather than a database: it's only read,
# changes a few times a semester, and is versioned and tested with the code
# (tests/testthat/test-exam-literature.R checks every area, type and link).
exam_lookup_path <- function() {
  system.file("extdata", EXAM_LOOKUP_FILE, package = "shinytigeR")
}

read_exam_lookup <- function(path = exam_lookup_path()) {
  df <- utils::read.csv(
    path,
    stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8",
    colClasses = "character"
  )
  df[df$learning_area %in% LEARNING_AREA_LEVELS, , drop = FALSE]
}

# Read once per R process (it's in the package, so it can't change at runtime)
.exam_lookup_cache <- new.env(parent = emptyenv())
exam_lookup <- function() {
  if (is.null(.exam_lookup_cache$df)) .exam_lookup_cache$df <- read_exam_lookup()
  .exam_lookup_cache$df
}

# The learning areas, weakest first: lowest saved estimate (theta) first,
# only areas whose estimate rests on at least `min_items` items (with fewer, an
# area looks weak by chance). Ties keep the LEARNING_AREA_LEVELS order. `comp`
# is the dashboard's competency() snapshot (latest_ability()), NULL if none.
# The card below uses the first one; the AI feedback is meant to use the first
# three of the same ranking, so both talk about the same areas.
rank_weak_areas <- function(comp, min_items = WEAK_AREA_MIN_ITEMS) {
  if (is.null(comp) || nrow(comp) == 0L) {
    return(character(0))
  }
  ok <- !is.na(comp$theta) & !is.na(comp$n_items) & comp$n_items >= min_items
  comp <- comp[ok, , drop = FALSE]
  ord <- order(comp$theta, match(as.character(comp$learning_area), LEARNING_AREA_LEVELS))
  as.character(comp$learning_area[ord])
}

# A link opening in a new tab, with the external-link icon. One HTML string:
# htmltools would put a line break between text and icon, which shows as an
# underlined space.
ext_link <- function(text, url) {
  tags$a(
    class = "exam-lit-item", href = url, target = "_blank", rel = "noopener",
    HTML(paste0(
      htmltools::htmlEscape(text),
      as.character(bsicons::bs_icon("box-arrow-up-right", class = "exam-lit-ext"))
    ))
  )
}

# Compact card below "Dein Lernstand": the weakest area, its textbook sections
# and pandaR articles, the book's citation below. Before any area has enough
# items, a short note instead.
exam_literature_card <- function(comp, lookup = exam_lookup(),
                                 min_items = WEAK_AREA_MIN_ITEMS) {
  area <- rank_weak_areas(comp, min_items)[1]
  header <- bslib::card_header(
    class = "d-flex justify-content-between align-items-center gap-2",
    div(
      class = "d-flex align-items-center gap-2",
      bsicons::bs_icon("book"),
      tags$b("Zum Nacharbeiten")
    ),
    tags$span(class = "exam-lit-tag", "Prüfungsrelevant")
  )
  if (is.na(area)) {
    return(bslib::card(
      class = "exam-lit-card",
      header,
      bslib::card_body(tags$p(
        class = "text-muted mb-0 exam-lit-note",
        sprintf(
          "Sobald du in einem Themenbereich mindestens %d Aufgaben bearbeitet hast, steht hier, was du zu deinem schwächsten Bereich nachlesen kannst.",
          min_items
        )
      ))
    ))
  }

  rows <- lookup[lookup$learning_area == area, , drop = FALSE]
  book <- rows[rows$type == "lehrbuch", , drop = FALSE]
  pandar <- rows[rows$type == "pandar", , drop = FALSE]
  cl <- competency_label(comp$theta[comp$learning_area == area], min_items)

  line <- function(label, items) {
    if (length(items) == 0L) {
      return(NULL)
    }
    div(
      class = "exam-lit-line",
      tags$span(class = "exam-lit-kind", label),
      tags$span(class = "exam-lit-items", items)
    )
  }
  book_items <- if (nrow(book)) {
    tagList(
      tags$span(class = "exam-lit-item", "Abschnitte ", tags$b(paste(book$text, collapse = ", "))),
      # the online version in the university library (university login)
      ext_link("E-Book in der UB", EXAM_BOOK_URL)
    )
  }
  # entries are separated by a gap (CSS), not a "·": on a phone a wrapped
  # line then starts with the entry, not with a dangling separator
  pandar_items <- lapply(seq_len(nrow(pandar)), function(i) {
    ext_link(pandar$text[i], pandar$url[i])
  })

  bslib::card(
    class = "exam-lit-card",
    header,
    bslib::card_body(
      tags$p(
        class = "exam-lit-lead mb-0",
        "Am meisten Nachholbedarf hast du gerade in dem Thema", tags$b(area),
        # "(▬ Aufbau)." with the level's segment from the strip in "Dein
        # Lernstand" (same classes as competency_scale_bar()), so the two can
        # be matched. One HTML string: htmltools would put spaces inside the
        # parentheses
        HTML(paste0(
          ' <span class="exam-lit-level">(',
          as.character(tags$span(
            class = "comp-seg is-filled is-current exam-lit-seg",
            style = sprintf("--seg:%s;", COMPETENCY_SCALE$color_hex[cl$level]),
            `aria-hidden` = "true"
          )),
          htmltools::htmlEscape(cl$label), ").</span>"
        ))
      ),
      line("Lehrbuch", book_items),
      line("pandaR", pandar_items),
      if (!is.null(book_items)) tags$p(class = "exam-lit-cite mb-0", EXAM_BOOK)
    )
  )
}
