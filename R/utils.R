# Protect fenced code blocks and inline code spans, then convert $...$ / $$...$$
# to MathJax delimiters \(...\) / \[...\] before markdownToHTML sees the text.
# Backslashes are doubled because commonmark strips one layer during the markdown pass,
# leaving the correct single-backslash MathJax delimiters in the final HTML.
protect_math_delimiters <- function(text) {
  # Step 0 — protect fenced code blocks (``` or ~~~, e.g. R code with
  # `income$salary` or an error message) as a whole. Without this, the inline
  # code-span pairing in step 1 only happens to cover a ``` block's content,
  # and breaks as soon as the code itself contains a backtick (or with ~~~),
  # letting `$...$` inside the code be converted to math. An unclosed fence
  # runs to the end of the text, as in commonmark.
  code_blocks <- regmatches(
    text,
    gregexpr(
      "(?ms)^[ \\t]*(`{3,}|~{3,})[^\\n]*\\n.*?(?:^[ \\t]*\\1[ \\t]*$|\\z)",
      text,
      perl = TRUE
    )
  )[[1]]
  block_ph <- sprintf("CODEBLOCK%03d", seq_along(code_blocks))
  for (i in seq_along(code_blocks)) {
    text <- sub(code_blocks[i], block_ph[i], text, fixed = TRUE)
  }

  # Step 1 — protect inline code spans (e.g. `income$salary`) from math detection
  code_spans <- regmatches(text, gregexpr("`[^`]*`", text, perl = TRUE))[[1]]
  code_ph <- character(0)
  if (length(code_spans) > 0L) {
    code_ph <- sprintf("CODESPAN%03d", seq_along(code_spans))
    for (i in seq_along(code_spans)) {
      text <- sub(code_spans[i], code_ph[i], text, fixed = TRUE)
    }
  }

  # Step 2 — inside math regions, escape * so markdown doesn't turn it into <em>.
  # We search for $...$ / $$...$$ and replace each * inside with \* (markdown
  # backslash-escape for a literal asterisk).  With fixed = TRUE both the search
  # pattern and replacement are treated as plain strings — no regex magic.
  # Escape both * and _ — both can trigger markdown emphasis inside math
  escape_md_meta <- function(region) {
    region <- gsub("*", "\\*", region, fixed = TRUE)
    region <- gsub("_", "\\_", region, fixed = TRUE)
    region
  }

  math_display <- regmatches(
    text,
    gregexpr("\\$\\$[\\s\\S]+?\\$\\$", text, perl = TRUE)
  )[[1]]
  for (r in math_display) {
    text <- sub(r, escape_md_meta(r), text, fixed = TRUE)
  }

  math_inline <- regmatches(
    text,
    gregexpr("\\$[^$\n]+?\\$", text, perl = TRUE)
  )[[1]]
  for (r in math_inline) {
    text <- sub(r, escape_md_meta(r), text, fixed = TRUE)
  }

  # Step 3 — convert $...$ / $$...$$ to MathJax \(...\) / \[...\].
  # Backslashes are doubled because commonmark strips one layer during the
  # markdown pass, leaving the single-backslash delimiters MathJax expects.
  text <- gsub(
    "\\$\\$([\\s\\S]+?)\\$\\$",
    "\\\\\\\\[\\1\\\\\\\\]",
    text,
    perl = TRUE
  )
  text <- gsub("\\$([^$\n]+?)\\$", "\\\\\\\\(\\1\\\\\\\\)", text, perl = TRUE)

  # Step 4 — restore code spans, then code blocks
  for (i in seq_along(code_spans)) {
    text <- sub(code_ph[i], code_spans[i], text, fixed = TRUE)
  }
  for (i in seq_along(code_blocks)) {
    text <- sub(block_ph[i], code_blocks[i], text, fixed = TRUE)
  }
  text
}

render_md <- function(text) {
  if (is.null(text) || is.na(text) || !nzchar(trimws(text))) {
    return("")
  }
  markdown::markdownToHTML(
    text = protect_math_delimiters(text),
    fragment.only = TRUE
  )
}

get_answeroptions <- function(item) {
  cols <- paste0("answeroption_0", 1:6)
  vals <- unlist(item[cols])
  vals[!is.na(vals) & nzchar(vals)]
}

get_feedbackoptions <- function(item) {
  cols <- paste0("if_answeroption_0", 1:6)
  vals <- unlist(item[cols])
  vals[!is.na(vals) & nzchar(vals)]
}

# answer_correct is stored as text: one 1-based option index for MC items,
# one or more ";"-separated indices for numeric items (e.g. "1;3"). Returns an
# integer vector (empty if NA/unparseable).
parse_answer_correct <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) {
    return(integer(0))
  }
  parts <- trimws(strsplit(as.character(x[1]), ";", fixed = TRUE)[[1]])
  idx <- suppressWarnings(as.integer(parts[nzchar(parts)]))
  idx[!is.na(idx)]
}

evaluate_answer <- function(item, answer_idx) {
  n <- length(get_answeroptions(item))
  if (answer_idx %in% parse_answer_correct(item$answer_correct)) {
    "correct"
  } else if (answer_idx == n) {
    "skip"
  } else {
    "incorrect"
  }
}

# `answer_idx` is the matched distractor index — NA for a numeric item whose
# typed value matched nothing, or for an explicit skip. `typed_value` is only
# meaningful for numeric items (NA for MC rows and for MC-style skips).
# `skipped` defaults to the MC last-option convention (answer_idx == n); numeric
# callers pass it explicitly since there's no last-option slot to compare against.
build_response_row <- function(
  item,
  answer_idx,
  user_id,
  session_token,
  typed_value = NA_real_,
  skipped = NULL
) {
  n <- length(get_answeroptions(item))
  if (is.null(skipped)) {
    skipped <- answer_idx == n
  }
  bool_correct <- if (skipped || is.na(answer_idx)) {
    NA
  } else {
    answer_idx %in% parse_answer_correct(item$answer_correct)
  }
  data.frame(
    id_user = as.character(user_id),
    id_session = as.character(session_token),
    id_date = as.integer(Sys.Date()),
    id_datetime = as.integer(Sys.time()),
    id_item = as.integer(item$id_item),
    learning_area = as.character(item$learning_area),
    selected_option = if (is.na(answer_idx)) NA_integer_ else as.integer(answer_idx),
    # Stored as the item's text as-is ("2", or "1;3" for a multi-correct
    # numeric item) — bool_correct is what scoring reads, not this column.
    answer_correct = as.character(item$answer_correct),
    bool_correct = bool_correct,
    skipped = skipped,
    typed_value = as.numeric(typed_value),
    stringsAsFactors = FALSE
  )
}

# Numeric-item input parsing: accept both "3.5" and "3,5" (German locale).
# Returns NA_real_ for empty/unparseable input.
parse_numeric_input <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(trimws(x))) {
    return(NA_real_)
  }
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# A numeric item's answer options, by column position (1..6) — NOT compacted
# like get_answeroptions(), so row i always is option i even if an earlier
# option column is empty. Returns a data.frame with one row per non-empty
# option: idx, value, lower, upper, feedback, is_correct. Bound columns that
# don't exist yet (DB not migrated — see dev/migrate_numeric_bounds.R) read as
# NA, i.e. exact match.
get_numeric_options <- function(item) {
  col <- function(prefix, i) {
    v <- item[[sprintf("%sansweroption_%02d", prefix, i)]]
    if (is.null(v) || length(v) == 0L) NA else v[[1]]
  }
  opts <- lapply(1:6, function(i) {
    raw <- col("", i)
    if (is.na(raw) || !nzchar(trimws(raw))) {
      return(NULL)
    }
    data.frame(
      idx = i,
      value = parse_numeric_input(as.character(raw)),
      lower = suppressWarnings(as.numeric(col("lower_", i))),
      upper = suppressWarnings(as.numeric(col("upper_", i))),
      feedback = as.character(col("if_", i)),
      stringsAsFactors = FALSE
    )
  })
  opts <- do.call(rbind, opts)
  if (is.null(opts)) {
    return(data.frame(
      idx = integer(0), value = numeric(0), lower = numeric(0),
      upper = numeric(0), feedback = character(0), is_correct = logical(0)
    ))
  }
  opts$is_correct <- opts$idx %in% parse_answer_correct(item$answer_correct)
  opts
}

# Matches a numeric answer against an item's answer options (see "Numeric item
# rules" in CLAUDE.md). Each option matches a closed range [lower, upper]; if
# either bound is NA, it matches its own value exactly (a tiny epsilon absorbs
# floating-point noise only). Validated items never have overlapping ranges,
# so at most one option matches — should an unvalidated item overlap anyway,
# the lowest-numbered matching option wins. Returns list(matched_idx, result),
# where result is "correct" (matched an option listed in answer_correct) /
# "incorrect" (matched any other option) / "unmatched" (matched_idx NA) —
# never "skip", which is handled by the caller's dedicated skip action.
evaluate_numeric_answer <- function(item, typed_value) {
  opts <- get_numeric_options(item)
  if (is.na(typed_value) || nrow(opts) == 0L) {
    return(list(matched_idx = NA_integer_, result = "unmatched"))
  }
  has_range <- !is.na(opts$lower) & !is.na(opts$upper)
  eps <- 1e-9 * pmax(1, abs(opts$value))
  hit <- ifelse(
    has_range,
    typed_value >= opts$lower - eps & typed_value <= opts$upper + eps,
    !is.na(opts$value) & abs(typed_value - opts$value) <= eps
  )
  hit[is.na(hit)] <- FALSE
  if (!any(hit)) {
    return(list(matched_idx = NA_integer_, result = "unmatched"))
  }
  m <- which(hit)[1]
  list(
    matched_idx = as.integer(opts$idx[m]),
    result = if (opts$is_correct[m]) "correct" else "incorrect"
  )
}

safe_sample <- function(x, size = length(x)) {
  if (length(x) == 0L) {
    return(x)
  }
  x[sample.int(length(x), size = min(size, length(x)))]
}

is_img_path <- function(x) {
  !is.na(x) &
    grepl("\\.(?:png|jpg|jpeg|svg|gif)$", x, ignore.case = TRUE, perl = TRUE)
}

# Item-ID badge that copies its own ID on click. Used by mod_practice.R (in the
# progress bar) and mod_inspect.R (above the item card) so both behave alike.
# Built on rclipboard::rclipButton (clipboard.js) rather than a hand-rolled
# navigator.clipboard call, for the bslib tooltip integration.
#
# `input_id` must be namespaced by the caller (`ns("copy_item_id")`) and unique
# within the app. The copy itself is entirely client-side — clipboard.js reads
# `data-clipboard-text` on click — so nothing observes this input server-side;
# the button's click counter resetting on every renderUI re-render (both call
# sites live inside one) is therefore harmless.
#
# Known upstream quirk (rclip_fun, rclipboard 0.2.1): the helper script it
# injects binds `new ClipboardJS(".btn", document.getElementById(inputId))`.
# The second argument is meant to be a ClipboardJS options object, not a DOM
# node, so passing an element there has no effect — the selector ends up
# matching every `.btn` in the document, not just this one, and a fresh
# instance is added on each re-render. Copying still works (each instance
# reads `data-clipboard-text` off whatever `.btn` was actually clicked), it
# just isn't scoped the way the package's own code implies.
item_id_badge <- function(id_item, input_id, class = NULL) {
  rclipboard::rclipButton(
    inputId = input_id,
    label = tagList(bsicons::bs_icon("clipboard"), sprintf("ID %d", as.integer(id_item))),
    clipText = as.character(as.integer(id_item)),
    class = paste("practice-item-id practice-item-id-btn", class),
    tooltip = "Aufgaben-ID kopieren"
  )
}
