# Protect inline code spans, then convert $...$ / $$...$$ to MathJax delimiters
# \(...\) / \[...\] before markdownToHTML sees the text.
# Backslashes are doubled because commonmark strips one layer during the markdown pass,
# leaving the correct single-backslash MathJax delimiters in the final HTML.
protect_math_delimiters <- function(text) {
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

  # Step 4 — restore code spans
  for (i in seq_along(code_spans)) {
    text <- sub(code_ph[i], code_spans[i], text, fixed = TRUE)
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

evaluate_answer <- function(item, answer_idx) {
  n <- length(get_answeroptions(item))
  if (answer_idx == item$answer_correct) {
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
    item$answer_correct == answer_idx
  }
  data.frame(
    id_user = as.character(user_id),
    id_session = as.character(session_token),
    id_date = as.integer(Sys.Date()),
    id_datetime = as.integer(Sys.time()),
    id_item = as.integer(item$id_item),
    learning_area = as.character(item$learning_area),
    selected_option = if (is.na(answer_idx)) NA_integer_ else as.integer(answer_idx),
    answer_correct = as.integer(item$answer_correct),
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

# Matches a numeric answer against an item's distractor values (reused
# answeroption_0X columns, parsed as numbers — no trailing skip slot for
# numeric items). A value is "within tolerance" of a distractor if it's
# within NUM_MATCH_REL_TOL of that distractor's value, with NUM_MATCH_ABS_FLOOR
# as a floor so distractors near zero don't get an unreasonably tight band.
# Ties (falls within tolerance of more than one distractor) resolve to the
# closest one by absolute distance. Returns list(matched_idx, result), where
# result is "correct" / "incorrect" / "unmatched" (matched_idx is NA for the
# latter) — never "skip", which is handled by the caller's dedicated skip
# action, not this matching logic.
evaluate_numeric_answer <- function(
  item,
  typed_value,
  rel_tol = NUM_MATCH_REL_TOL,
  abs_floor = NUM_MATCH_ABS_FLOOR
) {
  values <- suppressWarnings(as.numeric(get_answeroptions(item)))
  if (is.na(typed_value) || length(values) == 0L) {
    return(list(matched_idx = NA_integer_, result = "unmatched"))
  }
  tol <- pmax(abs(values) * rel_tol, abs_floor)
  dist <- abs(values - typed_value)
  within_tol <- !is.na(dist) & dist <= tol
  if (!any(within_tol)) {
    return(list(matched_idx = NA_integer_, result = "unmatched"))
  }
  candidates <- which(within_tol)
  matched_idx <- candidates[which.min(dist[candidates])]
  result <- if (matched_idx == as.integer(item$answer_correct)) "correct" else "incorrect"
  list(matched_idx = as.integer(matched_idx), result = result)
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
