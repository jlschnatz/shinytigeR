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

build_response_row <- function(item, answer_idx, user_id, session_token) {
  n <- length(get_answeroptions(item))
  skipped <- answer_idx == n
  data.frame(
    id_user = as.character(user_id),
    id_session = as.character(session_token),
    id_date = as.integer(Sys.Date()),
    id_datetime = as.integer(Sys.time()),
    id_item = as.integer(item$id_item),
    learning_area = as.character(item$learning_area),
    selected_option = as.integer(answer_idx),
    answer_correct = as.integer(item$answer_correct),
    bool_correct = if (skipped) NA else (item$answer_correct == answer_idx),
    skipped = skipped,
    stringsAsFactors = FALSE
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
