#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_na_feedback <- function(.feedback_col, .answer_correct) {
  fb_enq <- rlang::enquo(.feedback_col)
  fb_name <- rlang::quo_name(fb_enq)
  assertthat::assert_that(
    grepl("^if_answeroption_[0-9]{2}$", fb_name),
    msg = "Column does not match the naming convention if_answeroption_0<int>"
  )
  assertthat::assert_that(dplyr::between(.answer_correct, 1, 5))
  fb_number <- substr_nth(fb_name, n = 1, side = "right")

  if (is.na(.feedback_col)) {
    return(dplyr::if_else(
      fb_number == .answer_correct,
      paste0("Richtige Antwort. Gut gemacht! ", icon("smile")),
      "Falsche Antwort."
    ))
  } else {
    return(.feedback_col)
  }
}

# image validation function
is_image <- function(x) {
  pattern <- "(([\\/a-zA-Z0-9äöüÄÖÜ_-]*?)\\.(svg|pdf|png|jpeg|jpg|tiff))$"
  return(grepl(pattern = pattern, x))
}


# Useful helpers
substr_nth <- function(x, n = 1, side = "right") {
  rlang::arg_match(side, c("left", "right"))
  assertthat::assert_that(is.character(x))
  assertthat::assert_that(n < nchar(x))
  out <- switch(side,
                "right" = substr(x, nchar(x) - n + 1, nchar(x)),
                "left"  = substr(x, 1, n)
  )

  return(out)
}

get_answeroptions <- function(.df, .row) {
  na.omit(as.character(.df[.row, paste0("answeroption_0", seq_len(5))]))
}

## IRT functions
# 2PL model function
prob_2pl <- function(theta, discrimination, difficulty) {
  return(1 / (1 + exp(-discrimination * (theta - difficulty))))
}

# Negative log-likelihood function for the 2PL model
negative_log_likelihood <- function(theta, responses, discrimination, difficulty) {
  p <- prob_2pl(theta, discrimination, difficulty)
  return(-sum(responses * log(p) + (1 - responses) * log(1 - p)))
}

# Function to estimate theta for a person using Maximum Likelihood
estimate_theta <- function(responses, discrimination, difficulty) {
  result <- optim(0, negative_log_likelihood, method="L-BFGS-B",
                  lower=-4, upper=4, responses=responses,
                  discrimination=discrimination, difficulty=difficulty)
  return(result$par)
}


#### Goethe Color Palette ----
# thanks @ https://drsimonj.svbtle.com/
# define colors
goethe_colors <- c(`blue`='#00618f',
                   `yellow` = '#e3ba0f',
                   `magenta` = '#ad3b76',
                   `green` = '#737c45',
                   `orange` = '#c96215')

# define extractor
goethe_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) return(goethe_colors)
  else goethe_colors[cols]
}

# define palette
goethe_palettes <- list(
  `main` = goethe_cols('blue', 'yellow', 'magenta'),
  `long` = goethe_cols(),
  `reds` = goethe_cols('yellow', 'orange', 'magenta'),
  `blues` = goethe_cols('yellow', 'green', 'blue')
)

# palette grabber
goethe_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- goethe_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

# define scales for ggplot
scale_color_goethe <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- goethe_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("goethe_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_goethe <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- goethe_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("goethe_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

