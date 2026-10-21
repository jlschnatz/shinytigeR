# 1PL (Rasch) model: every item discriminates equally (a = 1), so only the
# difficulty b (`irt_diff` in db_item.sqlite) is used; `irt_discr` is ignored.

# Item response function: P(correct | theta, b)
irf_1pl <- function(theta, b) {
  stats::plogis(theta - b)
}

# Probability that the true theta lies in [lower, upper), using the normal
# approximation theta_true ~ N(theta, se^2) of the posterior (theta = EAP,
# se = posterior SD, see estimate_theta()). Drives the certainty dots of the
# dashboard's "Dein Lernstand" card (interval = the assigned label's).
prob_in_interval <- function(theta, se, lower, upper) {
  stats::pnorm((upper - theta) / se) - stats::pnorm((lower - theta) / se)
}

# Grid over THETA_RANGE on which the posterior is evaluated
THETA_GRID <- seq(THETA_RANGE[1], THETA_RANGE[2], length.out = 241)

# Posterior of theta on THETA_GRID: N(EAP_PRIOR_MEAN, EAP_PRIOR_SD^2) prior
# times the 1PL likelihood of the responses, normalized to sum to 1
theta_posterior <- function(responses, b, grid = THETA_GRID) {
  p <- irf_1pl(outer(grid, b, "-"), 0)
  p <- pmax(pmin(p, 1 - 1e-12), 1e-12)
  r <- matrix(responses, nrow = length(grid), ncol = length(b), byrow = TRUE)
  log_post <- rowSums(r * log(p) + (1 - r) * log(1 - p)) +
    stats::dnorm(grid, EAP_PRIOR_MEAN, EAP_PRIOR_SD, log = TRUE)
  w <- exp(log_post - max(log_post))
  w / sum(w)
}

# Expected a posteriori (EAP) estimate: the posterior mean and SD. Unlike the
# maximum-likelihood estimate it is finite for all-correct/all-wrong patterns
# and is pulled towards the prior mean when there are only few responses, so
# early estimates don't jump around. Returns list(theta, se), both NA when
# there is nothing to estimate from.
estimate_theta <- function(responses, b) {
  if (length(b) == 0L || anyNA(b) || anyNA(responses)) {
    return(list(theta = NA_real_, se = NA_real_))
  }
  w <- theta_posterior(responses, b)
  theta <- sum(THETA_GRID * w)
  list(theta = theta, se = sqrt(sum((THETA_GRID - theta)^2 * w)))
}

estimate_competency <- function(responses, items) {
  result <- lapply(LEARNING_AREA_LEVELS, function(area) {
    rows <- responses[
      !is.na(responses$learning_area) &
        responses$learning_area == area &
        !is.na(responses$bool_correct),
      ,
      drop = FALSE
    ]
    if (nrow(rows) < 1L) {
      return(list(theta = NA_real_, se = NA_real_, n = 0L))
    }
    idx <- match(rows$id_item, items$id_item)
    ok <- !is.na(idx) & !is.na(items$irt_diff[idx])
    rows <- rows[ok, , drop = FALSE]
    idx <- idx[ok]
    if (nrow(rows) < 1L) {
      return(list(theta = NA_real_, se = NA_real_, n = 0L))
    }
    b <- items$irt_diff[idx]
    est <- estimate_theta(as.integer(rows$bool_correct), b)
    list(theta = est$theta, se = est$se, n = nrow(rows))
  })

  data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = vapply(result, `[[`, numeric(1), "theta"),
    se = vapply(result, `[[`, numeric(1), "se"),
    n_items = vapply(result, function(x) as.integer(x$n), integer(1)),
    stringsAsFactors = FALSE
  )
}

# Computes a fresh competency snapshot from a user's full response history
# (deduped to the latest attempt per item) and persists it to db_ability.sqlite.
# The single call site for both the login-time auto-check and the dashboard's
# refresh button (see R/server.R and R/mod_dashboard.R).
compute_and_save_ability <- function(user_id, session_token, items,
                                     user_path = DB_USERS(),
                                     ability_path = DB_ABILITY()) {
  ud <- db_get_userdata(user_id, path = user_path)
  if (nrow(ud) == 0L) {
    return(invisible(NULL))
  }
  ud$bool_correct <- as.logical(ud$bool_correct)
  # Drop skips *before* picking the latest attempt: a skip carries no ability
  # information, so an item answered earlier and skipped on a repeat should
  # count with its latest real answer instead of dropping out entirely.
  # A user who only ever skipped still gets a batch (all areas NA), so the
  # dashboard shows "Keine Daten" and ability_needs_update() settles.
  ud <- ud[!is.na(ud$bool_correct), , drop = FALSE]
  la <- latest_attempts(ud)
  la$learning_area <- factor(la$learning_area, levels = LEARNING_AREA_LEVELS)
  comp <- estimate_competency(la, items)
  rows <- build_ability_rows(comp, user_id, session_token)
  db_write_ability(user_id, rows, path = ability_path)
  invisible(rows)
}
