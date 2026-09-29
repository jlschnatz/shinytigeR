# 1PL (Rasch) model: every item discriminates equally (a = 1), so only the
# difficulty b (`irt_diff` in db_item.sqlite) is used; `irt_discr` is ignored.

# Item response function: P(correct | theta, b)
irf_1pl <- function(theta, b) {
  stats::plogis(theta - b)
}

# Item information at theta: P * (1 - P)
iteminfo_1pl <- function(theta, b) {
  p <- irf_1pl(theta, b)
  p * (1 - p)
}

# Standard error of an ability estimate: 1 / sqrt(test information) at theta
sem_1pl <- function(theta, b) {
  1 / sqrt(sum(iteminfo_1pl(theta, b)))
}

# Probability that the true theta lies in [lower, upper), using the normal
# approximation theta_true ~ N(theta_hat, se^2). Drives the certainty dots of
# the dashboard's "Dein Lernstand" card (interval = the assigned label's).
prob_in_interval <- function(theta, se, lower, upper) {
  stats::pnorm((upper - theta) / se) - stats::pnorm((lower - theta) / se)
}

neg_log_lik <- function(theta, responses, b) {
  p <- irf_1pl(theta, b)
  p <- pmax(pmin(p, 1 - 1e-9), 1e-9)
  -sum(responses * log(p) + (1 - responses) * log(1 - p))
}

estimate_theta <- function(responses, b) {
  fit <- tryCatch(
    optim(
      0,
      neg_log_lik,
      method = "L-BFGS-B",
      lower = THETA_RANGE[1],
      upper = THETA_RANGE[2],
      responses = responses,
      b = b
    ),
    error = function(e) list(par = NA_real_)
  )
  fit$par
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
    theta <- estimate_theta(as.integer(rows$bool_correct), b)
    se <- if (is.na(theta)) NA_real_ else sem_1pl(theta, b)
    list(theta = theta, se = se, n = nrow(rows))
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
  la <- latest_attempts(ud)
  la$learning_area <- factor(la$learning_area, levels = LEARNING_AREA_LEVELS)
  comp <- estimate_competency(la, items)
  rows <- build_ability_rows(comp, user_id, session_token)
  db_write_ability(user_id, rows, path = ability_path)
  invisible(rows)
}
