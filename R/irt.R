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

# Which items only have a preliminary difficulty: the item DB's optional
# `irt_provisional` column (1 = preliminary, e.g. estimated before there were
# enough answers; 0/NULL = calibrated from data). Recalibration in tigertools
# sets it back to 0. Pools without the column count as calibrated throughout.
item_provisional <- function(items) {
  if (!"irt_provisional" %in% names(items)) {
    return(rep(FALSE, nrow(items)))
  }
  v <- suppressWarnings(as.integer(items$irt_provisional))
  !is.na(v) & v == 1L
}

# One row per learning area in `areas` (default: all, in LEARNING_AREA_LEVELS
# order). `n_items` counts the items the estimate is based on (those with a
# difficulty), `n_provisional` how many of them only have a preliminary one
# (item_provisional()). Each area is estimated on its own, so leaving areas out doesn't
# change the others' estimates; compute_and_save_ability() passes only the
# practised ones. (A future joint model across areas would break that.)
estimate_competency <- function(responses, items, areas = LEARNING_AREA_LEVELS) {
  areas <- intersect(LEARNING_AREA_LEVELS, areas)
  prov <- item_provisional(items)
  result <- lapply(areas, function(area) {
    rows <- responses[
      !is.na(responses$learning_area) &
        responses$learning_area == area &
        !is.na(responses$bool_correct),
      ,
      drop = FALSE
    ]
    if (nrow(rows) < 1L) {
      return(list(theta = NA_real_, se = NA_real_, n = 0L, n_prov = 0L))
    }
    idx <- match(rows$id_item, items$id_item)
    ok <- !is.na(idx) & !is.na(items$irt_diff[idx])
    rows <- rows[ok, , drop = FALSE]
    idx <- idx[ok]
    if (nrow(rows) < 1L) {
      return(list(theta = NA_real_, se = NA_real_, n = 0L, n_prov = 0L))
    }
    b <- items$irt_diff[idx]
    theta <- estimate_theta(as.integer(rows$bool_correct), b)
    se <- if (is.na(theta)) NA_real_ else sem_1pl(theta, b)
    list(theta = theta, se = se, n = nrow(rows), n_prov = sum(prov[idx]))
  })

  data.frame(
    learning_area = factor(areas, levels = LEARNING_AREA_LEVELS),
    theta = vapply(result, `[[`, numeric(1), "theta"),
    se = vapply(result, `[[`, numeric(1), "se"),
    n_items = vapply(result, function(x) as.integer(x$n), integer(1)),
    n_provisional = vapply(result, function(x) as.integer(x$n_prov), integer(1)),
    stringsAsFactors = FALSE
  )
}

# Computes a fresh competency estimate from a user's full response history
# (deduped to the latest attempt per item) and persists it to db_ability.sqlite,
# but only for the learning areas practised since the last saved estimate:
# one new row per such area, none for the others (their latest row stays
# current). So each area's trajectory has a point exactly where it was
# practised. The single call site for both the login-time auto-check and the
# dashboard's refresh button (see R/server.R and R/mod_dashboard.R).
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
  # Skips don't count as practice either (ability_needs_update() ignores them
  # too, so a skip-only session doesn't leave it stuck at TRUE).
  ud <- ud[!is.na(ud$bool_correct), , drop = FALSE]
  ab <- db_get_ability(user_id, path = ability_path)
  since <- if (nrow(ab) > 0L) max(ab$computed_at, na.rm = TRUE) else -Inf
  practised <- intersect(LEARNING_AREA_LEVELS, ud$learning_area[ud$id_datetime > since])
  if (length(practised) == 0L) {
    return(invisible(NULL))
  }
  la <- latest_attempts(ud)
  la$learning_area <- factor(la$learning_area, levels = LEARNING_AREA_LEVELS)
  # Only the practised areas are estimated (each from its full history);
  # the others keep their latest saved row
  comp <- estimate_competency(la, items, areas = practised)
  rows <- build_ability_rows(comp, user_id, session_token)
  db_write_ability(user_id, rows, path = ability_path)
  invisible(rows)
}
