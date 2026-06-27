prob_2pl <- function(theta, a, b) {
  1 / (1 + exp(-a * (theta - b)))
}

neg_log_lik <- function(theta, responses, a, b) {
  p <- prob_2pl(theta, a, b)
  p <- pmax(pmin(p, 1 - 1e-9), 1e-9)
  -sum(responses * log(p) + (1 - responses) * log(1 - p))
}

estimate_theta <- function(responses, a, b) {
  fit <- tryCatch(
    optim(
      0,
      neg_log_lik,
      method = "L-BFGS-B",
      lower = -3,
      upper = 3,
      responses = responses,
      a = a,
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
      return(list(theta = NA_real_, n = 0L))
    }
    idx <- match(rows$id_item, items$id_item)
    ok <- !is.na(idx) &
      !is.na(items$irt_discr[idx]) &
      !is.na(items$irt_diff[idx])
    rows <- rows[ok, , drop = FALSE]
    idx <- idx[ok]
    if (nrow(rows) < 1L) {
      return(list(theta = NA_real_, n = 0L))
    }
    theta <- estimate_theta(
      as.integer(rows$bool_correct),
      items$irt_discr[idx],
      items$irt_diff[idx]
    )
    list(theta = theta, n = nrow(rows))
  })

  data.frame(
    learning_area = factor(LEARNING_AREA_LEVELS, levels = LEARNING_AREA_LEVELS),
    theta = vapply(result, `[[`, numeric(1), "theta"),
    n_items = vapply(result, function(x) as.integer(x$n), integer(1)),
    stringsAsFactors = FALSE
  )
}
