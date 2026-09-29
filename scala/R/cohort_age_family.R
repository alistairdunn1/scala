# Conditional ordinal probabilities on the positive-integer-age support.
cohort_probabilities <- function(eta, cuts, upper, se = NULL) {
  n <- length(eta)
  n_class <- length(cuts) + 1L
  if (length(upper) != n || any(!is.finite(eta)) || any(!is.finite(cuts)) ||
      any(diff(cuts) <= 0) || anyNA(upper) || any(upper < 1 | upper > n_class)) {
    stop("Invalid linear predictors, cut-points or positive-age support.")
  }
  log_probability <- matrix(-Inf, n, n_class)
  log_probability[, 1L] <- stats::plogis(cuts[1L] - eta, log.p = TRUE)
  if (n_class > 2L) {
    for (j in 2:(n_class - 1L)) {
      # This form remains finite in either logistic tail.
      log_probability[, j] <- stats::plogis(cuts[j] - eta, log.p = TRUE) +
        stats::plogis(eta - cuts[j - 1L], log.p = TRUE) +
        log(-expm1(cuts[j - 1L] - cuts[j]))
    }
  }
  log_probability[, n_class] <- stats::plogis(eta - cuts[n_class - 1L], log.p = TRUE)
  log_mass <- numeric(n)
  truncated <- upper < n_class
  log_mass[truncated] <- stats::plogis(cuts[upper[truncated]] - eta[truncated], log.p = TRUE)
  probability <- exp(log_probability - log_mass)
  probability[col(probability) > upper] <- 0
  if (any(!is.finite(probability)) || any(abs(rowSums(probability) - 1) > 1e-8)) {
    stop("Conditional cohort probabilities are not finite and normalised.")
  }
  if (is.null(se)) return(probability)
  cumulative <- stats::plogis(outer(eta, cuts, function(e, cut) cut - e))
  valid_cumulative <- rep(1, n)
  valid_cumulative[truncated] <- cumulative[cbind(which(truncated), upper[truncated])]
  score <- cbind(cumulative, 1) + cbind(0, cumulative) - valid_cumulative
  list(fit = probability, se.fit = abs(probability * score) * as.numeric(se))
}

# An extended family supplies derivatives through fourth order so that mgcv
# estimates smoothness and cohort cut-points under the conditional likelihood.
cohort_age_family <- function(n_class, upper) {
  if (n_class < 2L || anyNA(upper) || any(upper < 1L | upper > n_class)) {
    stop("At least two cohorts and a valid positive-age support are required.")
  }
  base <- mgcv::ocat(R = n_class)
  family <- base
  support <- as.integer(upper)
  check_support <- function(y, mu) {
    if (length(y) != length(support) || length(mu) != length(support) ||
        any(!is.finite(y)) || any(y != floor(y)) || any(y < 1 | y > support)) {
      stop("Cohort observations do not match their positive-age support.")
    }
  }
  correction <- function(mu, theta) {
    cuts <- c(-1, -1 + cumsum(exp(theta)))
    t <- rep(Inf, length(mu))
    use <- support < n_class
    t[use] <- cuts[support[use]] - mu[use]
    f <- stats::plogis(t)
    q <- stats::plogis(-t)
    list(log_mass = stats::plogis(t, log.p = TRUE), h1 = q,
      h2 = -f * q, h3 = -f * q * (1 - 2 * f),
      h4 = -f * q * (1 - 6 * f + 6 * f^2))
  }
  family$dev.resids <- function(y, mu, wt, theta = NULL) {
    check_support(y, mu)
    if (is.null(theta)) theta <- base$getTheta()
    wt <- rep_len(wt, length(y))
    cuts <- c(-1, -1 + cumsum(exp(theta)))
    # Calculate log probabilities directly to retain information in the tails.
    lower <- c(-Inf, cuts)[y] - mu
    higher <- c(cuts, Inf)[y] - mu
    log_p <- stats::plogis(higher, log.p = TRUE) +
      stats::plogis(-lower, log.p = TRUE) + log(-expm1(lower - higher))
    result <- -2 * wt * (log_p - correction(mu, theta)$log_mass)
    result[support == 1L] <- 0
    attr(result, "sign") <- sign((lower + higher) / 2)
    result
  }
  family$Dd <- function(y, mu, theta, wt = NULL, level = 0) {
    check_support(y, mu)
    if (is.null(wt)) wt <- rep(1, length(y))
    wt <- rep_len(wt, length(y))
    out <- base$Dd(y, mu, theta, wt, level)
    h <- correction(mu, theta)
    w <- 2 * wt
    out$D <- family$dev.resids(y, mu, wt, theta)
    out$Dmu <- out$Dmu - w * h$h1
    out$Dmu2 <- out$Dmu2 + w * h$h2
    out$EDmu2 <- out$Dmu2
    n_theta <- length(theta)
    if (level > 0 && n_theta == 0L) {
      f <- stats::plogis(-1 - mu)
      q <- stats::plogis(mu + 1)
      out$Dmu3 <- -w * f * q * (1 - 2 * f) - w * h$h3
      if (level > 1) out$Dmu4 <- w * f * q * (1 - 6 * f + 6 * f^2) + w * h$h4
    }
    if (level > 0 && n_theta > 0L) {
      v <- outer(support, seq_len(n_theta), function(u, k) u > k & u < n_class)
      v <- sweep(v, 2, exp(theta), `*`)
      out$Dmu3 <- out$Dmu3 - w * h$h3
      out$Dth <- out$Dth + w * h$h1 * v
      out$Dmuth <- out$Dmuth - w * h$h2 * v
      out$Dmu2th <- out$Dmu2th + w * h$h3 * v
      out$EDmu2th <- out$Dmu2th
      if (level > 1) {
        out$Dmu4 <- out$Dmu4 + w * h$h4
        out$Dmu3th <- out$Dmu3th - w * h$h4 * v
        column <- 0L
        for (j in seq_len(n_theta)) for (k in j:n_theta) {
          column <- column + 1L
          product <- v[, j] * v[, k]
          second <- if (j == k) v[, j] else rep(0, length(y))
          out$Dth2[, column] <- out$Dth2[, column] + w * (h$h2 * product + h$h1 * second)
          out$Dmuth2[, column] <- out$Dmuth2[, column] - w * (h$h3 * product + h$h2 * second)
          out$Dmu2th2[, column] <- out$Dmu2th2[, column] + w * (h$h4 * product + h$h3 * second)
        }
      }
    }
    # A single admissible cohort has probability one and contributes no information.
    for (name in names(out)) if (!is.null(out[[name]])) {
      if (is.matrix(out[[name]])) out[[name]][support == 1L, ] <- 0
      else out[[name]][support == 1L] <- 0
    }
    out
  }
  family$aic <- function(y, mu, theta = NULL, wt, dev) {
    sum(family$dev.resids(y, mu, wt, theta))
  }
  family$rd <- function(mu, wt, scale) {
    p <- cohort_probabilities(mu, base$getTheta(TRUE), support)
    vapply(seq_along(mu), function(i) sample.int(n_class, 1L, prob = p[i, ]), integer(1))
  }
  family$predict <- function(...) {
    stop("Use stats::predict() on the cohort GAM so sampling-year age support is supplied.")
  }
  family$residuals <- function(object, type = c("deviance", "working", "response")) {
    type <- match.arg(type)
    if (type == "working") return(object$residuals)
    p <- cohort_probabilities(object$linear.predictors, base$getTheta(TRUE), support)
    difference <- object$y - as.vector(p %*% seq_len(n_class))
    if (type == "response") return(difference)
    sign(difference) * sqrt(pmax(0, family$dev.resids(object$y,
      object$linear.predictors, object$prior.weights)))
  }
  family$postproc <- function(family, y, prior.weights, fitted, linear.predictors,
                             offset, intercept) {
    result <- base$postproc(family, y, prior.weights, fitted, linear.predictors, offset, intercept)
    result$family <- "Positive-age conditional ordered categorical"
    result
  }
  family
}

#' Predict from a cohort GAM on positive-age support
#'
#' @description Calculates cohort probabilities conditional on positive integer
#'   ages in each sampling year. Other prediction types use the GAM predictor.
#' @param object Fitted cohort GAM
#' @param newdata Data frame containing sampling year and fitted covariates
#' @param type Prediction type, including \code{"response"}, \code{"link"} and \code{"lpmatrix"}
#' @param se.fit Whether to return standard errors
#' @param ... Additional arguments passed to \code{mgcv::predict.gam}
#' @return A prediction matrix or a list containing predictions and standard errors
#' @details Response standard errors use the delta method for the linear predictor,
#'   conditional on the estimated cut-points. Response predictions require finite
#'   covariates and an integer sampling year with at least one admissible cohort.
#' @export
predict.cohort_gam <- function(object, newdata, type = "link", se.fit = FALSE, ...) {
  if (missing(newdata)) newdata <- object$model
  if (type != "response") {
    return(mgcv::predict.gam(object, newdata = newdata, type = type, se.fit = se.fit, ...))
  }
  years <- newdata$year
  if (!is.numeric(years) || any(!is.finite(years)) || any(years != floor(years)) || !length(years)) {
    stop("Response predictions require finite integer sampling years.")
  }
  upper <- findInterval(years - object$age_offset - 1, object$cohorts)
  if (any(upper < 1L)) stop("No modelled cohort has a positive age in a requested sampling year.")
  prediction <- mgcv::predict.gam(object, newdata = newdata, type = "link", se.fit = se.fit, ...)
  if (se.fit) {
    result <- cohort_probabilities(as.numeric(prediction$fit), object$family$getTheta(TRUE),
      upper, prediction$se.fit)
    colnames(result$fit) <- colnames(result$se.fit) <- paste0("cohort_", object$cohorts)
  } else {
    result <- cohort_probabilities(as.numeric(prediction), object$family$getTheta(TRUE), upper)
    colnames(result) <- paste0("cohort_", object$cohorts)
  }
  result
}
