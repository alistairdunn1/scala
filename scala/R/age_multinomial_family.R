# Multinomial log likelihood with stable probabilities and observation weights.
# Derivative packing follows mgcv::multinom (Simon Wood, GPL-2 or later).
age_multinomial_family <- function(n_predictors) {
  family <- mgcv::multinom(K = n_predictors)
  family$ll <- function(y, X, coef, wt, family, offset = NULL, deriv = 0,
                       d1b = 0, d2b = 0, Hp = NULL, rank = 0, fh = NULL,
                       D = NULL, eta = NULL, ncv = FALSE, sandwich = FALSE) {
    n <- length(y)
    jj <- attr(X, "lpi")
    k <- family$nlp
    if (is.null(eta)) {
      if (!is.matrix(X)) stop("The multinomial age family requires a dense GAM model matrix.")
      eta <- matrix(0, n, k)
      for (i in seq_len(k)) {
        eta[, i] <- drop(X[, jj[[i]], drop = FALSE] %*% coef[jj[[i]]])
        if (!is.null(offset) && length(offset) >= i && !is.null(offset[[i]])) {
          eta[, i] <- eta[, i] + offset[[i]]
        }
      }
    }
    if (!is.matrix(eta) || ncol(eta) != k) stop("Invalid multinomial predictor dimensions.")
    if (is.null(wt)) wt <- rep(1, n)
    wt <- rep_len(wt, n)
    logits <- cbind(0, eta)
    shift <- apply(logits, 1L, max)
    ee <- exp(eta - shift)
    beta <- exp(-shift) + rowSums(ee)
    alpha <- shift + log(beta)
    l <- sum(wt * (logits[cbind(seq_len(n), y + 1L)] - alpha))
    l1 <- matrix(0, n, k)
    l2 <- l3 <- l4 <- 0
    tri <- family$tri
    if (deriv) {
      probability <- ee / beta
      l2 <- matrix(0, n, k * (k + 1) / 2)
      ii <- 0L
      for (i in seq_len(k)) for (j in i:k) {
        ii <- ii + 1L
        l2[, ii] <- probability[, i] * probability[, j] -
          if (i == j) probability[, i] else 0
      }
      for (i in seq_len(k)) l1[, i] <- as.numeric(y == i) - probability[, i]
    }
    if (deriv > 1) {
      l3 <- matrix(0, n, k * (k + 1) * (k + 2) / 6)
      ii <- 0L
      for (i in seq_len(k)) for (j in i:k) for (h in j:k) {
        ii <- ii + 1L
        if (i == j && j == h) {
          l3[, ii] <- l2[, tri$i2[i, i]] + 2 * probability[, i]^2 - 2 * probability[, i]^3
        } else if (i != j && j != h && i != h) {
          l3[, ii] <- -2 * probability[, i] * probability[, j] * probability[, h]
        } else {
          other <- if (i == j) h else j
          l3[, ii] <- l2[, tri$i2[i, other]] -
            2 * probability[, i] * probability[, j] * probability[, h]
        }
      }
    }
    if (deriv > 3) {
      l4 <- matrix(0, n, k * (k + 1) * (k + 2) * (k + 3) / 24)
      ii <- 0L
      for (i in seq_len(k)) for (j in i:k) for (h in j:k) for (g in h:k) {
        ii <- ii + 1L
        unique_index <- unique(c(i, j, h, g))
        product <- probability[, i] * probability[, j] * probability[, h] * probability[, g]
        if (length(unique_index) == 1L) {
          l4[, ii] <- l3[, tri$i3[i, i, i]] + 4 * probability[, i]^2 -
            10 * probability[, i]^3 + 6 * probability[, i]^4
        } else if (length(unique_index) == 4L) {
          l4[, ii] <- 6 * product
        } else if (length(unique_index) == 3L) {
          l4[, ii] <- l3[, tri$i3[unique_index[1], unique_index[2], unique_index[3]]] + 6 * product
        } else if (sum(unique_index[1] == c(i, j, h, g)) == 2L) {
          l4[, ii] <- l3[, tri$i3[unique_index[1], unique_index[2], unique_index[2]]] -
            2 * probability[, unique_index[1]]^2 * probability[, unique_index[2]] + 6 * product
        } else {
          if (sum(unique_index[1] == c(i, j, h, g)) == 1L) unique_index <- rev(unique_index)
          l4[, ii] <- l3[, tri$i3[unique_index[1], unique_index[1], unique_index[2]]] -
            4 * probability[, unique_index[1]]^2 * probability[, unique_index[2]] + 6 * product
        }
      }
    }
    if (deriv) {
      l1 <- l1 * wt
      l2 <- l2 * wt
      if (is.matrix(l3)) l3 <- l3 * wt
      if (is.matrix(l4)) l4 <- l4 * wt
      # Supply full Hessian derivatives for BFGS smoothing optimisation, retaining
      # matrix dimensions when a selection penalty has rank one.
      result <- mgcv::gamlss.gH(X, jj, l1, l2, tri$i2, l3 = l3, i3 = tri$i3,
        l4 = l4, i4 = tri$i4, d1b = d1b, d2b = d2b,
        deriv = if (deriv == 2L) 2L else deriv - 1L,
        fh = fh, D = D, sandwich = sandwich)
      if (ncv) {
        result$l1 <- l1
        result$l2 <- l2
        result$l3 <- l3
      }
    } else result <- list()
    result$l <- l
    result
  }
  family
}
