test_that("conditional cohort derivatives match finite differences", {
  theta <- log(c(0.8, 1.2, 0.6))
  upper <- c(1L, 2L, 3L, 4L, 5L, 5L)
  y <- c(1L, 1L, 2L, 4L, 3L, 5L)
  mu <- c(-2, -1.5, -0.2, 0.7, 1.2, 2.5)
  wt <- c(1, 0.5, 2, 1, 0, 3)
  family <- scala:::cohort_age_family(5L, upper)
  h <- 1e-5
  d <- family$Dd(y, mu, theta, wt, 2)
  plus <- family$Dd(y, mu + h, theta, wt, 2)
  minus <- family$Dd(y, mu - h, theta, wt, 2)
  expect_equal(d$Dmu, as.numeric((family$dev.resids(y, mu + h, wt, theta) -
    family$dev.resids(y, mu - h, wt, theta)) / (2 * h)), tolerance = 1e-7)
  for (pair in list(c('Dmu', 'Dmu2'), c('Dmu2', 'Dmu3'), c('Dmu3', 'Dmu4'))) {
    expect_equal(d[[pair[2]]], (plus[[pair[1]]] - minus[[pair[1]]]) / (2 * h), tolerance = 1e-7)
  }
  packed <- which(upper.tri(matrix(0, 3, 3), diag = TRUE), arr.ind = TRUE)
  # mgcv packs rows of the upper triangle, rather than columns.
  packed <- packed[order(packed[, 1], packed[, 2]), ]
  for (k in seq_along(theta)) {
    t_plus <- t_minus <- theta
    t_plus[k] <- t_plus[k] + h
    t_minus[k] <- t_minus[k] - h
    p <- family$Dd(y, mu, t_plus, wt, 2)
    m <- family$Dd(y, mu, t_minus, wt, 2)
    for (pair in list(c('D', 'Dth'), c('Dmu', 'Dmuth'),
      c('Dmu2', 'Dmu2th'), c('Dmu3', 'Dmu3th'))) {
      expect_equal(d[[pair[2]]][, k], as.numeric((p[[pair[1]]] - m[[pair[1]]]) / (2 * h)),
        tolerance = 1e-7)
    }
    for (j in seq_len(k)) {
      column <- which(packed[, 1] == j & packed[, 2] == k)
      for (pair in list(c('Dth', 'Dth2'), c('Dmuth', 'Dmuth2'), c('Dmu2th', 'Dmu2th2'))) {
        expect_equal(d[[pair[2]]][, column],
          (p[[pair[1]]][, j] - m[[pair[1]]][, j]) / (2 * h), tolerance = 1e-7)
      }
    }
  }
  expect_equal(unname(d$D[1]), 0)
  expect_equal(unname(d$Dmu[1]), 0)
})

test_that("conditional likelihood and prediction share the same support", {
  family <- scala:::cohort_age_family(5L, c(2L, 3L, 5L))
  theta <- log(c(0.8, 1.2, 0.6))
  cuts <- c(-1, -1 + cumsum(exp(theta)))
  eta <- c(-2, 0, 2)
  y <- c(2, 1, 4)
  p <- scala:::cohort_probabilities(eta, cuts, c(2L, 3L, 5L))
  f <- stats::plogis(outer(eta, cuts, function(e, c) c - e))
  raw <- cbind(f, 1) - cbind(0, f)
  raw[col(raw) > c(2, 3, 5)] <- 0
  expected <- raw / rowSums(raw)
  expect_equal(p, expected, tolerance = 1e-12)
  expect_equal(as.numeric(family$dev.resids(y, eta, c(1, 2, 0.5), theta)),
    -2 * c(1, 2, 0.5) * log(p[cbind(1:3, y)]), tolerance = 1e-12)
  tail <- scala:::cohort_probabilities(c(-1000, 1000), cuts, c(3L, 3L))
  expect_true(all(is.finite(tail)))
  expect_equal(rowSums(tail), c(1, 1), tolerance = 1e-10)
  expect_equal(tail[, 4:5], matrix(0, 2, 2))
  expect_error(scala:::cohort_probabilities(0, cuts, 0L), 'support')
})

test_that("binary and single-admissible-cohort likelihoods are valid", {
  f <- scala:::cohort_age_family(2L, c(1L, 2L))
  d <- f$Dd(c(1L, 2L), c(0, 0), numeric(0), c(1, 1), 2)
  expect_equal(as.numeric(d$D), c(0, -2 * stats::plogis(1, log.p = TRUE)))
  expect_equal(d$Dmu[1], 0)
  expect_equal(d$Dmu2[1], 0)
  expect_true(all(is.finite(d$Dmu3)))
  expect_true(all(is.finite(d$Dmu4)))
})

test_that("fitting, response prediction and assigned ages use positive ages", {
  set.seed(812)
  d <- data.frame(year = sample(2010:2017, 700, TRUE), length = stats::runif(700, 20, 80))
  cohorts <- 2003:2015
  cuts <- seq(-1, 5, length.out = length(cohorts) - 1L)
  eta <- -0.06 * (d$length - 50) + 0.7 * (d$year - 2010)
  p <- scala:::cohort_probabilities(eta, cuts, findInterval(d$year - 2, cohorts))
  index <- vapply(seq_len(nrow(d)), function(i) sample.int(length(cohorts), 1, prob = p[i, ]), integer(1))
  d$age <- d$year - cohorts[index] - 1
  fit <- fit_cohort_alk(d, by_sex = FALSE, k_length = 4, k_year = 4, verbose = FALSE)
  recovered <- stats::predict(fit$model, d, type = 'response')
  expect_lt(sqrt(mean((unname(recovered) - p)^2)), 0.04)
  expect_s3_class(fit$model, 'gam')
  expect_true(fit$age_support$conditional)
  expect_true(fit$model$converged)
  expect_true(all(is.finite(fit$model$sp)))
  nd <- data.frame(length = c(25, 50, 75), year = c(2010, 2013, 2017))
  p <- fit$predict_cohort(nd$length, nd$year)
  direct <- stats::predict(fit$model, nd, type = 'response')
  expect_equal(p, direct, tolerance = 1e-12)
  expect_equal(rowSums(p), rep(1, 3), tolerance = 1e-10)
  impossible <- outer(nd$year - 1, fit$cohorts, '-') <= 0
  expect_true(all(p[impossible] == 0))
  ages <- fit$predict_age(nd$length, nd$year)
  expect_equal(rowSums(ages), rep(1, 3), tolerance = 1e-10)
  for (i in 1:3) {
    a <- nd$year[i] - fit$cohorts - 1
    expect_equal(unname(ages[i, a[a > 0]]), unname(p[i, a > 0]))
  }
  response <- stats::predict(fit$model, nd, type = 'response', se.fit = TRUE)
  expect_equal(response$fit, p)
  expect_true(all(is.finite(response$se.fit)))
  restored <- unserialize(serialize(fit, NULL))
  expect_equal(restored$predict_age(nd$length, nd$year), ages)
  assigned <- assign_ages_from_cohort(nd, fit, method = 'random', seed = 19, verbose = FALSE)
  expect_true(all(assigned$age >= 1))
  expected <- assign_ages_from_cohort(nd, fit, method = 'expected', verbose = FALSE)
  expect_equal(expected$age, round(as.vector(ages %*% seq_len(ncol(ages)))))
  future <- data.frame(length = 40, year = 2019)
  expect_true(is.na(assign_ages_from_cohort(future, fit, verbose = FALSE)$age))
  expect_true(assign_ages_from_cohort(future, fit, predict_missing = TRUE,
    seed = 14, verbose = FALSE)$age >= 1)
  expect_error(fit$predict_age(40, min(fit$cohorts) + 1), 'No modelled cohort')
  expect_error(fit$predict_age(40, NA_real_), 'finite integer')
  expect_error(fit$predict_age(NA_real_, 2015), 'finite')
  d$age[1] <- 0
  expect_error(fit_cohort_alk(d, by_sex = FALSE), 'positive integers')
  d$age[1] <- 1.5
  expect_error(fit_cohort_alk(d, by_sex = FALSE), 'positive integers')
})

test_that("weighted sex and spatial models retain support through prediction", {
  set.seed(902)
  d <- data.frame(year = sample(2011:2018, 600, TRUE),
    length = stats::runif(600, 20, 80), depth = stats::runif(600, 500, 1500),
    sex = sample(c('male', 'female'), 600, TRUE))
  d$age <- pmax(1, round(2 + 0.1 * (d$length - 20) + stats::rnorm(600, 0, 1.5)))
  fit <- fit_cohort_alk(d, k_length = 4, k_year = 4, additional_terms = 's(depth, k = 4)',
    weights = rep(c(1, 2), 300), method = 'ML', verbose = FALSE)
  nd <- d[c(4, 3, 2, 1), ]
  direct <- stats::predict(fit$model, nd, type = 'response', block.size = 2)
  wrapped <- fit$predict_cohort(nd$length, nd$year, nd$sex, depth = nd$depth)
  expect_equal(direct, wrapped)
  expect_equal(rowSums(direct), rep(1, 4), tolerance = 1e-10)
  invalid <- outer(nd$year - fit$age_offset, fit$cohorts, '-') <= 0
  expect_true(all(direct[invalid] == 0))
  expect_true(all(assign_ages_from_cohort(nd, fit, seed = 32, verbose = FALSE)$age > 0))
  d$depth[1] <- NA_real_
  expect_error(fit_cohort_alk(d, k_length = 4, k_year = 4,
    additional_terms = 's(depth, k = 4)', verbose = FALSE), 'missing')
})
