test_that("plus-group thresholds are optional and validated by all fitters", {
  d <- data.frame(age = rep(1:5, 10), length = seq(20, 80, length.out = 50),
    year = rep(2010:2014, each = 10))
  for (f in list(fit_ordinal_alk, fit_multinomial_alk, fit_cohort_alk)) {
    expect_null(formals(f)$plus_group)
    for (p in list(0, -1, 2.5, NA_real_, Inf, c(4, 5), "5")) {
      expect_error(f(d, by_sex = FALSE, plus_group = p), "plus_group")
    }
    expect_error(f(d, by_sex = FALSE, plus_group = 1), "two observed ages|below plus_group")
  }
})

test_that("direct-age plus groups agree with manually pooled observations", {
  set.seed(281)
  d <- data.frame(length = stats::runif(300, 40, 120))
  d$age <- sample(c(2, 5, 9, 15), nrow(d), TRUE)
  for (f in list(fit_ordinal_alk, fit_multinomial_alk)) {
    fit <- f(d, by_sex = FALSE, k_length = 4, plus_group = 9, verbose = FALSE)
    pooled <- f(transform(d, age = pmin(age, 9)), by_sex = FALSE,
      k_length = 4, verbose = FALSE)
    expect_equal(fit$plus_group, 9)
    expect_equal(fit$ages, c(2, 5, 9))
    expect_equal(fit$predict_age(c(50, 100)), pooled$predict_age(c(50, 100)))
    expect_equal(colnames(fit$predict_age(80)), paste0("age_", 1:9))
    expect_equal(rowSums(fit$predict_age(c(50, 100))), c(1, 1), tolerance = 1e-12)
    high <- f(d, by_sex = FALSE, k_length = 4, plus_group = 20, verbose = FALSE)
    expect_equal(ncol(high$predict_age(80)), 20)
    expect_equal(as.numeric(high$predict_age(80)[, 16:20]), rep(0, 5))
  }
})

test_that("censored cohort likelihood and all derivatives match cumulative probabilities", {
  theta <- log(c(0.8, 1.2, 0.6))
  upper <- c(1L, 2L, 3L, 4L, 5L, 5L, 5L)
  y <- c(1L, 1L, 2L, 4L, 3L, 5L, 4L)
  mu <- c(-2, -1.5, -0.2, 0.7, 1.2, 2.5, -0.5)
  wt <- c(1, 0.5, 2, 1, 0, 3, 2)
  censored <- c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  family <- scala:::cohort_age_family(5L, upper, censored)
  cuts <- c(-1, -1 + cumsum(exp(theta)))
  p <- scala:::cohort_probabilities(mu, cuts, upper)
  observed <- vapply(seq_along(y), function(i) {
    if (censored[i]) sum(p[i, seq_len(y[i])]) else p[i, y[i]]
  }, numeric(1))
  expect_equal(as.numeric(family$dev.resids(y, mu, wt, theta)),
    -2 * wt * log(observed), tolerance = 1e-12)
  h <- 1e-5
  d <- family$Dd(y, mu, theta, wt, 2)
  plus <- family$Dd(y, mu + h, theta, wt, 2)
  minus <- family$Dd(y, mu - h, theta, wt, 2)
  for (pair in list(c("D", "Dmu"), c("Dmu", "Dmu2"),
    c("Dmu2", "Dmu3"), c("Dmu3", "Dmu4"))) {
    expect_equal(as.numeric(d[[pair[2]]]),
      as.numeric((plus[[pair[1]]] - minus[[pair[1]]]) / (2 * h)), tolerance = 1e-7)
  }
  packed <- which(upper.tri(matrix(0, 3, 3), diag = TRUE), arr.ind = TRUE)
  packed <- packed[order(packed[, 1], packed[, 2]), ]
  for (k in seq_along(theta)) {
    t_plus <- t_minus <- theta
    t_plus[k] <- t_plus[k] + h
    t_minus[k] <- t_minus[k] - h
    p <- family$Dd(y, mu, t_plus, wt, 2)
    m <- family$Dd(y, mu, t_minus, wt, 2)
    for (pair in list(c("D", "Dth"), c("Dmu", "Dmuth"),
      c("Dmu2", "Dmu2th"), c("Dmu3", "Dmu3th"))) {
      expect_equal(d[[pair[2]]][, k], as.numeric((p[[pair[1]]] - m[[pair[1]]]) / (2 * h)),
        tolerance = 1e-7)
    }
    for (j in seq_len(k)) {
      column <- which(packed[, 1] == j & packed[, 2] == k)
      for (pair in list(c("Dth", "Dth2"), c("Dmuth", "Dmuth2"), c("Dmu2th", "Dmu2th2"))) {
        expect_equal(d[[pair[2]]][, column],
          (p[[pair[1]]][, j] - m[[pair[1]]][, j]) / (2 * h), tolerance = 1e-7)
      }
    }
  }
  binary <- scala:::cohort_age_family(2L, c(2L, 2L), c(TRUE, TRUE))
  b <- binary$Dd(c(1L, 2L), c(0, 0), numeric(0), c(1, 1), 2)
  expect_equal(b$Dmu[2], 0)
  expect_equal(b$Dmu4[2], 0)
  expect_true(all(is.finite(b$Dmu3)))
})

test_that("cohort plus groups fit censored ages and aggregate predictions", {
  set.seed(812)
  d <- data.frame(year = sample(2010:2017, 700, TRUE), length = stats::runif(700, 20, 80))
  cohorts <- 2003:2015
  cuts <- seq(-1, 5, length.out = length(cohorts) - 1L)
  eta <- -0.06 * (d$length - 50) + 0.7 * (d$year - 2010)
  p <- scala:::cohort_probabilities(eta, cuts, findInterval(d$year - 2, cohorts))
  index <- vapply(seq_len(nrow(d)), function(i) sample.int(length(cohorts), 1, prob = p[i, ]), integer(1))
  d$age <- d$year - cohorts[index] - 1
  fit <- fit_cohort_alk(d, by_sex = FALSE, k_length = 4, k_year = 4,
    plus_group = 6, verbose = FALSE)
  changed <- d
  changed$age[changed$age >= 6] <- 100
  same <- fit_cohort_alk(changed, by_sex = FALSE, k_length = 4, k_year = 4,
    plus_group = 6, verbose = FALSE)
  expect_equal(stats::coef(fit$model), stats::coef(same$model), tolerance = 1e-10)
  expect_equal(fit$plus_group, 6)
  expect_true(fit$oldest_cohort_is_tail)
  nd <- data.frame(length = c(25, 50, 75), year = c(2010, 2013, 2017))
  age <- fit$predict_age(nd$length, nd$year)
  cp <- fit$predict_cohort(nd$length, nd$year)
  expect_equal(colnames(age), paste0("age_", 1:6))
  expect_equal(rowSums(age), rep(1, 3), tolerance = 1e-10)
  for (i in 1:3) {
    implied <- nd$year[i] - fit$cohorts - fit$age_offset
    expect_equal(unname(age[i, 6]), sum(cp[i, implied >= 6]), tolerance = 1e-12)
  }
  observed <- fit$predict_age(d$length, d$year)[cbind(seq_len(nrow(d)), pmin(d$age, 6))]
  expect_equal(as.numeric(stats::logLik(fit$model)), sum(log(observed)), tolerance = 1e-7)
  expect_equal(unserialize(serialize(fit, NULL))$predict_age(nd$length, nd$year), age)
  expect_error(fit$predict_age(50, 2009), "first training year")
  expect_equal(sum(fit$predict_age(50, 2019)), 1, tolerance = 1e-10)
})
