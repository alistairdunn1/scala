multinomial_test_data <- function(n = 400) {
  set.seed(921)
  d <- data.frame(length = stats::runif(n, 50, 150), year = rep(2010:2019, length.out = n),
    sex = sample(c("female", "male"), n, replace = TRUE), depth = stats::runif(n, 500, 1000))
  eta <- cbind(0, (d$length - 100) / 30 + 0.4 * sin((d$year - 2010) / 2),
    -(d$length - 100) / 40 + 0.5 * (d$sex == "female"))
  p <- exp(eta) / rowSums(exp(eta))
  d$age <- c(2, 9, 26)[rowSums(t(apply(p, 1, cumsum)) < stats::runif(n)) + 1L]
  d
}

test_that("multinomial fits category-specific length, year and sex probabilities", {
  d <- multinomial_test_data()
  fit <- fit_multinomial_alk(d, k_length = 4, k_year = 4,
    additional_terms = "s(depth, k = 4)", reference_age = 9, verbose = FALSE)
  expect_s3_class(fit, "multinomial_alk")
  expect_equal(fit$ages, c(2, 9, 26))
  expect_equal(fit$model_ages, c(9, 2, 26))
  expect_equal(fit$reference_age, 9)
  expect_equal(fit$model$family$nlp, 2)
  expect_equal(length(fit$model$smooth), 12)
  p <- fit$predict_function(d$length[1:20], d$sex[1:20], year = d$year[1:20], depth = d$depth[1:20])
  native <- stats::predict(fit$model, newdata = d[1:20, ], type = "response")
  expect_equal(unname(p), unname(native[, match(fit$ages, fit$model_ages)]), tolerance = 1e-10)
  expect_equal(unname(rowSums(p)), rep(1, 20), tolerance = 1e-12)
  full <- fit$predict_age(d$length[1:20], d$year[1:20], d$sex[1:20], depth = d$depth[1:20])
  expect_equal(unname(full[, fit$ages]), unname(p))
  expect_true(all(full[, setdiff(1:26, fit$ages)] == 0))
  expect_true(all(is.finite(fit$model$coefficients)))
  expect_equal(fit$model$outer.info$conv, "full convergence")
  expect_equal(fit$training_years, 2010:2019)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(fit, path)
  expect_equal(readRDS(path)$predict_age(100, 2015, "FEMALE", depth = 750),
    fit$predict_age(100, 2015, "female", depth = 750))
  extreme <- fit$predict_age(c(1, 1e6), 2015, "female", depth = 750)
  expect_true(all(is.finite(extreme)))
  expect_equal(rowSums(extreme), c(1, 1), tolerance = 1e-12)
  expect_error(fit$predict_function(100, "female"), "Missing prediction covariates")
  expect_error(fit$predict_age(100, 2015.5, "female", depth = 750), "integers")
  expect_error(fit$predict_age(100, 2015, "unknown", depth = 750), "sex values")
  expect_error(fit$predict_age(100, 2015, "female", depth = NA_real_), "observed values")
  expect_match(paste(capture.output(print(fit)), collapse = " "), "Reference age: 9")
})

test_that("multinomial likelihood weights agree with the fitted probabilities", {
  d <- multinomial_test_data(250)
  weights <- ifelse(d$age == 26, 3, 1)
  fit <- fit_multinomial_alk(d, by_sex = FALSE, k_length = 4,
    weights = weights, select = FALSE, gamma = 1, method = "REML", verbose = FALSE)
  expect_equal(fit$reference_age, 26)
  p <- fit$predict_function(d$length)
  observed <- p[cbind(seq_len(nrow(d)), match(d$age, fit$ages))]
  expect_equal(as.numeric(stats::logLik(fit$model)), sum(weights * log(observed)), tolerance = 1e-7)
  expect_equal(fit$model$prior.weights, weights)
  expect_equal(fit$model$method, "REML")
  duplicated <- fit_multinomial_alk(d[rep(seq_len(nrow(d)), weights), ],
    by_sex = FALSE, k_length = 4, select = FALSE, gamma = 1, verbose = FALSE)
  expect_equal(fit$predict_function(c(70, 100, 130)),
    duplicated$predict_function(c(70, 100, 130)), tolerance = 2e-4)
  expect_error(fit_multinomial_alk(d, method = "ML"), "ML is not supported")
})

test_that("two-age multinomial probabilities agree with a binomial fit", {
  d <- multinomial_test_data(300)
  d <- d[d$age != 9, ]
  fit <- fit_multinomial_alk(d, by_sex = FALSE, k_length = 4,
    reference_age = 2, gamma = 1, select = FALSE, verbose = FALSE)
  d$older <- as.integer(d$age == 26)
  binomial <- mgcv::gam(older ~ s(length, k = 4), data = d,
    family = stats::binomial(), method = "REML", gamma = 1, select = FALSE)
  expect_equal(unname(fit$predict_function(c(70, 100, 140))[, 2]),
    as.numeric(stats::predict(binomial, newdata = data.frame(length = c(70, 100, 140)),
      type = "response")), tolerance = 1e-4)
})

test_that("annual factors and explicit smooth by terms retain their meanings", {
  expect_equal(scala:::alk_sex_term("factor(year)", TRUE), "factor(year)")
  expect_equal(scala:::alk_sex_term("sex:factor(year)", TRUE), "sex:factor(year)")
  expect_equal(scala:::alk_sex_term("s(depth, by = sex, k = 4)", TRUE), "s(depth, by = sex, k = 4)")
  expect_equal(scala:::alk_sex_term("ti(length, year, k = c(4, 4))", TRUE),
    "ti(length, year, k = c(4, 4), by = sex)")
  d <- multinomial_test_data(300)
  fit <- fit_multinomial_alk(d, k_length = 4, additional_terms = "factor(year)", verbose = FALSE)
  expect_null(fit$k_year)
  expect_equal(fit$training_years, 2010:2019)
  expect_equal(sum(fit$predict_age(100, 2015, "female")), 1, tolerance = 1e-12)
  expect_error(fit$predict_age(100, 2030, "female"), "new level|not in original")
})

test_that("multinomial derivatives retain weighting and mgcv derivative packing", {
  set.seed(802)
  x <- matrix(stats::rnorm(160), 20, 8)
  attr(x, "lpi") <- split(seq_len(8), rep(1:4, each = 2))
  y <- rep(0:4, 4)
  coefficient <- seq(-0.4, 0.3, length.out = 8)
  family <- scala:::age_multinomial_family(4)
  native <- mgcv::multinom(K = 4)
  for (deriv in c(1, 3, 4)) {
    args <- list(y = y, X = x, coef = coefficient, wt = rep(1, 20),
      deriv = deriv, d1b = matrix(0.1, 8, 2), d2b = matrix(0.05, 8, 3),
      fh = chol(diag(8), pivot = TRUE), D = rep(1, 8), ncv = TRUE)
    observed <- do.call(family$ll, c(args, list(family = family)))
    expected <- do.call(native$ll, c(args, list(family = native)))
    for (name in setdiff(names(expected), "l")) {
      expect_equal(observed[[name]], expected[[name]], tolerance = 1e-10)
    }
  }
  weights <- rep(c(0, 0.5, 1, 2), 5)
  h <- 1e-5
  derivative <- family$ll(y, x, coefficient, weights, family, deriv = 1)
  for (j in seq_along(coefficient)) {
    plus <- minus <- coefficient
    plus[j] <- plus[j] + h
    minus[j] <- minus[j] - h
    p <- family$ll(y, x, plus, weights, family, deriv = 1)
    m <- family$ll(y, x, minus, weights, family, deriv = 1)
    expect_equal(as.numeric(derivative$lb[j]), (p$l - m$l) / (2 * h), tolerance = 1e-7)
    expect_equal(as.numeric(derivative$lbb[, j]), as.numeric((p$lb - m$lb) / (2 * h)),
      tolerance = 1e-7)
  }
})

test_that("invalid multinomial inputs stop rather than changing support", {
  d <- multinomial_test_data(100)
  for (age in c(-1, 0, 1.5, NA_real_, Inf)) {
    bad <- d
    bad$age[1] <- age
    expect_error(fit_multinomial_alk(bad), "age must contain")
  }
  expect_error(fit_multinomial_alk(transform(d, age = 2)), "two observed ages")
  expect_error(fit_multinomial_alk(d, reference_age = 3), "reference_age")
  expect_error(fit_multinomial_alk(d, weights = ifelse(d$age == 26, 0, 1)), "positive total observation weight")
  expect_error(fit_multinomial_alk(d, weights = -1), "weights")
  expect_error(fit_multinomial_alk(d, gamma = NA_real_), "gamma")
  expect_error(fit_multinomial_alk(d, method = "invalid"), "REML or ML")
  expect_error(fit_multinomial_alk(d, k_length = 4.5), "finite integers")
})
