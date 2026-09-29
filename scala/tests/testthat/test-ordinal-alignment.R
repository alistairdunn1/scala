test_that("direct-age probabilities retain older and non-consecutive ages", {
  set.seed(20260929)
  d <- data.frame(length = stats::runif(500, 50, 160),
    sex = rep(c("female", "male"), 250), year = rep(2010:2019, 50),
    depth = stats::runif(500, 500, 1500))
  eta <- (d$length - 100) / 35 + 0.2 * (d$sex == "female")
  cuts <- c(-1, 0.5, 2)
  category <- rowSums(outer(eta, cuts, function(e, cut) cut - e) < stats::qlogis(stats::runif(500))) + 1L
  d$age <- c(1, 3, 21, 35)[category]
  fit <- fit_ordinal_alk(d, k_length = 4, k_year = 4,
    additional_terms = "s(depth, k = 4)", verbose = FALSE)
  expect_equal(fit$ages, c(1, 3, 21, 35))
  p <- fit$predict_function(d$length[1:12], d$sex[1:12], year = d$year[1:12], depth = d$depth[1:12])
  reference <- stats::predict(fit$model, newdata = d[1:12, ], type = "response")
  expect_equal(unname(p), unname(reference), tolerance = 1e-10)
  expect_equal(rowSums(p), rep(1, 12), tolerance = 1e-12)
  expect_true(all(p >= 0))
  complete <- fit$predict_age(d$length[1:12], d$year[1:12], d$sex[1:12], depth = d$depth[1:12])
  expect_equal(unname(complete[, fit$ages]), unname(p), tolerance = 1e-12)
  expect_true(all(complete[, setdiff(1:35, fit$ages)] == 0))
  expect_equal(ncol(complete), 35)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(fit, path)
  loaded <- readRDS(path)
  expect_equal(loaded$predict_age(100, 2015, "FEMALE", depth = 800),
    fit$predict_age(100, 2015, "female", depth = 800))
  expect_error(fit$predict_function(100, "female"), "Missing prediction covariates")
  expect_error(fit$predict_age(c(100, 120), 2015.5, "female", depth = 800), "integers")
  expect_error(fit$predict_function(c(100, 120), "female", year = 2015, depth = 1:3), "same length")
  expect_error(fit$predict_function(100, "female", year = 2015, depth = NA_real_), "observed values")
  expect_error(fit$predict_function(0, "female"), "positive")
  expect_error(fit$predict_function(100, "unknown"), "sex values")
  expect_error(fit$predict_age(100, 2015, "female", year = 2015, depth = 800), "not both")
  expect_error(fit$predict_function(100, "female", 800), "unique names")
  expect_match(paste(capture.output(print(fit)), collapse = " "), "year")
})

test_that("shared model arguments construct matched predictors", {
  set.seed(472)
  d <- data.frame(length = stats::runif(360, 60, 150),
    age = sample(1:6, 360, replace = TRUE), year = rep(2011:2016, 60),
    depth = stats::runif(360), sex = rep(c("female", "male"), 180))
  arguments <- list(alk_data = d, by_sex = TRUE, k_length = 4, k_year = 4,
    additional_terms = "s(depth, k = 4)", select = FALSE, gamma = 1,
    method = "ML", weights = rep(c(1, 2), 180), verbose = FALSE)
  direct <- do.call(fit_ordinal_alk, arguments)
  cohort <- do.call(fit_cohort_alk, arguments)
  expect_equal(attr(stats::terms(direct$model), "term.labels"),
    attr(stats::terms(cohort$model), "term.labels"))
  expect_equal(lapply(direct$model$smooth, function(x) c(x$label, x$bs.dim)),
    lapply(cohort$model$smooth, function(x) c(x$label, x$bs.dim)))
  expect_equal(direct$model$prior.weights, cohort$model$prior.weights)
  expect_equal(direct$model$method, cohort$model$method)
  reference_data <- d
  reference_data$sex <- factor(reference_data$sex)
  reference_data$age <- match(reference_data$age, direct$ages)
  reference_data$.weight <- arguments$weights
  reference <- mgcv::gam(stats::formula(direct$model), data = reference_data,
    family = mgcv::ocat(R = length(direct$ages)), method = "ML", select = FALSE,
    gamma = 1, weights = .weight, na.action = stats::na.fail)
  expect_equal(stats::coef(direct$model), stats::coef(reference), tolerance = 1e-9)
  expect_equal(direct$model$sp, reference$sp, tolerance = 1e-9)
  expect_equal(direct$training_years, cohort$training_years)
  expect_equal(direct$year_range, cohort$year_range)
})

test_that("legacy length arguments and binary ages remain valid", {
  set.seed(621)
  d <- data.frame(length = stats::runif(200, 10, 50), age = rep(c(2, 26), 100))
  old <- fit_ordinal_alk(d, FALSE, 4, verbose = FALSE)
  new <- fit_ordinal_alk(d, by_sex = FALSE, k_length = 4, verbose = FALSE)
  expect_equal(old$predict_function(c(15, 30)), new$predict_function(c(15, 30)))
  expect_equal(colnames(new$predict_function(30)), c("age_2", "age_26"))
  expect_equal(sum(new$predict_age(30)), 1, tolerance = 1e-12)
  binary_data <- transform(d, age = as.integer(age == 26))
  binary <- mgcv::gam(age ~ s(length, k = 4), data = binary_data,
    family = stats::binomial(), method = "REML", select = TRUE, gamma = 1.4)
  expect_equal(unname(new$predict_function(c(15, 30))[, 2]),
    as.numeric(stats::predict(binary, newdata = data.frame(length = c(15, 30)), type = "response")),
    tolerance = 1e-5)
  expect_null(new$training_years)
  expect_error(fit_ordinal_alk(d, FALSE, k = 4, k_length = 5), "must agree")
  expect_error(fit_ordinal_alk(d, FALSE, k_year = 4), "year")
  for (age in c(-1, 0, 1.5, NA_real_, Inf)) {
    bad <- d
    bad$age[1] <- age
    expect_error(fit_ordinal_alk(bad, FALSE), "age must contain")
  }
  expect_error(fit_ordinal_alk(transform(d, age = 2), FALSE), "two observed ages")
  expect_error(fit_ordinal_alk(d, FALSE, weights = rep(0, nrow(d))), "weights")
  expect_error(fit_ordinal_alk(d, FALSE, weights = -1), "weights")
  expect_error(fit_ordinal_alk(d, FALSE, method = "GCV.Cp"), "REML or ML")
  expect_error(fit_ordinal_alk(d, FALSE, gamma = NA_real_), "gamma")
  d$depth <- seq_len(nrow(d))
  d$depth[1] <- NA_real_
  expect_error(fit_ordinal_alk(d, FALSE, additional_terms = "s(depth, k = 4)"), "missing")
})
