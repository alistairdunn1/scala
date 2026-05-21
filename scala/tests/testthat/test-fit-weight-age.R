generate_weight_age_data <- function(by_sex = TRUE, seed = 42) {
  set.seed(seed)
  # Balanced design: 50 per class, means spaced 0.13 apart, SD=0.04 so adjacent
  # classes overlap ~10% — realistic otolith data, numerically stable for ocat
  ages <- rep(1:6, each = 50)
  weights <- c(
    rnorm(50, 0.10, 0.04), rnorm(50, 0.23, 0.04),
    rnorm(50, 0.36, 0.05), rnorm(50, 0.49, 0.05),
    rnorm(50, 0.62, 0.06), rnorm(50, 0.75, 0.06)
  )
  weights <- pmax(0.01, weights)
  result <- data.frame(age = ages, weight = weights, stringsAsFactors = FALSE)
  if (by_sex) {
    result$sex <- rep(c("male", "female"), length.out = length(ages))
  }
  result
}

test_that("fit_weight_age basic functionality", {
  dat <- generate_weight_age_data(by_sex = FALSE)
  model <- fit_weight_age(dat, by_sex = FALSE, verbose = FALSE)

  expect_type(model, "list")
  expect_s3_class(model, "weight_age_model")

  expected_names <- c(
    "model", "predict_function", "model_summary", "deviance_explained",
    "by_sex", "ages", "sex_levels", "weight_range", "additional_terms"
  )
  expect_true(all(expected_names %in% names(model)))

  expect_false(model$by_sex)
  expect_null(model$sex_levels)
  expect_equal(length(model$weight_range), 2)
  expect_true(model$weight_range[1] < model$weight_range[2])
  expect_type(model$predict_function, "closure")
})

test_that("fit_weight_age with sex", {
  dat <- generate_weight_age_data(by_sex = TRUE)
  model <- fit_weight_age(dat, by_sex = TRUE, verbose = FALSE)

  expect_true(model$by_sex)
  expect_true(!is.null(model$sex_levels))
  expect_true(all(c("male", "female") %in% model$sex_levels))
})

test_that("fit_weight_age predict_function returns valid probabilities (no sex)", {
  dat <- generate_weight_age_data(by_sex = FALSE)
  model <- fit_weight_age(dat, by_sex = FALSE, verbose = FALSE)

  test_weights <- seq(0.05, 0.55, by = 0.05)
  probs <- model$predict_function(test_weights)

  expect_true(is.matrix(probs))
  expect_equal(nrow(probs), length(test_weights))
  expect_equal(ncol(probs), length(model$ages))

  row_sums <- rowSums(probs)
  expect_true(all(abs(row_sums - 1) < 1e-8))
  expect_true(all(probs >= 0))
})

test_that("fit_weight_age predict_function returns valid probabilities (with sex)", {
  dat <- generate_weight_age_data(by_sex = TRUE)
  model <- fit_weight_age(dat, by_sex = TRUE, verbose = FALSE)

  test_weights <- seq(0.05, 0.55, by = 0.10)
  test_sex <- rep("male", length(test_weights))

  probs <- model$predict_function(test_weights, test_sex)

  expect_true(is.matrix(probs))
  expect_equal(nrow(probs), length(test_weights))
  expect_equal(ncol(probs), length(model$ages))

  row_sums <- rowSums(probs)
  expect_true(all(abs(row_sums - 1) < 1e-8))
  expect_true(all(probs >= 0))
})

test_that("fit_weight_age deviance explained is reasonable", {
  dat <- generate_weight_age_data(by_sex = FALSE)
  model <- fit_weight_age(dat, by_sex = FALSE, verbose = FALSE)

  expect_true(model$deviance_explained >= 0)
  expect_true(model$deviance_explained <= 100)
  expect_true(model$deviance_explained > 10)
})

test_that("fit_weight_age input validation", {
  dat <- generate_weight_age_data()

  expect_error(fit_weight_age("not a data frame"), "must be a data frame")
  expect_error(fit_weight_age(data.frame(age = 1:3, x = 1:3)), "'age' and 'weight'")
  expect_error(
    fit_weight_age(data.frame(age = 1:3, weight = 1:3), by_sex = TRUE),
    "'sex' column"
  )
  expect_error(
    fit_weight_age(dat[, c("age", "weight")], by_sex = FALSE, additional_terms = 42),
    "character vector"
  )
})

test_that("fit_weight_age single-sex prediction (scalar sex recycled)", {
  dat <- generate_weight_age_data(by_sex = TRUE)
  model <- fit_weight_age(dat, by_sex = TRUE, verbose = FALSE)

  test_weights <- seq(0.10, 0.50, by = 0.10)
  probs_scalar <- model$predict_function(test_weights, "female")
  probs_vector <- model$predict_function(test_weights, rep("female", length(test_weights)))

  expect_equal(probs_scalar, probs_vector)
})

test_that("fit_weight_age print method works", {
  dat <- generate_weight_age_data(by_sex = FALSE)
  model <- fit_weight_age(dat, by_sex = FALSE, verbose = FALSE)
  expect_output(print(model), "Age-at-Weight")
})
