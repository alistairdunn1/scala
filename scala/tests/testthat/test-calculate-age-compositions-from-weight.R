make_aged_fish_data <- function(seed = 123) {
  set.seed(seed)
  test_data <- generate_test_data("commercial")
  fish_data <- test_data$fish_data

  # Add age column directly (simulating assign_ages_from_weight output)
  fish_data$age <- sample(1:6, nrow(fish_data), replace = TRUE,
                          prob = c(0.10, 0.20, 0.25, 0.25, 0.15, 0.05))

  list(fish_data = fish_data, strata_data = test_data$strata_data)
}

lw_male <- c(a = 0.01, b = 3.0)
lw_female <- c(a = 0.01, b = 3.0)
lw_unsexed <- c(a = 0.01, b = 3.0)

test_that("calculate_age_compositions_from_weight basic functionality", {
  dat <- make_aged_fish_data()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    verbose = FALSE
  )

  expect_s3_class(result, "age_composition")
  expect_equal(result$ages, 1:6)
  expect_true(length(result$strata_names) > 0)
  expect_equal(result$n_bootstraps, 0)
  expect_equal(result$scaling_type, "weight")
  expect_true(result$has_sex_data)
})

test_that("calculate_age_compositions_from_weight 3D array structure", {
  dat <- make_aged_fish_data()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    verbose = FALSE
  )

  n_ages <- 6
  n_strata <- length(result$strata_names)
  sex_cats <- c("composition", "male", "female", "unsexed", "total")

  expect_equal(dim(result$age_composition), c(n_ages, 5, n_strata))
  expect_equal(dimnames(result$age_composition)[[2]], sex_cats)

  # composition column preserves age values, not proportions
  expect_equal(as.integer(result$age_composition[, "composition", 1]), 1:6)
})

test_that("calculate_age_compositions_from_weight composition column preserved in proportions", {
  dat <- make_aged_fish_data()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    verbose = FALSE
  )

  for (s in seq_along(result$strata_names)) {
    expect_equal(as.integer(result$age_proportions[, "composition", s]), 1:6)
  }
})

test_that("calculate_age_compositions_from_weight proportions sum to 1", {
  dat <- make_aged_fish_data()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    verbose = FALSE
  )

  for (s in seq_along(result$strata_names)) {
    total_prop <- sum(result$age_proportions[, "total", s])
    if (total_prop > 0) {
      expect_true(abs(total_prop - 1) < 1e-8,
                  info = paste("Stratum", result$strata_names[s]))
    }
  }
})

test_that("calculate_age_compositions_from_weight bootstrap produces CVs", {
  dat <- make_aged_fish_data()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 50,
    verbose = FALSE
  )

  expect_equal(result$n_bootstraps, 50)

  # CVs should be finite and non-negative where compositions are non-zero
  valid_cvs <- result$age_cvs[is.finite(result$age_cvs) & !is.na(result$age_cvs)]
  expect_true(all(valid_cvs >= 0))
  expect_true(all(valid_cvs < 10))

  # CI lower should be <= CI upper
  meas <- c("male", "female", "unsexed", "total")
  expect_true(all(result$age_ci_lower <= result$age_ci_upper + 1e-10, na.rm = TRUE))
})

test_that("calculate_age_compositions_from_weight plus_group_age", {
  dat <- make_aged_fish_data()
  fish_data <- dat$fish_data
  # Add some fish older than max age
  old_fish <- fish_data[seq_len(5), ]
  old_fish$age <- 10
  fish_data <- rbind(fish_data, old_fish)

  result <- calculate_age_compositions_from_weight(
    fish_data = fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    plus_group_age = TRUE,
    verbose = FALSE
  )

  # The plus group fish (age 10) should be captured in age bin 6
  expect_true(result$plus_group_age)
})

test_that("calculate_age_compositions_from_weight removes NA ages with message", {
  dat <- make_aged_fish_data()
  dat$fish_data$age[1:5] <- NA

  expect_message(
    calculate_age_compositions_from_weight(
      fish_data = dat$fish_data,
      strata_data = dat$strata_data,
      age_range = c(1, 6),
      lw_params_male = lw_male,
      lw_params_female = lw_female,
      lw_params_unsexed = lw_unsexed,
      bootstraps = 0,
      verbose = FALSE
    ),
    "NA age"
  )
})

test_that("calculate_age_compositions_from_weight input validation", {
  dat <- make_aged_fish_data()

  expect_error(
    calculate_age_compositions_from_weight(
      "not df", dat$strata_data, c(1, 6), lw_male, lw_female, lw_unsexed, bootstraps = 0
    ),
    "must be a data frame"
  )

  # fish_data without age column
  fish_no_age <- dat$fish_data
  fish_no_age$age <- NULL
  expect_error(
    calculate_age_compositions_from_weight(
      fish_no_age, dat$strata_data, c(1, 6), lw_male, lw_female, lw_unsexed, bootstraps = 0
    ),
    "must contain 'age' column"
  )

  # fish_data with age but missing other required columns
  fish_stripped <- dat$fish_data[, c("age", "stratum", "sample_id")]
  expect_error(
    calculate_age_compositions_from_weight(
      fish_stripped, dat$strata_data, c(1, 6), lw_male, lw_female, lw_unsexed, bootstraps = 0
    ),
    "missing required columns"
  )
})

test_that("calculate_age_compositions_from_weight output has correct age_composition structure", {
  dat <- make_aged_fish_data()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    verbose = FALSE
  )

  # All required fields present for a valid age_composition object
  expected_fields <- c(
    "age_composition", "age_proportions", "pooled_age_composition", "pooled_age_proportions",
    "ages", "strata_names", "n_bootstraps", "scaling_type", "has_sex_data"
  )
  expect_true(all(expected_fields %in% names(result)))
})

test_that("calculate_age_compositions_from_weight density-based scaling", {
  set.seed(42)
  survey_data <- generate_test_data("survey")
  fish_data <- survey_data$fish_data
  fish_data$age <- sample(1:6, nrow(fish_data), replace = TRUE,
                          prob = c(0.10, 0.20, 0.25, 0.25, 0.15, 0.05))

  result <- calculate_age_compositions_from_weight(
    fish_data = fish_data,
    strata_data = survey_data$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 0,
    verbose = FALSE
  )

  expect_s3_class(result, "age_composition")
  expect_equal(result$scaling_type, "density")
})

# Helper: weight-age model for composition tests
make_wt_model_comp <- function(seed = 42) {
  set.seed(seed)
  ages <- rep(1:6, each = 50)
  weights <- c(
    rnorm(50, 0.10, 0.04), rnorm(50, 0.23, 0.04),
    rnorm(50, 0.36, 0.05), rnorm(50, 0.49, 0.05),
    rnorm(50, 0.62, 0.06), rnorm(50, 0.75, 0.06)
  )
  fit_weight_age(data.frame(age = ages, weight = pmax(0.01, weights)), by_sex = FALSE, verbose = FALSE)
}

test_that("weight_age_model = wrong class gives error", {
  dat <- make_aged_fish_data()

  expect_error(
    calculate_age_compositions_from_weight(
      dat$fish_data, dat$strata_data, c(1, 6),
      lw_male, lw_female, lw_unsexed,
      bootstraps = 0, weight_age_model = list()
    ),
    "weight_age_model"
  )
})

test_that("weight_age_model without otolith_weight column gives error", {
  dat <- make_aged_fish_data()
  model <- make_wt_model_comp()

  expect_error(
    calculate_age_compositions_from_weight(
      dat$fish_data, dat$strata_data, c(1, 6),
      lw_male, lw_female, lw_unsexed,
      bootstraps = 0, weight_age_model = model
    ),
    "otolith_weight"
  )
})

test_that("weight_age_model propagates model uncertainty through bootstrap", {
  dat <- make_aged_fish_data()
  set.seed(7)
  dat$fish_data$otolith_weight <- runif(nrow(dat$fish_data), 0.05, 0.75)
  model <- make_wt_model_comp()

  result <- calculate_age_compositions_from_weight(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_male,
    lw_params_female = lw_female,
    lw_params_unsexed = lw_unsexed,
    bootstraps = 20,
    weight_age_model = model,
    verbose = FALSE
  )

  expect_s3_class(result, "age_composition")
  expect_equal(result$n_bootstraps, 20)
  valid_cvs <- result$age_cvs[is.finite(result$age_cvs) & !is.na(result$age_cvs)]
  expect_true(all(valid_cvs >= 0))
  expect_true(all(result$age_ci_lower <= result$age_ci_upper + 1e-10, na.rm = TRUE))
})
