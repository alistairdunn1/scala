make_cohort_model_comp <- function(seed = 42) {
  set.seed(seed)
  ages <- rep(1:6, each = 20)
  years <- sample(2015:2020, 120, replace = TRUE)
  lengths <- pmax(5, round(ages * 5 + rnorm(120, 0, 2) + 10))
  fit_cohort_alk(data.frame(age = ages, length = lengths, year = years),
                 by_sex = FALSE, verbose = FALSE)
}

make_aged_cohort_fish_data <- function(seed = 123) {
  set.seed(seed)
  test_data <- generate_test_data("commercial")
  fish_data <- test_data$fish_data
  fish_data$year <- sample(2015:2020, nrow(fish_data), replace = TRUE)
  fish_data$age <- sample(1:6, nrow(fish_data), replace = TRUE,
                          prob = c(0.10, 0.20, 0.25, 0.25, 0.15, 0.05))
  list(fish_data = fish_data, strata_data = test_data$strata_data)
}

lw_m <- c(a = 0.01, b = 3.0)
lw_f <- c(a = 0.01, b = 3.0)
lw_u <- c(a = 0.01, b = 3.0)

test_that("calculate_age_compositions_from_cohort basic functionality", {
  dat <- make_aged_cohort_fish_data()

  result <- calculate_age_compositions_from_cohort(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_m,
    lw_params_female = lw_f,
    lw_params_unsexed = lw_u,
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

test_that("calculate_age_compositions_from_cohort no-bootstrap returns proportions", {
  dat <- make_aged_cohort_fish_data()

  result <- calculate_age_compositions_from_cohort(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_m,
    lw_params_female = lw_f,
    lw_params_unsexed = lw_u,
    bootstraps = 0,
    verbose = FALSE
  )

  expected_fields <- c(
    "age_composition", "age_proportions", "pooled_age_composition", "pooled_age_proportions",
    "ages", "strata_names", "n_bootstraps", "scaling_type", "has_sex_data"
  )
  expect_true(all(expected_fields %in% names(result)))

  # composition column preserved as age values
  expect_equal(as.integer(result$age_composition[, "composition", 1]), 1:6)
  expect_equal(as.integer(result$age_proportions[, "composition", 1]), 1:6)
})

test_that("calculate_age_compositions_from_cohort proportions sum to 1", {
  dat <- make_aged_cohort_fish_data()

  result <- calculate_age_compositions_from_cohort(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_m,
    lw_params_female = lw_f,
    lw_params_unsexed = lw_u,
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

test_that("calculate_age_compositions_from_cohort removes NA ages with message", {
  dat <- make_aged_cohort_fish_data()
  dat$fish_data$age[1:5] <- NA

  expect_message(
    calculate_age_compositions_from_cohort(
      fish_data = dat$fish_data,
      strata_data = dat$strata_data,
      age_range = c(1, 6),
      lw_params_male = lw_m,
      lw_params_female = lw_f,
      lw_params_unsexed = lw_u,
      bootstraps = 0,
      verbose = FALSE
    ),
    "NA age"
  )
})

test_that("calculate_age_compositions_from_cohort bootstrap produces CVs", {
  dat <- make_aged_cohort_fish_data()

  result <- calculate_age_compositions_from_cohort(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_m,
    lw_params_female = lw_f,
    lw_params_unsexed = lw_u,
    bootstraps = 30,
    verbose = FALSE
  )

  expect_equal(result$n_bootstraps, 30)
  valid_cvs <- result$age_cvs[is.finite(result$age_cvs) & !is.na(result$age_cvs)]
  expect_true(all(valid_cvs >= 0))
  expect_true(all(result$age_ci_lower <= result$age_ci_upper + 1e-10, na.rm = TRUE))
})

test_that("calculate_age_compositions_from_cohort input validation", {
  dat <- make_aged_cohort_fish_data()

  expect_error(
    calculate_age_compositions_from_cohort(
      "not df", dat$strata_data, c(1, 6), lw_m, lw_f, lw_u, bootstraps = 0
    ),
    "must be a data frame"
  )

  fish_no_age <- dat$fish_data
  fish_no_age$age <- NULL
  expect_error(
    calculate_age_compositions_from_cohort(
      fish_no_age, dat$strata_data, c(1, 6), lw_m, lw_f, lw_u, bootstraps = 0
    ),
    "'age' column"
  )

  fish_stripped <- dat$fish_data[, c("age", "stratum", "sample_id", "year")]
  expect_error(
    calculate_age_compositions_from_cohort(
      fish_stripped, dat$strata_data, c(1, 6), lw_m, lw_f, lw_u, bootstraps = 0
    ),
    "missing required columns"
  )
})

test_that("cohort_model = wrong class gives error", {
  dat <- make_aged_cohort_fish_data()

  expect_error(
    calculate_age_compositions_from_cohort(
      dat$fish_data, dat$strata_data, c(1, 6),
      lw_m, lw_f, lw_u,
      bootstraps = 0, cohort_model = list()
    ),
    "cohort_model"
  )
})

test_that("cohort_model propagates model uncertainty through bootstrap", {
  dat <- make_aged_cohort_fish_data()
  model <- make_cohort_model_comp()

  result <- calculate_age_compositions_from_cohort(
    fish_data = dat$fish_data,
    strata_data = dat$strata_data,
    age_range = c(1, 6),
    lw_params_male = lw_m,
    lw_params_female = lw_f,
    lw_params_unsexed = lw_u,
    bootstraps = 20,
    cohort_model = model,
    verbose = FALSE
  )

  expect_s3_class(result, "age_composition")
  expect_equal(result$n_bootstraps, 20)
  valid_cvs <- result$age_cvs[is.finite(result$age_cvs) & !is.na(result$age_cvs)]
  expect_true(all(valid_cvs >= 0))
  expect_true(all(result$age_ci_lower <= result$age_ci_upper + 1e-10, na.rm = TRUE))
})
