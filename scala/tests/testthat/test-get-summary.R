test_that("get_summary for length_composition objects", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()

  # Include bootstrap uncertainty without retaining individual replicates.
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 10
  )

  summary_result <- get_summary(lc_result)

  expect_type(summary_result, "list")
  expect_s3_class(summary_result, "length_composition_summary")

  expected_names <- c(
    "data_type", "n_lengths", "n_strata", "length_range",
    "strata_names", "total_fish", "fish_by_stratum",
    "fish_by_sex", "n_samples", "has_bootstraps"
  )
  expect_true(all(expected_names %in% names(summary_result)))

  # Test summary values
  expect_equal(summary_result$data_type, "length_composition")
  expect_equal(summary_result$n_lengths, length(lc_result$lengths))
  expect_equal(summary_result$n_strata, length(lc_result$strata_names))
  expect_equal(summary_result$strata_names, lc_result$strata_names)
})

test_that("get_summary for age_composition objects", {
  skip("Current implementation does not support age_composition objects")
})

test_that("get_summary with bootstrap data", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()

  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 10
  )

  summary_result <- get_summary(lc_result)

  # Check bootstrap data
  expect_null(lc_result$bootstraps)
  expect_null(lc_result$full_lc_bootstraps)
  expect_true(summary_result$has_bootstraps)
  expect_equal(summary_result$n_bootstraps, 10)

  # Should have additional bootstrap-related fields
  bootstrap_fields <- c("n_bootstraps", "cv_range", "ci_coverage")
  expect_true(all(bootstrap_fields %in% names(summary_result)))

  lc_result$n_bootstraps <- 0
  expect_error(get_summary(lc_result), "No CV data available", fixed = TRUE)
  expect_false(get_summary(lc_result, by_stratum = TRUE)$has_bootstraps)
})

test_that("get_summary parameter validation", {
  # Invalid object
  expect_error(
    get_summary("invalid"),
    "Input must be an object of class .length_composition."
  )
})

test_that("get_summary detailed statistics", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()

  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 20
  )

  summary_result <- get_summary(lc_result)

  # Test that summary includes reasonable values
  expect_true(summary_result$total_fish > 0)
  expect_equal(
    summary_result$total_fish,
    sum(test_data$fish_data[, c("male", "female", "unsexed")])
  )
  expected_by_stratum <- vapply(lc_result$strata_names, function(stratum_name) {
    fish <- test_data$fish_data[test_data$fish_data$stratum == stratum_name, ]
    sum(fish[, c("male", "female", "unsexed")])
  }, numeric(1))
  expect_equal(summary_result$fish_by_stratum, expected_by_stratum)
  expect_equal(length(summary_result$fish_by_stratum), summary_result$n_strata)
  expect_true(all(names(summary_result$fish_by_sex) %in% c("male", "female", "unsexed", "total")))

  # CV range should be reasonable for bootstrap data
  if (!is.null(summary_result$cv_range)) {
    expect_true(all(summary_result$cv_range >= 0))
    expect_true(summary_result$cv_range[1] <= summary_result$cv_range[2])
  }
})
