test_that("get_summary for length_composition objects", {
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
    bootstraps = 0
  )
  
  summary_result <- get_summary(lc_result)
  
  expect_type(summary_result, "list")
  expect_s3_class(summary_result, "length_composition_summary")
  
  expected_names <- c("data_type", "n_lengths", "n_strata", "length_range", 
                     "strata_names", "total_fish", "fish_by_stratum", 
                     "fish_by_sex", "n_samples", "has_bootstraps")
  expect_true(all(expected_names %in% names(summary_result)))
  
  # Test summary values
  expect_equal(summary_result$data_type, "length_composition")
  expect_equal(summary_result$n_lengths, length(lc_result$lengths))
  expect_equal(summary_result$n_strata, length(lc_result$strata_names))
  expect_equal(summary_result$strata_names, lc_result$strata_names)
  expect_false(summary_result$has_bootstraps)
  
  # Check fish counts are reasonable
  expect_true(summary_result$total_fish > 0)
  expect_true(all(summary_result$fish_by_stratum > 0))
  expect_true(all(summary_result$fish_by_sex >= 0))
})

test_that("get_summary for age_composition objects", {
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
    bootstraps = 0
  )
  
  simple_alk <- data.frame(
    length = rep(15:35, each = 5),
    age = rep(1:5, times = length(15:35)),
    proportion = rep(c(0.1, 0.2, 0.4, 0.2, 0.1), times = length(15:35))
  )
  
  age_result <- calculate_age_compositions(
    x = lc_result,
    age_length_key = simple_alk,
    age_range = c(1, 5)
  )
  
  summary_result <- get_summary(age_result)
  
  expect_type(summary_result, "list")
  expect_s3_class(summary_result, "age_composition_summary")
  
  expected_names <- c("data_type", "n_ages", "n_strata", "age_range", 
                     "strata_names", "total_fish", "fish_by_stratum", 
                     "fish_by_sex", "has_bootstraps", "alk_type")
  expect_true(all(expected_names %in% names(summary_result)))
  
  # Test summary values
  expect_equal(summary_result$data_type, "age_composition")
  expect_equal(summary_result$n_ages, length(age_result$ages))
  expect_equal(summary_result$n_strata, length(age_result$strata_names))
  expect_equal(summary_result$age_range, range(age_result$ages))
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
  
  expect_true(summary_result$has_bootstraps)
  expect_equal(summary_result$n_bootstraps, 10)
  
  # Should have additional bootstrap-related fields
  bootstrap_fields <- c("n_bootstraps", "cv_range", "ci_coverage")
  expect_true(all(bootstrap_fields %in% names(summary_result)))
})

test_that("get_summary parameter validation", {
  # Invalid object
  expect_error(
    get_summary("invalid"),
    "Object must be a length_composition or age_composition"
  )
  
  # Test with object missing required attributes
  bad_object <- list(some_data = 1:10)
  class(bad_object) <- "length_composition"
  
  expect_error(
    get_summary(bad_object)
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
  expect_equal(length(summary_result$fish_by_stratum), summary_result$n_strata)
  expect_true(all(names(summary_result$fish_by_sex) %in% c("male", "female", "unsexed", "total")))
  
  # CV range should be reasonable for bootstrap data
  if (!is.null(summary_result$cv_range)) {
    expect_true(all(summary_result$cv_range >= 0))
    expect_true(summary_result$cv_range[1] <= summary_result$cv_range[2])
  }
})
