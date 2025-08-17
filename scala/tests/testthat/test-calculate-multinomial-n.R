test_that("calculate_multinomial_n basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  # Get length compositions first
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 50
  )
  
  # Calculate multinomial n
  mult_n_result <- calculate_multinomial_n(lc_result)
  
  expect_type(mult_n_result, "list")
  expect_s3_class(mult_n_result, "multinomial_n")
  
  expected_names <- c("results", "summary", "input_type", "data_type")
  expect_true(all(expected_names %in% names(mult_n_result)))
  
  # Test results structure
  results <- mult_n_result$results
  expect_s3_class(results, "data.frame")
  
  expected_cols <- c("stratum", "sex", "n_eff", "cv_mean", "n_samples", 
                    "n_obs", "r_squared", "outliers_removed")
  expect_true(all(expected_cols %in% names(results)))
  
  # Check effective sample sizes are reasonable
  expect_true(all(results$n_eff > 0, na.rm = TRUE))
  expect_true(all(results$cv_mean >= 0, na.rm = TRUE))
  expect_true(all(results$r_squared >= 0 & results$r_squared <= 1, na.rm = TRUE))
})

test_that("calculate_multinomial_n with different input types", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  # Test with length compositions
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 20
  )
  
  mult_n_lc <- calculate_multinomial_n(lc_result)
  expect_equal(mult_n_lc$input_type, "length_composition")
  
  # Test with age compositions
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
  
  mult_n_age <- calculate_multinomial_n(age_result)
  expect_equal(mult_n_age$input_type, "age_composition")
})

test_that("calculate_multinomial_n parameter options", {
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
    bootstraps = 30
  )
  
  # Test with outlier removal disabled
  mult_n_no_outlier <- calculate_multinomial_n(lc_result, remove_outliers = FALSE)
  expect_true(all(mult_n_no_outlier$results$outliers_removed == 0))
  
  # Test with different minimum CV
  mult_n_min_cv <- calculate_multinomial_n(lc_result, min_cv = 0.1)
  expect_type(mult_n_min_cv, "list")
  
  # Test single stratum/sex combination
  mult_n_single <- calculate_multinomial_n(lc_result, 
                                          target_stratum = unique(lc_result$strata_names)[1],
                                          target_sex = "total")
  expect_equal(nrow(mult_n_single$results), 1)
})

test_that("calculate_multinomial_n validation", {
  # Test with invalid input
  expect_error(
    calculate_multinomial_n("invalid"),
    "Input must be a length_composition or age_composition object"
  )
  
  # Test with object without bootstraps
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  lc_no_bootstrap <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )
  
  expect_error(
    calculate_multinomial_n(lc_no_bootstrap),
    "Bootstrap results are required"
  )
})

test_that("calculate_multinomial_n summary statistics", {
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
    bootstraps = 25
  )
  
  mult_n_result <- calculate_multinomial_n(lc_result)
  
  # Check summary statistics
  summary_stats <- mult_n_result$summary
  expect_type(summary_stats, "list")
  
  expected_summary_names <- c("n_strata", "n_sex_categories", "total_combinations",
                             "successful_fits", "mean_n_eff", "mean_r_squared")
  expect_true(all(expected_summary_names %in% names(summary_stats)))
  
  # Summary values should be reasonable
  expect_true(summary_stats$n_strata > 0)
  expect_true(summary_stats$successful_fits <= summary_stats$total_combinations)
  expect_true(summary_stats$mean_n_eff > 0)
  expect_true(summary_stats$mean_r_squared >= 0 && summary_stats$mean_r_squared <= 1)
})
