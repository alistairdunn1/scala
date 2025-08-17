test_that("compare_alks basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Create empirical ALK
  empirical_alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    verbose = FALSE
  )
  
  # Create model-based ALK
  model_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  # Compare ALKs
  comparison <- compare_alks(
    empirical_alk = empirical_alk,
    model_alk = model_alk,
    lengths = 20:30,
    verbose = FALSE
  )
  
  expect_type(comparison, "list")
  expect_s3_class(comparison, "alk_comparison")
  
  expected_names <- c("comparison_data", "summary_stats", "empirical_type", 
                     "model_type", "comparison_lengths", "sex_categories")
  expect_true(all(expected_names %in% names(comparison)))
  
  # Test comparison data structure
  comp_data <- comparison$comparison_data
  expect_s3_class(comp_data, "data.frame")
  
  expected_cols <- c("length", "sex", "age", "empirical_prop", "model_prop", 
                    "difference", "abs_difference")
  expect_true(all(expected_cols %in% names(comp_data)))
  
  # Check proportions are valid
  expect_true(all(comp_data$empirical_prop >= 0 & comp_data$empirical_prop <= 1))
  expect_true(all(comp_data$model_prop >= 0 & comp_data$model_prop <= 1))
  
  # Check differences are calculated correctly
  expected_diff <- comp_data$model_prop - comp_data$empirical_prop
  expect_true(all(abs(comp_data$difference - expected_diff) < 1e-10))
})

test_that("compare_alks summary statistics", {
  set.seed(123)
  test_data <- generate_test_data()
  
  empirical_alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    verbose = FALSE
  )
  
  model_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  comparison <- compare_alks(
    empirical_alk = empirical_alk,
    model_alk = model_alk,
    lengths = 20:30,
    verbose = FALSE
  )
  
  # Test summary statistics
  summary_stats <- comparison$summary_stats
  expect_type(summary_stats, "list")
  
  expected_summary_names <- c("rmse_overall", "mae_overall", "max_abs_diff", 
                             "rmse_by_sex", "mae_by_sex", "correlation_by_sex")
  expect_true(all(expected_summary_names %in% names(summary_stats)))
  
  # Check summary values are reasonable
  expect_true(summary_stats$rmse_overall >= 0)
  expect_true(summary_stats$mae_overall >= 0)
  expect_true(summary_stats$max_abs_diff >= 0)
  expect_true(summary_stats$rmse_overall >= summary_stats$mae_overall) # RMSE >= MAE
  
  # Check sex-specific statistics
  for (sex in names(summary_stats$rmse_by_sex)) {
    expect_true(summary_stats$rmse_by_sex[[sex]] >= 0)
    expect_true(summary_stats$mae_by_sex[[sex]] >= 0)
    expect_true(abs(summary_stats$correlation_by_sex[[sex]]) <= 1)
  }
})

test_that("compare_alks parameter validation", {
  test_data <- generate_test_data()
  
  empirical_alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    verbose = FALSE
  )
  
  # Invalid empirical ALK
  expect_error(
    compare_alks(
      empirical_alk = "invalid",
      model_alk = fit_ordinal_alk(alk_data = generate_test_age_data()),
      lengths = 20:30
    )
  )
  
  # Invalid model ALK
  expect_error(
    compare_alks(
      empirical_alk = empirical_alk,
      model_alk = "invalid",
      lengths = 20:30
    )
  )
  
  # Empty lengths
  expect_error(
    compare_alks(
      empirical_alk = empirical_alk,
      model_alk = fit_ordinal_alk(alk_data = generate_test_age_data()),
      lengths = numeric(0)
    )
  )
})

test_that("compare_alks with different ALK types", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Create different types of ALKs
  empirical_alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:8,
    verbose = FALSE
  )
  
  # Model ALK without sex
  model_alk_no_sex <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  # This should work (model without sex can be compared to empirical with sex)
  comparison_no_sex <- compare_alks(
    empirical_alk = empirical_alk,
    model_alk = model_alk_no_sex,
    lengths = 20:30,
    verbose = FALSE
  )
  
  expect_s3_class(comparison_no_sex, "alk_comparison")
  expect_false(comparison_no_sex$model_type$by_sex)
  
  # Model ALK with sex
  model_alk_sex <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  comparison_sex <- compare_alks(
    empirical_alk = empirical_alk,
    model_alk = model_alk_sex,
    lengths = 20:30,
    verbose = FALSE
  )
  
  expect_s3_class(comparison_sex, "alk_comparison")
  expect_true(comparison_sex$model_type$by_sex)
})

test_that("compare_alks visualization flag", {
  set.seed(123)
  test_data <- generate_test_data()
  
  empirical_alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    verbose = FALSE
  )
  
  model_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  # Test with visualization (should not error, but won't test actual plot)
  comparison_viz <- compare_alks(
    empirical_alk = empirical_alk,
    model_alk = model_alk,
    lengths = 20:25,
    visualize = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(comparison_viz, "alk_comparison")
  
  # Test without visualization
  comparison_no_viz <- compare_alks(
    empirical_alk = empirical_alk,
    model_alk = model_alk,
    lengths = 20:25,
    visualize = FALSE,
    verbose = FALSE
  )
  
  expect_s3_class(comparison_no_viz, "alk_comparison")
})
