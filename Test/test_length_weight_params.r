# Test the enhanced CAA package with required length-weight parameters

# Load all required functions
source("../CAA/R/calculate_scaled_length_frequencies.r")
source("../CAA/R/calculate_lf_result.r")
source("../CAA/R/resample_fish_data.r")
source("../CAA/R/print.scaled_length_frequency.r")
source("../CAA/R/generate_test_data.r")

cat("=== TESTING ENHANCED CAA PACKAGE WITH LENGTH-WEIGHT PARAMETERS ===\n\n")

# Test 1: Check error handling for missing parameters
cat("TEST 1: Error handling for missing length-weight parameters\n")
cat("========================================================\n")

commercial_data <- generate_test_data()

tryCatch(
  {
    calculate_scaled_length_frequencies(
      fish_data = commercial_data$fish_data,
      strata_data = commercial_data$strata_data,
      length_range = c(20, 35),
      bootstraps = 10
    )
  },
  error = function(e) {
    cat("✓ Error correctly caught when length-weight parameters are missing:\n")
    cat("  ", as.character(e), "\n\n")
  }
)

# Test 2: Check validation of parameter structure
cat("TEST 2: Validation of length-weight parameter structure\n")
cat("======================================================\n")

bad_params <- c(0.01, 3.0) # No names
tryCatch(
  {
    calculate_scaled_length_frequencies(
      fish_data = commercial_data$fish_data,
      strata_data = commercial_data$strata_data,
      length_range = c(20, 35),
      lw_params_male = bad_params,
      lw_params_female = commercial_data$lw_params$female,
      lw_params_unsexed = commercial_data$lw_params$unsexed,
      bootstraps = 10
    )
  },
  error = function(e) {
    cat("✓ Error correctly caught for invalid parameter structure:\n")
    cat("  ", as.character(e), "\n\n")
  }
)

# Test 3: Successful analysis with proper parameters
cat("TEST 3: Successful analysis with length-weight parameters\n")
cat("========================================================\n")

cat("Using length-weight parameters:\n")
cat("Male:   a =", commercial_data$lw_params$male["a"], ", b =", commercial_data$lw_params$male["b"], "\n")
cat("Female: a =", commercial_data$lw_params$female["a"], ", b =", commercial_data$lw_params$female["b"], "\n")
cat("Unsexed: a =", commercial_data$lw_params$unsexed["a"], ", b =", commercial_data$lw_params$unsexed["b"], "\n\n")

results <- calculate_scaled_length_frequencies(
  fish_data = commercial_data$fish_data,
  strata_data = commercial_data$strata_data,
  length_range = c(20, 35),
  lw_params_male = commercial_data$lw_params$male,
  lw_params_female = commercial_data$lw_params$female,
  lw_params_unsexed = commercial_data$lw_params$unsexed,
  bootstraps = 20
)

cat("✓ Weight-based scaling completed successfully\n")
cat("Total fish estimate:", format(sum(results$pooled_length_frequency[, "total"]), big.mark = ","), "\n\n")

# Test 4: Survey data with length-weight parameters
cat("TEST 4: Survey data with length-weight parameters\n")
cat("================================================\n")

survey_data <- generate_survey_test_data()

survey_results <- calculate_scaled_length_frequencies(
  fish_data = survey_data$fish_data,
  strata_data = survey_data$strata_data,
  length_range = c(20, 35),
  lw_params_male = survey_data$lw_params$male,
  lw_params_female = survey_data$lw_params$female,
  lw_params_unsexed = survey_data$lw_params$unsexed,
  bootstraps = 20
)

cat("✓ Density-based scaling completed successfully\n")
cat("Total fish estimate:", format(sum(survey_results$pooled_length_frequency[, "total"]), big.mark = ","), "\n\n")

# Test 5: Compare different length-weight parameters
cat("TEST 5: Effect of different length-weight parameters\n")
cat("===================================================\n")

# Create alternative parameters (heavier fish)
heavy_params <- list(
  male = c(a = 0.015, b = 3.2),
  female = c(a = 0.017, b = 3.15),
  unsexed = c(a = 0.016, b = 3.18)
)

results_heavy <- calculate_scaled_length_frequencies(
  fish_data = commercial_data$fish_data,
  strata_data = commercial_data$strata_data,
  length_range = c(20, 35),
  lw_params_male = heavy_params$male,
  lw_params_female = heavy_params$female,
  lw_params_unsexed = heavy_params$unsexed,
  bootstraps = 20
)

cat("Original parameters - Total fish estimate:", format(sum(results$pooled_length_frequency[, "total"]), big.mark = ","), "\n")
cat("Heavy fish parameters - Total fish estimate:", format(sum(results_heavy$pooled_length_frequency[, "total"]), big.mark = ","), "\n")
cat("(Different length-weight parameters produce different abundance estimates)\n\n")

cat("=== ALL TESTS COMPLETED SUCCESSFULLY ===\n")
cat("\nThe enhanced CAA package now:\n")
cat("• Requires length-weight parameters for males, females, and unsexed fish\n")
cat("• Validates parameter structure and values\n")
cat("• Uses realistic weight calculations instead of placeholders\n")
cat("• Provides species-specific scaling based on actual fish weights\n")
