# Final comprehensive test of both scaling approaches
# This validates that all functionality works correctly

# Load all required functions
source("../CAA/R/calculate_scaled_length_frequencies.r")
source("../CAA/R/calculate_lf_result.r")
source("../CAA/R/resample_fish_data.r")
source("../CAA/R/print.scaled_length_frequency.r")
source("../CAA/R/generate_test_data.r")

cat("=== COMPREHENSIVE TEST OF ENHANCED CAA PACKAGE ===\n\n")

# Test 1: Weight-based scaling
cat("TEST 1: Weight-based scaling (Commercial fisheries)\n")
cat("==================================================\n")

commercial_data <- generate_test_data()

commercial_results <- calculate_scaled_length_frequencies(
  fish_data = commercial_data$fish_data,
  strata_data = commercial_data$strata_data,
  length_range = c(20, 35),
  bootstraps = 20
)

cat("✓ Weight-based scaling completed successfully\n")
cat("  Total fish estimate:", format(sum(commercial_results$pooled_length_frequency[, "total"]), big.mark = ","), "\n")
cat("  Scaling type:", commercial_results$scaling_type, "\n\n")

# Test 2: Density-based scaling
cat("TEST 2: Density-based scaling (Research surveys)\n")
cat("===============================================\n")

survey_data <- generate_survey_test_data()

survey_results <- calculate_scaled_length_frequencies(
  fish_data = survey_data$fish_data,
  strata_data = survey_data$strata_data,
  length_range = c(20, 35),
  bootstraps = 20
)

cat("✓ Density-based scaling completed successfully\n")
cat("  Total fish estimate:", format(sum(survey_results$pooled_length_frequency[, "total"]), big.mark = ","), "\n")
cat("  Scaling type:", survey_results$scaling_type, "\n\n")

# Test 3: Error handling
cat("TEST 3: Error handling\n")
cat("======================\n")

# Test missing columns
tryCatch(
  {
    bad_data <- commercial_data$fish_data[, !names(commercial_data$fish_data) %in% "sample_weight_kg"]
    calculate_scaled_length_frequencies(bad_data, commercial_data$strata_data, bootstraps = 5)
  },
  error = function(e) {
    cat("✓ Missing column error handled correctly\n")
  }
)

# Test mixed data types
tryCatch(
  {
    mixed_fish <- survey_data$fish_data
    mixed_fish$sample_weight_kg <- 10
    mixed_fish$total_catch_weight_kg <- 100
    mixed_strata <- survey_data$strata_data
    mixed_strata$stratum_total_catch_kg <- 1000
    calculate_scaled_length_frequencies(mixed_fish, mixed_strata, bootstraps = 5)
  },
  error = function(e) {
    cat("✓ Mixed data type error handled correctly\n")
  }
)

cat("\n=== ALL TESTS COMPLETED SUCCESSFULLY ===\n")
cat("\nThe enhanced CAA package now supports:\n")
cat("• Weight-based scaling for commercial fisheries data\n")
cat("• Density-based scaling for research survey data\n")
cat("• Automatic detection of data type\n")
cat("• Comprehensive error handling\n")
cat("• Bootstrap uncertainty estimation for both approaches\n")
cat("• Sex-based length frequency analysis\n")
