# Test density-based scaling functionality
# This script tests the new survey-based (density) scaling approach

# Load the functions
source("../CAA/R/calculate_scaled_length_frequencies.r")
source("../CAA/R/calculate_lf_result.r")
source("../CAA/R/resample_fish_data.r")
source("../CAA/R/print.scaled_length_frequency.r")
source("../CAA/R/generate_test_data.r")

cat("Testing density-based scaling functionality...\n\n")

# Generate survey test data
survey_data <- generate_survey_test_data()

cat("Survey fish data structure:\n")
print(head(survey_data$fish_data))

cat("\nSurvey strata data structure:\n")
print(survey_data$strata_data)

# Test the density-based scaling
cat("\nTesting density-based length frequency calculation...\n")

tryCatch(
  {
    survey_results <- calculate_scaled_length_frequencies(
      fish_data = survey_data$fish_data,
      strata_data = survey_data$strata_data,
      length_range = c(20, 35),
      bootstraps = 50 # Reduced for testing
    )

    cat("\nDensity-based scaling successful!\n")
    cat("Results summary:\n")
    print(survey_results)
  },
  error = function(e) {
    cat("Error in density-based scaling:", e$message, "\n")
  }
)

# Test error handling - mixed data types
cat("\n\nTesting error handling for mixed data types...\n")

mixed_fish_data <- survey_data$fish_data
mixed_fish_data$sample_weight_kg <- 10 # Add weight-based column
mixed_fish_data$total_catch_weight_kg <- 100

mixed_strata_data <- survey_data$strata_data
mixed_strata_data$stratum_total_catch_kg <- 1000 # Add weight-based column

tryCatch(
  {
    mixed_results <- calculate_scaled_length_frequencies(
      fish_data = mixed_fish_data,
      strata_data = mixed_strata_data,
      length_range = c(20, 35),
      bootstraps = 10
    )
  },
  error = function(e) {
    cat("Expected error for mixed data types:", e$message, "\n")
  }
)

cat("\nTesting completed.\n")
