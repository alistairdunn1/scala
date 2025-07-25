# Example comparing weight-based vs density-based scaling
# This demonstrates the differences between commercial and survey approaches

# Load the functions
source("../CAA/R/calculate_scaled_length_frequencies.r")
source("../CAA/R/calculate_lf_result.r")
source("../CAA/R/resample_fish_data.r")
source("../CAA/R/print.scaled_length_frequency.r")
source("../CAA/R/generate_test_data.r")

cat("=== Comparison of Weight-based vs Density-based Scaling ===\n\n")

# Generate both types of test data
commercial_data <- generate_test_data()
survey_data <- generate_survey_test_data()

cat("1. COMMERCIAL FISHERIES (Weight-based scaling)\n")
cat("===============================================\n")
cat("Data structure: Uses total catch weights\n")
cat("Sample from commercial_data$fish_data:\n")
print(head(commercial_data$fish_data[, c("stratum", "sample_id", "length", "total", "sample_weight_kg", "total_catch_weight_kg")], 3))
cat("\nStrata data:\n")
print(commercial_data$strata_data)

commercial_results <- calculate_scaled_length_frequencies(
  fish_data = commercial_data$fish_data,
  strata_data = commercial_data$strata_data,
  length_range = c(20, 35),
  bootstraps = 30
)

cat("\n2. RESEARCH SURVEYS (Density-based scaling)\n")
cat("===========================================\n")
cat("Data structure: Uses catch density and area coverage\n")
cat("Sample from survey_data$fish_data:\n")
print(head(survey_data$fish_data[, c("stratum", "sample_id", "length", "total", "sample_area_km2", "catch_density_kg_km2")], 3))
cat("\nStrata data:\n")
print(survey_data$strata_data)

survey_results <- calculate_scaled_length_frequencies(
  fish_data = survey_data$fish_data,
  strata_data = survey_data$strata_data,
  length_range = c(20, 35),
  bootstraps = 30
)

cat("\n3. COMPARISON OF RESULTS\n")
cat("========================\n")
cat("Commercial total fish estimate:", format(sum(commercial_results$pooled_length_frequency[, "total"]), big.mark = ","), "\n")
cat("Survey total fish estimate:", format(sum(survey_results$pooled_length_frequency[, "total"]), big.mark = ","), "\n")

cat("\nScaling approaches used:\n")
cat("Commercial:", commercial_results$scaling_type, "\n")
cat("Survey:", survey_results$scaling_type, "\n")

cat("\nBoth approaches provide population estimates, but:\n")
cat("- Commercial: Scales from sample catches to total commercial catch\n")
cat("- Survey: Scales from sample areas to total stratum areas using density\n")
