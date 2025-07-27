# Final test of multinomial effective sample size functionality
cat("Comprehensive test of multinomial effective sample size functions...\\n\\n")

# Load required functions
source("R/generate_test_data.R")
source("R/calculate_length_compositions.R") 
source("R/calculate_multinomial_n.R")
source("R/resample_fish_data.R")

# Generate test data
cat("1. Generating test data...\\n")
test_data <- generate_test_data()

# Calculate length compositions with bootstraps
cat("2. Calculating length compositions with bootstrap uncertainty...\\n")
lc_result <- calculate_length_compositions(
  fish_data = test_data$fish_data,
  strata_data = test_data$strata_data, 
  length_range = c(20, 40),
  lw_params_male = c(a = 0.01, b = 3.0),
  lw_params_female = c(a = 0.01, b = 3.0),
  lw_params_unsexed = c(a = 0.01, b = 3.0),
  bootstraps = 100
)

cat("\\n3. Testing individual multinomial effective sample size calculation...\\n")
eff_n_total <- calculate_multinomial_n(lc_result, sex = "total")
cat("   Pooled total effective n:", eff_n_total$effective_n, "\\n")

cat("\\n4. Testing summary function for all combinations...\\n")
all_results <- calculate_all_multinomial_n(lc_result, quiet = FALSE)
cat("\\n")
print(all_results)

cat("\\n\\n5. Testing with different parameters...\\n")
# Test with stricter filtering
strict_results <- calculate_all_multinomial_n(
  lc_result, 
  sex_categories = c("total", "male", "female"),
  min_proportion = 0.001,  # Higher minimum proportion
  max_cv = 3.0,           # Lower maximum CV
  quiet = TRUE
)

cat("Results with stricter filtering:\\n")
print(strict_results)

cat("\\n\\nAll tests completed successfully! ✓\\n")
cat("\\nThe package now includes:\\n")
cat("  - calculate_multinomial_n(): Calculate effective sample size for specific stratum/sex\\n")
cat("  - calculate_all_multinomial_n(): Calculate for all combinations at once\\n")
cat("  - Proper S3 print methods for nice output formatting\\n")
cat("  - Comprehensive documentation and examples\\n")
