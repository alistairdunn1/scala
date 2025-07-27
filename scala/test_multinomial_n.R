# Test the multinomial effective sample size function
cat("Testing calculate_multinomial_n function...\\n")

# Load the package functions
source("R/generate_test_data.R")
source("R/calculate_length_compositions.R") 
source("R/calculate_multinomial_n.R")
source("R/resample_fish_data.R")

# Generate test data
test_data <- generate_test_data()

# Calculate length compositions with bootstraps
lc_result <- calculate_length_compositions(
  fish_data = test_data$fish_data,
  strata_data = test_data$strata_data, 
  length_range = c(20, 40),
  lw_params_male = c(a = 0.01, b = 3.0),
  lw_params_female = c(a = 0.01, b = 3.0),
  lw_params_unsexed = c(a = 0.01, b = 3.0),
  bootstraps = 50  # Use fewer bootstraps for faster testing
)

cat("Length composition calculation completed.\\n")

# Test multinomial effective sample size calculation
cat("\\nTesting multinomial effective sample size for pooled total...\\n")
eff_n_total <- calculate_multinomial_n(lc_result, sex = "total")
print(eff_n_total)

cat("\\nTesting for males in stratum North...\\n")
eff_n_male_north <- calculate_multinomial_n(lc_result, stratum = "North", sex = "male")
print(eff_n_male_north)

cat("\\nTesting for females pooled...\\n")
eff_n_female <- calculate_multinomial_n(lc_result, sex = "female")
print(eff_n_female)

cat("\\nAll tests completed successfully!\\n")
