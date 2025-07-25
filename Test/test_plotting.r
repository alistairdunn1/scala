# Test script for the new plotting functionality
library(CAA)

# Check if ggplot2 is available
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  cat("Installing ggplot2...\n")
  install.packages("ggplot2")
}

library(ggplot2)

# Create test data manually since generate_test_data is internal
cat("Creating test data...\n")

# Commercial fisheries test data (weight-based)
set.seed(123)
fish_data_commercial <- data.frame(
  stratum = rep(c("North", "South"), each = 8),
  sample_id = paste0("Sample_", 1:16),
  length = c(
    20, 22, 24, 26, 28, 30, 32, 34, # North stratum
    21, 23, 25, 27, 29, 31, 33, 35
  ), # South stratum
  male = c(
    8, 12, 15, 18, 20, 16, 12, 8, # North stratum
    6, 10, 13, 16, 18, 14, 10, 6
  ), # South stratum
  female = c(
    12, 18, 22, 25, 28, 24, 18, 12, # North stratum
    9, 15, 19, 22, 25, 21, 15, 9
  ), # South stratum
  unsexed = c(
    3, 4, 5, 6, 7, 5, 4, 3, # North stratum
    2, 3, 4, 5, 6, 4, 3, 2
  ), # South stratum
  sample_weight_kg = rep(c(12, 15), each = 8),
  total_catch_weight_kg = rep(c(120, 150), each = 8)
)
fish_data_commercial$total <- fish_data_commercial$male + fish_data_commercial$female + fish_data_commercial$unsexed

strata_data_commercial <- data.frame(
  stratum = c("North", "South"),
  stratum_total_catch_kg = c(3000, 2500)
)

# Survey test data (density-based)
fish_data_survey <- data.frame(
  stratum = rep(c("Shallow", "Deep"), each = 6),
  sample_id = paste0("Survey_", 1:12),
  length = c(
    22, 24, 26, 28, 30, 32, # Shallow stratum
    25, 27, 29, 31, 33, 35
  ), # Deep stratum
  male = c(
    6, 9, 12, 15, 12, 9, # Shallow stratum
    4, 7, 10, 13, 10, 7
  ), # Deep stratum
  female = c(
    9, 13, 18, 22, 18, 13, # Shallow stratum
    6, 10, 15, 19, 15, 10
  ), # Deep stratum
  unsexed = c(
    2, 3, 4, 5, 4, 3, # Shallow stratum
    1, 2, 3, 4, 3, 2
  ), # Deep stratum
  sample_area_km2 = rep(c(0.5, 0.6), each = 6),
  catch_density_kg_km2 = rep(c(300, 180), each = 6)
)
fish_data_survey$total <- fish_data_survey$male + fish_data_survey$female + fish_data_survey$unsexed

strata_data_survey <- data.frame(
  stratum = c("Shallow", "Deep"),
  stratum_area_km2 = c(1200, 1800)
)

# Length-weight parameters
lw_params <- list(
  male = c(a = 0.0085, b = 3.10),
  female = c(a = 0.0092, b = 3.05),
  unsexed = c(a = 0.0088, b = 3.08)
)

cat("Testing commercial data (weight-based scaling)...\n")
# Calculate scaled length frequencies for commercial data
results_commercial <- calculate_scaled_length_frequencies(
  fish_data = fish_data_commercial,
  strata_data = strata_data_commercial,
  length_range = c(20, 35),
  lw_params_male = lw_params$male,
  lw_params_female = lw_params$female,
  lw_params_unsexed = lw_params$unsexed,
  bootstraps = 50 # Reduced for faster testing
)

cat("Testing survey data (density-based scaling)...\n")
# Calculate scaled length frequencies for survey data
results_survey <- calculate_scaled_length_frequencies(
  fish_data = fish_data_survey,
  strata_data = strata_data_survey,
  length_range = c(22, 35),
  lw_params_male = lw_params$male,
  lw_params_female = lw_params$female,
  lw_params_unsexed = lw_params$unsexed,
  bootstraps = 50 # Reduced for faster testing
)

# Test plotting functions
cat("Creating plots...\n")

# 1. Pooled plot for commercial data
cat("1. Pooled frequency plot (commercial)\n")
p1 <- plot_length_frequency(results_commercial, plot_type = "pooled", y_axis = "frequency")
print(p1)

# 2. By stratum plot for commercial data
cat("2. By-stratum frequency plot (commercial)\n")
p2 <- plot_length_frequency(results_commercial, plot_type = "by_stratum", y_axis = "frequency")
print(p2)

# 3. Pooled proportions plot for commercial data
cat("3. Pooled proportions plot (commercial)\n")
p3 <- plot_length_frequency(results_commercial, plot_type = "pooled", y_axis = "proportion")
print(p3)

# 4. Survey data pooled plot
cat("4. Pooled frequency plot (survey)\n")
p4 <- plot_length_frequency(results_survey, plot_type = "pooled", y_axis = "frequency")
print(p4)

# 5. Custom colors example
cat("5. Custom colors example\n")
custom_colors <- c(
  "male" = "#1f77b4", "female" = "#ff7f0e",
  "unsexed" = "#2ca02c", "total" = "#d62728"
)
p5 <- plot_length_frequency(results_commercial,
  plot_type = "pooled",
  sex_colors = custom_colors,
  title = "Custom Colored Length Frequency Plot",
  show_uncertainty = TRUE
)
print(p5)

cat("All plotting tests completed successfully!\n")
cat("Functions available:\n")
cat("- plot_length_frequency(): Main plotting function\n")
cat("- Arguments: plot_type ('pooled' or 'by_stratum'), y_axis ('frequency' or 'proportion')\n")
cat("- Optional: show_uncertainty, sex_colors, title\n")
