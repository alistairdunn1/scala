# Demo: Calculate Scaled Age Compositions Using Cohort Model
# This script demonstrates the complete workflow for:
# 1. Fitting a cohort-based age-length model
# 2. Assigning ages to raw length data
# 3. Calculating scaled age compositions with bootstrap uncertainty

library(scala)
set.seed(123)

cat("===================================================================\n")
cat("Demo: Scaled Age Compositions from Cohort Model\n")
cat("===================================================================\n\n")

# ============================================================================
# STEP 1: Generate test data
# ============================================================================
cat("STEP 1: Generating test data...\n")
cat("---------------------------\n\n")

# Generate aged subsample (this would come from otoliths in real data)
n_aged <- 200
aged_data <- data.frame(
  age = sample(1:8, n_aged, replace = TRUE, prob = c(0.05, 0.15, 0.20, 0.25, 0.20, 0.10, 0.03, 0.02)),
  year = sample(2018:2022, n_aged, replace = TRUE),
  sex = sample(c("male", "female"), n_aged, replace = TRUE)
)

# Add lengths based on age with sex-specific growth
aged_data$length <- with(
  aged_data,
  18 + age * 4.5 + ifelse(sex == "female", 2, 0) + rnorm(n_aged, 0, 2.5)
)

cat("Aged subsample:\n")
cat("  Sample size:", n_aged, "fish\n")
cat("  Age range:", min(aged_data$age), "to", max(aged_data$age), "\n")
cat("  Year range:", min(aged_data$year), "to", max(aged_data$year), "\n")
cat("  Length range:", round(min(aged_data$length), 1), "to", round(max(aged_data$length), 1), "cm\n\n")

# Generate full fish dataset (unaged lengths from sampling)
n_samples <- 15
n_fish_per_sample <- 40

fish_data <- data.frame(
  stratum = rep(c("North", "South", "Central"), length.out = n_samples),
  sample_id = rep(1:n_samples, each = n_fish_per_sample),
  year = rep(sample(2020:2022, n_samples, replace = TRUE), each = n_fish_per_sample)
)

# Add lengths (unaged)
fish_data$length <- 20 + rnorm(nrow(fish_data), mean = 15, sd = 8)
fish_data$length <- pmax(15, pmin(55, fish_data$length))

# Add sex information
fish_data$sex <- sample(c("male", "female", "unsexed"), nrow(fish_data),
  replace = TRUE, prob = c(0.45, 0.45, 0.1)
)

# Add fish counts by sex (simulating length frequency data)
fish_data$male <- ifelse(fish_data$sex == "male", 1, 0)
fish_data$female <- ifelse(fish_data$sex == "female", 1, 0)
fish_data$unsexed <- ifelse(fish_data$sex == "unsexed", 1, 0)

# Add weight-based scaling information
fish_data$sample_weight_kg <- ave(fish_data$sample_id, fish_data$sample_id,
  FUN = function(x) runif(1, 15, 30)
)
fish_data$total_catch_weight_kg <- fish_data$sample_weight_kg * runif(nrow(fish_data), 8, 15)

cat("Full fish dataset (unaged lengths):\n")
cat("  Total fish:", nrow(fish_data), "\n")
cat("  Samples:", n_samples, "\n")
cat("  Strata:", paste(unique(fish_data$stratum), collapse = ", "), "\n")
cat("  Length range:", round(min(fish_data$length), 1), "to", round(max(fish_data$length), 1), "cm\n\n")

# Create strata data
strata_data <- data.frame(
  stratum = c("North", "South", "Central"),
  stratum_total_catch_kg = c(5000, 7000, 6000)
)

cat("Strata information:\n")
print(strata_data)
cat("\n")

# ============================================================================
# STEP 2: Fit cohort model on aged subsample
# ============================================================================
cat("\nSTEP 2: Fitting cohort age-length model...\n")
cat("------------------------------------------\n\n")

cohort_model <- fit_cohort_alk(
  alk_data = aged_data,
  by_sex = TRUE,
  age_offset = 1,
  verbose = TRUE
)

cat("\n")
print(cohort_model)
cat("\n")

# ============================================================================
# STEP 3: Assign ages to full dataset using cohort model
# ============================================================================
cat("\nSTEP 3: Assigning ages to unaged fish...\n")
cat("----------------------------------------\n\n")

# Method 1: Mode (most probable age)
fish_with_ages_mode <- assign_ages_from_cohort(
  fish_data = fish_data,
  cohort_model = cohort_model,
  method = "mode",
  verbose = TRUE
)

cat("\nAge distribution (mode method):\n")
print(table(fish_with_ages_mode$age))
cat("\n")

# Method 2: Random sampling (for uncertainty propagation)
fish_with_ages_random <- assign_ages_from_cohort(
  fish_data = fish_data,
  cohort_model = cohort_model,
  method = "random",
  seed = 456,
  verbose = FALSE
)

cat("Age distribution (random method):\n")
print(table(fish_with_ages_random$age))
cat("\n")

# ============================================================================
# STEP 4: Calculate scaled age compositions
# ============================================================================
cat("\nSTEP 4: Calculating scaled age compositions...\n")
cat("---------------------------------------------\n\n")

# Length-weight parameters (example for a generic fish species)
lw_male <- c(a = 0.01, b = 3.0)
lw_female <- c(a = 0.011, b = 2.95)
lw_unsexed <- c(a = 0.0105, b = 2.98)

# Calculate age compositions with bootstrap uncertainty
age_comps <- calculate_age_compositions_from_cohort(
  fish_data = fish_with_ages_mode,
  strata_data = strata_data,
  age_range = c(1, 10),
  lw_params_male = lw_male,
  lw_params_female = lw_female,
  lw_params_unsexed = lw_unsexed,
  bootstraps = 100,
  plus_group_age = TRUE,
  minus_group_age = FALSE,
  verbose = TRUE
)

cat("\n")

# ============================================================================
# STEP 5: Display results
# ============================================================================
cat("\nSTEP 5: Results summary\n")
cat("----------------------\n\n")

cat("Pooled age composition (across all strata):\n")
print(round(age_comps$pooled_age_composition, 2))
cat("\n")

cat("Pooled age proportions:\n")
print(round(age_comps$pooled_age_proportions[, c("male", "female", "unsexed", "total")], 4))
cat("\n")

cat("Coefficients of variation (CV) for pooled age composition:\n")
print(round(age_comps$pooled_age_cv[, c("male", "female", "total")], 3))
cat("\n")

cat("95% Confidence intervals for total age composition:\n")
ci_df <- data.frame(
  Age = age_comps$ages,
  Estimate = age_comps$pooled_age_composition[, "total"],
  Lower_2.5 = age_comps$pooled_age_ci_lower[, "total"],
  Upper_97.5 = age_comps$pooled_age_ci_upper[, "total"]
)
print(round(ci_df, 2))
cat("\n")

# ============================================================================
# STEP 6: Compare methods
# ============================================================================
cat("\nSTEP 6: Comparing age assignment methods\n")
cat("----------------------------------------\n\n")

# Calculate compositions using random method
age_comps_random <- calculate_age_compositions_from_cohort(
  fish_data = fish_with_ages_random,
  strata_data = strata_data,
  age_range = c(1, 10),
  lw_params_male = lw_male,
  lw_params_female = lw_female,
  lw_params_unsexed = lw_unsexed,
  bootstraps = 0, # Skip bootstrap for speed
  plus_group_age = TRUE,
  verbose = FALSE
)

cat("Comparison of pooled proportions (mode vs random):\n")
comparison <- data.frame(
  Age = age_comps$ages,
  Mode_Total = age_comps$pooled_age_proportions[, "total"],
  Random_Total = age_comps_random$age_proportions[, "total", 1]
)
print(round(comparison, 4))
cat("\n")

cat("===================================================================\n")
cat("Demo complete!\n")
cat("===================================================================\n\n")

cat("Summary:\n")
cat("  - Fitted cohort model on", n_aged, "aged fish\n")
cat("  - Assigned ages to", nrow(fish_data), "unaged fish\n")
cat("  - Calculated scaled age compositions across", n_strata, "strata\n")
cat("  - Estimated uncertainty using", age_comps$n_bootstraps, "bootstrap iterations\n")
cat("\n")

cat("Next steps:\n")
cat("  1. Use age_comps for stock assessment models\n")
cat("  2. Compare with traditional ALK methods\n")
cat("  3. Analyze age structure trends over time\n")
cat("  4. Evaluate sampling design efficiency\n")
