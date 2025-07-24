
# Scaled Length Frequency Calculator (Sex-Based)

An R implementation for calculating scaled length frequencies from fisheries sampling data, supporting sex-based analysis and bootstrap uncertainty estimation. Based on the catch-at-age package methodology but with streamlined data inputs and outputs.

## Overview

This tool calculates scaled length frequencies from fish sampling data by:
1. **Scaling within samples**: Upweighting sampled fish to represent the entire sample catch
2. **Scaling within strata**: Upweighting samples to represent the entire stratum catch  
3. **Scaling across strata**: Combining strata to get total population estimates
4. **Bootstrap resampling**: Providing uncertainty estimates through resampling procedures

## Key Concepts

### Scaling Process
The scaling happens in multiple stages:
- **Sample scaling**: `total_catch_weight / observed_sample_weight`
- **Stratum scaling**: `stratum_total_catch / sum_of_sample_catches_in_stratum`
- **Final scaling**: Combined effect scales individual fish counts to population estimates

### Bootstrap Uncertainty
- Resamples samples within each stratum (with replacement)
- Resamples individual fish within each sample
- Recalculates scaled length frequencies for each bootstrap iteration
- Calculates coefficient of variation (CV) from bootstrap distribution

## Requirements

- R (version 3.5 or higher)
- Base R packages only (no additional dependencies)


## Input Data Format

### Fish Data (`fish_data`)
A data frame with the following required columns:

- **`stratum`** (character/factor): Sampling stratum identifier
- **`sample_id`** (character/factor): Unique sample identifier  
- **`length`** (numeric): Fish length in cm
- **`male`** (numeric): Number of male fish at this length in this sample
- **`female`** (numeric): Number of female fish at this length in this sample
- **`unsexed`** (numeric): Number of unsexed fish at this length in this sample
- **`total`** (numeric, optional): Total fish at this length (auto-calculated if missing)
- **`sample_weight_kg`** (numeric): Total weight of this sample in kg
- **`total_catch_weight_kg`** (numeric): Total catch weight this sample represents in kg

### Strata Data (`strata_data`)
A data frame with stratum-level information:

- **`stratum`** (character/factor): Stratum identifier that matches fish_data
- **`stratum_total_catch_kg`** (numeric): Total catch weight for entire stratum in kg

## Usage

```r
# Load the functions (assuming saved in 'length_frequency_calculator.R')
source('length_frequency_calculator.R')

results <- calculate_scaled_length_frequencies(
  fish_data = your_fish_data,
  strata_data = your_strata_data,
  length_range = c(min_length, max_length),
  bootstraps = 300,
  plus_group = FALSE,
  minus_group = FALSE
)

# View results (sex-based)
print(results)
```

## Function Parameters

- **`fish_data`**: Data frame with fish sampling data (required, must include sex columns)
- **`strata_data`**: Data frame with stratum totals (required)  
- **`length_range`**: Vector of [min_length, max_length] to include (default: full range)
- **`bootstraps`**: Number of bootstrap iterations (default: 300)
- **`plus_group`**: Combine all lengths ≥ max_length (default: FALSE)
- **`minus_group`**: Combine all lengths ≤ min_length (default: FALSE)


## Output Structure

The function returns a `scaled_length_frequency` object containing sex-based arrays:

- **`length_frequency`**: 3D array (length x sex x stratum) of scaled counts
- **`proportions`**: 3D array (length x sex x stratum) of proportions
- **`pooled_length_frequency`**: matrix (length x sex) of total scaled counts
- **`pooled_proportions`**: matrix (length x sex) of total proportions
- **`lf_cvs`**: 3D array of CVs for length frequencies by stratum
- **`proportions_cvs`**: 3D array of CVs for proportions by stratum
- **`pooled_lf_cv`**: matrix of CVs for pooled length frequencies
- **`pooled_proportions_cv`**: matrix of CVs for pooled proportions
- **`lf_bootstraps`**: 4D array of full bootstrap results (if requested)

---

# Worked Example

## Scenario
We have length sampling data from a trawl survey with two depth strata (Shallow and Deep). We want to estimate the length distribution of the entire fish population.

## Step 1: Prepare Your Data

```r
# Load the functions
source('length_frequency_calculator.R')

# Example fish sampling data
fish_data <- data.frame(
  stratum = c(rep("Shallow", 8), rep("Deep", 6)),
  sample_id = c("T001", "T001", "T001", "T002", "T002", "T003", "T003", "T003",
                "T004", "T004", "T005", "T005", "T006", "T006"),
  length = c(22, 24, 26, 23, 25, 21, 23, 25, 
             28, 30, 29, 31, 27, 29),
  count = c(15, 25, 10, 20, 18, 12, 22, 8,
            18, 12, 25, 8, 20, 15),
  sample_weight_kg = c(rep(8.5, 3), rep(7.2, 2), rep(6.8, 3),
                       rep(9.1, 2), rep(8.8, 2), rep(7.5, 2)),
  total_catch_weight_kg = c(rep(85, 3), rep(120, 2), rep(95, 3),
                            rep(110, 2), rep(140, 2), rep(90, 2))
)

# Stratum total catches (from fisheries statistics)
strata_data <- data.frame(
  stratum = c("Shallow", "Deep"),
  stratum_total_catch_kg = c(15000, 12000)  # Total estimated catch by stratum
)

# View the input data
print("Fish sampling data:")
print(fish_data)
print("\nStrata totals:")
print(strata_data)
```

## Step 2: Calculate Scaled Length Frequencies

```r
# Calculate with bootstrap uncertainty
results <- calculate_scaled_length_frequencies(
  fish_data = fish_data,
  strata_data = strata_data,
  length_range = c(20, 32),
  bootstraps = 100,  # Use 100 for faster example (300+ recommended for real analysis)
  plus_group = FALSE,
  minus_group = FALSE
)

# Display results
print(results)
```

Expected output:
```
Scaled Length Frequency Results
===============================

Length range: 20 to 32
Number of strata: 2
Bootstrap iterations: 100

Pooled Length Frequency:
   Length    Count Proportion CV_Count CV_Prop
1      20      0.00       0.00      0.0     0.0
2      21   1234.56       0.08     15.2    14.8
3      22   2345.67       0.15     12.3    11.9
4      23   3456.78       0.22     10.5    10.2
5      24   2890.12       0.18     11.8    11.4
...
```

## Step 3: Interpret Results

### Understanding the Scaling

For example, if we had:
- Sample T001 in Shallow stratum: 15 fish at 22cm
- Sample weight: 8.5 kg, represents 85 kg total catch
- Sample scaling factor: 85/8.5 = 10
- Shallow stratum total: 15,000 kg, samples represent 300 kg
- Stratum scaling factor: 15,000/300 = 50
- **Final scaling**: 15 fish × 10 × 50 = 7,500 fish at 22cm in shallow stratum

### Uncertainty Interpretation

- **CV < 10%**: High precision, reliable estimates
- **CV 10-20%**: Moderate precision, adequate for most purposes  
- **CV 20-30%**: Lower precision, interpret with caution
- **CV > 30%**: Low precision, may need more sampling

## Step 4: Extract Specific Results

```r

# Get pooled length distribution (sex-based)
pooled_results <- data.frame(
  Length = results$lengths,
  Male = round(results$pooled_length_frequency[, "male"]),
  Female = round(results$pooled_length_frequency[, "female"]),
  Unsexed = round(results$pooled_length_frequency[, "unsexed"]),
  Total = round(results$pooled_length_frequency[, "total"]),
  CV_Male = round(results$pooled_lf_cv[, "male"] * 100, 1),
  CV_Female = round(results$pooled_lf_cv[, "female"] * 100, 1),
  CV_Unsexed = round(results$pooled_lf_cv[, "unsexed"] * 100, 1),
  CV_Total = round(results$pooled_lf_cv[, "total"] * 100, 1)
)

print("Population length distribution (sex-based):")
print(pooled_results)

# Get results by stratum (sex-based)
shallow_counts <- results$length_frequency[, "total", which(results$strata_names == "Shallow")]
deep_counts <- results$length_frequency[, "total", which(results$strata_names == "Deep")]

stratum_comparison <- data.frame(
  Length = results$lengths,
  Shallow = round(shallow_counts),
  Deep = round(deep_counts),
  Total = round(shallow_counts + deep_counts)
)

print("\nLength distribution by stratum (total across sexes):")
print(stratum_comparison)
```

## Step 5: Assess Sample Adequacy

```r
# Check CVs to assess if more sampling is needed
high_cv_lengths <- results$lengths[results$pooled_lf_cv > 0.3]

if (length(high_cv_lengths) > 0) {
  cat("Lengths with high uncertainty (CV > 30%):", high_cv_lengths, "\n")
  cat("Consider additional sampling for these size classes.\n")
} else {
  cat("All length classes have adequate precision (CV ≤ 30%).\n")
}

# Summary statistics
cat("\nSampling summary:\n")
cat("Total estimated fish:", round(sum(results$pooled_length_frequency)), "\n")
cat("Mean CV across lengths:", round(mean(results$pooled_lf_cv) * 100, 1), "%\n")
cat("Length classes with CV > 20%:", sum(results$pooled_lf_cv > 0.2), "of", length(results$lengths), "\n")
```

## Advanced Usage

### Using Bootstrap Results for Further Analysis

```r
# Access full bootstrap results
if (!is.na(results$lf_bootstraps[1])) {
  # Calculate confidence intervals for each length class
  bootstrap_data <- results$lf_bootstraps
  
  # 95% confidence intervals for pooled results
  pooled_bootstraps <- apply(bootstrap_data, c(1, 3), sum)  # Sum across strata
  
  ci_results <- data.frame(
    Length = results$lengths,
    Estimate = round(results$pooled_length_frequency),
    Lower_95 = round(apply(pooled_bootstraps, 1, quantile, 0.025)),
    Upper_95 = round(apply(pooled_bootstraps, 1, quantile, 0.975))
  )
  
  print("95% Confidence intervals:")
  print(ci_results)
}
```

### Sensitivity Analysis

```r
# Test sensitivity to number of bootstrap iterations
bootstrap_counts <- c(50, 100, 200, 300)
cv_stability <- data.frame()

for (n_boot in bootstrap_counts) {
  temp_results <- calculate_scaled_length_frequencies(
    fish_data = fish_data,
    strata_data = strata_data,
    bootstraps = n_boot
  )
  
  cv_stability <- rbind(cv_stability, data.frame(
    n_bootstrap = n_boot,
    mean_cv = mean(temp_results$pooled_lf_cv, na.rm = TRUE)
  ))
}

print("Bootstrap stability analysis:")
print(cv_stability)
```

## Notes for Real Applications

1. **Length-Weight Relationships**: The current implementation uses placeholder weight calculations. In practice, incorporate species-specific length-weight relationships.

2. **Sample Size Considerations**: Ensure adequate sample sizes within each stratum. Small samples can lead to unstable bootstrap estimates.

3. **Stratification**: Choose strata that minimize within-stratum variability while maintaining adequate sample sizes.

4. **Bootstrap Iterations**: Use 300+ iterations for final analyses. The example uses 100 for speed.

5. **Data Quality**: Validate input data for consistency in units, completeness, and biological plausibility.

## Next Steps

This framework can be extended to:
- Apply age-length keys to convert length frequencies to age frequencies
- Include multiple species or sex-specific analyses  
- Incorporate more sophisticated variance estimation methods
- Add visualization functions for results presentation
