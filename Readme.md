# scala: Scaled Catch at length and Age Compositions

An R implementation for calculating scaled length compositions from fisheries sampling data, supporting sex-based analysis and bootstrap uncertainty estimation. 

Supports both **commercial fisheries** (weight-based scaling) and **research surveys** (density-based scaling).

[![R Package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-0.1-orange.svg)](https://github.com/alistairdunn1/scala)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Overview](#overview)
- [Key Concepts](#key-concepts)
- [Requirements](#requirements)
- [Available Functions](#available-functions)
- [Input Data Format](#input-data-format)
- [Usage](#usage)
- [Data Visualization](#data-visualization)
- [Function Parameters](#function-parameters)
- [Output Structure](#output-structure)
- [Worked Example](#worked-example)
- [Advanced Usage](#advanced-usage)
- [Notes for Real Applications](#notes-for-real-applications)
- [Package Information](#package-information)
- [Citation](#citation)
- [Contributing](#contributing)
- [Troubleshooting](#troubleshooting)
- [Next Steps](#next-steps)

## Installation

### Prerequisites

- R (version 4.0 or higher)
- Base R packages: `stats`

### Installing the Package

#### From Source (Local Development)

1. Clone or download the repository
2. In R, navigate to the package directory and install:

```r
# Install development dependencies (optional)
install.packages(c("roxygen2", "devtools"))

# Build and install the package
devtools::install(".")

# Or using R CMD (from command line)
R CMD build scala
R CMD INSTALL scala_0.1.tar.gz
```

#### Quick Build (Windows)

For Windows users, use the provided build script:

```batch
build.bat
```

This script will:
- Generate documentation using roxygen2
- Build the package
- Run R CMD check for validation
- Install the package locally

### Loading the Package

```r
library(scala)
```

### Optional Dependencies

For enhanced plotting capabilities, install suggested packages:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "RColorBrewer", "viridis", "patchwork"))
```

## Quick Start

Here's a minimal example to get you started:

```r
# Load the package
library(scala)

# Generate test data (commercial fisheries example)
test_data <- generate_test_data(data_type = "commercial")

# Get default length-weight parameters
lw_params <- get_default_lw_params()

# Calculate scaled length compositions
results <- calculate_length_compositions(
  fish_data = test_data$fish_data,
  strata_data = test_data$strata_data,
  length_range = c(20, 35),
  lw_params_male = lw_params$male,
  lw_params_female = lw_params$female,  
  lw_params_unsexed = lw_params$unsexed,
  bootstraps = 100
)

# View results
print(results)

# Create publication-quality plots with lines and uncertainty ribbons
if (requireNamespace("ggplot2", quietly = TRUE)) {
  # Pooled plot with uncertainty ribbons
  plot(results, plot_type = "pooled", show_uncertainty = TRUE)
  
  # Faceted plot: rows=strata, columns=sex categories
  plot(results, plot_type = "by_stratum", show_uncertainty = TRUE)
}
```

## Overview

This tool calculates scaled length compositions from fish sampling data by:

1. **Scaling within samples**: Upweighting sampled fish to represent the entire sample catch (weight-based) or area coverage (density-based)
2. **Scaling within strata**: Upweighting samples to represent the entire stratum catch or area
3. **Scaling across strata**: Combining strata to get total population estimates
4. **Bootstrap resampling**: Providing uncertainty estimates through resampling procedures

## Recent Enhancements

### Version 0.1 - Performance & Visualization Improvements

**🚀 Optimized Performance**
- **Vectorized calculations**: Replaced nested loops with advanced dplyr operations for significantly faster execution
- **Bootstrap optimization**: Streamlined resampling procedures for improved computational efficiency
- **Memory efficiency**: Reduced memory usage through optimized data structures

**📊 Enhanced Visualization**
- **Modern line plots**: Replaced traditional bar charts with clean line visualizations
- **Uncertainty ribbons**: Bootstrap confidence regions displayed as intuitive shaded areas
- **Faceted layouts**: Professional multi-panel displays with rows for strata and columns for sex categories
- **Publication-ready**: Customizable themes and colors for scientific publications

**📈 Key Benefits**
- Faster analysis workflows with optimized calculations
- Cleaner, more interpretable visualizations
- Better uncertainty communication through ribbon displays
- Professional multi-panel layouts for comprehensive data exploration

## Key Concepts

### Scaling Process

The package supports two scaling approaches:

#### Weight-Based Scaling (Commercial Fisheries)

- **Sample scaling**: `total_catch_weight / observed_sample_weight`
- **Stratum scaling**: `stratum_total_catch / sum_of_sample_catches_in_stratum`
- **Final scaling**: Combined effect scales individual fish counts to population estimates

#### Density-Based Scaling (Research Surveys)

- **Sample scaling**: `(catch_density * sample_area) / observed_sample_weight`
- **Stratum scaling**: `stratum_area / sum_of_sample_areas_in_stratum`
- **Final scaling**: Combined effect scales individual fish counts to population estimates based on area coverage

### Bootstrap Uncertainty

- Resamples samples within each stratum (with replacement)
- Resamples individual fish within each sample
- Recalculates scaled length compositions for each bootstrap iteration
- Calculates coefficient of variation (CV) from bootstrap distribution

## Requirements

- R (version 4.0 or higher)
- **Required packages**: `stats` (included with base R)
- **Suggested packages**: `ggplot2`, `dplyr`, `tidyr`, `RColorBrewer`, `viridis`, `patchwork`, `plotly`, `gridExtra`, `rmarkdown`, `testthat`
- **Memory**: Sufficient RAM for bootstrap operations (depends on data size and bootstrap iterations)
- **Storage**: Minimal disk space required (package size ~1MB)

### Performance Considerations

- **Bootstrap iterations**: More iterations provide better uncertainty estimates but increase computation time
- **Data size**: Large datasets with many strata/samples will require more memory and processing time
- **Recommended**: 300+ bootstrap iterations for final analyses, fewer (50-100) for exploratory work

## Available Functions

The package provides the following main functions:

- **`calculate_length_compositions()`**: Main function for calculating length compositions with optional bootstrap uncertainty estimation
- **`generate_test_data()`**: Generate sample datasets for testing and examples  
- **`generate_commercial_test_data()`**: Generate commercial fisheries test data
- **`generate_survey_test_data()`**: Generate research survey test data
- **`get_default_lw_params()`**: Get default length-weight parameters for testing
- **`plot.length_composition()`**: Create professional visualizations with lines and uncertainty ribbons (requires ggplot2)
- **`resample_fish_data()`**: Internal function for bootstrap resampling

### Enhanced Plotting Features

The plotting system now includes:
- **Line plots** with uncertainty ribbons instead of traditional bar charts
- **Faceted layouts** for multi-stratum, multi-sex visualization
- **Bootstrap uncertainty** visualization as shaded confidence regions
- **Customizable themes** and color schemes for publication-quality figures

For detailed documentation of any function, use `?function_name` in R (e.g., `?calculate_length_compositions`).

## Input Data Format

The package supports two types of scaling approaches:

### Weight-Based Scaling (Commercial Fisheries)

#### Fish Data (`fish_data`)

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

#### Strata Data (`strata_data`)

A data frame with stratum-level information:

- **`stratum`** (character/factor): Stratum identifier that matches fish_data
- **`stratum_total_catch_kg`** (numeric): Total catch weight for entire stratum in kg

### Density-Based Scaling (Research Surveys)

#### Fish Data (`fish_data`)

A data frame with the following required columns:

- **`stratum`** (character/factor): Sampling stratum identifier
- **`sample_id`** (character/factor): Unique sample identifier
- **`length`** (numeric): Fish length in cm
- **`male`** (numeric): Number of male fish at this length in this sample
- **`female`** (numeric): Number of female fish at this length in this sample
- **`unsexed`** (numeric): Number of unsexed fish at this length in this sample
- **`total`** (numeric, optional): Total fish at this length (auto-calculated if missing)
- **`sample_area_km2`** (numeric): Area sampled in this sample in km²
- **`catch_density_kg_km2`** (numeric): Catch density observed in this sample in kg/km²

#### Strata Data (`strata_data`)

A data frame with stratum-level information:

- **`stratum`** (character/factor): Stratum identifier that matches fish_data
- **`stratum_area_km2`** (numeric): Total area of the stratum in km²

## Usage

The function automatically detects whether you're using weight-based or density-based data based on the column names provided.

**Length-Weight Parameters Required:** All analyses require species and sex-specific length-weight parameters using the allometric relationship: Weight (g) = a × Length(cm)^b

### Commercial Fisheries Example (Weight-based Scaling)

```r
# Load test data for commercial fisheries
test_data <- generate_test_data(data_type = "commercial")

# Example length-weight parameters
lw_male <- c(a = 0.0085, b = 3.10)      # Males
lw_female <- c(a = 0.0092, b = 3.05)    # Females 
lw_unsexed <- c(a = 0.0089, b = 3.08)   # Unsexed

# Calculate scaled length compositions
result <- calculate_length_compositions(
  fish_data = test_data$fish_data,
  strata_data = test_data$strata_data,
  lw_params_male = lw_male,
  lw_params_female = lw_female,
  lw_params_unsexed = lw_unsexed,
  length_range = c(20, 80),
  bootstraps = 300
)

# View results
print(result)
```

## Data Visualization

The package includes professional plotting capabilities to visualize scaled length composition results using **lines with uncertainty ribbons**:

### Enhanced Plotting Features

- **Line plots** with uncertainty ribbons (replacing traditional bar charts)
- **Faceted layouts** for by-stratum visualization (rows = strata, columns = sex categories)
- **Bootstrap uncertainty** displayed as shaded ribbons (±1 standard error)
- **Customizable appearance** with color themes and plot options

### Basic Plotting

```r
# Install ggplot2 if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Plot pooled results across all strata (lines with uncertainty ribbons)
plot(result, plot_type = "pooled", show_uncertainty = TRUE)

# Plot results by stratum (faceted layout: rows=strata, columns=sex categories)
plot(result, plot_type = "by_stratum", show_uncertainty = TRUE)

# Show proportions instead of absolute compositions
plot(result, plot_type = "pooled", y_axis = "proportion")

# Customize plot appearance with ocean theme
ocean_colors <- c("male" = "#004466", "female" = "#006699", 
                  "unsexed" = "#0099CC", "pooled" = "#FF6600")
plot(result, 
     plot_type = "pooled",
     sex_colors = ocean_colors,
     title = "Length Distribution by Sex",
     show_uncertainty = TRUE)
```

### Advanced Visualization Options

```r
# By-stratum faceted plot with custom layout
# Rows represent different strata (e.g., "Coastal", "Offshore")  
# Columns represent sex categories (Male, Female, Unsexed, Pooled)
plot(result, 
     plot_type = "by_stratum",
     show_uncertainty = TRUE,
     title = "Length Composition by Sex and Stratum")

# Proportional view for comparing distribution shapes
plot(result, 
     plot_type = "by_stratum", 
     y_axis = "proportion",
     title = "Relative Length Composition Patterns")
```

### Plot Options

- **`plot_type`**: 
  - `"pooled"` - Combined across strata with separate lines for each sex
  - `"by_stratum"` - Faceted layout (rows = strata, columns = sex categories)
- **`y_axis`**: 
  - `"composition"` - Absolute scaled compositions (number of fish)
  - `"proportion"` - Relative proportions (0-1 scale)
- **`show_uncertainty`**: 
  - `TRUE` - Show uncertainty ribbons based on bootstrap CV estimates
  - `FALSE` - Show only the mean length composition lines
- **`sex_colors`**: Named vector to customize colors for sex categories (`"male"`, `"female"`, `"unsexed"`, `"pooled"`)
- **`title`**: Custom plot title

### Visualization Features

- **Lines**: Clean representation of length composition trends
- **Uncertainty ribbons**: Shaded areas showing ±1 standard error from bootstrap estimates
- **Faceted layout**: Professional multi-panel display for comparing strata and sex categories
- **Color customization**: Flexible theming options for publication-quality figures

### Complete Visualization Example

```r
# Load test data for research surveys
test_data <- generate_test_data(data_type = "survey")

# Example length-weight parameters for different species/populations
lw_male <- c(a = 0.0067, b = 3.15)      # Males
lw_female <- c(a = 0.0071, b = 3.12)    # Females
lw_unsexed <- c(a = 0.0069, b = 3.14)   # Unsexed

# Calculate scaled length compositions with bootstrap uncertainty
result <- calculate_length_compositions(
  fish_data = test_data$fish_data,
  strata_data = test_data$strata_data,
  lw_params_male = lw_male,
  lw_params_female = lw_female,
  lw_params_unsexed = lw_unsexed,
  length_range = c(25, 85),
  bootstraps = 300
)

# Create comprehensive visualizations
library(ggplot2)

# 1. Pooled results with uncertainty ribbons
pooled_plot <- plot(result, 
                    plot_type = "pooled", 
                    show_uncertainty = TRUE,
                    title = "Length Composition by Sex (All Strata Combined)")

# 2. Faceted by-stratum plot showing all sex×stratum combinations
faceted_plot <- plot(result, 
                     plot_type = "by_stratum", 
                     show_uncertainty = TRUE,
                     title = "Length Composition by Sex and Stratum")

# 3. Proportional comparison for pattern analysis
proportion_plot <- plot(result, 
                        plot_type = "by_stratum", 
                        y_axis = "proportion",
                        title = "Relative Length Distribution Patterns")

# Display plots
print(pooled_plot)
print(faceted_plot) 
print(proportion_plot)
```

### Density-Based Example (Research Surveys)

```r
# Load the functions
source('length_composition_calculator.R')

# Define length-weight parameters (same as above)
lw_params_male <- c(a = 0.0085, b = 3.10)
lw_params_female <- c(a = 0.0092, b = 3.05)
lw_params_unsexed <- c(a = 0.0088, b = 3.08)

# Density-based data
survey_results <- calculate_length_compositions(
  fish_data = your_survey_fish_data,  # Contains sample_area_km2, catch_density_kg_km2
  strata_data = your_survey_strata_data,  # Contains stratum_area_km2
  length_range = c(min_length, max_length),
  lw_params_male = lw_params_male,
  lw_params_female = lw_params_female,
  lw_params_unsexed = lw_params_unsexed,
  bootstraps = 300,
  plus_group = FALSE,
  minus_group = FALSE
)

# View results
print(survey_results)
```

## Function Parameters

- **`fish_data`**: Data frame with fish sampling data (required, must include sex columns)
- **`strata_data`**: Data frame with stratum totals (required)
- **`length_range`**: Vector of [min_length, max_length] to include (default: full range)
- **`lw_params_male`**: Named vector with length-weight parameters for males: c(a = 0.01, b = 3.0) (required)
- **`lw_params_female`**: Named vector with length-weight parameters for females: c(a = 0.01, b = 3.0) (required)
- **`lw_params_unsexed`**: Named vector with length-weight parameters for unsexed fish: c(a = 0.01, b = 3.0) (required)
- **`bootstraps`**: Number of bootstrap iterations (default: 300)
- **`plus_group`**: Combine all lengths ≥ max_length (default: FALSE)
- **`minus_group`**: Combine all lengths ≤ min_length (default: FALSE)

### Length-Weight Parameter Notes

- Parameters follow the allometric relationship: **Weight (g) = a × Length(cm)^b**
- Both `a` and `b` must be positive values
- Parameters must be provided as named vectors with elements 'a' and 'b'
- Different parameters for each sex category allow for sexual dimorphism in growth
- Example parameters for common species:
  - **Snapper**: Male c(a = 0.0085, b = 3.10), Female c(a = 0.0092, b = 3.05)
  - **Haddock**: Male c(a = 0.0067, b = 3.15), Female c(a = 0.0071, b = 3.12)

## Output Structure

The function returns a `scaled_length_composition` object containing sex-based arrays:

- **`length_composition`**: 3D array (length x sex x stratum) of scaled counts
- **`proportions`**: 3D array (length x sex x stratum) of proportions
- **`pooled_length_composition`**: matrix (length x sex) of total scaled counts
- **`pooled_proportions`**: matrix (length x sex) of total proportions
- **`lf_cvs`**: 3D array of CVs for length compositions by stratum
- **`proportions_cvs`**: 3D array of CVs for proportions by stratum
- **`pooled_lf_cv`**: matrix of CVs for pooled length compositions
- **`pooled_proportions_cv`**: matrix of CVs for pooled proportions
- **`lf_bootstraps`**: 4D array of full bootstrap results (if requested)

---

# Worked Example

## Scenario

We have length sampling data from a trawl survey with two depth strata (Shallow and Deep). We want to estimate the length distribution of the entire fish population.

## Step 1: Prepare Your Data

```r
# Load the functions
source('length_composition_calculator.R')

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
results <- calculate_length_compositions(
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
  Male = round(results$pooled_length_composition[, "male"]),
  Female = round(results$pooled_length_composition[, "female"]),
  Unsexed = round(results$pooled_length_composition[, "unsexed"]),
  Total = round(results$pooled_length_composition[, "total"]),
  CV_Male = round(results$pooled_lf_cv[, "male"] * 100, 1),
  CV_Female = round(results$pooled_lf_cv[, "female"] * 100, 1),
  CV_Unsexed = round(results$pooled_lf_cv[, "unsexed"] * 100, 1),
  CV_Total = round(results$pooled_lf_cv[, "total"] * 100, 1)
)

print("Population length distribution (sex-based):")
print(pooled_results)

# Get results by stratum (sex-based)
shallow_counts <- results$length_composition[, "total", which(results$strata_names == "Shallow")]
deep_counts <- results$length_composition[, "total", which(results$strata_names == "Deep")]

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
cat("Total estimated fish:", round(sum(results$pooled_length_composition)), "\n")
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
    Estimate = round(results$pooled_length_composition),
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
  temp_results <- calculate_length_compositions(
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

### When to Use Each Approach

**Weight-based scaling** is appropriate when:

- Working with commercial fisheries data
- You have total catch weights at sample and stratum levels
- Scaling from samples to represent total commercial catch
- Working with observer data or port sampling

**Density-based scaling** is appropriate when:

- Working with research survey data
- You have swept area or sampling area information
- You know catch density (biomass per unit area)
- You want to estimate population abundance over specific areas

### Implementation Notes

1. **Length-Weight Relationships**: The current implementation uses placeholder weight calculations. In practice, incorporate species-specific length-weight relationships.
2. **Sample Size Considerations**: Ensure adequate sample sizes within each stratum. Small samples can lead to unstable bootstrap estimates.
3. **Stratification**: Choose strata that minimize within-stratum variability while maintaining adequate sample sizes.
4. **Bootstrap Iterations**: Use 300+ iterations for final analyses. The examples use fewer for speed.
5. **Data Quality**: Validate input data for consistency in units, completeness, and biological plausibility.
6. **Survey Design**: For density-based scaling, ensure that sampling areas are representative of the strata and that density estimates are reliable.

## Package Information

**Version**: 0.1  
**Author**: Alistair Dunn  
**Maintainer**: Alistair Dunn <alistair.dunn@OceanEnvironmental.co.nz>  
**License**: GPL (>= 3)  
**Encoding**: UTF-8

## Citation

To cite the `scala` package in publications, please use:

```r
# Get citation information
citation("scala")
```

**Suggested citation:**

Dunn, A. (2025). scala: Calculate scaled catch and length and age compositions. R package version 0.1. https://github.com/alistairdunn1/scala

## Contributing

This package is under active development. For bug reports, feature requests, or contributions:

1. Check existing issues on the GitHub repository
2. Submit bug reports with reproducible examples
3. Suggest improvements or new features
4. Submit pull requests with proposed changes

## Package Development

### Building from Source

The package includes development tools for maintainers:

```r
# Generate documentation
roxygen2::roxygenise()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build package
devtools::build()
```

### Windows Build Script

Use `build.bat` for automated building on Windows systems:

```batch
build.bat
```

This script handles documentation generation, building, checking, and installation.

## Troubleshooting

### Common Issues

**Error: "Package not found"**
- Ensure you have built and installed the package correctly
- Try `devtools::install(".")` from the package directory

**Error: "Length-weight parameters missing"**
- All three sex-specific length-weight parameters are required
- Use `get_default_lw_params()` for testing purposes

**Error: "Column not found in data"**
- Check that your data has the required column names
- For weight-based: `sample_weight_kg`, `total_catch_weight_kg`, `stratum_total_catch_kg`
- For density-based: `sample_area_km2`, `catch_density_kg_km2`, `stratum_area_km2`

**High CVs (> 30%)**
- Increase sample sizes within strata
- Consider increasing bootstrap iterations (300+ recommended)
- Review stratification scheme

**Plotting errors**
- Install ggplot2: `install.packages("ggplot2")`
- Check that results object is valid

### Getting Help

- Use `?function_name` for function documentation
- Check function examples with `example(function_name)`
- Review the worked examples in this README
- Submit issues on the GitHub repository for bugs or questions

## Next Steps

This framework can be extended to:

- Apply age-length keys to convert length compositions to age compositions
- Include multiple species or sex-specific analyses
- Incorporate more sophisticated variance estimation methods
- Add visualization functions for results presentation
- Include swept area calculations for trawl surveys
- Add support for different survey gear types
