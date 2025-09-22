# scala: Scaled catch at length and age composition analyses

An R implementation for calculating scaled length compositions and age compositions from fisheries sampling data, supporting sex-based analysis and bootstrap uncertainty estimation.

Supports both **commercial fisheries** (weight-based scaling) and **research surveys** (density-based scaling).

**Note, this package is in development. Functionality may be not be fully complete in some cases.**

[![R Package](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-2025--08-orange.svg)](https://github.com/alistairdunn1/scala)
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
- [Data Visualisation](#data-visualisation)
- [Function Parameters](#function-parameters)
- [Output Structure](#output-structure)
- [Worked Example](#worked-example)
- [Advanced Usage](#advanced-usage)
- [Notes for Real Applications](#notes-for-real-applications)
- [Package Information](#package-information)
- [Citation](#citation)
- [Contributing](#contributing)
- [Troubleshooting](#troubleshooting)

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
devtools::install("alistairdunn1/scala", subdir = "scala")

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

For plotting capabilities, install suggested packages:

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

# Generate age-length key for age composition analysis
age_key <- generate_age_length_key(
  length_range = c(20, 35),
  age_range = c(2, 8),
  growth_type = "vonbert"
)

# For incomplete age-length keys, create a complete key first
complete_alk <- create_alk(
  age_length_key = age_key,
  lengths = 20:35,  # All length bins in your data
  ages = 2:8,       # All age bins you want
  verbose = TRUE
)

# Calculate age compositions (auto-detects complete vs incomplete keys)
age_results <- calculate_age_compositions(
  x = results,
  age_length_key = complete_alk,  # Uses complete key
  age_range = c(2, 8)
)

# View age composition results
print(age_results)

# Create publication-quality plots with lines and uncertainty ribbons
if (requireNamespace("ggplot2", quietly = TRUE)) {
  # Length composition plots
  plot(results, by_stratum = FALSE, show_CIs = TRUE)  # Pooled
  plot(results, by_stratum = TRUE, show_CIs = TRUE)   # Faceted
  
  # Age composition plots (same interface!)
  plot(age_results, by_stratum = FALSE, show_CIs = TRUE)  # Pooled
  plot(age_results, by_stratum = TRUE, show_CIs = TRUE)   # Faceted
}
```

## Overview

This tool calculates scaled length compositions and age compositions from fish sampling data by:

1. **Scaling within samples**: Upweighting sampled fish to represent the entire sample catch (weight-based) or area coverage (density-based)
2. **Scaling within strata**: Upweighting samples to represent the entire stratum catch or area
3. **Scaling across strata**: Combining strata to get total population estimates
4. **Bootstrap resampling**: Providing uncertainty estimates through resampling procedures
5. **Age composition conversion**: Converting length compositions to age compositions using complete age-length keys with streamlined workflow and clear warnings for incomplete data

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
- Propagates uncertainty through age-length key application for age compositions
- Calculates coefficient of variation (CV) from bootstrap distribution

## Requirements

- R (version 4.0 or higher)
- Required packages (per DESCRIPTION): `ggplot2`, `rlang`, `dplyr`, `stats`, `zoo`, `mgcv`
- Suggested packages: `tidyr`, `RColorBrewer`, `viridis`, `patchwork`, `plotly`, `gridExtra`, `rmarkdown`, `testthat`
- Memory: Sufficient RAM for bootstrap operations (depends on data size and bootstrap iterations)
- Storage: Minimal disk space required (package size ~1MB)

### Performance Considerations

- **Bootstrap iterations**: More iterations provide better uncertainty estimates but increase computation time
- **Data size**: Large datasets with many strata/samples will require more memory and processing time
- **Recommended**: 300+ bootstrap iterations for final analyses, fewer (50-100) for exploratory work

## Available Functions

The package provides the following main functions:

- **`calculate_length_compositions()`**: Main function for calculating length compositions with optional bootstrap uncertainty estimation
- **`calculate_age_compositions()`**: Convert length compositions to age compositions using age-length keys with **smart detection** for complete vs incomplete keys and **warning system** for missing length data
- **`create_alk()`**: Create complete age-length keys from raw age data with interpolation/extrapolation for missing length-age combinations, supporting both single and sex-specific keys, length binning, and comprehensive sampling requirement analysis
- **`evaluate_sample_sizes()`**: Evaluate whether sample sizes are adequate for reliable bootstrap uncertainty estimation by checking fish per sample and samples per stratum against recommended thresholds
- **`filter_small_samples()`**: Create filtered datasets by excluding specific samples identified by sample evaluation (simplified interface)
- **`generate_test_data()`**: Generate sample datasets for testing and examples
- **`generate_commercial_test_data()`**: Generate commercial fisheries test data
- **`generate_survey_test_data()`**: Generate research survey test data
- **`generate_age_length_key()`**: Create sample age-length keys with various growth models
- **`get_default_lw_params()`**: Get default length-weight parameters for testing
- **`get_summary()`**: Get comprehensive summary statistics including mean weighted CV, fish counts, and haul counts from length composition results
- **`calculate_multinomial_n()`**: Calculate multinomial effective sample size from length or age composition proportions and CVs
- **`fit_ordinal_alk()`**: Fit an ordinal GAM (cumulative logit) age-at-length model (optionally by sex) and return a prediction function for age probabilities by length
- **`fit_cohort_alk()`**: Fit a cohort-based ordinal GAM model to estimate year classes from length, year, and sex, with age back-calculation capability
- **`compare_alks()`**: Compare age-length keys from empirical (`create_alk`) and model-based (`fit_ordinal_alk`) methods with summary metrics and optional visualisation
- **`plot.length_composition()`**: Create professional visualisations with lines and uncertainty ribbons (requires ggplot2)
- **`plot.age_composition()`**: Visualise age compositions with uncertainty ribbons using **identical interface** to length plotting
- **`plot_length_composition_comparison()`**: Create multi-panel comparison plots for multiple length composition results
- **`plot_alk()`**: Visualise age-length keys as heatmaps or points (with optional y-axis rug for length distribution)
- **`plot_sample_size_distribution()`**: Create stacked histograms showing sample size distributions by strata, with samples categorised by adequacy (non-representative small, representative small, adequate)
- **`resample_fish_data()`**: Internal function for bootstrap resampling

### Plotting Features

The plotting system includes:

- **Consistent interface**: Both length and age composition plotting functions use identical parameter names and functionality
- **Line plots** with uncertainty ribbons instead of traditional bar charts
- **Faceted layouts** for multi-stratum, multi-sex visualisation
- **Bootstrap uncertainty** visualisation as shaded confidence regions
- **Flexible binning** for both length (`length_bin_size`) and age (`age_bin_size`) data aggregation
- **Length binning** for aggregated visualisation at coarser scales
- **Sex category filtering** with optional inclusion of unsexed fish

### Multinomial Effective Sample Size

Functionality for calculating multinomial effective sample sizes from both length and age compositions:

- **Individual calculations** for specific stratum and sex combinations
- **Batch processing** for all combinations simultaneously
- **Robust fitting** with outlier detection and removal options
- **Quality diagnostics** including model fit statistics
- **Compatible with both length and age composition data**

This is particularly useful for:

- Stock assessment applications requiring effective sample sizes
- Data weighting in integrated models
- Quality control of length composition data

### Age Composition Analysis

The package provides a workflow for converting length compositions to age compositions:

**Smart Age-Length Key Detection**:

- **Complete keys**: `calculate_age_compositions()` automatically detects complete age-length keys (created by `create_alk()`) and applies them directly
- **Incomplete keys**: Traditional age-length keys are detected and users are warned about missing length data instead of silent interpolation
- **Warnings**: Explicit warnings identify which lengths are missing from incomplete keys

**Two-Step Workflow for Incomplete Keys**:

1. **`create_alk()`**: Creates complete age-length keys with interpolation/extrapolation for missing combinations
2. **`calculate_age_compositions()`**: Applies the complete key with full uncertainty propagation

**Key Features**:

- **Exact-match processing**: Simplified age-length key application using only exact length matches (no interpolation during application)
- **Linear interpolation**: Smart interpolation between known length-age combinations in `create_alk()`
- **Tail extrapolation**: Automatic extrapolation for lengths beyond the key range, with optional user-specified tail ages
- **Sex-specific support**: Full support for male, female, and unsexed fish categories
- **Bootstrap uncertainty**: Uncertainty propagation through all age composition calculations
- **Comprehensive visualisation**: Plots with uncertainty ribbons and multi-panel layouts

**Benefits of the Simplified Approach**:

- **Clear separation**: Data preparation (`create_alk()`) vs calculation (`calculate_age_compositions()`)
- **Explicit warnings**: Users know exactly which length data is problematic
- **Robust processing**: Complete keys ensure all length bins are covered
- **Maintainable code**: Single-responsibility functions that are easier to debug and extend

This enables:

- Full age composition analysis from length-based sampling with complete uncertainty estimation
- Robust handling of incomplete age-length keys through explicit interpolation step
- Clear user feedback about data completeness and processing decisions
- Stock assessment ready outputs with proper uncertainty quantification
- Stock assessment ready outputs with proper uncertainty quantification

For detailed documentation of any function, use `?function_name` in R (e.g., `?calculate_length_compositions`).

### Ordinal Age-at-Length Modelling (Optional). WARNING: In development

When raw age-length observations are available, you can fit a smooth ordinal age-at-length model and predict an ALK over desired lengths (and sex):

```r
# Fit model from raw age-length-sex data (one row per aged fish)
ord <- fit_ordinal_alk(alk_data = age_data, by_sex = TRUE)

# Predict per-age probabilities for given lengths and sex
lengths <- 20:70
sex <- rep(c("male", "female"), each = length(lengths))
probs <- ord$predict_function(lengths, sex)

# Combine into an ALK-like data.frame if needed
pred_alk <- cbind(
  data.frame(length = rep(lengths, 2), sex = sex),
  as.data.frame(probs)
)
```

Notes:

- Uses mgcv::ocat with cumulative logit; returns a robust prediction function.
- Probabilities are normalized per row and non-negative; ages increase with length on average.

### Cohort-Based Age-at-Length Modelling (Optional). WARNING: In development)

When raw age-length-year observations are available, you can fit a cohort-based ordinal GAM model to estimate year classes from length, year, and optionally sex:

```r
# Fit cohort model from raw age-length-year data 
cohort_model <- fit_cohort_alk(alk_data = age_data, by_sex = TRUE, age_offset = 1)

# Predict cohort probabilities for given lengths and years
lengths <- c(30, 35, 40)
years <- c(2018, 2019, 2020)
sex <- rep("female", length(lengths))
cohort_probs <- cohort_model$predict_cohort(lengths, years, sex)

# Back-calculate age probabilities for a specific sampling year
sampling_year <- 2020
age_probs <- cohort_model$predict_age(lengths, sampling_year, sex)
```

**Key Features**:

- **Year Class Calculation**: Cohorts defined as YC = (Year - Age) - age_offset (default age_offset = 1)
- **Flexible Offset**: Configurable age_offset parameter to match different fisheries conventions
- **Dual Predictions**: Estimate cohort probabilities OR back-calculate age probabilities
- **Sex-Specific Models**: Optional sex-specific smooths for length and year effects
- **Robust Fitting**: Uses mgcv::ocat with cumulative logit link for stable ordinal modeling

**Algorithm**: The model fits cohort ~ s(length) + s(year) with optional sex interactions, where cohorts are calculated using the configurable year class equation. This allows tracking of specific birth cohorts through time and estimation of cohort-specific length distributions.

## Creating Complete Age-Length Keys

The `create_alk()` function provides a comprehensive solution for creating complete age-length keys from raw age data, addressing common problems in fisheries analysis where age-length coverage is incomplete or sparse.

### Key Features

- **Raw Data Input**: Takes raw age-length data (one row per aged fish) rather than pre-processed age-length keys
- **Automatic Key Creation**: Converts raw data to proportional age-length keys with comprehensive interpolation/extrapolation
- **Length Binning**: Optional binning of length data (e.g., 2cm or 5cm bins) to consolidate sparse data
- **Interpolation Control**: Optional linear interpolation between observed length bins
- **Extrapolation Control**: Optional extrapolation beyond the observed length range
- **Sex-Specific Support**: Handles male, female, and combined unsexed keys automatically
- **Sampling Analysis**: Calculates current otolith counts and additional requirements for minimum (3 per length bin) and optimum (10 per length bin) coverage
- **User-Specified Tails**: Optional specification of age assignments for extreme lengths
- **Validation**: Functions to undertake input validation and progress reporting
- **Normalisation**: Ensures age proportions sum to 1.0 within each length bin

The default optimum target of 10 otoliths per length bin is based on a simulation study that recommends the number of otoliths for reliable age composition estimates (Coggins et al., 2013).

### Interpolation and Extrapolation Algorithms

The `create_alk()` function uses sophisticated algorithms to fill missing length-age combinations, applied in priority order:

#### 1. User-Specified Tail Ages)

User-defined length-age pairs are applied first for extreme lengths:

- Applied to lengths at or beyond the observed data range
- Takes precedence over automatic interpolation/extrapolation
- Allows expert knowledge to override automatic methods

#### 2. Linear Interpolation (Between Observed Data)

For missing lengths that fall **between** observed data points:

- **Method**: Distance-weighted linear interpolation
- **Calculation**: `interpolated_prop = weight_lower × lower_prop + weight_upper × upper_prop`
- **Weights**: Based on distance from bracketing lengths
- **Assumption**: Age composition changes gradually between observed lengths

**Example**:

- Observed: Length 20 (Age 2: 60%, Age 3: 40%), Length 24 (Age 2: 30%, Age 3: 70%)
- Missing: Length 22 (halfway between)
- Result: Length 22 gets (Age 2: 45%, Age 3: 55%)

#### 3. Constant Extrapolation (Beyond Observed Range)

For missing lengths **outside** the observed data range:

- **Method**: Copy age proportions from nearest observed length
- **Lower tail**: Copy from smallest observed length
- **Upper tail**: Copy from largest observed length
- **Assumption**: Age composition remains constant beyond observed range

**Example**:

- Observed range: Lengths 22-28
- Missing length 20: Copies all proportions from length 22
- Missing length 30: Copies all proportions from length 28

#### Algorithm Priority and Control

The algorithms are applied in this order for each missing length:

1. Check for user-specified tail ages → if found, use exact specification
2. If not found and length is between observed data → use linear interpolation (if enabled)
3. If not found and length is beyond observed range → use constant extrapolation (if enabled)
4. If disabled, the length remains unfilled (user must handle manually)

This hierarchical approach ensures expert knowledge takes precedence while providing automatic methods for routine data gaps.

### Basic Usage

```r
# Create age data frame (one row per aged fish)
age_data <- data.frame(
  age = c(2, 3, 3, 4, 4, 4, 5, 5, 6),
  length = c(22, 25, 26, 28, 29, 30, 32, 33, 35),
  sex = c("male", "male", "female", "male", "female", "male", "female", "male", "female")
)

# Create complete age-length key from raw data
complete_alk <- create_alk(
  age_data = age_data,
  lengths = 20:40,           # All lengths in your length composition data
  ages = 1:8,               # All ages you want to include
  verbose = TRUE            # Show progress and interpolation summary
)

# Use the complete key in age composition calculation
age_results <- calculate_age_compositions(
  x = length_composition_results,
  age_length_key = complete_alk,
  age_range = c(1, 8)
)
```

### Advanced Usage with Length Binning and Controls

```r
# Use length binning and control interpolation/extrapolation
complete_alk <- create_alk(
  age_data = age_data,
  lengths = 20:40,
  ages = 1:8,
  length_bin_size = 2,           # Use 2cm length bins
  interpolate = TRUE,            # Allow linear interpolation (default)
  extrapolate = FALSE,           # Disable extrapolation beyond observed range
  min_ages_per_length = 5,       # Minimum otoliths per length bin
  optimum_ages_per_length = 15,  # Optimum otoliths per length bin
  verbose = TRUE
)
```

### Sampling Requirement Analysis

The function provides detailed analysis of current sampling coverage and additional requirements:

```r
# The summary table shows for each sex category:
# - missing_lengths: Number of length bins requiring interpolation/extrapolation
# - interpolation_rate: Percentage of length bins requiring linear interpolation
# - extrapolation_rate: Percentage requiring extrapolation beyond observed range  
# - current_otoliths: Number of aged fish currently available
# - min_additional_required: Additional otoliths needed for minimum coverage (3 per length)
# - optimum_additional_required: Additional otoliths needed for optimum coverage (10 per length)
```

### Usage with Tail Specification

```r
# Specify ages for extreme lengths that might not interpolate well
complete_alk <- create_alk(
  age_data = age_data,
  lengths = 15:45,
  ages = 1:10,
  tail_ages = list(
    min_lengths = data.frame(length = c(13, 14), age = c(1, 1)),  # Young fish
    max_lengths = data.frame(length = c(46, 47), age = c(10, 10)) # Old fish
  ),
  verbose = TRUE
)
```

### Integration with calculate_age_compositions()

The `calculate_age_compositions()` function automatically detects complete age-length keys created by `create_alk()` and processes them efficiently:

```r
# Complete workflow with raw age data
complete_key <- create_alk(age_data, lengths = 15:35, ages = 1:8)
age_results <- calculate_age_compositions(lc_results, complete_key, age_range = c(1, 8))
```

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

## Data Visualisation

The package includes professional plotting capabilities to visualise both **length and age composition** results using **lines with uncertainty ribbons**. Both plotting functions share a consistent interface for ease of use:

### Basic Plotting

```r
# Install ggplot2 if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# LENGTH COMPOSITIONS
# Plot pooled results across all strata (lines with confidence intervals)
plot(lc_result, by_stratum = FALSE, show_CIs = TRUE)

# Plot results by stratum (faceted layout: rows=strata, columns=sex categories)
plot(lc_result, by_stratum = TRUE, show_CIs = TRUE)

# Plot only a specific stratum (with sex facets)
plot(lc_result, stratum = "North", show_CIs = TRUE)

# AGE COMPOSITIONS (same interface!)
# Plot pooled age compositions across all strata
plot(age_result, by_stratum = FALSE, show_CIs = TRUE)

# Plot age compositions by stratum with faceting
plot(age_result, by_stratum = TRUE, show_CIs = TRUE)

# Plot specific stratum for age compositions
plot(age_result, stratum = "North", show_CIs = TRUE)

# COMMON OPTIONS FOR BOTH FUNCTIONS
# Show proportions instead of absolute compositions
plot(lc_result, by_stratum = FALSE, type = "proportion")
plot(age_result, by_stratum = FALSE, type = "proportion")

# Hide confidence intervals
plot(lc_result, by_stratum = FALSE, show_CIs = FALSE)
plot(age_result, by_stratum = FALSE, show_CIs = FALSE)

# Use partial matching for convenience
plot(lc_result, type = "prop")  # equivalent to type = "proportion"

# Aggregate data into larger bins for summarized view
plot(lc_result, length_bin_size = 2)   # 2 cm length bins
plot(age_result, age_bin_size = 2)     # 2 year age bins

# Combine specific stratum with other options
plot(lc_result, stratum = "South", type = "proportion", length_bin_size = 5)
plot(age_result, stratum = "South", type = "proportion", age_bin_size = 2)

# Include or exclude unsexed fish category
plot(lc_result, unsexed = FALSE)  # Default: exclude unsexed category
plot(lc_result, unsexed = TRUE)   # Include unsexed category
plot(age_result, unsexed = FALSE) # Same options for age compositions
plot(age_result, unsexed = TRUE)
```

### Advanced Visualisation Options

```r
# By-stratum faceted plot with confidence intervals for length compositions
# Rows represent different strata (e.g., "North", "South")  
# Columns represent sex categories (Male, Female, Unsexed, Total)
plot(lc_result, 
     by_stratum = TRUE,
     show_CIs = TRUE,
     type = "composition")

# Same layout for age compositions - consistent interface!
plot(age_result, 
     by_stratum = TRUE,
     show_CIs = TRUE,
     type = "composition")

# Proportional view for comparing distribution shapes
plot(lc_result, 
     by_stratum = TRUE, 
     type = "proportion",
     show_CIs = TRUE)

plot(age_result, 
     by_stratum = TRUE, 
     type = "proportion",
     show_CIs = TRUE)

# Combine binning with other options for simplified views
plot(lc_result, 
     by_stratum = TRUE,
     length_bin_size = 5,
     type = "proportion")

plot(age_result, 
     by_stratum = TRUE,
     age_bin_size = 2,
     type = "proportion")

# Include unsexed category with other options
plot(lc_result, 
     by_stratum = TRUE,
     unsexed = TRUE,
     show_CIs = TRUE)

plot(age_result, 
     by_stratum = TRUE,
     unsexed = TRUE,
     show_CIs = TRUE)

# Clean plot without confidence intervals for publication
plot(lc_result, 
     by_stratum = FALSE,
     show_CIs = FALSE, 
     type = "proportion")

plot(age_result, 
     by_stratum = FALSE,
     show_CIs = FALSE, 
     type = "proportion")
```

### Plot Options

**Both `plot.length_composition()` and `plot.age_composition()` share identical parameters:**

- **`by_stratum`**:
  - `FALSE` - Pooled across strata with separate lines for each sex
  - `TRUE` - Faceted layout (rows = strata, columns = sex categories)
- **`stratum`**:
  - `NULL` (default) - Use `by_stratum` setting
  - Character string - Plot only the specified stratum (overrides `by_stratum`)
- **`type`**:
  - `"composition"` - Absolute scaled compositions (number of fish)
  - `"proportion"` - Relative proportions (0-1 scale)
- **`show_CIs`**:
  - `TRUE` - Display bootstrap confidence interval ribbons (95% CI)
  - `FALSE` - Show only the point estimates (lines without ribbons)
- **`length_bin_size`** / **`age_bin_size`**:
  - `NULL` (default) - Use original 1 cm/1 year bins
  - Numeric value (e.g., 2, 5, 10) - Aggregate data into larger bins
- **`unsexed`**:
  - `FALSE` (default) - Exclude unsexed fish category from plots
  - `TRUE` - Include unsexed fish category alongside male, female, and total

### Length and Age Binning Features

The **binning** feature allows aggregation of fine-scale data into larger, more manageable bins:

- **Flexible bin sizes**: Choose any bin size for both length (e.g., 2 cm, 5 cm) and age (e.g., 2 years, 3 years)
- **Automatic aggregation**: Compositions and confidence intervals are properly summed across bins
- **Cleaner visualisation**: Reduces noise in data visualisation for broader pattern analysis
- **Compatible with all options**: Works with both pooled and by-stratum plots, confidence intervals, and proportion scaling
- **Consistent interface**: Same parameter structure for both length (`length_bin_size`) and age (`age_bin_size`) binning

The plotting system generates **line plots with confidence interval ribbons** when bootstrap results are available, providing a clear and professional visualisation of uncertainty in both length and age compositions.

**Key Parameter Updates:**

- **Consistent Interface**: Both `plot.length_composition()` and `plot.age_composition()` use identical parameter names and functionality
- **`by_stratum`**: Logical (TRUE/FALSE) for faceted by-stratum display
- **`stratum`**: Character string to plot only a specific stratum (overrides `by_stratum`)
- **`show_CIs`**: Display bootstrap confidence interval ribbons (replaces old `include_uncertainty`)
- **`type`**: "composition" (default) or "proportion" for y-axis scaling
- **Binning parameters**: `length_bin_size` and `age_bin_size` for data aggregation
- **`unsexed`**: Logical to include/exclude unsexed fish category
- Removed deprecated parameters from age plotting: `strata`, `sex_categories`, `include_pooled`, `age_bins`

### Visualisation Features

- **Lines**: Clean representation of length composition trends
- **Confidence interval ribbons**: Shaded areas showing 95% bootstrap confidence intervals
- **Faceted layout**: Professional multi-panel display for comparing strata and sex categories
- **Colour customisation**: Flexible theming options for publication-quality figures

### Visualisation Example

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

# Create comprehensive visualisations
library(ggplot2)

# 1. Pooled results with confidence interval ribbons
pooled_plot <- plot(result, 
                    by_stratum = FALSE, 
                    show_CIs = TRUE)

# 2. Faceted by-stratum plot showing all sex×stratum combinations
faceted_plot <- plot(result, 
                     by_stratum = TRUE, 
                     show_CIs = TRUE)

# 3. Proportional comparison for pattern analysis
proportion_plot <- plot(result, 
                        by_stratum = TRUE, 
                        type = "proportion")

# 4. 5cm length binning for simplified view
binned_plot <- plot(result,
                    length_bin_size = 5,
                    show_CIs = TRUE)

# Display plots
print(pooled_plot)
print(faceted_plot) 
print(proportion_plot)
print(binned_plot)
```

## Comparison Plotting

The `plot_length_composition_comparison()` function allows for side-by-side comparison of multiple length composition results in a multi-panel layout.

### Function Signature

```r
plot_length_composition_comparison(
  x,                    # List of length_composition objects
  by_stratum = FALSE,   # Use pooled (FALSE) or by-stratum (TRUE) data
  stratum = NULL,       # Specific stratum name for by-stratum plots
  type = "composition", # "composition" or "proportion"
  length_bin_size = NULL, # Optional length binning (e.g., 2, 5, 10)
  unsexed = FALSE,      # Include unsexed category
  show_CIs = TRUE       # Show confidence intervals
)
```

### Basic Comparison Usage

```r
# Calculate length compositions for different scenarios
commercial_result <- calculate_length_compositions(...)
survey_result <- calculate_length_compositions(...)

# Create comparison plots
results_list <- list(
  "Commercial" = commercial_result,
  "Survey" = survey_result
)

# Basic comparison (pooled across strata)
comparison_plot <- plot_length_composition_comparison(results_list)

# Comparison including unsexed fish
comparison_with_unsexed <- plot_length_composition_comparison(
  results_list, 
  unsexed = TRUE
)

# Comparison with 5cm length binning
binned_comparison <- plot_length_composition_comparison(
  results_list,
  length_bin_size = 5,
  type = "proportion"
)

# By-stratum comparison for first available stratum
stratum_comparison <- plot_length_composition_comparison(
  results_list,
  by_stratum = TRUE
)

# Specific stratum comparison
specific_stratum <- plot_length_composition_comparison(
  results_list,
  by_stratum = TRUE,
  stratum = "North"
)
```

### Comparison Layout

The comparison plots use a **multi-panel layout**:

- **Rows**: Each element from the input list (e.g., different scenarios/datasets)
- **Columns**: Sex categories (Male, Female, Total, and optionally Unsexed)
- **Shared scales**: Consistent x and y axes across all panels for easy comparison

### Advanced Comparison Features

- **Named scenarios**: Use named lists for better plot labels
- **Length binning**: Apply consistent binning across all scenarios
- **Sex filtering**: Include/exclude unsexed category across all scenarios
- **Confidence intervals**: Show uncertainty consistently across comparisons
- **Flexible data**: Mix pooled and by-stratum data as needed

```r
# Advanced comparison example
advanced_comparison <- plot_length_composition_comparison(
  list(
    "Scenario A" = result_a,
    "Scenario B" = result_b,  
    "Scenario C" = result_c
  ),
  type = "proportion",
  length_bin_size = 5,
  unsexed = TRUE,
  show_CIs = TRUE
)
```

## Uncertainty Analysis

### Summary Statistics

The `get_summary()` function provides comprehensive summary statistics from length composition results, including mean weighted coefficient of variation (CV), number of fish sampled by sex category, and total number of hauls sampled.

#### Function Signature

```r
get_summary(
  x,                    # length_composition object
  by_stratum = FALSE,   # Use pooled (FALSE) or by-stratum (TRUE) data
  type = "composition", # "composition" or "proportion"
  sex = c("male", "female", "total"), # Sex categories to include
  stratum = NULL,       # Specific stratum for by-stratum analysis
  length_range = NULL   # Length range c(min, max) to include
)
```

#### Return Structure

The function returns a list containing:

- **`mean_weighted_cv`**: Mean weighted coefficient of variation
- **`n_fish`**: Number of fish sampled by sex category (for pooled data)
- **`total_fish`**: Total number of fish sampled
- **`n_hauls`**: Total number of hauls/samples
- **`fish_haul_summary`**: Detailed breakdown by stratum (for by-stratum data)

#### Basic Summary Usage

```r
# Calculate length compositions with bootstrap uncertainty
result <- calculate_length_compositions(
  fish_data = test_data$fish_data,
  strata_data = test_data$strata_data,
  bootstraps = 300
)

# Basic summary statistics (pooled across strata)
summary_stats <- get_summary(result)
print(summary_stats$mean_weighted_cv)  # Overall uncertainty
print(summary_stats$total_fish)        # Total fish count
print(summary_stats$n_hauls)           # Number of hauls

# Summary by sex category
summary_by_sex <- get_summary(result, sex = c("male", "female", "total"))
print(summary_by_sex$n_fish)  # Fish counts by sex

# Summary for proportions instead of absolute compositions
summary_proportions <- get_summary(result, type = "proportion")

# Summary by stratum
summary_by_stratum <- get_summary(result, by_stratum = TRUE)
print(summary_by_stratum$fish_haul_summary)  # Detailed stratum breakdown

# Summary for specific length range
summary_length_range <- get_summary(result, length_range = c(30, 50))

# Summary for specific stratum and sex
summary_specific <- get_summary(
  result, 
  by_stratum = TRUE, 
  stratum = "North", 
  sex = "total"
)
```

#### CV Interpretation

The weighted CV is calculated as:

$$
CV_{weighted} = \frac{\sum_{i} w_i \times CV_i}{\sum_{i} w_i}
$$

where $w_i$ are the composition values (weights) and $CV_i$ are the coefficient of variation values at each length bin.

**Typical CV ranges:**

- **CV < 0.20**: Low uncertainty, high precision
- **CV 0.20-0.40**: Moderate uncertainty
- **CV > 0.40**: High uncertainty, results should be interpreted with caution

#### Advanced Summary Analysis

```r
# Compare uncertainty and sample sizes across different scenarios
commercial_summary <- get_summary(commercial_result)
survey_summary <- get_summary(survey_result)

print("Commercial fisheries:")
print(paste("CV:", round(commercial_summary$mean_weighted_cv, 3)))
print(paste("Fish:", commercial_summary$total_fish))
print(paste("Hauls:", commercial_summary$n_hauls))

print("Research survey:")
print(paste("CV:", round(survey_summary$mean_weighted_cv, 3)))
print(paste("Fish:", survey_summary$total_fish))
print(paste("Hauls:", survey_summary$n_hauls))

# Stratum-specific uncertainty and sample size assessment
stratum_summary <- get_summary(result, by_stratum = TRUE)
for (stratum in names(stratum_summary$fish_haul_summary)) {
  cv_val <- stratum_summary$mean_weighted_cv[stratum, "total"]
  fish_count <- stratum_summary$fish_haul_summary[[stratum]]$total_fish
  haul_count <- stratum_summary$fish_haul_summary[[stratum]]$n_hauls
  
  print(paste(stratum, "- CV:", round(cv_val, 3), 
              "Fish:", fish_count, "Hauls:", haul_count))
}

# Length-specific uncertainty patterns with sample information
early_summary <- get_summary(result, length_range = c(25, 40))
late_summary <- get_summary(result, length_range = c(40, 60))

print(paste("Early lengths - CV:", round(early_summary$mean_weighted_cv, 3),
            "Fish:", early_summary$total_fish))
print(paste("Late lengths - CV:", round(late_summary$mean_weighted_cv, 3),
            "Fish:", late_summary$total_fish))
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

### Sample Size Distribution Visualisation

The `plot_sample_size_distribution()` function creates stacked histograms showing the distribution of sample sizes by strata, with samples categorised by adequacy for bootstrap analysis. This visualisation helps identify potential issues with bootstrap uncertainty estimation before running analyses.

#### Basic Usage

```r
# Generate test data
test_data <- generate_test_data()

# Evaluate sample sizes for bootstrap adequacy
evaluation <- evaluate_sample_sizes(test_data$fish_data)

# Create sample size distribution plot
plot_sample_size_distribution(evaluation)

# Customized plot with smaller bins and specific axis range
plot_sample_size_distribution(
  evaluation,
  bin_width = 3,                    # 3 fish per bin
  max_fish = 100,                   # Show up to 100 fish
  show_thresholds = TRUE            # Show minimum size threshold lines
)
```

#### Sample Categories

The plot uses colour coding to show different sample categories:

- **Non-representative Small** (red): Small samples with poor representativeness (< 20% of total catch)
- **Representative Small** (amber): Small samples that are highly representative (≥ 20% of total catch)
- **Adequate** (green): Samples meeting minimum fish count thresholds (≥ 15 fish by default)

When sample weight data is not available, all small samples are grouped as "Small Samples" since representativeness cannot be assessed.

#### Integration with Sample Size Evaluation

```r
# Comprehensive sample size workflow
test_data <- generate_test_data()

# Step 1: Evaluate sample adequacy
evaluation <- evaluate_sample_sizes(test_data$fish_data)

# Step 2: Review recommendations
cat("Sample Size Recommendations:\n")
for (rec in evaluation$recommendations) {
  cat("-", rec, "\n")
}

# Step 3: Visualise sample distribution
p <- plot_sample_size_distribution(evaluation)

# Step 4: Filter problematic samples if needed
if (nrow(evaluation$non_representative_small_samples) > 0) {
  filtered_data <- filter_small_samples(evaluation)
  cat("Filtered data contains", nrow(filtered_data), "samples\n")
}

# Step 5: Save visualisation
ggplot2::ggsave("sample_size_distribution.png", p, width = 10, height = 6)
```

This workflow helps ensure robust bootstrap uncertainty estimation by:

- Identifying samples with inadequate fish counts
- Assessing representativeness of small samples when weight data is available
- Providing visual confirmation of sample size patterns across strata
- Supporting informed decisions about sample filtering

## Function Parameters

### calculate_length_compositions()

- **`fish_data`**: Data frame with fish sampling data including columns: stratum, sample_id, length, male, female, unsexed, plus either sample_weight_kg + total_catch_weight_kg (weight-based) or sample_area_km2 + catch_density_kg_km2 (density-based)
- **`strata_data`**: Data frame with columns: stratum, plus either stratum_total_catch_kg (weight-based) or stratum_area_km2 (density-based)
- **`length_range`**: Numeric vector of min and max lengths to include (e.g., c(15, 35))
- **`lw_params_male`**: Named vector with length-weight parameters for males: c(a = 0.01, b = 3.0) (required)
- **`lw_params_female`**: Named vector with length-weight parameters for females: c(a = 0.01, b = 3.0) (required)
- **`lw_params_unsexed`**: Named vector with length-weight parameters for unsexed fish: c(a = 0.01, b = 3.0) (required)
- **`bootstraps`**: Integer, number of bootstrap iterations. Set to 0 for no bootstrapping (default: 300)
- **`plus_group`**: Logical, combine lengths >= max length into a plus group (default: FALSE)
- **`minus_group`**: Logical, combine lengths <= min length into a minus group (default: FALSE)
- **`return_full_bootstraps`**: Logical, whether to return all individual bootstrap results along with summaries (default: FALSE)
- **`verbose`**: Logical, whether to print progress messages (default: TRUE)

### calculate_age_compositions()

- **`x`**: A length_composition object from calculate_length_compositions()
- **`age_length_key`**: Either a single data frame or a named list of sex-specific data frames containing age-length key data
- **`age_range`**: Numeric vector of min and max ages to include (e.g., c(1, 10))
- **`plus_group_age`**: Logical, combine ages >= max age into a plus group (default: TRUE)
- **`minus_group_age`**: Logical, combine ages <= min age into a minus group (default: FALSE)
- **`verbose`**: Logical, whether to print progress messages (default: TRUE)

### create_alk()

- **`age_data`**: Data frame containing age-length data with required columns: 'age', 'length', and optionally 'sex'
- **`lengths`**: Vector of all length bins that will be encountered (required)
- **`ages`**: Vector of all age bins (required)
- **`tail_ages`**: Named list specifying length-age pairs for lengths at tails (optional)
- **`min_ages_per_length`**: Minimum number of ages required per length bin (default: 3)
- **`optimum_ages_per_length`**: Optimum number of ages per length bin (default: 10)
- **`length_bin_size`**: Numeric value for binning lengths (e.g., 2 for 2cm bins). If NULL (default), uses raw length values
- **`interpolate`**: Logical, whether to allow linear interpolation for missing length bins (default: TRUE)
- **`extrapolate`**: Logical, whether to allow extrapolation for lengths outside the observed range (default: TRUE)
- **`verbose`**: Logical, whether to print progress messages

### plot_alk()

- **`alk`**: Age-length key data frame
- **`by_sex`**: Logical, whether to plot by sex (default: TRUE)
- **`type`**: Character, type of plot: "heatmap" (heatmap visualisation) or "points" (point plot) (default: "heatmap")
- **`rug`**: Logical, whether to add a rug on the y-axis showing length distribution (default: FALSE)

### generate_test_data()

- **`data_type`**: Character, either "commercial" for weight-based data or "survey" for density-based data

### calculate_multinomial_n()

- **`x`**: A length_composition object from calculate_length_compositions() or an age_composition object from calculate_age_compositions()
- **`stratum`**: Character, name of stratum to analyse. If NULL (default), uses pooled data across all strata
- **`sex`**: Character, sex category to analyse: "male", "female", "unsexed", or "total" (default)
- **`all`**: Logical, whether to calculate for all combinations of strata and sex categories (default: FALSE)
- **`sex_categories`**: Character vector of sex categories to analyse when all = TRUE (default: c("male", "female", "unsexed", "total"))
- **`include_pooled`**: Logical, whether to include pooled results when all = TRUE (default: TRUE)
- **`remove_outliers`**: Numeric, proportion of outliers to remove (0-1, default: 0.05)
- **`min_proportion`**: Numeric, minimum proportion threshold to include in analysis (default: 0.0001)
- **`max_cv`**: Numeric, maximum CV threshold to include in analysis (default: 5.0)
- **`trace`**: Logical, whether to show fitting details (default: FALSE)
- **`quiet`**: Logical, whether to suppress individual fitting messages when all = TRUE (default: TRUE)

### get_default_lw_params()

- **No parameters**: Returns default length-weight parameters for Ross Sea toothfish

### resample_fish_data()

- **`fish_data`**: Data frame with fish observation data including columns: stratum, sample_id, length, male, female, unsexed, total

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

### Bootstrap Sample Size Evaluation

Bootstrap uncertainty estimation requires adequate sample sizes at both the sample and stratum levels. Use `evaluate_sample_sizes()` to check whether your data meets the recommended thresholds and assess sample representativeness:

```r
# Evaluate sample sizes with default thresholds
sample_eval <- evaluate_sample_sizes(fish_data)

# Check overall data adequacy
cat("Sample size evaluation:\n")
cat("Total samples:", nrow(sample_eval$sample_sizes), "\n")
cat("Samples with < 15 fish:", nrow(sample_eval$all_small_samples), "\n")
cat("Strata with < 8 samples:", nrow(sample_eval$small_strata), "\n")

# Review representativeness analysis (if weight data available)
if (nrow(sample_eval$representative_small_samples) > 0) {
  cat("\nHighly representative small samples (≥20% of catch - RETAIN):\n")
  print(sample_eval$representative_small_samples[c("sample_id", "total", "sample_proportion")])
}

if (nrow(sample_eval$non_representative_small_samples) > 0) {
  cat("\nProblematic small samples (<20% of catch - consider exclusion):\n")
  print(sample_eval$non_representative_small_samples[c("sample_id", "total", "sample_proportion")])
}

# Display problematic samples if any
if (nrow(sample_eval$all_small_samples) > 0) {
  cat("\nSamples with insufficient fish (< 15):\n")
  print(sample_eval$all_small_samples[order(sample_eval$all_small_samples$total), ])
}

# Display problematic strata if any
if (nrow(sample_eval$small_strata) > 0) {
  cat("\nStrata with insufficient samples (< 8):\n")
  print(sample_eval$small_strata)
}

# Review specific recommendations
cat("\nRecommendations for handling small samples:\n")
for(rec in sample_eval$recommendations) {
  cat("-", rec, "\n")
}

# Check exclusion impact summary
cat("\nImpact of excluding small samples:\n")
print(sample_eval$exclusion_summary)

# Example: Smart filtering that uses specific sample list
filtered_data <- filter_small_samples(
  fish_data,
  sample_eval$non_representative_small_samples  # Only exclude problematic samples
)

cat("\nSmart filtering impact (excludes only problematic samples):\n")
print(filtered_data$filter_summary)

# Alternative: Exclude all small samples if preferred
filtered_data_all <- filter_small_samples(
  fish_data,
  sample_eval$all_small_samples  # Exclude all small samples
)

cat("\nFull filtering impact (excludes all small samples):\n")
print(filtered_data_all$filter_summary)
  
  # Compare bootstrap results with and without small samples
  cat("\nSensitivity analysis - comparing results:\n")
  
  # Original data results
  results_original <- calculate_length_compositions(
    fish_data = fish_data,
    strata_data = strata_data,
    bootstraps = 100
  )
  
  # Filtered data results  
  results_filtered <- calculate_length_compositions(
    fish_data = filtered_data$filtered_fish_data,
    strata_data = strata_data,
    bootstraps = 100
  )
  
  # Compare mean CVs
  cv_original <- mean(results_original$pooled_lf_cv, na.rm = TRUE)
  cv_filtered <- mean(results_filtered$pooled_lf_cv, na.rm = TRUE)
  
  cat("Mean CV - Original data:", round(cv_original, 3), "\n")
  cat("Mean CV - Filtered data:", round(cv_filtered, 3), "\n")
  cat("CV improvement:", round((cv_original - cv_filtered) / cv_original * 100, 1), "%\n")
}

# Use stricter thresholds for critical analyses
strict_eval <- evaluate_sample_sizes(
  fish_data,
  min_fish_per_sample = 20,      # Stricter fish threshold
  min_samples_per_stratum = 10   # Stricter sample threshold
)

# Compare sample size distributions
hist(sample_eval$sample_sizes$total, 
     main = "Distribution of Fish per Sample",
     xlab = "Number of Fish", 
     breaks = 20)
abline(v = 15, col = "red", lty = 2, lwd = 2)  # Minimum threshold
text(15, par("usr")[4] * 0.9, "Min threshold", pos = 4, col = "red")
```

**Sample Representativeness Assessment:**

When sample weight and total catch weight data are available, the function evaluates whether small samples represent a significant proportion of the total catch:

- **Highly Representative** (≥20% of total catch): Small samples that are retained despite low fish counts because they provide adequate coverage of the catch
- **Poorly Representative** (<20% of total catch): Small samples that may be excluded because they provide limited information relative to the total catch
- **Smart Filtering**: The `filter_small_samples()` function can automatically preserve representative samples while excluding problematic ones

**Key Insight**: A sample with only 10 fish might be perfectly adequate if it represents 50% of the total catch, while a sample with 12 fish representing only 2% of the catch may be problematic.

**Sample Size Guidelines for Bootstrap Analysis:**

- **Minimum fish per sample**: 15 fish (default) provides stable within-sample resampling
- **Minimum samples per stratum**: 8 samples (default) provides stable between-sample resampling
- 

### fit_ordinal_alk()

- **`alk_data`**: Data frame (or named list of sex-specific frames) with columns: `age`, `length`, and optionally `sex` (one row per aged fish)
- **`by_sex`**: Logical, whether to fit sex-specific smooths (default: TRUE)
- **`k`**: Basis dimension for smooth terms; -1 uses mgcv’s automatic selection (default: -1)
- **`method`**: Smoothing parameter estimation method for mgcv (default: "REML")
- **`weights`**: Optional observation weights (default: NULL)
- **`verbose`**: Logical, print model details (default: TRUE)

Returns a list with:

- `model`: fitted mgcv::gam object (ocat family)
- `predict_function(lengths, sex)`: function returning a matrix of per-age probabilities
- `summary`: key model metrics
- `age_levels`, `sex_levels`, `by_sex`
- **Recommended fish per sample**: 20+ fish for critical stock assessment applications
- **Recommended samples per stratum**: 10+ samples for robust uncertainty estimation

**Consequences of Insufficient Sample Sizes:**

- **Small samples** (< 15 fish): Unreliable within-sample bootstrap distributions
- **Few strata samples** (< 8): Unstable between-sample bootstrap distributions
- **Both issues**: Biased variance estimates and unreliable confidence intervals

**Data Quality Recommendations:**

- Run `evaluate_sample_sizes()` before any bootstrap analysis
- Review specific recommendations provided by the evaluation function
- Consider filtering only extremely small samples (< 5 fish) that represent minimal data loss
- Always perform sensitivity analysis comparing results with and without small samples
- Prefer combining adjacent strata over excluding samples when possible
- Consider combining adjacent strata if sample sizes are inadequate
- Use fewer bootstrap iterations (50-100) for exploratory analysis with marginal sample sizes
- Increase to 300+ bootstrap iterations only when sample sizes are adequate

**Decision Framework for Small Sample Handling:**

1. **Assess impact**: Check what percentage of data would be lost
2. **Review coverage**: Ensure filtering doesn't create spatial/temporal gaps
3. **Test sensitivity**: Compare results with and without filtering
4. **Document decisions**: Record rationale for inclusion/exclusion choices
5. **Consider alternatives**: Stratum combination, weighted analysis, or conditional bootstrapping

```

### Multinomial Effective Sample Size Analysis

```r
# Calculate effective sample sizes for all combinations
all_n <- calculate_all_multinomial_n(results)
print(all_n)

# Calculate for specific stratum and sex
eff_n_male <- calculate_multinomial_n(results, stratum = "North", sex = "male")
cat("Effective sample size for North stratum males:", eff_n_male$effective_n, "\n")

# Calculate for pooled data (recommended for stock assessment)
eff_n_total <- calculate_multinomial_n(results, sex = "total")
cat("Pooled effective sample size:", eff_n_total$effective_n, "\n")
cat("Based on", eff_n_total$n_bins, "length bins\n")
cat("Model fit quality (residual SE):", round(eff_n_total$fit_summary$sigma, 4), "\n")

# Use stricter filtering for robust estimates
robust_n <- calculate_all_multinomial_n(
  results, 
  min_proportion = 0.001,  # Exclude very small proportions
  max_cv = 3.0,           # Exclude high-CV length bins
  remove_outliers = 0.1   # Remove 10% of worst-fitting points
)
print("Robust effective sample sizes:")
print(robust_n)
```

### Age Composition Analysis

```r
# Create age-length key using von Bertalanffy 
# Note, use this functions only for testing and simulation purposes. This should 
# not be used to generate an age-length-key that represents biological reality. 
age_key <- generate_age_length_key(
  length_range = c(20, 40),
  age_range = c(1, 8),
  growth_type = "vonbert",
  growth_params = list(linf = 50, k = 0.3, t0 = -0.5),
  cv = 0.25
)

# Visualise the age-length key
plot_alk(age_key)

# Convert length compositions to age compositions using complete age-length key
age_results <- calculate_age_compositions(
  x = results,
  age_length_key = age_key,
  age_range = c(1, 8)
)

# View age composition results
print(age_results)

# Plot age compositions with consistent interface (same as length compositions!)
plot(age_results, by_stratum = FALSE, show_CIs = TRUE)  # Pooled across strata
plot(age_results, by_stratum = TRUE, show_CIs = TRUE)   # Faceted by stratum
plot(age_results, stratum = "North", show_CIs = TRUE)   # Specific stratum only

# Plot age proportions instead of absolute compositions
plot(age_results, type = "proportion", show_CIs = TRUE)

# Use age binning to aggregate into larger age groups
plot(age_results, age_bin_size = 2, show_CIs = TRUE)  # 2-year age bins

# Include unsexed fish in age composition plots
plot(age_results, unsexed = TRUE, show_CIs = TRUE)

# Calculate effective sample sizes for age compositions
age_eff_n <- calculate_multinomial_n(age_results, sex = "total")
cat("Age composition effective sample size:", age_eff_n$effective_n, "\n")

# Compare length vs age effective sample sizes
length_eff_n <- calculate_multinomial_n(results, sex = "total")
cat("\nComparison of effective sample sizes:\n")
cat("Length compositions:", length_eff_n$effective_n, "based on", length_eff_n$n_bins, "length bins\n")
cat("Age compositions:", age_eff_n$effective_n, "based on", age_eff_n$n_bins, "age bins\n")

# Calculate effective sample sizes for all combinations (both length and age)
all_length_n <- calculate_all_multinomial_n(results)
all_age_n <- calculate_all_multinomial_n(age_results)
cat("\nEffective sample sizes by stratum and sex:\n")
print("Length compositions:")
print(all_length_n)
print("Age compositions:")
print(all_age_n)
```

#### Handling Incomplete Age-Length Keys with create_alk()

In real-world applications, age-length keys often don't cover the full length range of the composition data. The `create_alk()` function provides a streamlined solution:

```r
# Create a limited age-length key (30-35 cm range)
limited_age_key <- generate_age_length_key(
  length_range = c(30, 35),  # Limited range
  age_range = c(3, 6)
)

# Length compositions might span 20-40 cm, creating gaps
print("Length composition range vs age-length key coverage:")
cat("Length compositions: 20-40 cm\n")
cat("Age-length key: 30-35 cm\n")
cat("Gaps: 20-29 cm and 36-40 cm need interpolation\n")

# Step 1: Create complete age-length key from raw age data with interpolation
complete_age_key <- create_alk(
  age_data = age_sample_data,    # Raw age data (one row per aged fish)
  lengths = 20:40,               # Full length range in composition data
  ages = 1:8,                    # Full age range desired
  verbose = TRUE                 # Show interpolation details
)

# Step 2: Use complete key for age composition calculation
age_results_complete <- calculate_age_compositions(
  x = results,
  age_length_key = complete_age_key,  # Complete key automatically detected
  age_range = c(1, 8)
)

# Alternative: Specify tail ages for better biological realism
tail_ages <- list(
  min_lengths = data.frame(
    length = c(20, 22, 25),  # Specific lengths below key range  
    age = c(1, 1, 2)         # Corresponding ages
  ),
  max_lengths = data.frame(
    length = c(38, 40),      # Specific lengths above key range
    age = c(7, 8)            # Corresponding ages
  )
)

complete_age_key_tails <- create_alk(
  age_data = age_sample_data,
  lengths = 20:40,
  ages = 1:8,
  tail_ages = tail_ages,
  verbose = TRUE
)

age_results_tails <- calculate_age_compositions(
  x = results,
  age_length_key = complete_age_key_tails,
  age_range = c(1, 8)
)

# For comparison: Using incomplete key directly (will show warnings)
cat("\nTrying incomplete key (will show warnings):\n")
age_results_incomplete <- calculate_age_compositions(
  x = results,
  age_length_key = limited_age_key,  # Incomplete key
  age_range = c(1, 8)
)

cat("\nComparison of approaches:\n")
cat("Complete key (create_alk): 100% of fish allocated to ages\n")
cat("Incomplete key: Only fish with exact length matches allocated\n")
cat("Missing lengths generate explicit warnings\n")
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

1. **Length-Weight Relationships**: The examples uses placeholder weight calculations. In practice, incorporate species-specific length-weight relationships.
2. **Sample Size Considerations**: Ensure adequate sample sizes within each stratum. Small samples can lead to unstable bootstrap estimates.
3. **Stratification**: Choose strata that minimize within-stratum variability while maintaining adequate sample sizes.
4. **Bootstrap Iterations**: Use 300+ iterations for final analyses. The examples use fewer for speed.
5. **Data Quality**: Validate input data for consistency in units, completeness, and biological plausibility.
6. **Survey Design**: For density-based scaling, ensure that sampling areas are representative of the strata and that density estimates are reliable.

## Package Information

**Version**: 2025-07 (automatically updated from git commit date)
**Author**: Alistair Dunn
**Maintainer**: Alistair Dunn <alistair.dunn@OceanEnvironmental.co.nz>
**License**: GPL (>= 3)
**Encoding**: UTF-8

### Versioning Scheme

The package uses a **year-month (YYYY-MM)** versioning scheme:

- Version number is automatically generated from the git commit date
- Format: `YYYY-MM` (e.g., `2025-07` for July 2025)
- Package date is set to the exact commit date (`YYYY-MM-DD`)
- This ensures version numbers are meaningful and chronologically ordered

## References

Coggins, L.G.; Gwinn, D.C.; Allen, M.S. (2013). Evaluation of Age–Length Key Sample Sizes Required to Estimate Fish Total Mortality and Growth. *Transactions of the American Fisheries Society*, 142(3), 832–840. https://doi.org/10.1080/00028487.2013.768550

## Citation

To cite the `scala` package in publications, please use:

```r
# Get citation information
citation("scala")
```

**Suggested citation:**

Dunn, A. (2025). scala: Scaled catch at length and age composition analyses. R package version 2025-07. https://github.com/alistairdunn1/scala

## Contributing

This package is under active development. For bug reports, feature requests, or contributions:

1. Check existing issues on the GitHub repository
2. Submit bug reports with reproducible examples
3. Suggest improvements or additional features
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

This script automatically:

1. **Updates version and date** from git commit information
2. **Generates documentation** with roxygen2
3. **Builds the package** with updated version number
4. **Installs the package** locally
5. **Runs R CMD check** for validation

**Manual Version Update:**
To update version without full build:

```batch
update-version.bat
```

**View Current Version:**

```r
# In R console
source('show-version.R')
```

The build process ensures the package version always reflects the actual development state from git.

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
