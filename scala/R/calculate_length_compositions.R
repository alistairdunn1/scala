#' Calculate Length Compositions with Optional Bootstrap Uncertainty (Sex-Based)
#'
#' Calculates length compositions for fish data by sex and stratum, applying upweighting and optionally bootstrap resampling to estimate uncertainty.
#' Supports both weight-based scaling (commercial fisheries) and density-based scaling (surveys).
#'
#' @param fish_data Data frame with columns: stratum, sample_id, length, male, female, unsexed, sample_weight_kg, total_catch_weight_kg (weight-based) OR stratum, sample_id, length, male, female, unsexed, sample_area_km2, catch_density_kg_km2 (density-based)
#' @param strata_data Data frame with columns: stratum, stratum_total_catch_kg (weight-based) OR stratum, stratum_area_km2 (density-based)
#' @param length_range Numeric vector of min and max lengths to include (e.g., c(15, 35))
#' @param lw_params_male Named vector with length-weight parameters for males: c(a = 0.01, b = 3.0)
#' @param lw_params_female Named vector with length-weight parameters for females: c(a = 0.01, b = 3.0)
#' @param lw_params_unsexed Named vector with length-weight parameters for unsexed fish: c(a = 0.01, b = 3.0)
#' @param bootstraps Integer, number of bootstrap iterations. Set to 0 for no bootstrapping (default 300)
#' @param plus_group Logical, combine lengths >= max length into a plus group (default FALSE)
#' @param minus_group Logical, combine lengths <= min length into a minus group (default FALSE)
#' @param return_full_bootstraps Logical, whether to return all individual bootstrap results along with summaries (default FALSE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @importFrom stats sd
#' @importFrom dplyr group_by summarise ungroup mutate left_join bind_rows
#' @return When bootstraps > 0: List containing:
#'   \itemize{
#'     \item length_composition: 3D array (length x sex x stratum) of scaled length compositions
#'     \item proportions: 3D array (length x sex x stratum) of proportions
#'     \item pooled_length_composition: matrix (length x sex) of pooled compositions
#'     \item pooled_proportions: matrix (length x sex) of pooled proportions
#'     \item lc_cvs: 3D array of CVs for length compositions
#'     \item proportions_cvs: 3D array of CVs for proportions
#'     \item pooled_lc_cv: matrix of pooled CVs
#'     \item pooled_proportions_cv: matrix of pooled proportion CVs
#'     \item lc_ci_lower: 3D array of 2.5th percentile values for length compositions
#'     \item lc_ci_upper: 3D array of 97.5th percentile values for length compositions
#'     \item pooled_lc_ci_lower: matrix of 2.5th percentile values for pooled compositions
#'     \item pooled_lc_ci_upper: matrix of 97.5th percentile values for pooled compositions
#'     \item proportions_ci_lower: 3D array of 2.5th percentile values for proportions
#'     \item proportions_ci_upper: 3D array of 97.5th percentile values for proportions
#'     \item pooled_proportions_ci_lower: matrix of 2.5th percentile values for pooled proportions
#'     \item pooled_proportions_ci_upper: matrix of 97.5th percentile values for pooled proportions
#'     \item lc_bootstraps: 4D array of bootstrap results (length x sex x stratum x bootstrap)
#'     \item full_lc_bootstraps: 4D array of all individual bootstrap length compositions (only when return_full_bootstraps = TRUE)
#'     \item full_pooled_lc_bootstraps: 3D array of all individual pooled bootstrap compositions (only when return_full_bootstraps = TRUE)
#'     \item full_prop_bootstraps: 4D array of all individual bootstrap proportions (only when return_full_bootstraps = TRUE)
#'     \item full_pooled_prop_bootstraps: 3D array of all individual pooled bootstrap proportions (only when return_full_bootstraps = TRUE)
#'     \item lengths: vector of length bins
#'     \item strata_names: vector of stratum names
#'     \item n_bootstraps: number of bootstrap iterations
#'     \item plus_group: logical, plus group used
#'     \item minus_group: logical, minus group used
#'     \item has_sex_data: logical, TRUE if sex-based columns present
#'     \item scaling_type: character, type of scaling used ("weight" or "density")
#'     \item summary_stats: list containing pre-computed summary statistics with components:
#'       \itemize{
#'         \item total_summary: list with n_fish (named vector of fish counts by sex) and n_hauls (total number of hauls)
#'         \item stratum_summary: named list by stratum, each containing n_fish and n_hauls for that stratum
#'       }
#'   }
#'   When bootstraps = 0: List containing:
#'   \itemize{
#'     \item \code{length_compositions}: 3D array (length x sex x stratum) of scaled length compositions
#'     \item \code{lengths}: Vector of length bin centers
#'     \item \code{strata_names}: Vector of stratum names
#'     \item \code{n_bootstraps}: 0 (no bootstraps performed)
#'     \item \code{plus_group}: Plus group setting
#'     \item \code{minus_group}: Minus group setting
#'     \item \code{has_sex_data}: TRUE (indicates sex-specific analysis)
#'     \item \code{scaling_type}: Type of scaling applied
#'   }
#'
#' @details
#' The function applies a two-stage upweighting process and uses bootstrap resampling to estimate uncertainty in length composition and proportion estimates.
#'
#' **Weight-based scaling (commercial fisheries):**
#' - Sample scaling: total_catch_weight_kg / observed_sample_weight
#' - Stratum scaling: stratum_total_catch_kg / sum_of_sample_catches_in_stratum
#'
#' **Density-based scaling (surveys):**
#' - Sample scaling: catch_density_kg_km2 * sample_area_km2 / observed_sample_weight
#' - Stratum scaling: stratum_area_km2 / sum_of_sample_areas_in_stratum
#'
#' **Length-weight relationships:**
#' Uses the allometric relationship: Weight = a * Length^b
#' where a and b are species and sex-specific parameters.
#'
#' **Bootstrap uncertainty estimation:**
#' When bootstraps > 0, the function provides multiple uncertainty measures using a hierarchical resampling approach:
#' - **Sample-level resampling**: Samples are resampled with replacement within each stratum to capture spatial/temporal variation
#' - **Fish-level resampling**: Individual fish are resampled with replacement within each sample to capture within-sample variation
#' - **Coefficient of Variation (CV)**: Relative variability as standard deviation / mean across bootstrap iterations
#' - **Empirical 95 percent confidence intervals**: 2.5th and 97.5th percentiles of bootstrap distribution
#'
#' This hierarchical approach captures both between-sample and within-sample uncertainty, providing more comprehensive
#' and realistic uncertainty estimates than single-level resampling. The confidence intervals are non-parametric
#' and do not assume any distributional form.
#'
#' **Data validation:**
#' The function validates that all strata present in fish_data are also present in strata_data.
#' This ensures that stratum-level scaling factors can be calculated for all fish observations.
#' If strata exist in strata_data but not in fish_data, a warning is issued to alert users
#' of potentially unused stratum information.
#'
#' Sex categories are: male, female, unsexed, and total. Plus and minus group functionality is available for aggregating extreme length bins.
#'
#' @examples
#' \dontrun{
#' # Length-weight parameters (example for snapper)
#' lw_male <- c(a = 0.0085, b = 3.1)
#' lw_female <- c(a = 0.0092, b = 3.05)
#' lw_unsexed <- c(a = 0.0088, b = 3.08)
#'
#' # Weight-based example (commercial fisheries)
#' fish_data <- data.frame(
#'   stratum = c("A", "A", "B", "B"),
#'   sample_id = c(1, 1, 2, 2),
#'   length = c(20, 25, 22, 28),
#'   male = c(2, 1, 3, 2),
#'   female = c(2, 1, 2, 1),
#'   unsexed = c(1, 1, 2, 1),
#'   sample_weight_kg = c(10, 10, 15, 15),
#'   total_catch_weight_kg = c(100, 100, 150, 150)
#' )
#' strata_data <- data.frame(
#'   stratum = c("A", "B"),
#'   stratum_total_catch_kg = c(1000, 2000)
#' )
#'
#' results <- calculate_length_compositions(
#'   fish_data = fish_data,
#'   strata_data = strata_data,
#'   length_range = c(15, 35),
#'   lw_params_male = lw_male,
#'   lw_params_female = lw_female,
#'   lw_params_unsexed = lw_unsexed,
#'   bootstraps = 100
#' )
#' print(results)
#'
#' # For age composition analysis, get full bootstrap results
#' results_full <- calculate_length_compositions(
#'   fish_data = fish_data,
#'   strata_data = strata_data,
#'   length_range = c(15, 35),
#'   lw_params_male = lw_male,
#'   lw_params_female = lw_female,
#'   lw_params_unsexed = lw_unsexed,
#'   bootstraps = 100,
#'   return_full_bootstraps = TRUE
#' )
#' # Access individual bootstrap iteration results
#' bootstrap_1 <- results_full$full_lc_bootstraps[, , , 1]
#' }
#'
#' @seealso
#' \code{\link{plot.length_composition}} for plotting results,
#' \code{\link{print.length_composition}} for printing summaries,
#' \code{\link{get_default_lw_params}} for default length-weight parameters,
#' \code{\link{generate_test_data}} for creating test datasets
#'
#' @export
calculate_length_compositions <- function(fish_data,
                                          strata_data,
                                          length_range = c(min(fish_data$length), max(fish_data$length)),
                                          lw_params_male,
                                          lw_params_female,
                                          lw_params_unsexed,
                                          bootstraps = 0,
                                          plus_group = FALSE,
                                          minus_group = FALSE,
                                          return_full_bootstraps = FALSE,
                                          verbose = TRUE) {
  # Validate length-weight parameters
  if (missing(lw_params_male) || missing(lw_params_female) || missing(lw_params_unsexed)) {
    stop("Length-weight parameters are required. Please provide lw_params_male, lw_params_female, and lw_params_unsexed.")
  }

  # Validate length-weight parameter structure
  validate_lw_params <- function(params, name) {
    if (!is.numeric(params) || length(params) != 2 || is.null(names(params)) ||
      !all(c("a", "b") %in% names(params))) {
      stop(paste0(name, " must be a named numeric vector with elements 'a' and 'b'. Example: c(a = 0.01, b = 3.0)"))
    }
    if (params["a"] <= 0 || params["b"] < 0) {
      stop(paste0(name, " parameter 'a' must be positive and 'b' must be non-negative."))
    }
  }

  validate_lw_params(lw_params_male, "lw_params_male")
  validate_lw_params(lw_params_female, "lw_params_female")
  validate_lw_params(lw_params_unsexed, "lw_params_unsexed")
  # Detect data type (weight-based vs density-based)
  weight_based_fish_cols <- c("sample_weight_kg", "total_catch_weight_kg")
  density_based_fish_cols <- c("sample_area_km2", "catch_density_kg_km2")
  weight_based_strata_cols <- c("stratum_total_catch_kg")
  density_based_strata_cols <- c("stratum_area_km2")

  has_weight_data <- all(weight_based_fish_cols %in% names(fish_data)) &&
    all(weight_based_strata_cols %in% names(strata_data))
  has_density_data <- all(density_based_fish_cols %in% names(fish_data)) &&
    all(density_based_strata_cols %in% names(strata_data))

  if (!has_weight_data && !has_density_data) {
    stop(
      "Data must contain either:\n",
      "1. Weight-based columns: fish_data (", paste(weight_based_fish_cols, collapse = ", "),
      ") and strata_data (", paste(weight_based_strata_cols, collapse = ", "), ")\n",
      "2. Density-based columns: fish_data (", paste(density_based_fish_cols, collapse = ", "),
      ") and strata_data (", paste(density_based_strata_cols, collapse = ", "), ")"
    )
  }

  if (has_weight_data && has_density_data) {
    stop("Data contains both weight-based and density-based columns. Please use only one approach.")
  }

  # Determine scaling type
  scaling_type <- if (has_weight_data) "weight" else "density"

  # Validate common required columns
  required_common_fish_cols <- c("stratum", "sample_id", "length", "male", "female", "unsexed")
  required_common_strata_cols <- c("stratum")

  if (!all(required_common_fish_cols %in% names(fish_data))) {
    stop(
      "fish_data missing required columns: ",
      paste(setdiff(required_common_fish_cols, names(fish_data)), collapse = ", ")
    )
  }

  if (!all(required_common_strata_cols %in% names(strata_data))) {
    stop(
      "strata_data missing required columns: ",
      paste(setdiff(required_common_strata_cols, names(strata_data)), collapse = ", ")
    )
  }

  # Validate that all strata in fish_data are present in strata_data
  fish_strata <- unique(fish_data$stratum)
  strata_data_strata <- unique(strata_data$stratum)
  missing_strata <- setdiff(fish_strata, strata_data_strata)

  if (length(missing_strata) > 0) {
    stop(
      "The following strata are present in fish_data but missing from strata_data: ",
      paste(missing_strata, collapse = ", "), "\n",
      "All strata in fish_data must have corresponding entries in strata_data."
    )
  }

  # Check for strata in strata_data that are not present in fish_data
  unused_strata <- setdiff(strata_data_strata, fish_strata)
  if (length(unused_strata) > 0) {
    warning(
      "The following strata are present in strata_data but not in fish_data: ",
      paste(unused_strata, collapse = ", "), "\n",
      "These strata will not be used in the calculations."
    )
  }

  # Calculate total column if not present
  if (!"total" %in% names(fish_data)) {
    fish_data$total <- fish_data$male + fish_data$female + fish_data$unsexed
  }

  # Set up length bins
  lengths <- length_range[1]:length_range[2]
  n_lengths <- length(lengths)
  strata_names <- unique(fish_data$stratum)
  n_strata <- length(strata_names)

  # Calculate main length composition
  if (verbose) cat("Calculating length compositions by sex using", scaling_type, "approach...\n")

  # Core calculation logic (integrated from calculate_length_compositions)
  main_lf <- calculate_length_compositions_core(fish_data, strata_data, length_range, plus_group, minus_group, scaling_type, lw_params_male, lw_params_female, lw_params_unsexed, lengths, strata_names)

  # If no bootstrapping requested, return simple result
  if (bootstraps == 0) {
    if (verbose) {
      cat("No bootstrap uncertainty estimation requested.\n")
    }

    # Create a simple result object with class
    simple_result <- list(
      length_compositions = main_lf,
      lengths = lengths,
      strata_names = strata_names,
      n_bootstraps = 0,
      plus_group = plus_group,
      minus_group = minus_group,
      has_sex_data = TRUE,
      scaling_type = scaling_type
    )
    class(simple_result) <- "length_composition"
    return(simple_result)
  }

  # Calculate pooled results across all strata
  pooled_lf <- apply(main_lf, c(1, 2), sum) # Sum across strata, keep sex dimension

  # Calculate proportions using vectorized operations
  proportions <- main_lf
  pooled_proportions <- pooled_lf

  # Calculate stratum totals vectorized
  stratum_totals <- apply(main_lf[, "total", , drop = FALSE], 3, sum)

  for (i in 1:n_strata) {
    if (stratum_totals[i] > 0) {
      proportions[, , i] <- main_lf[, , i] / stratum_totals[i]
    }
  }

  pooled_total <- sum(pooled_lf[, "total"])
  if (pooled_total > 0) {
    pooled_proportions <- pooled_lf / pooled_total
  }

  # Hierarchical bootstrap if requested
  if (bootstraps > 0) {
    if (verbose) {
      cat("Running", bootstraps, "bootstrap iterations with hierarchical resampling...\n")
      cat("  - Resampling samples within strata\n")
      cat("  - Resampling fish within samples\n")
    }

    # Set up bootstrap arrays
    lf_bootstraps <- array(0, dim = c(n_lengths, 5, n_strata, bootstraps))
    dimnames(lf_bootstraps) <- list(
      lengths, c("length", "male", "female", "unsexed", "total"),
      strata_names, 1:bootstraps
    )

    # Run bootstrap iterations
    for (b in 1:bootstraps) {
      if (verbose && b %% 50 == 0) cat("Bootstrap iteration", b, "of", bootstraps, "\n")
      # Resample and calculate
      boot_data <- resample_fish_data(fish_data)
      boot_lf <- calculate_length_compositions_core(boot_data, strata_data, length_range, plus_group, minus_group, scaling_type, lw_params_male, lw_params_female, lw_params_unsexed, lengths, strata_names)
      lf_bootstraps[, , , b] <- boot_lf
    }

    # Calculate CVs from bootstrap results
    lf_means <- apply(lf_bootstraps, c(1, 2, 3), mean)
    lf_sds <- apply(lf_bootstraps, c(1, 2, 3), sd)
    lf_cvs <- ifelse(lf_means > 0, lf_sds / lf_means, 0)

    # Pooled bootstrap results
    pooled_bootstraps <- apply(lf_bootstraps, c(1, 2, 4), sum) # Sum across strata
    pooled_mean <- apply(pooled_bootstraps, c(1, 2), mean)
    pooled_sd <- apply(pooled_bootstraps, c(1, 2), sd)
    pooled_cv <- ifelse(pooled_mean > 0, pooled_sd / pooled_mean, 0)

    # Bootstrap proportions - optimized using vectorized operations
    prop_bootstraps <- lf_bootstraps

    # Calculate stratum totals for all bootstraps at once
    stratum_totals <- apply(lf_bootstraps[, "total", , , drop = FALSE], c(3, 4), sum)

    # Vectorized proportion calculation
    for (i in 1:n_strata) {
      for (b in 1:bootstraps) {
        if (stratum_totals[i, b] > 0) {
          prop_bootstraps[, , i, b] <- lf_bootstraps[, , i, b] / stratum_totals[i, b]
        }
      }
    }

    prop_means <- apply(prop_bootstraps, c(1, 2, 3), mean)
    prop_sds <- apply(prop_bootstraps, c(1, 2, 3), sd)
    prop_cvs <- ifelse(prop_means > 0, prop_sds / prop_means, 0)

    # Pooled proportion bootstrap - optimized using vectorized operations
    pooled_prop_bootstraps <- pooled_bootstraps

    # Calculate totals for all bootstraps at once
    pooled_totals <- apply(pooled_bootstraps[, "total", , drop = FALSE], 3, sum)

    # Vectorized proportion calculation
    for (b in 1:bootstraps) {
      if (!is.na(pooled_totals[b]) && pooled_totals[b] > 0) {
        pooled_prop_bootstraps[, , b] <- pooled_bootstraps[, , b] / pooled_totals[b]
      }
    }

    pooled_prop_mean <- apply(pooled_prop_bootstraps, c(1, 2), mean)
    pooled_prop_sd <- apply(pooled_prop_bootstraps, c(1, 2), sd)
    pooled_prop_cv <- ifelse(pooled_prop_mean > 0, pooled_prop_sd / pooled_prop_mean, 0)

    # Calculate empirical 95% confidence intervals
    if (verbose) cat("Calculating empirical 95% confidence intervals...\n")

    # Length composition confidence intervals (by stratum)
    lf_ci_lower <- apply(lf_bootstraps, c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
    lf_ci_upper <- apply(lf_bootstraps, c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

    # Pooled length composition confidence intervals
    pooled_ci_lower <- apply(pooled_bootstraps, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
    pooled_ci_upper <- apply(pooled_bootstraps, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)

    # Proportion confidence intervals (by stratum)
    prop_ci_lower <- apply(prop_bootstraps, c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
    prop_ci_upper <- apply(prop_bootstraps, c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

    # Pooled proportion confidence intervals
    pooled_prop_ci_lower <- apply(pooled_prop_bootstraps, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
    pooled_prop_ci_upper <- apply(pooled_prop_bootstraps, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)
  } else {
    lf_bootstraps <- NA
    lf_cvs <- NA
    lf_ci_lower <- NA
    lf_ci_upper <- NA
    pooled_ci_lower <- NA
    pooled_ci_upper <- NA
    prop_ci_lower <- NA
    prop_ci_upper <- NA
    pooled_prop_ci_lower <- NA
    pooled_prop_ci_upper <- NA
    prop_cvs <- NA
    pooled_cv <- NA
    pooled_prop_cv <- NA
  }

  # Calculate summary statistics for get_summary function
  calculate_summary_stats <- function(fish_data, strata_data) {
    # Overall summary (pooled across all strata)
    total_fish_by_sex <- c(
      male = sum(fish_data$male, na.rm = TRUE),
      female = sum(fish_data$female, na.rm = TRUE),
      unsexed = sum(fish_data$unsexed, na.rm = TRUE)
    )
    total_fish_by_sex["total"] <- sum(total_fish_by_sex)
    total_hauls <- length(unique(fish_data$sample_id))

    # By-stratum summary
    stratum_summary <- list()
    for (stratum_name in strata_names) {
      stratum_fish <- fish_data[fish_data$stratum == stratum_name, ]
      if (nrow(stratum_fish) > 0) {
        stratum_fish_by_sex <- c(
          male = sum(stratum_fish$male, na.rm = TRUE),
          female = sum(stratum_fish$female, na.rm = TRUE),
          unsexed = sum(stratum_fish$unsexed, na.rm = TRUE)
        )
        stratum_fish_by_sex["total"] <- sum(stratum_fish_by_sex)
        stratum_hauls <- length(unique(stratum_fish$sample_id))

        stratum_summary[[stratum_name]] <- list(
          n_fish = stratum_fish_by_sex,
          n_hauls = stratum_hauls
        )
      } else {
        # Handle empty strata
        stratum_summary[[stratum_name]] <- list(
          n_fish = c(male = 0, female = 0, unsexed = 0, total = 0),
          n_hauls = 0
        )
      }
    }

    return(list(
      total_summary = list(
        n_fish = total_fish_by_sex,
        n_hauls = total_hauls
      ),
      stratum_summary = stratum_summary
    ))
  }

  # Calculate summary statistics
  summary_stats <- calculate_summary_stats(fish_data, strata_data)

  # Format results
  results <- list(
    # Main results
    length_composition = main_lf,
    proportions = proportions,
    pooled_length_composition = pooled_lf,
    pooled_proportions = pooled_proportions,

    # Uncertainty estimates
    lc_cvs = lf_cvs,
    proportions_cvs = prop_cvs,
    pooled_lc_cv = pooled_cv,
    pooled_proportions_cv = pooled_prop_cv,

    # Empirical 95% confidence intervals
    lc_ci_lower = lf_ci_lower,
    lc_ci_upper = lf_ci_upper,
    pooled_lc_ci_lower = pooled_ci_lower,
    pooled_lc_ci_upper = pooled_ci_upper,
    proportions_ci_lower = prop_ci_lower,
    proportions_ci_upper = prop_ci_upper,
    pooled_proportions_ci_lower = pooled_prop_ci_lower,
    pooled_proportions_ci_upper = pooled_prop_ci_upper,

    # Bootstrap results
    lc_bootstraps = lf_bootstraps,

    # Metadata
    lengths = lengths,
    strata_names = strata_names,
    n_bootstraps = bootstraps,
    plus_group = plus_group,
    minus_group = minus_group,
    has_sex_data = TRUE,
    scaling_type = scaling_type,

    # Summary statistics for get_summary function
    summary_stats = summary_stats
  )

  # Add full bootstrap results if requested
  if (return_full_bootstraps && bootstraps > 0) {
    results$full_lc_bootstraps <- lf_bootstraps
    results$full_pooled_lc_bootstraps <- pooled_bootstraps
    results$full_prop_bootstraps <- prop_bootstraps
    results$full_pooled_prop_bootstraps <- pooled_prop_bootstraps
  }

  class(results) <- "length_composition"
  return(results)
}

##' Core calculation function for length compositions
##' @keywords internal
calculate_length_compositions_core <- function(fish_data, strata_data, length_range, plus_group, minus_group, scaling_type, lw_params_male, lw_params_female, lw_params_unsexed, lengths, strata_names) {
  n_strata <- length(strata_names)
  n_lengths <- length(lengths)

  # Initialize result array: length x sex x stratum
  lc_result <- array(0, dim = c(n_lengths, 5, n_strata))
  dimnames(lc_result) <- list(lengths, c("composition", "male", "female", "unsexed", "total"), strata_names)

  # Set length values
  lc_result[, "composition", ] <- lengths

  # Pre-compute fish weights for all data using vectorized operations
  # This avoids redundant calculations in each stratum loop
  fish_data$fish_weight <- with(
    fish_data,
    male * lw_params_male["a"] * (length^lw_params_male["b"]) / 1000 +
      female * lw_params_female["a"] * (length^lw_params_female["b"]) / 1000 +
      unsexed * lw_params_unsexed["a"] * (length^lw_params_unsexed["b"]) / 1000
  )

  # Pre-compute weight by sample for all data to avoid repeated aggregations
  weight_by_sample <- aggregate(fish_weight ~ sample_id, data = fish_data, sum)
  names(weight_by_sample)[2] <- "observed_weight_kg"

  # Process each stratum
  for (s in 1:n_strata) {
    stratum <- strata_names[s]
    stratum_data <- fish_data[fish_data$stratum == stratum, ]

    if (nrow(stratum_data) == 0) next

    if (scaling_type == "weight") {
      # Weight-based scaling approach - optimized with pre-computed weights

      # Calculate sample-level summaries using pre-aggregated data
      sample_summary <- aggregate(
        cbind(male, female, unsexed, total) ~
          sample_id + sample_weight_kg + total_catch_weight_kg,
        data = stratum_data, sum
      )

      # Use pre-computed weights - filter for this stratum's samples
      stratum_sample_ids <- unique(stratum_data$sample_id)
      stratum_weights <- weight_by_sample[weight_by_sample$sample_id %in% stratum_sample_ids, ]

      # Merge with sample_summary using vectorized operation
      sample_summary <- merge(sample_summary, stratum_weights, by = "sample_id")

      # Calculate upweighting factor using vectorized division
      sample_summary$upweight_factor <- sample_summary$total_catch_weight_kg / sample_summary$observed_weight_kg

      # Get stratum total catch
      stratum_total <- strata_data$stratum_total_catch_kg[strata_data$stratum == stratum][1]
      stratum_sample_total <- sum(sample_summary$total_catch_weight_kg)
      stratum_upweight <- stratum_total / stratum_sample_total
    } else if (scaling_type == "density") {
      # Density-based scaling approach - optimized with pre-computed weights

      # Calculate sample-level summaries
      sample_summary <- aggregate(
        cbind(male, female, unsexed, total) ~
          sample_id + sample_area_km2 + catch_density_kg_km2,
        data = stratum_data, sum
      )

      # Use pre-computed weights - filter for this stratum's samples
      stratum_sample_ids <- unique(stratum_data$sample_id)
      stratum_weights <- weight_by_sample[weight_by_sample$sample_id %in% stratum_sample_ids, ]

      # Merge with sample_summary using vectorized operation
      sample_summary <- merge(sample_summary, stratum_weights, by = "sample_id")

      # Calculate expected weight based on density and area using vectorized operations
      sample_summary$expected_weight_kg <- sample_summary$catch_density_kg_km2 * sample_summary$sample_area_km2

      # Calculate upweighting factor using vectorized division
      sample_summary$upweight_factor <- sample_summary$expected_weight_kg / sample_summary$observed_weight_kg

      # Get stratum total area and calculate stratum upweight
      stratum_area <- strata_data$stratum_area_km2[strata_data$stratum == stratum][1]
      stratum_sample_area <- sum(sample_summary$sample_area_km2)
      stratum_upweight <- stratum_area / stratum_sample_area
    } else {
      stop("scaling_type must be either 'weight' or 'density'")
    }

    # Apply upweighting to length frequencies using optimized vectorized operations
    # Create lookup table for upweight factors
    upweight_lookup <- sample_summary[, c("sample_id", "upweight_factor")]

    # Merge upweight factors with stratum data efficiently
    stratum_data_weighted <- merge(stratum_data, upweight_lookup, by = "sample_id", sort = FALSE)

    # Calculate total upweight using vectorized operations
    stratum_data_weighted$total_upweight <- stratum_data_weighted$upweight_factor * stratum_upweight

    # Filter for lengths in range using vectorized comparison
    length_range_filter <- stratum_data_weighted$length >= min(lengths) &
      stratum_data_weighted$length <= max(lengths)
    valid_lengths <- stratum_data_weighted[length_range_filter, ]

    if (nrow(valid_lengths) > 0) {
      # Calculate weighted contributions for each sex category using vectorized operations
      valid_lengths$male_weighted <- valid_lengths$male * valid_lengths$total_upweight
      valid_lengths$female_weighted <- valid_lengths$female * valid_lengths$total_upweight
      valid_lengths$unsexed_weighted <- valid_lengths$unsexed * valid_lengths$total_upweight
      valid_lengths$total_weighted <- valid_lengths$total * valid_lengths$total_upweight

      # Aggregate by length using optimized aggregation
      length_aggregated <- aggregate(
        cbind(male_weighted, female_weighted, unsexed_weighted, total_weighted) ~ length,
        data = valid_lengths, sum
      )

      # Update results array using vectorized indexing
      # Create length index lookup for faster matching
      length_indices <- match(length_aggregated$length, lengths)
      valid_indices <- !is.na(length_indices)

      if (any(valid_indices)) {
        valid_length_indices <- length_indices[valid_indices]
        valid_aggregated <- length_aggregated[valid_indices, ]

        # Vectorized updates to results array
        lc_result[valid_length_indices, "male", s] <- lc_result[valid_length_indices, "male", s] + valid_aggregated$male_weighted
        lc_result[valid_length_indices, "female", s] <- lc_result[valid_length_indices, "female", s] + valid_aggregated$female_weighted
        lc_result[valid_length_indices, "unsexed", s] <- lc_result[valid_length_indices, "unsexed", s] + valid_aggregated$unsexed_weighted
        lc_result[valid_length_indices, "total", s] <- lc_result[valid_length_indices, "total", s] + valid_aggregated$total_weighted
      }
    }

    # Apply plus/minus groups if specified using optimized vectorized operations
    if (plus_group) {
      # Sum all lengths > max into the plus group (last length bin)
      max_length_idx <- length(lengths)
      max_length <- max(lengths)

      # Find fish with lengths greater than max using vectorized operations
      plus_group_filter <- stratum_data_weighted$length > max_length
      plus_group_fish <- stratum_data_weighted[plus_group_filter, ]

      if (nrow(plus_group_fish) > 0) {
        # Vectorized calculation of weighted totals
        weighted_male <- sum(plus_group_fish$male * plus_group_fish$total_upweight)
        weighted_female <- sum(plus_group_fish$female * plus_group_fish$total_upweight)
        weighted_unsexed <- sum(plus_group_fish$unsexed * plus_group_fish$total_upweight)
        weighted_total <- sum(plus_group_fish$total * plus_group_fish$total_upweight)

        lc_result[max_length_idx, "male", s] <- lc_result[max_length_idx, "male", s] + weighted_male
        lc_result[max_length_idx, "female", s] <- lc_result[max_length_idx, "female", s] + weighted_female
        lc_result[max_length_idx, "unsexed", s] <- lc_result[max_length_idx, "unsexed", s] + weighted_unsexed
        lc_result[max_length_idx, "total", s] <- lc_result[max_length_idx, "total", s] + weighted_total
      }
    }

    if (minus_group) {
      # Sum all lengths < min into the minus group (first length bin)
      min_length_idx <- 1
      min_length <- min(lengths)

      # Find fish with lengths less than min using vectorized operations
      minus_group_filter <- stratum_data_weighted$length < min_length
      minus_group_fish <- stratum_data_weighted[minus_group_filter, ]

      if (nrow(minus_group_fish) > 0) {
        # Vectorized calculation of weighted totals
        weighted_male <- sum(minus_group_fish$male * minus_group_fish$total_upweight)
        weighted_female <- sum(minus_group_fish$female * minus_group_fish$total_upweight)
        weighted_unsexed <- sum(minus_group_fish$unsexed * minus_group_fish$total_upweight)
        weighted_total <- sum(minus_group_fish$total * minus_group_fish$total_upweight)

        lc_result[min_length_idx, "male", s] <- lc_result[min_length_idx, "male", s] + weighted_male
        lc_result[min_length_idx, "female", s] <- lc_result[min_length_idx, "female", s] + weighted_female
        lc_result[min_length_idx, "unsexed", s] <- lc_result[min_length_idx, "unsexed", s] + weighted_unsexed
        lc_result[min_length_idx, "total", s] <- lc_result[min_length_idx, "total", s] + weighted_total
      }
    }
  }

  return(lc_result)
}
