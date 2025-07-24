##' Calculate Scaled Length Frequencies with Bootstrap Uncertainty (Sex-Based)
##'
##' Calculates scaled length frequencies for fish data by sex and stratum, applying upweighting and bootstrap resampling to estimate uncertainty.
##'
##' @param fish_data Data frame with columns: stratum, sample_id, length, male, female, unsexed, sample_weight_kg, total_catch_weight_kg
##' @param strata_data Data frame with columns: stratum, stratum_total_catch_kg
##' @param length_range Numeric vector of min and max lengths to include (e.g., c(15, 35))
##' @param bootstraps Integer, number of bootstrap iterations (default 300)
##' @param plus_group Logical, combine lengths >= max length into a plus group (default FALSE)
##' @param minus_group Logical, combine lengths <= min length into a minus group (default FALSE)
##'
##' @return List containing:
##'   \itemize{
##'     \item length_frequency: 3D array (length x sex x stratum) of scaled length frequencies
##'     \item proportions: 3D array (length x sex x stratum) of proportions
##'     \item pooled_length_frequency: matrix (length x sex) of pooled frequencies
##'     \item pooled_proportions: matrix (length x sex) of pooled proportions
##'     \item lf_cvs: 3D array of CVs for length frequencies
##'     \item proportions_cvs: 3D array of CVs for proportions
##'     \item pooled_lf_cv: matrix of pooled CVs
##'     \item pooled_proportions_cv: matrix of pooled proportion CVs
##'     \item lf_bootstraps: 4D array of bootstrap results
##'     \item lengths: vector of length bins
##'     \item strata_names: vector of stratum names
##'     \item n_bootstraps: number of bootstrap iterations
##'     \item plus_group: logical, plus group used
##'     \item minus_group: logical, minus group used
##'     \item has_sex_data: logical, TRUE if sex-based columns present
##'   }
##'
##' @details
##' The function applies a two-stage upweighting process and uses bootstrap resampling to estimate uncertainty in length frequency and proportion estimates. Sex categories are: male, female, unsexed, and total. Plus and minus group functionality is available for aggregating extreme length bins.
##'
##' @examples
##' \dontrun{
##' fish_data <- data.frame(
##'   stratum = c("A", "A", "B", "B"),
##'   sample_id = c(1, 1, 2, 2),
##'   length = c(20, 25, 22, 28),
##'   male = c(2, 1, 3, 2),
##'   female = c(2, 1, 2, 1),
##'   unsexed = c(1, 1, 2, 1),
##'   sample_weight_kg = c(10, 10, 15, 15),
##'   total_catch_weight_kg = c(100, 100, 150, 150)
##' )
##' strata_data <- data.frame(
##'   stratum = c("A", "B"),
##'   stratum_total_catch_kg = c(1000, 2000)
##' )
##' results <- calculate_scaled_length_frequencies(
##'   fish_data = fish_data,
##'   strata_data = strata_data,
##'   length_range = c(15, 35),
##'   bootstraps = 100
##' )
##' print(results)
##' }
##'
##' @export
calculate_scaled_length_frequencies <- function(fish_data,
                                                strata_data,
                                                length_range = c(min(fish_data$length), max(fish_data$length)),
                                                bootstraps = 300,
                                                plus_group = FALSE,
                                                minus_group = FALSE) {
  # Validate inputs
  required_fish_cols <- c(
    "stratum", "sample_id", "length", "male", "female", "unsexed",
    "sample_weight_kg", "total_catch_weight_kg"
  )
  required_strata_cols <- c("stratum", "stratum_total_catch_kg")

  if (!all(required_fish_cols %in% names(fish_data))) {
    stop(
      "fish_data missing required columns: ",
      paste(setdiff(required_fish_cols, names(fish_data)), collapse = ", ")
    )
  }

  if (!all(required_strata_cols %in% names(strata_data))) {
    stop(
      "strata_data missing required columns: ",
      paste(setdiff(required_strata_cols, names(strata_data)), collapse = ", ")
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

  # Calculate main length frequency
  cat("Calculating scaled length frequencies by sex...\n")
  main_lf <- calculate_lf_result(fish_data, strata_data, lengths, plus_group, minus_group)

  # Calculate pooled results across all strata
  pooled_lf <- apply(main_lf, c(1, 2), sum) # Sum across strata, keep sex dimension

  # Calculate proportions
  proportions <- main_lf
  pooled_proportions <- pooled_lf

  for (i in 1:n_strata) {
    stratum_total <- sum(main_lf[, "total", i])
    if (stratum_total > 0) {
      proportions[, , i] <- main_lf[, , i] / stratum_total
    }
  }

  pooled_total <- sum(pooled_lf[, "total"])
  if (pooled_total > 0) {
    pooled_proportions <- pooled_lf / pooled_total
  }

  # Bootstrap if requested
  if (bootstraps > 0) {
    cat("Running", bootstraps, "bootstrap iterations...\n")

    # Set up bootstrap arrays
    lf_bootstraps <- array(0, dim = c(n_lengths, 5, n_strata, bootstraps))
    dimnames(lf_bootstraps) <- list(
      lengths, c("length", "male", "female", "unsexed", "total"),
      strata_names, 1:bootstraps
    )

    # Run bootstrap iterations
    for (b in 1:bootstraps) {
      if (b %% 50 == 0) cat("Bootstrap iteration", b, "of", bootstraps, "\n")

      # Resample and calculate
      boot_data <- resample_fish_data(fish_data)
      boot_lf <- calculate_lf_result(boot_data, strata_data, lengths, plus_group, minus_group)
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

    # Bootstrap proportions
    prop_bootstraps <- lf_bootstraps
    for (b in 1:bootstraps) {
      for (i in 1:n_strata) {
        stratum_total <- sum(lf_bootstraps[, "total", i, b])
        if (stratum_total > 0) {
          prop_bootstraps[, , i, b] <- lf_bootstraps[, , i, b] / stratum_total
        }
      }
    }

    prop_means <- apply(prop_bootstraps, c(1, 2, 3), mean)
    prop_sds <- apply(prop_bootstraps, c(1, 2, 3), sd)
    prop_cvs <- ifelse(prop_means > 0, prop_sds / prop_means, 0)

    # Pooled proportion bootstrap
    pooled_prop_bootstraps <- pooled_bootstraps
    for (b in 1:bootstraps) {
      total <- sum(pooled_bootstraps[, "total", b])
      if (total > 0) {
        pooled_prop_bootstraps[, , b] <- pooled_bootstraps[, , b] / total
      }
    }

    pooled_prop_mean <- apply(pooled_prop_bootstraps, c(1, 2), mean)
    pooled_prop_sd <- apply(pooled_prop_bootstraps, c(1, 2), sd)
    pooled_prop_cv <- ifelse(pooled_prop_mean > 0, pooled_prop_sd / pooled_prop_mean, 0)
  } else {
    lf_bootstraps <- NA
    lf_cvs <- NA
    prop_cvs <- NA
    pooled_cv <- NA
    pooled_prop_cv <- NA
  }

  # Format results
  results <- list(
    # Main results
    length_frequency = main_lf,
    proportions = proportions,
    pooled_length_frequency = pooled_lf,
    pooled_proportions = pooled_proportions,

    # Uncertainty estimates
    lf_cvs = lf_cvs,
    proportions_cvs = prop_cvs,
    pooled_lf_cv = pooled_cv,
    pooled_proportions_cv = pooled_prop_cv,

    # Bootstrap results
    lf_bootstraps = lf_bootstraps,

    # Metadata
    lengths = lengths,
    strata_names = strata_names,
    n_bootstraps = bootstraps,
    plus_group = plus_group,
    minus_group = minus_group,
    has_sex_data = TRUE
  )

  class(results) <- "scaled_length_frequency"
  return(results)
}
