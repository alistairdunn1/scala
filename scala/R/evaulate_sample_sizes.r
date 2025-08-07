#' @title Evaluate Sample Sizes for Bootstrap Analysis
#' @description Evaluates whether the sample sizes in fisheries data are adequate for reliable
#'   bootstrap uncertainty estimation. This function checks both the number of fish
#'   per sample and the number of samples per stratum against recommended minimum
#'   thresholds for hierarchical bootstrap resampling.
#'
#' @param fish_data A data frame containing fisheries sampling data with columns:
#'   \describe{
#'     \item{sample_id}{Unique identifier for each sample}
#'     \item{stratum}{Stratum identifier (e.g., area, season, gear type)}
#'     \item{total}{Number of fish in each length/sex/age bin}
#'     \item{sample_weight_kg}{(Optional) Weight of fish sampled (kg)}
#'     \item{total_catch_weight_kg}{(Optional) Total catch weight for sample_id (kg)}
#'   }
#' @param min_fish_per_sample Minimum number of fish required per sample for
#'   reliable bootstrap resampling (default: 15). Samples with fewer fish may
#'   produce unreliable uncertainty estimates unless they represent a high
#'   proportion of the total catch.
#' @param min_samples_per_stratum Minimum number of samples required per stratum
#'   for reliable bootstrap resampling (default: 8). Strata with fewer samples
#'   may produce unreliable uncertainty estimates.
#' @param min_sample_proportion Minimum proportion of total catch that a small
#'   sample should represent to be considered adequately representative
#'   (default: 0.2 or 20%). Small samples with high representativeness may be
#'   retained despite low fish counts.
#'
#' @return A list containing:
#'   \describe{
#'     \item{sample_sizes}{Data frame with sample sizes for each sample_id and stratum}
#'     \item{all_small_samples}{Data frame of samples with fewer than minimum fish}
#'     \item{representative_small_samples}{Data frame of small samples that are highly representative}
#'     \item{non_representative_small_samples}{Data frame of small samples with poor representativeness}
#'     \item{small_strata}{Data frame of strata with fewer than minimum samples}
#'     \item{exclusion_summary}{Data frame summarizing the impact of excluding small samples}
#'     \item{recommendations}{Character vector with specific recommendations for handling small samples}
#'   }
#'
#' @details
#' Hierarchical bootstrap resampling requires adequate sample sizes at both levels:
#' \itemize{
#'   \item \strong{Sample level}: Each sample should contain at least 15 fish to ensure
#'     stable resampling of individual fish within samples
#'   \item \strong{Stratum level}: Each stratum should contain at least 8 samples to ensure
#'     stable resampling of samples within strata
#' }
#'
#' Insufficient sample sizes can lead to:
#' \itemize{
#'   \item Unreliable confidence intervals
#'   \item Unstable bootstrap distributions
#'   \item Poor uncertainty estimation
#'   \item Biased variance estimates
#' }
#'
#' The function provides recommendations for handling small samples:
#' \itemize{
#'   \item \strong{Retain}: When small samples provide important coverage, represent small fraction of data, or are highly representative of their total catch
#'   \item \strong{Combine strata}: When multiple strata have few samples each
#'   \item \strong{Consider exclusion}: When samples are extremely small (< 5 fish, regardless of user threshold) and have poor representativeness
#'   \item \strong{Sensitivity analysis}: Always compare results with and without questionable samples
#' }
#'
#' \strong{Representativeness Assessment}: When sample weight and total catch weight
#' information is available, the function evaluates whether small samples represent
#' a significant proportion of the total catch. Small samples that represent >= 20%
#' of the total catch are considered adequately representative despite low fish counts.
#'
#' @examples
#' \dontrun{
#' # Generate test data
#' test_data <- generate_test_data()
#'
#' # Evaluate sample sizes with default thresholds
#' evaluation <- evaluate_sample_sizes(test_data$fish_data)
#'
#' # Check for any problematic samples or strata
#' if (nrow(evaluation$small_samples) > 0) {
#'   print("Samples with insufficient fish:")
#'   print(evaluation$small_samples)
#' }
#'
#' # Review recommendations for handling small samples
#' cat("Recommendations:\n")
#' for (rec in evaluation$recommendations) {
#'   cat("-", rec, "\n")
#' }
#'
#' # Check impact of excluding small samples
#' print(evaluation$exclusion_summary)
#'
#' # Review representative vs problematic small samples
#' if (nrow(evaluation$representative_small_samples) > 0) {
#'   cat("\nHighly representative small samples (retain):\n")
#'   print(evaluation$representative_small_samples[c("sample_id", "total", "sample_proportion")])
#' }
#'
#' if (nrow(evaluation$non_representative_small_samples) > 0) {
#'   cat("\nProblematic small samples (consider exclusion):\n")
#'   print(evaluation$non_representative_small_samples[c("sample_id", "total", "sample_proportion")])
#' }
#'
#' # Use custom thresholds
#' strict_evaluation <- evaluate_sample_sizes(
#'   test_data$fish_data,
#'   min_fish_per_sample = 20,
#'   min_samples_per_stratum = 10,
#'   min_sample_proportion = 0.3 # Require 30% representativeness
#' )
#' }
#' @seealso \code{\link{calculate_length_compositions}} for the main bootstrap analysis function
#'
#' @export
evaluate_sample_sizes <- function(fish_data, min_fish_per_sample = 15, min_samples_per_stratum = 8, min_sample_proportion = 0.2) {
  # Check fish per sample
  sample_sizes <- aggregate(total ~ sample_id + stratum, data = fish_data, sum)
  all_small_samples <- sample_sizes[sample_sizes$total < min_fish_per_sample, ]

  # Check if we have representativeness information (sample weights and total catch weights)
  has_weight_info <- all(c("sample_weight_kg", "total_catch_weight_kg") %in% colnames(fish_data))

  # Initialize representativeness analysis
  representative_small_samples <- data.frame()
  non_representative_small_samples <- all_small_samples

  if (has_weight_info && nrow(all_small_samples) > 0) {
    # Calculate sample representativeness for small samples
    weight_summary <- aggregate(
      cbind(sample_weight_kg, total_catch_weight_kg) ~ sample_id + stratum,
      data = fish_data,
      FUN = function(x) x[1] # Take first value (should be same for all rows in sample)
    )

    # Merge with small samples to get representativeness
    small_with_weights <- merge(all_small_samples, weight_summary, by = c("sample_id", "stratum"))
    small_with_weights$sample_proportion <- small_with_weights$sample_weight_kg / small_with_weights$total_catch_weight_kg

    # Identify representative vs problematic small samples
    representative_small_samples <- small_with_weights[small_with_weights$sample_proportion >= min_sample_proportion, ]
    non_representative_small_samples <- small_with_weights[small_with_weights$sample_proportion < min_sample_proportion, ]

    # Add representativeness info to the data frames
    if (nrow(representative_small_samples) > 0) {
      representative_small_samples$representativeness <- "High"
    }
    if (nrow(non_representative_small_samples) > 0) {
      non_representative_small_samples$representativeness <- "Low"
    }
  }

  # Check samples per stratum
  samples_per_stratum <- aggregate(sample_id ~ stratum, data = sample_sizes, function(x) length(unique(x)))
  small_strata <- samples_per_stratum[samples_per_stratum$sample_id < min_samples_per_stratum, ]

  if (nrow(all_small_samples) > 0) {
    warning(
      "Small samples detected (< ", min_fish_per_sample, " fish): ",
      paste(all_small_samples$sample_id, collapse = ", ")
    )
  }

  if (nrow(small_strata) > 0) {
    warning(
      "Strata with few samples (< ", min_samples_per_stratum, " samples): ",
      paste(small_strata$stratum, collapse = ", ")
    )
  }

  # Calculate impact of excluding small samples
  total_fish <- sum(sample_sizes$total)
  total_samples <- nrow(sample_sizes)
  total_strata <- length(unique(sample_sizes$stratum))

  small_fish <- sum(all_small_samples$total)
  excluded_samples <- nrow(all_small_samples)
  affected_strata <- length(unique(all_small_samples$stratum))

  exclusion_summary <- data.frame(
    metric = c(
      "Total Fish", "Total Samples", "Total Strata",
      "Fish in Small Samples", "Small Samples", "Strata with Small Samples",
      "% Fish Lost if Excluded", "% Samples Lost if Excluded"
    ),
    value = c(
      total_fish, total_samples, total_strata,
      small_fish, excluded_samples, affected_strata,
      round(100 * small_fish / total_fish, 1),
      round(100 * excluded_samples / total_samples, 1)
    )
  )

  # Generate recommendations
  recommendations <- character(0)

  # Check severity of small sample issues
  very_small_samples <- all_small_samples[all_small_samples$total < 5, ]
  pct_fish_lost <- 100 * small_fish / total_fish
  pct_samples_lost <- 100 * excluded_samples / total_samples

  if (nrow(all_small_samples) == 0) {
    recommendations <- c("No sample size issues detected. Proceed with bootstrap analysis.")
  } else {
    # Account for representativeness in recommendations
    if (has_weight_info) {
      if (nrow(representative_small_samples) > 0) {
        recommendations <- c(
          recommendations,
          sprintf(
            "RETAIN %d small but highly representative samples (>= %.0f%% of total catch) - these provide adequate coverage despite being below your threshold of %d fish",
            nrow(representative_small_samples), min_sample_proportion * 100, min_fish_per_sample
          )
        )
      }

      if (nrow(non_representative_small_samples) > 0) {
        very_small_problematic <- non_representative_small_samples[non_representative_small_samples$total < 5, ]
        if (nrow(very_small_problematic) > 0) {
          recommendations <- c(
            recommendations,
            sprintf(
              "Consider excluding %d extremely small samples (< 5 fish) with poor representativeness (< %.0f%% of catch)",
              nrow(very_small_problematic), min_sample_proportion * 100
            )
          )
        }

        if (nrow(non_representative_small_samples) > nrow(very_small_problematic)) {
          recommendations <- c(
            recommendations,
            sprintf(
              "Review %d small samples (< %d fish) with poor representativeness - consider case-by-case evaluation",
              nrow(non_representative_small_samples) - nrow(very_small_problematic), min_fish_per_sample
            )
          )
        }
      }
    } else {
      # Original logic when no weight information available
      if (nrow(very_small_samples) > 0) {
        recommendations <- c(
          recommendations,
          sprintf("Consider excluding %d extremely small samples (< 5 fish) - no representativeness data available", nrow(very_small_samples))
        )
      }

      if (nrow(all_small_samples) > nrow(very_small_samples)) {
        recommendations <- c(
          recommendations,
          sprintf(
            "Review %d small samples (< %d fish but >= 5 fish) - no representativeness data available for informed decisions",
            nrow(all_small_samples) - nrow(very_small_samples), min_fish_per_sample
          )
        )
      }

      recommendations <- c(
        recommendations,
        "No sample weight/catch data available - cannot assess representativeness of small samples"
      )
    }

    if (pct_fish_lost < 5) {
      recommendations <- c(
        recommendations,
        sprintf("Small samples (< %d fish) represent < 5%% of total fish - exclusion may be reasonable", min_fish_per_sample)
      )
    } else if (pct_fish_lost > 20) {
      recommendations <- c(
        recommendations,
        sprintf("Small samples (< %d fish) represent > 20%% of total fish - exclusion not recommended", min_fish_per_sample)
      )
    }

    if (affected_strata > total_strata / 2) {
      recommendations <- c(
        recommendations,
        "Consider combining adjacent strata to improve sample sizes"
      )
    }

    if (nrow(small_strata) > 0) {
      recommendations <- c(
        recommendations,
        sprintf(
          "Strata with few samples: %s - consider combining with adjacent strata",
          paste(small_strata$stratum, collapse = ", ")
        )
      )
    }

    recommendations <- c(
      recommendations,
      "Always run sensitivity analysis comparing results with and without small samples"
    )

    if (pct_samples_lost > 30) {
      recommendations <- c(
        recommendations,
        "High proportion of small samples suggests systematic sampling issues"
      )
    }
  }

  return(list(
    sample_sizes = sample_sizes,
    all_small_samples = all_small_samples,
    representative_small_samples = representative_small_samples,
    non_representative_small_samples = non_representative_small_samples,
    small_strata = small_strata,
    exclusion_summary = exclusion_summary,
    recommendations = recommendations
  ))
}
