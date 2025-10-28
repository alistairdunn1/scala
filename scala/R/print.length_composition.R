#' Print Length Composition Results (Sex-Based)
#'
#' Prints a summary of length composition results by sex, including pooled and stratum-specific compositions, proportions, and uncertainty estimates (CVs) if available.
#'
#' @param x An object of class \code{length_composition} as returned by \code{calculate_length_compositions}.
#' @param show_cvs Logical. Whether to display coefficient of variation (CV) columns. Default is TRUE.
#' @param show_sexes Logical. Whether to display all sex categories. Default is TRUE.
#' @param digits Integer. Number of digits to round counts and proportions. Default is 2.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns a list of printed data frames for pooled and stratum-specific results. Each data frame contains length composition counts by sex, and optionally CV columns if available.
#'
#' @details
#' The function prints:
#' - Pooled length composition results by sex (male, female, unsexed, total)
#' - Stratum-specific length composition results if multiple strata are present
#' - Summary statistics for total, male, female, and unsexed fish
#' - CV columns if \code{show_cvs = TRUE} and uncertainty estimates are available
#' - Confidence interval columns (CI_Lower and CI_Upper) for each sex category when bootstrap results are available
#' - Weighted mean CVs across length classes (for bootstrap results only)
#'
#' The weighted mean CV is calculated as the composition-weighted average CV across length classes:
#' Weighted Mean CV = Sum(CV_i * N_i) / Sum(N_i), where CV_i is the coefficient of variation
#' and N_i is the number of fish for length class i.
#'
#' @examples
#' \dontrun{
#' test_data <- generate_test_data()
#' results <- calculate_length_compositions(
#'   fish_data = test_data$fish_data,
#'   strata_data = test_data$strata_data,
#'   length_range = c(15, 35),
#'   bootstraps = 100
#' )
#' print(results)
#' }
#'
#' @seealso
#' \code{\link{calculate_length_compositions}} for generating length composition results,
#' \code{\link{plot.length_composition}} for plotting results
#'
#' @export
print.length_composition <- function(x, show_cvs = TRUE, show_sexes = TRUE, digits = 2, ...) {
  cat("Length Composition Summary\n")
  cat("==========================\n\n")
  cat("Data Type: Sex-based length compositions\n")

  cat("Length Range:", min(x$lengths), "to", max(x$lengths), "\n")
  cat("Strata:", length(x$strata_names), "\n")
  cat("Bootstrap iterations:", x$n_bootstraps, "\n")
  if (!is.null(x$scaling_type)) {
    scaling_desc <- ifelse(x$scaling_type == "weight", "Weight-based (commercial)", "Density-based (survey)")
    cat("Scaling approach:", scaling_desc, "\n")
  }
  cat("Sex categories: Male, Female, Unsexed, Total\n\n")

  # Determine if this is bootstrap or non-bootstrap result
  is_bootstrap <- x$n_bootstraps > 0

  if (is_bootstrap) {
    # Bootstrap results - show pooled results
    cat("Pooled Length Composition:\n")

    # Only show lengths with fish
    has_fish <- x$pooled_length_composition[, "total"] > 0

    if (any(has_fish)) {
      pooled_df <- data.frame(
        Length = x$lengths[has_fish],
        Male = round(x$pooled_length_composition[has_fish, "male"], digits),
        Female = round(x$pooled_length_composition[has_fish, "female"], digits),
        Unsexed = round(x$pooled_length_composition[has_fish, "unsexed"], digits),
        Total = round(x$pooled_length_composition[has_fish, "total"], digits)
      )

      if (show_cvs && !is.null(x$pooled_lc_cv)) {
        pooled_df$CV_Male <- round(x$pooled_lc_cv[has_fish, "male"] * 100, 1)
        pooled_df$CV_Female <- round(x$pooled_lc_cv[has_fish, "female"] * 100, 1)
        pooled_df$CV_Total <- round(x$pooled_lc_cv[has_fish, "total"] * 100, 1)

        # Add confidence intervals if available
        if (!is.null(x$pooled_lc_ci_lower) && !is.null(x$pooled_lc_ci_upper)) {
          pooled_df$CI_Lower_Male <- round(x$pooled_lc_ci_lower[has_fish, "male"], digits)
          pooled_df$CI_Upper_Male <- round(x$pooled_lc_ci_upper[has_fish, "male"], digits)
          pooled_df$CI_Lower_Female <- round(x$pooled_lc_ci_lower[has_fish, "female"], digits)
          pooled_df$CI_Upper_Female <- round(x$pooled_lc_ci_upper[has_fish, "female"], digits)
          pooled_df$CI_Lower_Total <- round(x$pooled_lc_ci_lower[has_fish, "total"], digits)
          pooled_df$CI_Upper_Total <- round(x$pooled_lc_ci_upper[has_fish, "total"], digits)
        }
      } else if (!is.null(x$pooled_lc_ci_lower) && !is.null(x$pooled_lc_ci_upper)) {
        # Add confidence intervals even if CVs are not shown
        pooled_df$CI_Lower_Male <- round(x$pooled_lc_ci_lower[has_fish, "male"], digits)
        pooled_df$CI_Upper_Male <- round(x$pooled_lc_ci_upper[has_fish, "male"], digits)
        pooled_df$CI_Lower_Female <- round(x$pooled_lc_ci_lower[has_fish, "female"], digits)
        pooled_df$CI_Upper_Female <- round(x$pooled_lc_ci_upper[has_fish, "female"], digits)
        pooled_df$CI_Lower_Total <- round(x$pooled_lc_ci_lower[has_fish, "total"], digits)
        pooled_df$CI_Upper_Total <- round(x$pooled_lc_ci_upper[has_fish, "total"], digits)
      }

      print(pooled_df)

      # Summary statistics
      cat("\nSummary:\n")
      total_fish <- sum(x$pooled_length_composition[, "total"])
      male_fish <- sum(x$pooled_length_composition[, "male"])
      female_fish <- sum(x$pooled_length_composition[, "female"])
      unsexed_fish <- sum(x$pooled_length_composition[, "unsexed"])

      # Calculate weighted mean CVs if available
      if (show_cvs && !is.null(x$pooled_lc_cv)) {
        # Function to calculate weighted mean CV
        calc_weighted_mean_cv <- function(cvs, weights) {
          valid_idx <- !is.na(cvs) & !is.na(weights) & weights > 0
          if (sum(valid_idx) > 0) {
            return(sum(cvs[valid_idx] * weights[valid_idx]) / sum(weights[valid_idx]))
          } else {
            return(NA)
          }
        }

        # Calculate weighted mean CVs for each sex category
        weights_male <- x$pooled_length_composition[, "male"]
        weights_female <- x$pooled_length_composition[, "female"]
        weights_total <- x$pooled_length_composition[, "total"]

        wmean_cv_male <- calc_weighted_mean_cv(x$pooled_lc_cv[, "male"], weights_male)
        wmean_cv_female <- calc_weighted_mean_cv(x$pooled_lc_cv[, "female"], weights_female)
        wmean_cv_total <- calc_weighted_mean_cv(x$pooled_lc_cv[, "total"], weights_total)

        cat("\nWeighted Mean CVs:\n")
        if (!is.na(wmean_cv_male)) {
          cat("Male:", round(wmean_cv_male * 100, 1), "%\n")
        }
        if (!is.na(wmean_cv_female)) {
          cat("Female:", round(wmean_cv_female * 100, 1), "%\n")
        }
        if (!is.na(wmean_cv_total)) {
          cat("Total:", round(wmean_cv_total * 100, 1), "%\n")
        }
      }
    }
  } else {
    # Non-bootstrap results - show summed across strata
    cat("Length Composition (summed across strata):\n")

    # Sum across strata (3rd dimension)
    pooled_lc <- apply(x$length_compositions, c(1, 2), sum)

    # Only show lengths with fish
    has_fish <- pooled_lc[, "total"] > 0

    if (any(has_fish)) {
      pooled_df <- data.frame(
        Length = x$lengths[has_fish],
        Male = round(pooled_lc[has_fish, "male"], digits),
        Female = round(pooled_lc[has_fish, "female"], digits),
        Unsexed = round(pooled_lc[has_fish, "unsexed"], digits),
        Total = round(pooled_lc[has_fish, "total"], digits)
      )

      print(pooled_df)

      # Summary statistics
      cat("\nSummary:\n")
      total_fish <- sum(pooled_lc[, "total"])
      male_fish <- sum(pooled_lc[, "male"])
      female_fish <- sum(pooled_lc[, "female"])
      unsexed_fish <- sum(pooled_lc[, "unsexed"])
    }
  }

  if (any(has_fish)) {
    cat("Total estimated fish:", format(round(total_fish), big.mark = ","), "\n")
    cat(
      "Male:", format(round(male_fish), big.mark = ","),
      "(", round(male_fish / total_fish * 100, 1), "%)\n"
    )
    cat(
      "Female:", format(round(female_fish), big.mark = ","),
      "(", round(female_fish / total_fish * 100, 1), "%)\n"
    )
    cat(
      "Unsexed:", format(round(unsexed_fish), big.mark = ","),
      "(", round(unsexed_fish / total_fish * 100, 1), "%)\n"
    )
  } else {
    cat("No fish found in specified length range.\n")
  }

  # Show by stratum if multiple strata
  if (length(x$strata_names) > 1) {
    cat("\nBy Stratum:\n")

    # Use appropriate field based on result type
    lc_data <- if (is_bootstrap) x$length_composition else x$length_compositions

    for (i in seq_along(x$strata_names)) {
      stratum_total <- sum(lc_data[, "total", i])
      if (stratum_total > 0) {
        cat(
          "\nStratum:", x$strata_names[i],
          "(Total:", format(round(stratum_total), big.mark = ","), "fish)\n"
        )

        stratum_has_fish <- lc_data[, "total", i] > 0
        if (any(stratum_has_fish)) {
          stratum_df <- data.frame(
            Length = x$lengths[stratum_has_fish],
            Male = round(lc_data[stratum_has_fish, "male", i], digits),
            Female = round(lc_data[stratum_has_fish, "female", i], digits),
            Unsexed = round(lc_data[stratum_has_fish, "unsexed", i], digits),
            Total = round(lc_data[stratum_has_fish, "total", i], digits)
          )

          if (show_cvs && !is.null(x$lc_cvs)) {
            stratum_df$CV_Total <- round(x$lc_cvs[stratum_has_fish, "total", i] * 100, 1)

            # Add confidence intervals for stratum if available
            if (!is.null(x$lc_ci_lower) && !is.null(x$lc_ci_upper)) {
              stratum_df$CI_Lower_Male <- round(x$lc_ci_lower[stratum_has_fish, "male", i], digits)
              stratum_df$CI_Upper_Male <- round(x$lc_ci_upper[stratum_has_fish, "male", i], digits)
              stratum_df$CI_Lower_Female <- round(x$lc_ci_lower[stratum_has_fish, "female", i], digits)
              stratum_df$CI_Upper_Female <- round(x$lc_ci_upper[stratum_has_fish, "female", i], digits)
              stratum_df$CI_Lower_Total <- round(x$lc_ci_lower[stratum_has_fish, "total", i], digits)
              stratum_df$CI_Upper_Total <- round(x$lc_ci_upper[stratum_has_fish, "total", i], digits)
            }
          } else if (!is.null(x$lc_ci_lower) && !is.null(x$lc_ci_upper) && is_bootstrap) {
            # Add confidence intervals even if CVs are not shown
            stratum_df$CI_Lower_Male <- round(x$lc_ci_lower[stratum_has_fish, "male", i], digits)
            stratum_df$CI_Upper_Male <- round(x$lc_ci_upper[stratum_has_fish, "male", i], digits)
            stratum_df$CI_Lower_Female <- round(x$lc_ci_lower[stratum_has_fish, "female", i], digits)
            stratum_df$CI_Upper_Female <- round(x$lc_ci_upper[stratum_has_fish, "female", i], digits)
            stratum_df$CI_Lower_Total <- round(x$lc_ci_lower[stratum_has_fish, "total", i], digits)
            stratum_df$CI_Upper_Total <- round(x$lc_ci_upper[stratum_has_fish, "total", i], digits)
          }

          print(stratum_df)

          # Add weighted mean CV for this stratum if available
          if (show_cvs && !is.null(x$lc_cvs) && is_bootstrap) {
            # Function to calculate weighted mean CV (defined locally)
            calc_weighted_mean_cv <- function(cvs, weights) {
              valid_idx <- !is.na(cvs) & !is.na(weights) & weights > 0
              if (sum(valid_idx) > 0) {
                return(sum(cvs[valid_idx] * weights[valid_idx]) / sum(weights[valid_idx]))
              } else {
                return(NA)
              }
            }

            # Calculate for this stratum
            stratum_weights_total <- lc_data[, "total", i]
            stratum_wmean_cv <- calc_weighted_mean_cv(x$lc_cvs[, "total", i], stratum_weights_total)

            if (!is.na(stratum_wmean_cv)) {
              cat("Weighted Mean CV:", round(stratum_wmean_cv * 100, 1), "%\n")
            }
          }
        }
      }
    }
  }
}
