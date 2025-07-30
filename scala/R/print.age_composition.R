#' Print Age Composition Results (Sex-Based)
#'
#' Prints a summary of age composition results by sex, including pooled and stratum-specific compositions, proportions, and uncertainty estimates (CVs) if available.
#'
#' @param x An object of class \code{age_composition} as returned by \code{calculate_age_compositions}.
#' @param show_cvs Logical. Whether to display coefficient of variation (CV) columns. Default is TRUE.
#' @param show_sexes Logical. Whether to display all sex categories. Default is TRUE.
#' @param digits Integer. Number of digits to round counts and proportions. Default is 2.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns a list of printed data frames for pooled and stratum-specific results. Each data frame contains age composition counts by sex, and optionally CV columns if available.
#'
#' @details
#' The function prints:
#' - Pooled age composition results by sex (male, female, unsexed, total)
#' - Stratum-specific age composition results if multiple strata are present
#' - Summary statistics for total, male, female, and unsexed fish
#' - CV columns if \code{show_cvs = TRUE} and uncertainty estimates are available
#' - Confidence interval columns (CI_Lower and CI_Upper) for each sex category when bootstrap results are available
#' - Weighted mean CVs across age classes (for bootstrap results only)
#' - Information about sex-specific age-length keys if used
#'
#' The weighted mean CV is calculated as the composition-weighted average CV across age classes:
#' Weighted Mean CV = Sum(CV_i * N_i) / Sum(N_i), where CV_i is the coefficient of variation
#' and N_i is the number of fish for age class i.
#'
#' @examples
#' \dontrun{
#' test_data <- generate_test_data()
#' lc_result <- calculate_length_compositions(
#'   fish_data = test_data$fish_data,
#'   strata_data = test_data$strata_data,
#'   length_range = c(20, 40),
#'   bootstraps = 100
#' )
#'
#' age_key <- generate_age_length_key(c(20, 40), c(1, 8))
#' age_result <- calculate_age_compositions(lc_result, age_key)
#' print(age_result)
#' }
#'
#' @seealso
#' \code{\link{calculate_age_compositions}} for generating age composition results,
#' \code{\link{plot.age_composition}} for plotting results
#'
#' @export
print.age_composition <- function(x, show_cvs = TRUE, show_sexes = TRUE, digits = 2, ...) {
  cat("Age Composition Results (Sex-based)\n")
  cat("===================================\n\n")

  cat("Age range:", min(x$ages), "to", max(x$ages), "\n")
  cat("Number of strata:", length(x$strata_names), "\n")
  cat("Bootstrap iterations:", x$n_bootstraps, "\n")
  if (!is.null(x$scaling_type)) {
    scaling_desc <- ifelse(x$scaling_type == "weight", "Weight-based (commercial)", "Density-based (survey)")
    cat("Scaling approach:", scaling_desc, "\n")
  }
  if (!is.null(x$sex_specific_keys)) {
    key_type <- ifelse(x$sex_specific_keys, "Sex-specific age-length keys", "Single age-length key")
    cat("Age-length key type:", key_type, "\n")
  }
  cat("Sex categories: Male, Female, Unsexed, Total\n\n")

  # Determine if this is bootstrap or non-bootstrap result
  is_bootstrap <- !is.null(x$n_bootstraps) && x$n_bootstraps > 0

  if (is_bootstrap) {
    # Bootstrap results - show pooled results
    cat("Pooled Age Composition:\n")

    # Only show ages with fish
    has_fish <- x$pooled_age_composition[, "total"] > 0

    if (any(has_fish)) {
      pooled_df <- data.frame(
        Age = x$ages[has_fish],
        Male = round(x$pooled_age_composition[has_fish, "male"], digits),
        Female = round(x$pooled_age_composition[has_fish, "female"], digits),
        Unsexed = round(x$pooled_age_composition[has_fish, "unsexed"], digits),
        Total = round(x$pooled_age_composition[has_fish, "total"], digits)
      )

      if (show_cvs && !is.null(x$pooled_age_cv)) {
        pooled_df$CV_Male <- round(x$pooled_age_cv[has_fish, "male"] * 100, 1)
        pooled_df$CV_Female <- round(x$pooled_age_cv[has_fish, "female"] * 100, 1)
        pooled_df$CV_Total <- round(x$pooled_age_cv[has_fish, "total"] * 100, 1)

        # Add confidence intervals if available
        if (!is.null(x$pooled_age_ci_lower) && !is.null(x$pooled_age_ci_upper)) {
          pooled_df$CI_Lower_Male <- round(x$pooled_age_ci_lower[has_fish, "male"], digits)
          pooled_df$CI_Upper_Male <- round(x$pooled_age_ci_upper[has_fish, "male"], digits)
          pooled_df$CI_Lower_Female <- round(x$pooled_age_ci_lower[has_fish, "female"], digits)
          pooled_df$CI_Upper_Female <- round(x$pooled_age_ci_upper[has_fish, "female"], digits)
          pooled_df$CI_Lower_Total <- round(x$pooled_age_ci_lower[has_fish, "total"], digits)
          pooled_df$CI_Upper_Total <- round(x$pooled_age_ci_upper[has_fish, "total"], digits)
        }
      } else if (!is.null(x$pooled_age_ci_lower) && !is.null(x$pooled_age_ci_upper)) {
        # Add confidence intervals even if CVs are not shown
        pooled_df$CI_Lower_Male <- round(x$pooled_age_ci_lower[has_fish, "male"], digits)
        pooled_df$CI_Upper_Male <- round(x$pooled_age_ci_upper[has_fish, "male"], digits)
        pooled_df$CI_Lower_Female <- round(x$pooled_age_ci_lower[has_fish, "female"], digits)
        pooled_df$CI_Upper_Female <- round(x$pooled_age_ci_upper[has_fish, "female"], digits)
        pooled_df$CI_Lower_Total <- round(x$pooled_age_ci_lower[has_fish, "total"], digits)
        pooled_df$CI_Upper_Total <- round(x$pooled_age_ci_upper[has_fish, "total"], digits)
      }

      print(pooled_df)

      # Summary statistics
      cat("\nSummary:\n")
      total_fish <- sum(x$pooled_age_composition[, "total"])
      male_fish <- sum(x$pooled_age_composition[, "male"])
      female_fish <- sum(x$pooled_age_composition[, "female"])
      unsexed_fish <- sum(x$pooled_age_composition[, "unsexed"])

      # Calculate weighted mean CVs if available
      if (show_cvs && !is.null(x$pooled_age_cv)) {
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
        weights_male <- x$pooled_age_composition[, "male"]
        weights_female <- x$pooled_age_composition[, "female"]
        weights_total <- x$pooled_age_composition[, "total"]

        wmean_cv_male <- calc_weighted_mean_cv(x$pooled_age_cv[, "male"], weights_male)
        wmean_cv_female <- calc_weighted_mean_cv(x$pooled_age_cv[, "female"], weights_female)
        wmean_cv_total <- calc_weighted_mean_cv(x$pooled_age_cv[, "total"], weights_total)

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
    cat("Age Composition (summed across strata):\n")

    # Sum across strata (3rd dimension)
    pooled_ac <- apply(x$age_composition, c(1, 2), sum)

    # Only show ages with fish
    has_fish <- pooled_ac[, "total"] > 0

    if (any(has_fish)) {
      pooled_df <- data.frame(
        Age = x$ages[has_fish],
        Male = round(pooled_ac[has_fish, "male"], digits),
        Female = round(pooled_ac[has_fish, "female"], digits),
        Unsexed = round(pooled_ac[has_fish, "unsexed"], digits),
        Total = round(pooled_ac[has_fish, "total"], digits)
      )

      print(pooled_df)

      # Summary statistics
      cat("\nSummary:\n")
      total_fish <- sum(pooled_ac[, "total"])
      male_fish <- sum(pooled_ac[, "male"])
      female_fish <- sum(pooled_ac[, "female"])
      unsexed_fish <- sum(pooled_ac[, "unsexed"])
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
    cat("No fish found in specified age range.\n")
  }

  # Show by stratum if multiple strata
  if (length(x$strata_names) > 1) {
    cat("\nBy Stratum:\n")

    # Use appropriate field based on result type
    ac_data <- x$age_composition

    for (i in seq_along(x$strata_names)) {
      stratum_total <- sum(ac_data[, "total", i])
      if (stratum_total > 0) {
        cat(
          "\nStratum:", x$strata_names[i],
          "(Total:", format(round(stratum_total), big.mark = ","), "fish)\n"
        )

        stratum_has_fish <- ac_data[, "total", i] > 0
        if (any(stratum_has_fish)) {
          stratum_df <- data.frame(
            Age = x$ages[stratum_has_fish],
            Male = round(ac_data[stratum_has_fish, "male", i], digits),
            Female = round(ac_data[stratum_has_fish, "female", i], digits),
            Unsexed = round(ac_data[stratum_has_fish, "unsexed", i], digits),
            Total = round(ac_data[stratum_has_fish, "total", i], digits)
          )

          if (show_cvs && !is.null(x$age_cvs)) {
            stratum_df$CV_Total <- round(x$age_cvs[stratum_has_fish, "total", i] * 100, 1)

            # Add confidence intervals for stratum if available
            if (!is.null(x$age_ci_lower) && !is.null(x$age_ci_upper)) {
              stratum_df$CI_Lower_Male <- round(x$age_ci_lower[stratum_has_fish, "male", i], digits)
              stratum_df$CI_Upper_Male <- round(x$age_ci_upper[stratum_has_fish, "male", i], digits)
              stratum_df$CI_Lower_Female <- round(x$age_ci_lower[stratum_has_fish, "female", i], digits)
              stratum_df$CI_Upper_Female <- round(x$age_ci_upper[stratum_has_fish, "female", i], digits)
              stratum_df$CI_Lower_Total <- round(x$age_ci_lower[stratum_has_fish, "total", i], digits)
              stratum_df$CI_Upper_Total <- round(x$age_ci_upper[stratum_has_fish, "total", i], digits)
            }
          } else if (!is.null(x$age_ci_lower) && !is.null(x$age_ci_upper) && is_bootstrap) {
            # Add confidence intervals even if CVs are not shown
            stratum_df$CI_Lower_Male <- round(x$age_ci_lower[stratum_has_fish, "male", i], digits)
            stratum_df$CI_Upper_Male <- round(x$age_ci_upper[stratum_has_fish, "male", i], digits)
            stratum_df$CI_Lower_Female <- round(x$age_ci_lower[stratum_has_fish, "female", i], digits)
            stratum_df$CI_Upper_Female <- round(x$age_ci_upper[stratum_has_fish, "female", i], digits)
            stratum_df$CI_Lower_Total <- round(x$age_ci_lower[stratum_has_fish, "total", i], digits)
            stratum_df$CI_Upper_Total <- round(x$age_ci_upper[stratum_has_fish, "total", i], digits)
          }

          print(stratum_df)

          # Add weighted mean CV for this stratum if available
          if (show_cvs && !is.null(x$age_cvs) && is_bootstrap) {
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
            stratum_weights_total <- ac_data[, "total", i]
            stratum_wmean_cv <- calc_weighted_mean_cv(x$age_cvs[, "total", i], stratum_weights_total)

            if (!is.na(stratum_wmean_cv)) {
              cat("Weighted Mean CV:", round(stratum_wmean_cv * 100, 1), "%\n")
            }
          }
        }
      }
    }
  }

  cat("\n")

  # Report interpolation information if available
  if (!is.null(x$interpolation_info)) {
    interpolation_table <- x$interpolation_info

    # Check if interpolation table is the new format (data.frame)
    if (is.data.frame(interpolation_table)) {
      # New table format - check if any interpolation was used
      non_original_methods <- interpolation_table[interpolation_table$method != "Original key data", ]

      if (nrow(non_original_methods) > 0) {
        cat("Interpolation Summary:\n")
        cat("===================\n")

        # Print the table in a readable format
        if ("sex" %in% names(interpolation_table)) {
          # Sex-specific table
          cat("Method details by sex:\n")
          print(interpolation_table)
        } else {
          # Single key table
          cat("Method details:\n")
          for (i in seq_len(nrow(interpolation_table))) {
            method_info <- interpolation_table[i, ]
            if (method_info$method != "Original key data") {
              cat(sprintf(
                "  %s: %d lengths (%s)\n",
                method_info$method, method_info$count, method_info$lengths
              ))
            }
          }
        }
        cat("\n")
      }
    } else {
      # Legacy list format - maintain backward compatibility
      interpolation_info <- interpolation_table

      # Check if any interpolation was used
      has_interpolation <- (
        length(interpolation_info$linear_interpolation) > 0 ||
          length(interpolation_info$tail_extrapolation) > 0 ||
          length(interpolation_info$user_specified_tails) > 0
      )

      if (has_interpolation) {
        cat("Interpolation Summary:\n")
        cat("===================\n")

        if (length(interpolation_info$user_specified_tails) > 0) {
          cat(
            "User-specified tail ages used for lengths:",
            paste(sort(unique(interpolation_info$user_specified_tails)), collapse = ", "), "cm\n"
          )
        }

        if (length(interpolation_info$linear_interpolation) > 0) {
          cat(
            "Linear interpolation used for lengths:",
            paste(sort(unique(interpolation_info$linear_interpolation)), collapse = ", "), "cm\n"
          )
        }

        if (length(interpolation_info$tail_extrapolation) > 0) {
          cat(
            "Tail extrapolation used for lengths:",
            paste(sort(unique(interpolation_info$tail_extrapolation)), collapse = ", "), "cm\n"
          )
        }

        # Report sex-specific details if available
        if (!is.null(interpolation_info$sex_specific_info) && length(interpolation_info$sex_specific_info) > 0) {
          cat("\nSex-specific interpolation details:\n")
          for (sex in names(interpolation_info$sex_specific_info)) {
            sex_info <- interpolation_info$sex_specific_info[[sex]]
            if (length(sex_info$linear_interpolation) > 0 ||
              length(sex_info$tail_extrapolation) > 0 ||
              length(sex_info$user_specified_tails) > 0) {
              cat("  ", toupper(sex), ":\n")
              if (length(sex_info$user_specified_tails) > 0) {
                cat("    User-specified tails:", paste(sort(unique(sex_info$user_specified_tails)), collapse = ", "), "cm\n")
              }
              if (length(sex_info$linear_interpolation) > 0) {
                cat("    Linear interpolation:", paste(sort(unique(sex_info$linear_interpolation)), collapse = ", "), "cm\n")
              }
              if (length(sex_info$tail_extrapolation) > 0) {
                cat("    Tail extrapolation:", paste(sort(unique(sex_info$tail_extrapolation)), collapse = ", "), "cm\n")
              }
            }
          }
        }
        cat("\n")
      } else {
        cat("Age-length key covered all length bins - no interpolation required.\n\n")
      }
    }
  }

  cat("Use plot(x) to visualise age compositions\n")
  if (!is.null(x$n_bootstraps) && x$n_bootstraps > 0) {
    cat("Use calculate_multinomial_n(x) to estimate effective sample sizes\n")
  }
}
