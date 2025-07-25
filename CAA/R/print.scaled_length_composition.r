##' Print Scaled Length Composition Results (Sex-Based)
##'
##' Prints a summary of scaled length composition results by sex, including pooled and stratum-specific compositions, proportions, and uncertainty estimates (CVs) if available.
##'
##' @param x An object of class \code{scaled_length_composition} as returned by \code{calculate_scaled_length_compositions}.
##' @param show_cvs Logical. Whether to display coefficient of variation (CV) columns. Default is TRUE.
##' @param show_sexes Logical. Whether to display all sex categories. Default is TRUE.
##' @param digits Integer. Number of digits to round counts and proportions. Default is 2.
##' @param ... Additional arguments (not used).
##'
##' @return Invisibly returns a list of printed data frames for pooled and stratum-specific results. Each data frame contains length composition counts by sex, and optionally CV columns if available.
##'
##' @details
##' The function prints:
##' - Pooled length composition results by sex (male, female, unsexed, total)
##' - Stratum-specific length composition results if multiple strata are present
##' - Summary statistics for total, male, female, and unsexed fish
##' - CV columns if \code{show_cvs = TRUE} and uncertainty estimates are available
##'
##' @examples
##' \dontrun{
##' test_data <- generate_test_data()
##' results <- calculate_scaled_length_compositions(
##'   fish_data = test_data$fish_data,
##'   strata_data = test_data$strata_data,
##'   length_range = c(15, 35),
##'   bootstraps = 100
##' )
##' print(results)
##' }
##'
##' @export
print.scaled_length_composition <- function(x, show_cvs = TRUE, show_sexes = TRUE, digits = 2, ...) {
  cat("Scaled Length Composition Results (Sex-based)\n")
  cat("===========================================\n\n")

  cat("Length range:", min(x$lengths), "to", max(x$lengths), "\n")
  cat("Number of strata:", length(x$strata_names), "\n")
  cat("Bootstrap iterations:", x$n_bootstraps, "\n")
  if (!is.null(x$scaling_type)) {
    scaling_desc <- ifelse(x$scaling_type == "weight", "Weight-based (commercial)", "Density-based (survey)")
    cat("Scaling approach:", scaling_desc, "\n")
  }
  cat("Sex categories: Male, Female, Unsexed, Total\n\n")

  # Show pooled results
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
    }

    print(pooled_df)

    # Summary statistics
    cat("\nSummary:\n")
    total_fish <- sum(x$pooled_length_composition[, "total"])
    male_fish <- sum(x$pooled_length_composition[, "male"])
    female_fish <- sum(x$pooled_length_composition[, "female"])
    unsexed_fish <- sum(x$pooled_length_composition[, "unsexed"])

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
    for (i in seq_along(x$strata_names)) {
      stratum_total <- sum(x$length_composition[, "total", i])
      if (stratum_total > 0) {
        cat(
          "\nStratum:", x$strata_names[i],
          "(Total:", format(round(stratum_total), big.mark = ","), "fish)\n"
        )

        stratum_has_fish <- x$length_composition[, "total", i] > 0
        if (any(stratum_has_fish)) {
          stratum_df <- data.frame(
            Length = x$lengths[stratum_has_fish],
            Male = round(x$length_composition[stratum_has_fish, "male", i], digits),
            Female = round(x$length_composition[stratum_has_fish, "female", i], digits),
            Unsexed = round(x$length_composition[stratum_has_fish, "unsexed", i], digits),
            Total = round(x$length_composition[stratum_has_fish, "total", i], digits)
          )

          if (show_cvs && !is.null(x$lc_cvs)) {
            stratum_df$CV_Total <- round(x$lc_cvs[stratum_has_fish, "total", i] * 100, 1)
          }

          print(stratum_df)
        }
      }
    }
  }
}
