##' Print Method for scaled_length_frequency Objects with Sex Categories
##'
##' Prints a summary of scaled length frequency results by sex, including pooled and stratum-specific frequencies, proportions, and uncertainty estimates (CVs) if available.
##'
##' @param x An object of class \code{scaled_length_frequency} as returned by \code{calculate_scaled_length_frequencies}
##' @param show_cvs Logical, whether to display coefficient of variation (CV) columns (default TRUE)
##' @param show_sexes Logical, whether to display all sex categories (default TRUE)
##' @param digits Integer, number of digits to round counts and proportions (default 2)
##' @param ... Additional arguments (not used)
##'
##' @return Invisibly returns the printed data frames.
##'
##' @examples
##' \dontrun{
##' test_data <- generate_test_data()
##' results <- calculate_scaled_length_frequencies(
##'   fish_data = test_data$fish_data,
##'   strata_data = test_data$strata_data,
##'   length_range = c(15, 35),
##'   bootstraps = 100
##' )
##' print(results)
##' }
##'
##' @export
print.scaled_length_frequency <- function(x, show_cvs = TRUE, show_sexes = TRUE, digits = 2, ...) {
  cat("Scaled Length Frequency Results (Sex-based)\n")
  cat("===========================================\n\n")

  cat("Length range:", min(x$lengths), "to", max(x$lengths), "\n")
  cat("Number of strata:", length(x$strata_names), "\n")
  cat("Bootstrap iterations:", x$n_bootstraps, "\n")
  cat("Sex categories: Male, Female, Unsexed, Total\n\n")

  # Show pooled results
  cat("Pooled Length Frequency:\n")

  # Only show lengths with fish
  has_fish <- x$pooled_length_frequency[, "total"] > 0

  if (any(has_fish)) {
    pooled_df <- data.frame(
      Length = x$lengths[has_fish],
      Male = round(x$pooled_length_frequency[has_fish, "male"], digits),
      Female = round(x$pooled_length_frequency[has_fish, "female"], digits),
      Unsexed = round(x$pooled_length_frequency[has_fish, "unsexed"], digits),
      Total = round(x$pooled_length_frequency[has_fish, "total"], digits)
    )

    if (show_cvs && !is.null(x$pooled_lf_cv)) {
      pooled_df$CV_Male <- round(x$pooled_lf_cv[has_fish, "male"] * 100, 1)
      pooled_df$CV_Female <- round(x$pooled_lf_cv[has_fish, "female"] * 100, 1)
      pooled_df$CV_Total <- round(x$pooled_lf_cv[has_fish, "total"] * 100, 1)
    }

    print(pooled_df)

    # Summary statistics
    cat("\nSummary:\n")
    total_fish <- sum(x$pooled_length_frequency[, "total"])
    male_fish <- sum(x$pooled_length_frequency[, "male"])
    female_fish <- sum(x$pooled_length_frequency[, "female"])
    unsexed_fish <- sum(x$pooled_length_frequency[, "unsexed"])

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
      stratum_total <- sum(x$length_frequency[, "total", i])
      if (stratum_total > 0) {
        cat(
          "\nStratum:", x$strata_names[i],
          "(Total:", format(round(stratum_total), big.mark = ","), "fish)\n"
        )

        stratum_has_fish <- x$length_frequency[, "total", i] > 0
        if (any(stratum_has_fish)) {
          stratum_df <- data.frame(
            Length = x$lengths[stratum_has_fish],
            Male = round(x$length_frequency[stratum_has_fish, "male", i], digits),
            Female = round(x$length_frequency[stratum_has_fish, "female", i], digits),
            Unsexed = round(x$length_frequency[stratum_has_fish, "unsexed", i], digits),
            Total = round(x$length_frequency[stratum_has_fish, "total", i], digits)
          )

          if (show_cvs && !is.null(x$lf_cvs)) {
            stratum_df$CV_Total <- round(x$lf_cvs[stratum_has_fish, "total", i] * 100, 1)
          }

          print(stratum_df)
        }
      }
    }
  }
}
