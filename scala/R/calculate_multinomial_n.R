#' Calculate Multinomial Effective Sample Size
#'
#' Calculates the multinomial effective sample size from length or age composition proportions and
#' their coefficients of variation. Can analyze a single combination or all combinations of strata and sex categories.
#'
#' @param x A length_composition object from calculate_length_compositions() or an age_composition object from calculate_age_compositions()
#' @param stratum Character, name of stratum to analyze. If NULL (default), uses pooled data across all strata. Ignored when all = TRUE
#' @param sex Character, sex category to analyze: "male", "female", "unsexed", or "total" (default). Ignored when all = TRUE
#' @param all Logical, whether to calculate for all combinations of strata and sex categories (default FALSE)
#' @param sex_categories Character vector of sex categories to analyze when all = TRUE. Default: c("male", "female", "unsexed", "total")
#' @param include_pooled Logical, whether to include pooled results when all = TRUE (default TRUE)
#' @param remove_outliers Numeric, proportion of outliers to remove (0-1). Default 0.05 removes worst 5 percent of fits
#' @param min_proportion Numeric, minimum proportion threshold to include in analysis (default 0.0001)
#' @param max_cv Numeric, maximum CV threshold to include in analysis (default 5.0)
#' @param trace Logical, whether to show fitting details (default FALSE)
#' @param quiet Logical, whether to suppress individual fitting messages when all = TRUE (default TRUE)
#'
#' @importFrom stats nls coef quantile
#'
#' @return When all = FALSE: Named list containing:
#'   \itemize{
#'     \item \code{effective_n}: The estimated multinomial effective sample size
#'     \item \code{proportions}: Vector of proportions used in the analysis
#'     \item \code{cvs}: Vector of CVs used in the analysis
#'     \item \code{n_bins}: Number of length or age bins included in the analysis
#'     \item \code{fit_summary}: Summary of the nonlinear model fit
#'   }
#'
#'   When all_combinations = TRUE: Data frame with columns:
#'   \itemize{
#'     \item \code{stratum}: Stratum name ("Pooled" for pooled results)
#'     \item \code{sex}: Sex category
#'     \item \code{effective_n}: Estimated effective sample size
#'     \item \code{n_bins}: Number of length or age bins used
#'     \item \code{fit_quality}: Model fit quality (residual standard error)
#'   }
#'
#' @details
#' The function fits a nonlinear model based on the multinomial distribution relationship:
#' CV = sqrt(n * P * (1 - P)) / (n * P)
#'
#' where:
#' - CV is the coefficient of variation
#' - P is the proportion
#' - n is the effective sample size (parameter to be estimated)
#'
#' Bins with very small proportions or very large CVs are excluded as they
#' can unduly influence the fit. An optional outlier removal step can be applied
#' to improve the robustness of the estimate.
#'
#' @examples
#' \dontrun{
#' # For length compositions
#' test_data <- generate_test_data()
#' lc_result <- calculate_length_compositions(
#'   fish_data = test_data$fish_data,
#'   strata_data = test_data$strata_data,
#'   length_range = c(20, 40),
#'   lw_params_male = c(a = 0.01, b = 3.0),
#'   lw_params_female = c(a = 0.01, b = 3.0),
#'   lw_params_unsexed = c(a = 0.01, b = 3.0),
#'   bootstraps = 100
#' )
#'
#' # Calculate effective sample size for total (pooled across strata)
#' eff_n <- calculate_multinomial_n(lc_result, sex = "total")
#' print(eff_n$effective_n)
#'
#' # Calculate for specific stratum and sex
#' eff_n_male_a <- calculate_multinomial_n(lc_result, stratum = "A", sex = "male")
#'
#' # Calculate for all combinations
#' all_n <- calculate_multinomial_n(lc_result, all_combinations = TRUE)
#' print(all_n)
#'
#' # Calculate only for total sex category, all strata
#' total_n <- calculate_multinomial_n(lc_result,
#'   all_combinations = TRUE,
#'   sex_categories = "total"
#' )
#'
#' # For age compositions
#' age_key <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 10),
#'   growth_model = "von_bertalanffy",
#'   params = list(L_inf = 35, k = 0.3, t0 = -0.1)
#' )
#' ac_result <- calculate_age_compositions(lc_result, age_key)
#' eff_n_age <- calculate_multinomial_n(ac_result, sex = "total")
#' all_n_age <- calculate_multinomial_n(ac_result, all_combinations = TRUE)
#' }
#'
#' @references
#' Pennington, M. 1996. Estimating the mean and variance from highly skewed marine data.
#' Fisheries Bulletin 94: 498-505.
#'
#' @seealso
#' \code{\link{calculate_length_compositions}} for generating length composition data,
#' \code{\link{calculate_age_compositions}} for generating age composition data,
#' \code{\link{get_summary}} for summary statistics including CVs
#'
#' @export
calculate_multinomial_n <- function(x,
                                    stratum = NULL,
                                    sex = "total",
                                    all = FALSE,
                                    sex_categories = c("male", "female", "unsexed", "total"),
                                    include_pooled = TRUE,
                                    remove_outliers = 0.05,
                                    min_proportion = 0.0001,
                                    max_cv = 5.0,
                                    trace = FALSE,
                                    quiet = TRUE) {
  # Validate input
  if (!inherits(x, c("length_composition", "age_composition"))) {
    stop("x must be a length_composition or age_composition object")
  }

  if (x$n_bootstraps == 0) {
    stop("Bootstrap results are required to calculate CVs. Please run with bootstraps > 0.")
  }

  # If all_combinations = TRUE, delegate to the all combinations logic
  if (all) {
    return(calculate_all_combinations_internal(
      x, sex_categories, include_pooled,
      remove_outliers, min_proportion, max_cv, quiet
    ))
  }

  # Single combination logic (original function behavior)
  # Validate sex parameter
  valid_sexes <- c("male", "female", "unsexed", "total")
  if (!sex %in% valid_sexes) {
    stop("sex must be one of: ", paste(valid_sexes, collapse = ", "))
  }

  # Use internal function for single combination
  return(calculate_single_combination_internal(
    x, stratum, sex, remove_outliers,
    min_proportion, max_cv, trace
  ))
}

#' Print method for multinomial_n objects
#'
#' @param x A multinomial_n object from calculate_multinomial_n()
#' @param ... Additional arguments (not used)
#' @export
print.multinomial_n <- function(x, ...) {
  cat("Multinomial Effective Sample Size Analysis\n")
  cat("=========================================\n\n")

  cat("Analysis type:", x$analysis_type, "\n")
  cat("Sex category:", x$sex, "\n")
  if (!is.null(x$stratum)) {
    cat("Stratum:", x$stratum, "\n")
  }
  cat("\n")

  cat("Results:\n")
  cat("  Effective sample size (n):", x$effective_n, "\n")
  cat("  Bins used:", x$n_bins, "\n")
  cat("  Proportion range:", sprintf("%.4f - %.4f", min(x$proportions), max(x$proportions)), "\n")
  cat("  CV range:", sprintf("%.3f - %.3f", min(x$cvs), max(x$cvs)), "\n")
  cat("\n")

  cat("Model fit:\n")
  cat("  Residual standard error:", sprintf("%.4f", x$fit_summary$sigma), "\n")
  cat("  Degrees of freedom:", x$fit_summary$df[2], "\n")
}

#' Internal function to calculate all combinations
#' @keywords internal
calculate_all_combinations_internal <- function(x, sex_categories, include_pooled,
                                                remove_outliers, min_proportion, max_cv, quiet) {
  results_list <- list()

  # Get list of strata to analyze
  strata_to_analyze <- if (include_pooled) c("Pooled", x$strata_names) else x$strata_names

  for (stratum in strata_to_analyze) {
    for (sex in sex_categories) {
      tryCatch(
        {
          # Determine if this is pooled or stratum-specific
          stratum_param <- if (stratum == "Pooled") NULL else stratum

          # Calculate effective sample size using the single combination logic
          result <- calculate_single_combination_internal(
            x = x,
            stratum = stratum_param,
            sex = sex,
            remove_outliers = remove_outliers,
            min_proportion = min_proportion,
            max_cv = max_cv,
            trace = FALSE
          )

          # Store results
          results_list[[paste(stratum, sex, sep = "_")]] <- data.frame(
            stratum = stratum,
            sex = sex,
            effective_n = result$effective_n,
            n_bins = result$n_bins,
            fit_quality = round(result$fit_summary$sigma, 4),
            stringsAsFactors = FALSE
          )

          if (!quiet) {
            cat("Completed:", stratum, "-", sex, "- n =", result$effective_n, "\n")
          }
        },
        error = function(e) {
          if (!quiet) {
            cat("Failed:", stratum, "-", sex, "-", e$message, "\n")
          }
          # Store failed result
          results_list[[paste(stratum, sex, sep = "_")]] <- data.frame(
            stratum = stratum,
            sex = sex,
            effective_n = NA,
            n_bins = NA,
            fit_quality = NA,
            stringsAsFactors = FALSE
          )
        }
      )
    }
  }

  # Combine all results
  final_results <- do.call(rbind, results_list)
  rownames(final_results) <- NULL

  # Order by stratum then sex
  final_results$stratum <- factor(final_results$stratum, levels = strata_to_analyze)
  final_results$sex <- factor(final_results$sex, levels = sex_categories)
  final_results <- final_results[order(final_results$stratum, final_results$sex), ]

  class(final_results) <- c("multinomial_n_summary", "data.frame")
  return(final_results)
}

#' Internal function to calculate single combination
#' @keywords internal
calculate_single_combination_internal <- function(x, stratum, sex, remove_outliers,
                                                  min_proportion, max_cv, trace) {
  # Extract proportions and CVs based on stratum specification
  if (is.null(stratum)) {
    # Use pooled data across all strata
    if (inherits(x, "length_composition")) {
      proportions <- x$pooled_proportions[, sex]
      # Check which CV field is available and use it
      if (is.matrix(x$pooled_proportions_cv)) {
        cvs <- x$pooled_proportions_cv[, sex]
      } else if (is.matrix(x$pooled_lc_cv)) {
        # Calculate CVs for proportions from length composition CVs
        # CV_prop ≈ CV_lc when total is stable
        cvs <- x$pooled_lc_cv[, sex]
      } else {
        stop("No valid CV data found. Bootstrap results are required.")
      }
      bins <- x$lengths
      analysis_type <- "pooled"
    } else { # age_composition
      proportions <- x$pooled_age_proportions[, sex]
      if (is.matrix(x$pooled_age_proportions_cv)) {
        cvs <- x$pooled_age_proportions_cv[, sex]
      } else {
        stop("No valid age proportion CV data found. Bootstrap results are required.")
      }
      bins <- x$ages
      analysis_type <- "pooled"
    }
  } else {
    # Use specific stratum
    if (!stratum %in% x$strata_names) {
      stop("stratum '", stratum, "' not found. Available strata: ", paste(x$strata_names, collapse = ", "))
    }
    if (inherits(x, "length_composition")) {
      proportions <- x$proportions[, sex, stratum]
      # Check which CV field is available and use it
      if (is.array(x$proportions_cvs) && length(dim(x$proportions_cvs)) == 3) {
        cvs <- x$proportions_cvs[, sex, stratum]
      } else if (is.array(x$lc_cvs) && length(dim(x$lc_cvs)) == 3) {
        # Use length composition CVs as approximation
        cvs <- x$lc_cvs[, sex, stratum]
      } else {
        stop("No valid CV data found for stratum-specific analysis.")
      }
      bins <- x$lengths
      analysis_type <- paste("stratum", stratum)
    } else { # age_composition
      proportions <- x$age_proportions[, sex, stratum]
      if (is.array(x$age_proportions_cvs) && length(dim(x$age_proportions_cvs)) == 3) {
        cvs <- x$age_proportions_cvs[, sex, stratum]
      } else {
        stop("No valid age proportion CV data found for stratum-specific analysis.")
      }
      bins <- x$ages
      analysis_type <- paste("stratum", stratum)
    }
  }

  # Create data frame and apply filters
  data_df <- data.frame(
    bin = bins,
    proportion = proportions,
    cv = cvs
  )

  # Filter out invalid values
  valid_idx <- (!is.na(data_df$proportion) & data_df$proportion > min_proportion) &
    (!is.na(data_df$cv) & data_df$cv > 0 & data_df$cv <= max_cv)

  if (sum(valid_idx) < 3) {
    bin_type <- if (inherits(x, "length_composition")) "length" else "age"
    stop(
      "Insufficient valid data points for analysis. Need at least 3 ", bin_type, " bins with proportion > ",
      min_proportion, " and 0 < CV <= ", max_cv
    )
  }

  # Extract valid data
  valid_data <- data_df[valid_idx, ]

  # Normalize proportions to sum to 1
  valid_data$proportion <- valid_data$proportion / sum(valid_data$proportion)

  # Define the multinomial relationship as a formula for nls
  # CV = sqrt(n * P * (1 - P)) / (n * P) = sqrt((1-P)/(n*P))
  # log(CV) = 0.5 * (log(1-P) - log(n) - log(P))
  # Rearranging: log(CV) = 0.5 * log(1-P) - 0.5 * log(P) - 0.5 * log(n)

  # Create the model formula
  model_data <- data.frame(
    logcv = log(valid_data$cv),
    P = valid_data$proportion
  )

  # Fit the nonlinear model
  tryCatch(
    {
      # Initial fit
      fit <- nls(logcv ~ 0.5 * log(1 - P) - 0.5 * log(P) - 0.5 * log(n),
        data = model_data,
        start = list(n = 10),
        trace = trace
      )

      n_estimate <- coef(fit)["n"]

      # Calculate predicted CVs for outlier detection if requested
      if (remove_outliers > 0 && remove_outliers < 1) {
        predicted_cv <- sqrt((1 - valid_data$proportion) / (n_estimate * valid_data$proportion))
        residuals_squared <- (predicted_cv - valid_data$cv)^2

        # Remove worst outliers
        outlier_threshold <- quantile(residuals_squared, probs = 1 - remove_outliers)
        keep_idx <- residuals_squared <= outlier_threshold

        if (sum(keep_idx) >= 3) {
          # Refit without outliers
          model_data_clean <- model_data[keep_idx, ]
          valid_data_clean <- valid_data[keep_idx, ]

          fit <- nls(logcv ~ 0.5 * log(1 - P) - 0.5 * log(P) - 0.5 * log(n),
            data = model_data_clean,
            start = list(n = n_estimate),
            trace = trace
          )

          n_estimate <- coef(fit)["n"]
          valid_data <- valid_data_clean
        }
      }

      # Prepare results
      results <- list(
        effective_n = as.numeric(floor(n_estimate)),
        proportions = valid_data$proportion,
        cvs = valid_data$cv,
        n_bins = nrow(valid_data),
        fit_summary = summary(fit),
        analysis_type = analysis_type,
        sex = sex,
        stratum = stratum
      )

      class(results) <- "multinomial_n"
      return(results)
    },
    error = function(e) {
      stop(
        "Failed to fit multinomial model: ", e$message,
        "\nTry adjusting min_proportion, max_cv, or remove_outliers parameters."
      )
    }
  )
}

#' Print method for multinomial_n_summary objects
#'
#' @param x A multinomial_n_summary object from calculate_multinomial_n() when all_combinations = TRUE
#' @param ... Additional arguments (not used)
#' @export
print.multinomial_n_summary <- function(x, ...) {
  cat("Multinomial Effective Sample Size Summary\n")
  cat("========================================\n\n")

  # Print the data frame with nice formatting
  print(as.data.frame(x), row.names = FALSE)

  cat("\n")
  successful_fits <- sum(!is.na(x$effective_n))
  total_fits <- nrow(x)
  cat("Successful fits:", successful_fits, "out of", total_fits, "\n")

  if (successful_fits > 0) {
    cat("Effective n range:", min(x$effective_n, na.rm = TRUE), "-", max(x$effective_n, na.rm = TRUE), "\n")
  }
}
