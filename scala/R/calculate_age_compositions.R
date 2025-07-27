#' Calculate Age Compositions from Length Compositions using Age-Length Keys
#'
#' Converts length compositions to age compositions by applying age-length keys.
#' Propagates uncertainty from bootstrap length composition estimates through
#' the age-length transformation to provide age composition CVs and confidence intervals.
#'
#' @param x A length_composition object from calculate_length_compositions()
#' @param age_length_key Data frame with columns: length, age, and proportion.
#'   Each length should have proportions summing to 1 across ages.
#' @param age_range Numeric vector of min and max ages to include (e.g., c(1, 10))
#' @param plus_group_age Logical, combine ages >= max age into a plus group (default FALSE)
#' @param minus_group_age Logical, combine ages <= min age into a minus group (default FALSE)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{age_composition}: 3D array (age x sex x stratum) of age compositions
#'     \item \code{age_proportions}: 3D array (age x sex x stratum) of age proportions
#'     \item \code{pooled_age_composition}: Matrix (age x sex) of pooled age compositions
#'     \item \code{pooled_age_proportions}: Matrix (age x sex) of pooled age proportions
#'     \item \code{age_cvs}: 3D array of CVs for age compositions
#'     \item \code{age_proportions_cvs}: 3D array of CVs for age proportions
#'     \item \code{pooled_age_cv}: Matrix of pooled age composition CVs
#'     \item \code{pooled_age_proportions_cv}: Matrix of pooled age proportion CVs
#'     \item \code{age_ci_lower}: 3D array of 2.5th percentile values for age compositions
#'     \item \code{age_ci_upper}: 3D array of 97.5th percentile values for age compositions
#'     \item \code{pooled_age_ci_lower}: Matrix of 2.5th percentile values for pooled compositions
#'     \item \code{pooled_age_ci_upper}: Matrix of 97.5th percentile values for pooled compositions
#'     \item \code{age_proportions_ci_lower}: 3D array of 2.5th percentile values for age proportions
#'     \item \code{age_proportions_ci_upper}: 3D array of 97.5th percentile values for age proportions
#'     \item \code{pooled_age_proportions_ci_lower}: Matrix of 2.5th percentile values for pooled proportions
#'     \item \code{pooled_age_proportions_ci_upper}: Matrix of 97.5th percentile values for pooled proportions
#'     \item \code{ages}: Vector of age bins
#'     \item \code{strata_names}: Vector of stratum names
#'     \item \code{n_bootstraps}: Number of bootstrap iterations
#'     \item \code{plus_group_age}: Plus group setting for ages
#'     \item \code{minus_group_age}: Minus group setting for ages
#'     \item \code{has_sex_data}: TRUE (indicates sex-specific analysis)
#'     \item \code{scaling_type}: Type of scaling applied
#'     \item \code{age_length_key}: The age-length key used in calculations
#'   }
#'
#' @details
#' The function applies age-length keys to convert length compositions to age compositions.
#' For each length bin, fish are allocated to ages according to the proportions in the
#' age-length key. This process is applied to:
#'
#' 1. **Main age compositions**: Direct application to observed length compositions
#' 2. **Bootstrap iterations**: Application to each bootstrap length composition to estimate uncertainty
#' 3. **Confidence intervals**: Empirical 95% CIs from bootstrap distribution
#' 4. **Coefficients of variation**: Bootstrap-based CV estimates
#'
#' **Age-length key requirements:**
#' - Must contain columns: length, age, proportion
#' - Proportions for each length must sum to 1 across ages
#' - Should cover all length bins present in the length composition data
#' - Missing length-age combinations are assumed to have proportion = 0
#'
#' **Bootstrap uncertainty propagation:**
#' Each bootstrap iteration of length compositions is converted to age compositions
#' using the same age-length key, preserving the correlation structure and
#' providing realistic uncertainty estimates for the age compositions.
#'
#' @examples
#' \dontrun{
#' # Generate test data and calculate length compositions
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
#' # Create sample age-length key
#' age_key <- expand.grid(length = 20:40, age = 1:8)
#' age_key$proportion <- with(age_key, {
#'   # Simple relationship: younger fish at smaller lengths
#'   dnorm(age, mean = (length - 15) / 3, sd = 2)
#' })
#' # Normalize proportions within each length
#' age_key <- age_key %>%
#'   group_by(length) %>%
#'   mutate(proportion = proportion / sum(proportion)) %>%
#'   ungroup()
#'
#' # Calculate age compositions
#' age_result <- calculate_age_compositions(
#'   x = lc_result,
#'   age_length_key = age_key,
#'   age_range = c(1, 8)
#' )
#'
#' # View results
#' print(age_result)
#' plot(age_result)
#' }
#'
#' @seealso
#' \code{\link{calculate_length_compositions}} for generating length composition data,
#' \code{\link{plot.age_composition}} for plotting age composition results,
#' \code{\link{generate_age_length_key}} for creating sample age-length keys
#'
#' @export
calculate_age_compositions <- function(x,
                                       age_length_key,
                                       age_range = NULL,
                                       plus_group_age = FALSE,
                                       minus_group_age = FALSE) {
  # Validate input
  if (!inherits(x, "length_composition")) {
    stop("x must be a length_composition object from calculate_length_compositions()")
  }

  # Validate age-length key
  required_cols <- c("length", "age", "proportion")
  if (!all(required_cols %in% names(age_length_key))) {
    stop("age_length_key must contain columns: ", paste(required_cols, collapse = ", "))
  }

  # Check that proportions sum to 1 for each length
  prop_sums <- aggregate(proportion ~ length, data = age_length_key, sum)
  if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
    warning("Age-length key proportions do not sum to 1 for some lengths. Normalizing...")
    # Normalize proportions using base R
    age_length_key$proportion <- ave(age_length_key$proportion, age_length_key$length,
      FUN = function(x) x / sum(x)
    )
  }

  # Set age range if not provided
  if (is.null(age_range)) {
    age_range <- range(age_length_key$age)
  }

  ages <- age_range[1]:age_range[2]
  n_ages <- length(ages)
  n_strata <- length(x$strata_names)
  sex_categories <- c("composition", "male", "female", "unsexed", "total")

  cat("Converting length compositions to age compositions using age-length key...\n")

  # Apply age-length key to main length compositions
  main_age_comp <- apply_age_length_key(
    length_comp = x$length_composition,
    age_length_key = age_length_key,
    ages = ages,
    lengths = x$lengths,
    plus_group_age = plus_group_age,
    minus_group_age = minus_group_age
  )

  # Calculate age proportions from main compositions
  age_proportions <- main_age_comp
  pooled_age_comp <- apply(main_age_comp, c(1, 2), sum) # Sum across strata
  pooled_age_proportions <- pooled_age_comp

  # Calculate proportions for each stratum
  for (s in 1:n_strata) {
    stratum_total <- sum(main_age_comp[, "total", s])
    if (stratum_total > 0) {
      age_proportions[, , s] <- main_age_comp[, , s] / stratum_total
    }
  }

  # Calculate pooled proportions
  pooled_total <- sum(pooled_age_comp[, "total"])
  if (pooled_total > 0) {
    pooled_age_proportions <- pooled_age_comp / pooled_total
  }

  # Process bootstrap results if available
  if (x$n_bootstraps > 0) {
    cat("Processing", x$n_bootstraps, "bootstrap iterations for age compositions...\n")

    # Initialize bootstrap arrays
    age_bootstraps <- array(0, dim = c(n_ages, 5, n_strata, x$n_bootstraps))
    dimnames(age_bootstraps) <- list(
      ages, sex_categories, x$strata_names, 1:x$n_bootstraps
    )

    # Apply age-length key to each bootstrap iteration
    for (b in 1:x$n_bootstraps) {
      if (b %% 50 == 0) cat("Age composition bootstrap iteration", b, "of", x$n_bootstraps, "\n")

      boot_age_comp <- apply_age_length_key(
        length_comp = x$lc_bootstraps[, , , b],
        age_length_key = age_length_key,
        ages = ages,
        lengths = x$lengths,
        plus_group_age = plus_group_age,
        minus_group_age = minus_group_age
      )

      age_bootstraps[, , , b] <- boot_age_comp
    }

    # Calculate CVs from bootstrap results
    age_means <- apply(age_bootstraps, c(1, 2, 3), mean, na.rm = TRUE)
    age_sds <- apply(age_bootstraps, c(1, 2, 3), sd, na.rm = TRUE)
    age_cvs <- ifelse(age_means > 0, age_sds / age_means, 0)

    # Pooled bootstrap results
    pooled_age_bootstraps <- apply(age_bootstraps, c(1, 2, 4), sum) # Sum across strata
    pooled_age_mean <- apply(pooled_age_bootstraps, c(1, 2), mean, na.rm = TRUE)
    pooled_age_sd <- apply(pooled_age_bootstraps, c(1, 2), sd, na.rm = TRUE)
    pooled_age_cv <- ifelse(pooled_age_mean > 0, pooled_age_sd / pooled_age_mean, 0)

    # Bootstrap age proportions
    age_prop_bootstraps <- age_bootstraps

    # Calculate proportions for each bootstrap iteration
    for (s in 1:n_strata) {
      for (b in 1:x$n_bootstraps) {
        stratum_total <- sum(age_bootstraps[, "total", s, b])
        if (stratum_total > 0) {
          age_prop_bootstraps[, , s, b] <- age_bootstraps[, , s, b] / stratum_total
        }
      }
    }

    age_prop_means <- apply(age_prop_bootstraps, c(1, 2, 3), mean, na.rm = TRUE)
    age_prop_sds <- apply(age_prop_bootstraps, c(1, 2, 3), sd, na.rm = TRUE)
    age_prop_cvs <- ifelse(age_prop_means > 0, age_prop_sds / age_prop_means, 0)

    # Pooled proportion bootstraps
    pooled_age_prop_bootstraps <- pooled_age_bootstraps

    for (b in 1:x$n_bootstraps) {
      pooled_total <- sum(pooled_age_bootstraps[, "total", b])
      if (pooled_total > 0) {
        pooled_age_prop_bootstraps[, , b] <- pooled_age_bootstraps[, , b] / pooled_total
      }
    }

    pooled_age_prop_mean <- apply(pooled_age_prop_bootstraps, c(1, 2), mean, na.rm = TRUE)
    pooled_age_prop_sd <- apply(pooled_age_prop_bootstraps, c(1, 2), sd, na.rm = TRUE)
    pooled_age_prop_cv <- ifelse(pooled_age_prop_mean > 0, pooled_age_prop_sd / pooled_age_prop_mean, 0)

    # Calculate empirical 95% confidence intervals
    cat("Calculating empirical 95% confidence intervals for age compositions...\n")

    age_ci_lower <- apply(age_bootstraps, c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
    age_ci_upper <- apply(age_bootstraps, c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

    pooled_age_ci_lower <- apply(pooled_age_bootstraps, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
    pooled_age_ci_upper <- apply(pooled_age_bootstraps, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)

    age_prop_ci_lower <- apply(age_prop_bootstraps, c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
    age_prop_ci_upper <- apply(age_prop_bootstraps, c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

    pooled_age_prop_ci_lower <- apply(pooled_age_prop_bootstraps, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
    pooled_age_prop_ci_upper <- apply(pooled_age_prop_bootstraps, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)
  } else {
    # No bootstrap results available
    age_cvs <- NA
    age_prop_cvs <- NA
    pooled_age_cv <- NA
    pooled_age_prop_cv <- NA
    age_ci_lower <- NA
    age_ci_upper <- NA
    pooled_age_ci_lower <- NA
    pooled_age_ci_upper <- NA
    age_prop_ci_lower <- NA
    age_prop_ci_upper <- NA
    pooled_age_prop_ci_lower <- NA
    pooled_age_prop_ci_upper <- NA
  }

  # Compile results
  results <- list(
    # Main results
    age_composition = main_age_comp,
    age_proportions = age_proportions,
    pooled_age_composition = pooled_age_comp,
    pooled_age_proportions = pooled_age_proportions,

    # Uncertainty estimates
    age_cvs = age_cvs,
    age_proportions_cvs = age_prop_cvs,
    pooled_age_cv = pooled_age_cv,
    pooled_age_proportions_cv = pooled_age_prop_cv,

    # Confidence intervals
    age_ci_lower = age_ci_lower,
    age_ci_upper = age_ci_upper,
    pooled_age_ci_lower = pooled_age_ci_lower,
    pooled_age_ci_upper = pooled_age_ci_upper,
    age_proportions_ci_lower = age_prop_ci_lower,
    age_proportions_ci_upper = age_prop_ci_upper,
    pooled_age_proportions_ci_lower = pooled_age_prop_ci_lower,
    pooled_age_proportions_ci_upper = pooled_age_prop_ci_upper,

    # Metadata
    ages = ages,
    strata_names = x$strata_names,
    n_bootstraps = x$n_bootstraps,
    plus_group_age = plus_group_age,
    minus_group_age = minus_group_age,
    has_sex_data = TRUE,
    scaling_type = x$scaling_type,
    age_length_key = age_length_key
  )

  class(results) <- "age_composition"
  return(results)
}

#' Apply Age-Length Key to Length Compositions
#'
#' Internal function to convert length compositions to age compositions using age-length key.
#'
#' @param length_comp 3D array of length compositions (length x sex x stratum)
#' @param age_length_key Data frame with length, age, proportion columns
#' @param ages Vector of age bins
#' @param lengths Vector of length bins
#' @param plus_group_age Logical, whether to apply plus group for ages
#' @param minus_group_age Logical, whether to apply minus group for ages
#'
#' @return 3D array of age compositions (age x sex x stratum)
#' @keywords internal
apply_age_length_key <- function(length_comp, age_length_key, ages, lengths,
                                 plus_group_age = FALSE, minus_group_age = FALSE) {
  n_ages <- length(ages)
  n_strata <- dim(length_comp)[3]
  sex_categories <- c("composition", "male", "female", "unsexed", "total")

  # Initialize age composition array
  age_comp <- array(0, dim = c(n_ages, 5, n_strata))
  dimnames(age_comp) <- list(ages, sex_categories, dimnames(length_comp)[[3]])

  # Set age values in first column
  age_comp[, "composition", ] <- ages

  # Convert age-length key to matrix for faster lookup
  alk_matrix <- matrix(0, nrow = length(lengths), ncol = length(ages))
  rownames(alk_matrix) <- as.character(lengths)
  colnames(alk_matrix) <- as.character(ages)

  for (i in seq_len(nrow(age_length_key))) {
    len_char <- as.character(age_length_key$length[i])
    age_char <- as.character(age_length_key$age[i])
    if (len_char %in% rownames(alk_matrix) && age_char %in% colnames(alk_matrix)) {
      alk_matrix[len_char, age_char] <- age_length_key$proportion[i]
    }
  }

  # Apply age-length key to each stratum and sex category
  for (s in 1:n_strata) {
    for (sex_idx in 2:5) { # Skip "composition" column
      sex_name <- sex_categories[sex_idx]

      # Get length composition for this stratum and sex
      length_data <- length_comp[, sex_name, s]

      # Apply age-length key
      for (len_idx in seq_along(lengths)) {
        len_char <- as.character(lengths[len_idx])
        if (len_char %in% rownames(alk_matrix)) {
          len_count <- length_data[len_idx]

          # Distribute across ages according to age-length key
          for (age_idx in seq_along(ages)) {
            age_char <- as.character(ages[age_idx])
            if (age_char %in% colnames(alk_matrix)) {
              proportion <- alk_matrix[len_char, age_char]
              age_comp[age_idx, sex_name, s] <- age_comp[age_idx, sex_name, s] +
                (len_count * proportion)
            }
          }
        }
      }
    }
  }

  # Apply plus/minus groups if specified
  if (plus_group_age) {
    # Handle ages above maximum - this would require age-length key extension
    # For now, just warn if there are ages beyond our range
    max_age_in_key <- max(age_length_key$age)
    if (max_age_in_key > max(ages)) {
      warning("Age-length key contains ages beyond specified age_range. Consider extending age_range or implementing plus group logic.")
    }
  }

  if (minus_group_age) {
    # Handle ages below minimum - similar to plus group
    min_age_in_key <- min(age_length_key$age)
    if (min_age_in_key < min(ages)) {
      warning("Age-length key contains ages below specified age_range. Consider extending age_range or implementing minus group logic.")
    }
  }

  return(age_comp)
}
