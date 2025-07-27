#' Calculate Age Compositions from Length Compositions using Age-Length Keys
#'
#' Converts length compositions to age compositions by applying age-length keys.
#' Propagates uncertainty from bootstrap length composition estimates through
#' the age-length transformation to provide age composition CVs and confidence intervals.
#' Supports sex-specific age-length keys for improved biological realism.
#'
#' @param x A length_composition object from calculate_length_compositions()
#' @param age_length_key Either:
#'   \itemize{
#'     \item A single data frame with columns: length, age, and proportion (applied to all sex categories)
#'     \item A named list with elements: male, female, unsexed (each a data frame with length, age, proportion)
#'   }
#'   Each length should have proportions summing to 1 across ages.
#' @param age_range Numeric vector of min and max ages to include (e.g., c(1, 10))
#' @param plus_group_age Logical, combine ages >= max age into a plus group (default FALSE)
#' @param minus_group_age Logical, combine ages <= min age into a minus group (default FALSE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
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
#'     \item \code{age_length_key}: The age-length key(s) used in calculations
#'     \item \code{sex_specific_keys}: Logical indicating whether sex-specific keys were used
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
#' **Age-length key formats:**
#'
#' *Single key (applied to all sexes):*
#' - Data frame with columns: length, age, proportion
#' - Proportions for each length must sum to 1 across ages
#'
#' *Sex-specific keys:*
#' - Named list with elements: male, female, unsexed
#' - Each element is a data frame with columns: length, age, proportion
#' - Allows for sex-specific growth patterns and maturation
#' - If unsexed fish are present but no unsexed key provided, male key is used
#'
#' **Requirements:**
#' - Should cover all length bins present in the length composition data
#' - Missing length-age combinations are assumed to have proportion = 0
#' - Proportions for each length must sum to 1 across ages (within each sex)
#'
#' **Bootstrap uncertainty propagation:**
#' Each bootstrap iteration of length compositions is converted to age compositions
#' using the same age-length key(s), preserving the correlation structure and
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
#' # Example 1: Single age-length key (applied to all sexes)
#' age_key <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 8),
#'   growth_type = "vonbert"
#' )
#'
#' age_result <- calculate_age_compositions(
#'   x = lc_result,
#'   age_length_key = age_key,
#'   age_range = c(1, 8)
#' )
#'
#' # Example 2: Sex-specific age-length keys
#' # Males mature faster (younger at given length)
#' male_key <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 8),
#'   growth_type = "vonbert",
#'   growth_params = list(linf = 35, k = 0.35, t0 = -0.5)
#' )
#'
#' # Females grow larger/older
#' female_key <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 8),
#'   growth_type = "vonbert",
#'   growth_params = list(linf = 40, k = 0.25, t0 = -0.8)
#' )
#'
#' # Unsexed fish use intermediate growth
#' unsexed_key <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 8),
#'   growth_type = "vonbert",
#'   growth_params = list(linf = 37, k = 0.30, t0 = -0.6)
#' )
#'
#' sex_specific_keys <- list(
#'   male = male_key,
#'   female = female_key,
#'   unsexed = unsexed_key
#' )
#'
#' age_result_sex <- calculate_age_compositions(
#'   x = lc_result,
#'   age_length_key = sex_specific_keys,
#'   age_range = c(1, 8)
#' )
#'
#' # View and plot results
#' print(age_result_sex)
#' plot(age_result_sex)
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
                                       minus_group_age = FALSE,
                                       verbose = TRUE) {
  # Validate input
  if (!inherits(x, "length_composition")) {
    stop("x must be a length_composition object from calculate_length_compositions()")
  }

  # Determine if sex-specific keys are provided
  sex_specific_keys <- is.list(age_length_key) && !is.data.frame(age_length_key)

  if (sex_specific_keys) {
    # Validate sex-specific age-length keys
    required_sexes <- c("male", "female")
    optional_sexes <- c("unsexed")

    if (!all(required_sexes %in% names(age_length_key))) {
      stop("Sex-specific age_length_key must contain at least 'male' and 'female' elements")
    }

    # Check each sex-specific key
    for (sex_name in names(age_length_key)) {
      if (!sex_name %in% c(required_sexes, optional_sexes)) {
        warning("Unknown sex category '", sex_name, "' in age_length_key. Valid options: male, female, unsexed")
      }

      key <- age_length_key[[sex_name]]
      required_cols <- c("length", "age", "proportion")
      if (!all(required_cols %in% names(key))) {
        stop("Age-length key for '", sex_name, "' must contain columns: ", paste(required_cols, collapse = ", "))
      }

      # Check proportions sum to 1 for each length within this sex
      prop_sums <- aggregate(proportion ~ length, data = key, sum)
      if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
        if (verbose) cat("Normalizing age-length key proportions for", sex_name, "...\n")
        age_length_key[[sex_name]]$proportion <- ave(key$proportion, key$length,
          FUN = function(x) x / sum(x)
        )
      }
    }

    # Use male key for unsexed if not provided
    if (!"unsexed" %in% names(age_length_key)) {
      if (verbose) cat("Using male age-length key for unsexed fish...\n")
      age_length_key$unsexed <- age_length_key$male
    }
  } else {
    # Single age-length key validation
    required_cols <- c("length", "age", "proportion")
    if (!all(required_cols %in% names(age_length_key))) {
      stop("age_length_key must contain columns: ", paste(required_cols, collapse = ", "))
    }

    # Check that proportions sum to 1 for each length
    prop_sums <- aggregate(proportion ~ length, data = age_length_key, sum)
    if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
      if (verbose) cat("Normalizing age-length key proportions...\n")
      age_length_key$proportion <- ave(age_length_key$proportion, age_length_key$length,
        FUN = function(x) x / sum(x)
      )
    }
  }

  # Set age range if not provided
  if (is.null(age_range)) {
    if (sex_specific_keys) {
      # Get age range from all sex-specific keys
      all_ages <- unlist(lapply(age_length_key, function(key) key$age))
      age_range <- range(all_ages)
    } else {
      age_range <- range(age_length_key$age)
    }
  }

  ages <- age_range[1]:age_range[2]
  n_ages <- length(ages)
  n_strata <- length(x$strata_names)
  sex_categories <- c("composition", "male", "female", "unsexed", "total")

  if (verbose) cat("Converting length compositions to age compositions using age-length key...\n")

  # Apply age-length key to main length compositions
  main_age_comp <- apply_age_length_key(
    length_comp = x$length_composition,
    age_length_key = age_length_key,
    ages = ages,
    lengths = x$lengths,
    plus_group_age = plus_group_age,
    minus_group_age = minus_group_age,
    sex_specific_keys = sex_specific_keys
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
    if (verbose) cat("Processing", x$n_bootstraps, "bootstrap iterations for age compositions...\n")

    # Initialize bootstrap arrays
    age_bootstraps <- array(0, dim = c(n_ages, 5, n_strata, x$n_bootstraps))
    dimnames(age_bootstraps) <- list(
      ages, sex_categories, x$strata_names, 1:x$n_bootstraps
    )

    # Apply age-length key to each bootstrap iteration
    for (b in 1:x$n_bootstraps) {
      if (verbose && b %% 50 == 0) cat("Age composition bootstrap iteration", b, "of", x$n_bootstraps, "\n")

      boot_age_comp <- apply_age_length_key(
        length_comp = x$lc_bootstraps[, , , b],
        age_length_key = age_length_key,
        ages = ages,
        lengths = x$lengths,
        plus_group_age = plus_group_age,
        minus_group_age = minus_group_age,
        sex_specific_keys = sex_specific_keys
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
    if (verbose) cat("Calculating empirical 95% confidence intervals for age compositions...\n")

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
    age_length_key = age_length_key,
    sex_specific_keys = sex_specific_keys
  )

  class(results) <- "age_composition"
  return(results)
}
