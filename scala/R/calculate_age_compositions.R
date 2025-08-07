#' @title Calculate Age Compositions from Length Compositions using Age-Length Keys
#' @description Converts length compositions to age compositions by applying age-length keys.
#'   Propagates uncertainty from bootstrap length composition estimates through
#'   the age-length transformation to provide age composition CVs and confidence intervals.
#'   Supports sex-specific age-length keys for improved biological realism.
#' @param x A length_composition object from calculate_length_compositions()
#' @param age_length_key Either:
#'   \itemize{
#'     \item A single data frame with columns: length, age, and proportion (applied to all sex categories)
#'     \item A named list with elements: male, female, unsexed (each a data frame with length, age, proportion)
#'     \item A complete ALK object from create_alk() (automatically detected)
#'   }
#'   Each length should have proportions summing to 1 across ages.
#' @param age_range Numeric vector of min and max ages to include (e.g., c(1, 10))
#' @param plus_group_age Logical, combine ages >= max age into a plus group (default TRUE)
#' @param minus_group_age Logical, combine ages <= min age into a minus group (default FALSE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#' @importFrom stats aggregate ave quantile
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
#' - Age-length key must cover all length bins present in the length composition data
#' - Missing length-age combinations will result in a warning. Use create_alk() to create a complete key with interpolation
#' - Proportions for each length must sum to 1 across ages (within each sex)
#'
#' **Missing lengths:**
#' If the age-length key does not include all length bins from the length composition data,
#' a warning will be issued with details about missing lengths. For scenarios with missing
#' lengths, use create_alk() to create a complete age-length key with interpolation.
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
#' # Example 3: Using create_alk() for complete coverage with interpolation
#' # Age-length key with limited range (30-35 cm) but length compositions go 20-40 cm
#' limited_age_key <- generate_age_length_key(
#'   length_range = c(30, 35),
#'   age_range = c(3, 6)
#' )
#'
#' # Create complete ALK with interpolation for missing lengths
#' tail_ages <- list(
#'   min_lengths = data.frame(length = c(20, 22, 25), age = c(1, 1, 2)),
#'   max_lengths = data.frame(length = c(38, 40), age = c(7, 8))
#' )
#'
#' complete_alk <- create_alk(
#'   age_length_key = limited_age_key,
#'   lengths = 20:40,
#'   ages = 1:8,
#'   tail_ages = tail_ages
#' )
#'
#' age_result_complete <- calculate_age_compositions(
#'   x = lc_result,
#'   age_length_key = complete_alk,
#'   age_range = c(1, 8)
#' )
#'
#' # Example 4: Limited key without interpolation (will show warning)
#' age_result_limited <- calculate_age_compositions(
#'   x = lc_result,
#'   age_length_key = limited_age_key,
#'   age_range = c(3, 6)
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
#' @export
calculate_age_compositions <- function(x,
                                       age_length_key,
                                       age_range = NULL,
                                       plus_group_age = TRUE,
                                       minus_group_age = FALSE,
                                       verbose = TRUE) {
  # Validate input
  if (!inherits(x, "length_composition")) {
    stop("x must be a length_composition object from calculate_length_compositions()")
  }

  # Determine if sex-specific keys are provided
  # Check if it's from create_alk (has attributes) or determine from structure
  if (!is.null(attr(age_length_key, "is_sex_specific"))) {
    sex_specific_keys <- attr(age_length_key, "is_sex_specific")
  } else {
    sex_specific_keys <- is.list(age_length_key) && !is.data.frame(age_length_key)
  }

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

  # Check for missing lengths in age-length key and warn user
  check_missing_lengths <- function(age_length_key, lengths, sex_specific_keys) {
    if (sex_specific_keys) {
      all_missing <- c()
      for (sex in names(age_length_key)) {
        key_lengths <- unique(age_length_key[[sex]]$length)
        missing_lengths <- setdiff(lengths, key_lengths)
        if (length(missing_lengths) > 0) {
          all_missing <- unique(c(all_missing, missing_lengths))
        }
      }
    } else {
      key_lengths <- unique(age_length_key$length)
      all_missing <- setdiff(lengths, key_lengths)
    }

    if (length(all_missing) > 0) {
      missing_pct <- round(length(all_missing) / length(lengths) * 100, 1)
      warning(
        "Age-length key is missing ", length(all_missing), " of ", length(lengths),
        " length bins (", missing_pct, "%). ",
        "Missing lengths: ", paste(sort(all_missing), collapse = ", "),
        ". Use create_alk() to create a complete age-length key with interpolation."
      )
    }
  }

  if (verbose) cat("Converting length compositions to age compositions using age-length key...\n")

  # Check for missing lengths and warn user
  check_missing_lengths(age_length_key, x$lengths, sex_specific_keys)

  # Apply age-length key to main length compositions
  main_result <- apply_age_length_key(
    length_comp = x$length_composition,
    age_length_key = age_length_key,
    ages = ages,
    lengths = x$lengths,
    plus_group_age = plus_group_age,
    minus_group_age = minus_group_age,
    sex_specific_keys = sex_specific_keys
  )

  main_age_comp <- main_result$age_compositions

  # Get interpolation info from age_length_key attributes if available
  interpolation_table <- attr(age_length_key, "interpolation_info")

  # Calculate age proportions from main compositions (vectorized)
  age_proportions <- main_age_comp
  pooled_age_comp <- apply(main_age_comp, c(1, 2), sum) # Sum across strata

  # Vectorized proportion calculation for strata
  stratum_totals <- apply(main_age_comp[, "total", , drop = FALSE], 3, sum)
  valid_strata <- stratum_totals > 0
  if (any(valid_strata)) {
    for (s in which(valid_strata)) {
      age_proportions[, , s] <- main_age_comp[, , s] / stratum_totals[s]
    }
  }

  # Calculate pooled proportions
  pooled_total <- sum(pooled_age_comp[, "total"])
  pooled_age_proportions <- if (pooled_total > 0) pooled_age_comp / pooled_total else pooled_age_comp

  # Process bootstrap results if available
  if (x$n_bootstraps > 0 && !is.na(x$lc_bootstraps[1])) {
    if (verbose) cat("Processing", x$n_bootstraps, "bootstrap iterations for age compositions...\n")

    # Initialize bootstrap arrays
    age_bootstraps <- array(0, dim = c(n_ages, 5, n_strata, x$n_bootstraps))
    dimnames(age_bootstraps) <- list(
      ages, sex_categories, x$strata_names, 1:x$n_bootstraps
    )

    # Optimized batch processing of bootstrap iterations
    batch_size <- min(50, x$n_bootstraps) # Process in batches for memory efficiency

    for (start_idx in seq(1, x$n_bootstraps, batch_size)) {
      end_idx <- min(start_idx + batch_size - 1, x$n_bootstraps)
      batch_indices <- start_idx:end_idx

      if (verbose && start_idx %% 100 == 1) {
        cat("Processing bootstrap batch", start_idx, "to", end_idx, "of", x$n_bootstraps, "\n")
      }

      # Process batch of bootstraps
      for (b in batch_indices) {
        boot_result <- apply_age_length_key(
          length_comp = x$lc_bootstraps[, , , b],
          age_length_key = age_length_key,
          ages = ages,
          lengths = x$lengths,
          plus_group_age = plus_group_age,
          minus_group_age = minus_group_age,
          sex_specific_keys = sex_specific_keys
        )

        age_bootstraps[, , , b] <- boot_result$age_compositions
      }
    }

    # Highly optimized bootstrap statistics calculation
    if (verbose) cat("Calculating bootstrap statistics...\n")

    # Efficient combined statistics calculation - single pass through data
    calculate_statistics_vectorized <- function(data_array) {
      dims <- length(dim(data_array))

      if (dims == 4) {
        # 4D array: calculate over c(1, 2, 3) (age x sex x stratum)
        margin <- c(1, 2, 3)
      } else if (dims == 3) {
        # 3D array: calculate over c(1, 2) (age x sex)
        margin <- c(1, 2)
      } else {
        stop("Unsupported array dimensions for statistics calculation")
      }

      # Calculate all statistics in one optimized pass
      means_array <- apply(data_array, margin, mean, na.rm = TRUE)
      sds_array <- apply(data_array, margin, sd, na.rm = TRUE)
      cvs_array <- ifelse(means_array > 0, sds_array / means_array, 0)

      # Calculate quantiles efficiently
      quantiles_array <- apply(data_array, margin, quantile,
        probs = c(0.025, 0.975), na.rm = TRUE
      )

      if (dims == 4) {
        return(list(
          means = means_array,
          sds = sds_array,
          cvs = cvs_array,
          ci_lower = quantiles_array[1, , , ],
          ci_upper = quantiles_array[2, , , ]
        ))
      } else {
        return(list(
          means = means_array,
          sds = sds_array,
          cvs = cvs_array,
          ci_lower = quantiles_array[1, , ],
          ci_upper = quantiles_array[2, , ]
        ))
      }
    }

    # Calculate age composition statistics
    age_stats <- calculate_statistics_vectorized(age_bootstraps)
    age_cvs <- age_stats$cvs
    age_ci_lower <- age_stats$ci_lower
    age_ci_upper <- age_stats$ci_upper

    # Pooled bootstrap results (optimized with single operation)
    pooled_age_bootstraps <- apply(age_bootstraps, c(1, 2, 4), sum)
    pooled_stats <- calculate_statistics_vectorized(pooled_age_bootstraps)
    pooled_age_cv <- pooled_stats$cvs
    pooled_age_ci_lower <- pooled_stats$ci_lower
    pooled_age_ci_upper <- pooled_stats$ci_upper

    # Highly optimized proportion calculations using vectorized operations
    if (verbose) cat("Calculating bootstrap proportions...\n")

    # Vectorized proportion calculation function
    calculate_proportions_vectorized <- function(data_array, total_col_name = "total") {
      dims <- dim(data_array)
      result_array <- array(0, dim = dims)
      dimnames(result_array) <- dimnames(data_array)

      if (length(dims) == 4) {
        # Stratum-level calculations - vectorized approach
        total_col_idx <- which(dimnames(data_array)[[2]] == total_col_name)

        # Extract totals for all strata and bootstraps at once
        totals <- data_array[, total_col_idx, , ] # [age, stratum, bootstrap]
        stratum_totals <- apply(totals, c(2, 3), sum) # [stratum, bootstrap]

        # Vectorized division - broadcast totals across ages and sex categories
        for (s in seq_len(dims[3])) { # strata
          for (b in seq_len(dims[4])) { # bootstraps
            if (stratum_totals[s, b] > 0) {
              result_array[, , s, b] <- data_array[, , s, b] / stratum_totals[s, b]
            }
          }
        }
      } else if (length(dims) == 3) {
        # Pooled calculations - more efficient vectorization
        total_col_idx <- which(dimnames(data_array)[[2]] == total_col_name)
        totals <- apply(data_array[, total_col_idx, ], 2, sum) # [bootstrap]

        for (b in seq_len(dims[3])) {
          if (totals[b] > 0) {
            result_array[, , b] <- data_array[, , b] / totals[b]
          }
        }
      }
      return(result_array)
    }

    # Calculate proportions efficiently
    age_prop_bootstraps <- calculate_proportions_vectorized(age_bootstraps)
    pooled_age_prop_bootstraps <- calculate_proportions_vectorized(pooled_age_bootstraps)

    # Calculate proportion statistics using the same vectorized approach
    age_prop_stats <- calculate_statistics_vectorized(age_prop_bootstraps)
    age_prop_cvs <- age_prop_stats$cvs
    age_prop_ci_lower <- age_prop_stats$ci_lower
    age_prop_ci_upper <- age_prop_stats$ci_upper

    pooled_age_prop_stats <- calculate_statistics_vectorized(pooled_age_prop_bootstraps)
    pooled_age_prop_cv <- pooled_age_prop_stats$cvs
    pooled_age_prop_ci_lower <- pooled_age_prop_stats$ci_lower
    pooled_age_prop_ci_upper <- pooled_age_prop_stats$ci_upper
  } else {
    # No bootstrap results available or bootstrap data is missing
    if (x$n_bootstraps > 0 && verbose) {
      cat("Warning: Bootstrap iterations requested but bootstrap data not available from length compositions\n")
    }
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
    sex_specific_keys = sex_specific_keys,
    interpolation_info = interpolation_table
  )

  class(results) <- "age_composition"
  return(results)
}
