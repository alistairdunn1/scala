#' Calculate age compositions using a shared model-assignment workflow
#'
#' @description Calculates scaled age compositions from assigned ages and optionally
#'   repeats age assignment during hierarchical bootstrap resampling.
#' @inheritParams calculate_age_compositions_from_weight
#' @param model Optional fitted cohort, ordinal, multinomial or otolith-weight model,
#'   or traditional age-length key accepted by \code{\link{assign_ages}}
#' @param length_bin_size Optional traditional-key bin width passed to
#'   \code{\link{assign_ages}} during bootstrap assignment
#' @return List of scaled age compositions, proportions, bootstrap uncertainty
#'   summaries and age, sex and stratum metadata
#' @details The input age column supplies the point composition. When model is
#'   supplied, each bootstrap sample receives random ages through assign_ages,
#'   with predict_missing = TRUE. This includes conditional assignment variability
#'   and sampling variability, but does not refit the model or propagate fitted
#'   coefficient or key-proportion uncertainty. The model has no effect when
#'   bootstraps = 0. Without a model, supplied ages are resampled.
#'
#' Model-specific cohort, ordinal and multinomial composition functions delegate
#' to this implementation. Set age_range and plus_group_age consistently with
#' any plus group used for fitting; a pooled age category cannot be split.
#' @seealso \code{\link{assign_ages}}, \code{\link{calculate_age_compositions_from_cohort}},
#'   \code{\link{calculate_age_compositions_from_ordinal}},
#'   \code{\link{calculate_age_compositions_from_multinomial}}
#' @export
calculate_age_compositions_from_model <- function(fish_data,
                                                   strata_data,
                                                   age_range,
                                                   lw_params_male,
                                                   lw_params_female,
                                                   lw_params_unsexed,
                                                   bootstraps = 300,
                                                   plus_group_age = TRUE,
                                                   minus_group_age = FALSE,
                                                   model = NULL,
                                                   verbose = TRUE, length_bin_size = NULL) {
  # Validate inputs
  if (!is.data.frame(fish_data)) {
    stop("fish_data must be a data frame")
  }

  if (!is.data.frame(strata_data)) {
    stop("strata_data must be a data frame")
  }

  # Check for age column
  if (!"age" %in% names(fish_data)) {
    stop("fish_data must contain 'age' column. Use assign_ages() first.")
  }

  # Check required columns for fish_data
  required_fish_cols <- c("age", "length", "stratum", "sample_id", "male", "female", "unsexed")
  missing_fish_cols <- setdiff(required_fish_cols, names(fish_data))
  if (length(missing_fish_cols) > 0) {
    stop("fish_data is missing required columns: ", paste(missing_fish_cols, collapse = ", "))
  }

  # Determine scaling type
  if ("sample_weight_kg" %in% names(fish_data) && "total_catch_weight_kg" %in% names(fish_data)) {
    scaling_type <- "weight"
    if (!"stratum_total_catch_kg" %in% names(strata_data)) {
      stop("strata_data must contain 'stratum_total_catch_kg' for weight-based scaling")
    }
  } else if ("sample_area_km2" %in% names(fish_data) && "catch_density_kg_km2" %in% names(fish_data)) {
    scaling_type <- "density"
    if (!"stratum_area_km2" %in% names(strata_data)) {
      stop("strata_data must contain 'stratum_area_km2' for density-based scaling")
    }
  } else {
    stop(
      "fish_data must contain either (sample_weight_kg, total_catch_weight_kg) for weight-based ",
      "or (sample_area_km2, catch_density_kg_km2) for density-based scaling"
    )
  }

  # Validate strata
  fish_strata <- unique(fish_data$stratum)
  strata_data_strata <- unique(strata_data$stratum)

  missing_strata <- setdiff(fish_strata, strata_data_strata)
  if (length(missing_strata) > 0) {
    stop(
      "The following strata in fish_data are not present in strata_data: ",
      paste(missing_strata, collapse = ", ")
    )
  }

  if (!is.null(model)) {
    if (!inherits(model, c("cohort_alk", "ordinal_alk", "multinomial_alk", "weight_age_model", "age_length_key")) && !is.data.frame(model) && !is.list(model)) {
      stop("model must be a supported fitted model or age-length key")
    }
  }

  # Remove fish with NA age from composition calculations
  n_na_age <- sum(is.na(fish_data$age))
  if (n_na_age > 0) {
    message("Removing ", n_na_age, " fish with NA age from composition calculations.")
    fish_data <- fish_data[!is.na(fish_data$age), ]
  }

  # Calculate total column if not present
  if (!"total" %in% names(fish_data)) {
    fish_data$total <- fish_data$male + fish_data$female + fish_data$unsexed
  }

  # Set up age bins
  ages <- age_range[1]:age_range[2]
  n_ages <- length(ages)
  strata_names <- unique(fish_data$stratum)
  n_strata <- length(strata_names)

  if (verbose) {
    cat("Calculating age compositions using cohort model predictions...\n")
    cat("Age range:", age_range[1], "to", age_range[2], "\n")
    cat("Scaling method:", scaling_type, "\n")
    cat("Number of strata:", n_strata, "\n")
  }

  # Calculate main age composition
  main_ac <- calculate_age_compositions_core(
    fish_data, strata_data, age_range,
    plus_group_age, minus_group_age, scaling_type,
    lw_params_male, lw_params_female, lw_params_unsexed,
    ages, strata_names
  )

  # Calculate pooled results (always needed)
  pooled_ac <- apply(main_ac, c(1, 2), sum)

  # Calculate proportions (always needed)
  proportions <- main_ac
  pooled_proportions <- pooled_ac

  stratum_totals <- apply(main_ac[, "total", , drop = FALSE], 3, sum)
  meas <- c("male", "female", "unsexed", "total")

  for (i in seq_len(n_strata)) {
    if (stratum_totals[i] > 0) {
      proportions[, meas, i] <- main_ac[, meas, i] / stratum_totals[i]
    }
  }
  proportions[, "composition", ] <- array(rep(ages, times = n_strata), dim = c(n_ages, n_strata))

  pooled_total <- sum(pooled_ac[, "total"])
  if (pooled_total > 0) {
    pooled_proportions[, meas] <- pooled_ac[, meas] / pooled_total
  }
  pooled_proportions[, "composition"] <- ages

  if (bootstraps == 0) {
    if (verbose) cat("No bootstrap uncertainty estimation requested.\n")
    result <- list(
      age_composition = main_ac,
      age_proportions = proportions,
      pooled_age_composition = pooled_ac,
      pooled_age_proportions = pooled_proportions,
      age_cvs = NA, age_proportions_cvs = NA,
      pooled_age_cv = NA, pooled_age_proportions_cv = NA,
      age_ci_lower = NA, age_ci_upper = NA,
      pooled_age_ci_lower = NA, pooled_age_ci_upper = NA,
      age_proportions_ci_lower = NA, age_proportions_ci_upper = NA,
      pooled_age_proportions_ci_lower = NA, pooled_age_proportions_ci_upper = NA,
      ages = ages, strata_names = strata_names, n_bootstraps = 0,
      plus_group_age = plus_group_age, minus_group_age = minus_group_age,
      has_sex_data = TRUE, scaling_type = scaling_type
    )
    class(result) <- "age_composition"
    return(result)
  }

  # Hierarchical bootstrap
  if (bootstraps > 0) {
    if (verbose) {
      cat("Running", bootstraps, "bootstrap iterations...\n")
    }

    ac_bootstraps <- array(0, dim = c(n_ages, 5, n_strata, bootstraps))
    dimnames(ac_bootstraps) <- list(
      ages, c("composition", "male", "female", "unsexed", "total"),
      strata_names, seq_len(bootstraps)
    )

    for (b in seq_len(bootstraps)) {
      if (verbose && b %% 50 == 0) cat("Bootstrap iteration", b, "of", bootstraps, "\n")

      boot_data <- resample_fish_data(fish_data)
      if (!is.null(model)) {
        boot_data <- assign_ages(boot_data, model,
                                             method = "random", predict_missing = TRUE, length_bin_size = length_bin_size,
                                             verbose = FALSE)
        boot_data <- boot_data[!is.na(boot_data$age), ]
      }
      boot_ac <- calculate_age_compositions_core(
        boot_data, strata_data, age_range,
        plus_group_age, minus_group_age, scaling_type,
        lw_params_male, lw_params_female, lw_params_unsexed,
        ages, strata_names
      )
      ac_bootstraps[, , , b] <- boot_ac
    }

    # Calculate CVs
    ac_means <- apply(ac_bootstraps[, meas, , , drop = FALSE], c(1, 2, 3), mean, na.rm = TRUE)
    ac_sds <- apply(ac_bootstraps[, meas, , , drop = FALSE], c(1, 2, 3), sd, na.rm = TRUE)
    ac_cvs <- ifelse(ac_means > 0, ac_sds / ac_means, NA_real_)

    # Pooled bootstrap results
    pooled_bootstraps <- apply(ac_bootstraps[, meas, , , drop = FALSE], c(1, 2, 4), sum, na.rm = TRUE)
    pooled_mean <- apply(pooled_bootstraps, c(1, 2), mean, na.rm = TRUE)
    pooled_sd <- apply(pooled_bootstraps, c(1, 2), sd, na.rm = TRUE)
    pooled_cv <- ifelse(pooled_mean > 0, pooled_sd / pooled_mean, NA_real_)

    # Bootstrap proportions
    prop_bootstraps <- ac_bootstraps[, meas, , , drop = FALSE]
    stratum_totals_boot <- apply(ac_bootstraps[, "total", , , drop = FALSE], c(3, 4), sum, na.rm = TRUE)

    for (i in seq_len(n_strata)) {
      for (b in seq_len(bootstraps)) {
        if (is.finite(stratum_totals_boot[i, b]) && stratum_totals_boot[i, b] > 0) {
          prop_bootstraps[, , i, b] <- ac_bootstraps[, meas, i, b] / stratum_totals_boot[i, b]
        } else {
          prop_bootstraps[, , i, b] <- NA_real_
        }
      }
    }

    prop_means <- apply(prop_bootstraps, c(1, 2, 3), mean, na.rm = TRUE)
    prop_sds <- apply(prop_bootstraps, c(1, 2, 3), sd, na.rm = TRUE)
    prop_cvs <- ifelse(prop_means > 0, prop_sds / prop_means, NA_real_)

    # Pooled proportion bootstrap
    pooled_prop_bootstraps <- pooled_bootstraps
    pooled_totals <- apply(pooled_bootstraps[, "total", , drop = FALSE], 3, sum, na.rm = TRUE)
    for (b in seq_len(bootstraps)) {
      if (is.finite(pooled_totals[b]) && pooled_totals[b] > 0) {
        pooled_prop_bootstraps[, , b] <- pooled_bootstraps[, , b] / pooled_totals[b]
      } else {
        pooled_prop_bootstraps[, , b] <- NA_real_
      }
    }

    pooled_prop_mean <- apply(pooled_prop_bootstraps, c(1, 2), mean, na.rm = TRUE)
    pooled_prop_sd <- apply(pooled_prop_bootstraps, c(1, 2), sd, na.rm = TRUE)
    pooled_prop_cv <- ifelse(pooled_prop_mean > 0, pooled_prop_sd / pooled_prop_mean, NA_real_)

    # Confidence intervals
    ac_ci_lower <- apply(ac_bootstraps[, meas, , , drop = FALSE], c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
    ac_ci_upper <- apply(ac_bootstraps[, meas, , , drop = FALSE], c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

    pooled_ci_lower <- apply(pooled_bootstraps, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
    pooled_ci_upper <- apply(pooled_bootstraps, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)

    prop_ci_lower <- apply(prop_bootstraps, c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
    prop_ci_upper <- apply(prop_bootstraps, c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

    pooled_prop_ci_lower <- apply(pooled_prop_bootstraps, c(1, 2), quantile, probs = 0.025, na.rm = TRUE)
    pooled_prop_ci_upper <- apply(pooled_prop_bootstraps, c(1, 2), quantile, probs = 0.975, na.rm = TRUE)
  }

  # Construct result
  result <- list(
    age_composition = main_ac,
    age_proportions = proportions,
    pooled_age_composition = pooled_ac,
    pooled_age_proportions = pooled_proportions,
    age_cvs = ac_cvs,
    age_proportions_cvs = prop_cvs,
    pooled_age_cv = pooled_cv,
    pooled_age_proportions_cv = pooled_prop_cv,
    age_ci_lower = ac_ci_lower,
    age_ci_upper = ac_ci_upper,
    pooled_age_ci_lower = pooled_ci_lower,
    pooled_age_ci_upper = pooled_ci_upper,
    age_proportions_ci_lower = prop_ci_lower,
    age_proportions_ci_upper = prop_ci_upper,
    pooled_age_proportions_ci_lower = pooled_prop_ci_lower,
    pooled_age_proportions_ci_upper = pooled_prop_ci_upper,
    ages = ages,
    strata_names = strata_names,
    n_bootstraps = bootstraps,
    plus_group_age = plus_group_age,
    minus_group_age = minus_group_age,
    has_sex_data = TRUE,
    scaling_type = scaling_type
  )

  class(result) <- "age_composition"
  return(result)
}


#' @title Core Age Composition Calculation (Internal)
#' @description Internal function to calculate age compositions with scaling.
#'   Uses the same scaling methodology as calculate_length_compositions_core,
#'   but bins by age instead of length.
#' @keywords internal
calculate_age_compositions_core <- function(fish_data, strata_data, age_range,
                                            plus_group_age, minus_group_age, scaling_type,
                                            lw_params_male, lw_params_female, lw_params_unsexed,
                                            ages, strata_names) {
  n_ages <- length(ages)
  n_strata <- length(strata_names)

  # Initialise result array
  age_comp <- array(0, dim = c(n_ages, 5, n_strata))
  dimnames(age_comp) <- list(
    ages,
    c("composition", "male", "female", "unsexed", "total"),
    strata_names
  )

  # Set composition column to age values
  age_comp[, "composition", ] <- ages

  # Apply plus/minus groups to ages
  fish_data$age_binned <- fish_data$age
  if (plus_group_age) {
    fish_data$age_binned[fish_data$age >= max(ages)] <- max(ages)
  }
  if (minus_group_age) {
    fish_data$age_binned[fish_data$age <= min(ages)] <- min(ages)
  }

  # Pre-compute fish weights for all data using vectorized operations
  # (same approach as calculate_length_compositions_core)
  fish_data$fish_weight <- with(
    fish_data,
    male * lw_params_male["a"] * (length^lw_params_male["b"]) / 1000 +
      female * lw_params_female["a"] * (length^lw_params_female["b"]) / 1000 +
      unsexed * lw_params_unsexed["a"] * (length^lw_params_unsexed["b"]) / 1000
  )

  # Pre-compute weight by sample for all data
  weight_by_sample <- aggregate(fish_weight ~ sample_id, data = fish_data, sum)
  names(weight_by_sample)[2] <- "observed_weight_kg"

  # Process each stratum
  for (s in seq_len(n_strata)) {
    stratum <- strata_names[s]
    stratum_data_subset <- fish_data[fish_data$stratum == stratum, ]

    if (nrow(stratum_data_subset) == 0) next

    if (scaling_type == "weight") {
      # Weight-based scaling (same as length compositions)
      sample_summary <- aggregate(
        cbind(male, female, unsexed, total) ~
          sample_id + total_catch_weight_kg,
        data = stratum_data_subset, sum
      )

      stratum_sample_ids <- unique(stratum_data_subset$sample_id)
      stratum_weights <- weight_by_sample[weight_by_sample$sample_id %in% stratum_sample_ids, ]
      sample_summary <- merge(sample_summary, stratum_weights, by = "sample_id")

      sample_summary$upweight_factor <- sample_summary$total_catch_weight_kg / sample_summary$observed_weight_kg

      stratum_total <- strata_data$stratum_total_catch_kg[strata_data$stratum == stratum][1]
      stratum_sample_total <- sum(sample_summary$total_catch_weight_kg)
      stratum_upweight <- stratum_total / stratum_sample_total

    } else if (scaling_type == "density") {
      # Density-based scaling (same as length compositions)
      sample_summary <- aggregate(
        cbind(male, female, unsexed, total) ~
          sample_id + sample_area_km2 + catch_density_kg_km2,
        data = stratum_data_subset, sum
      )

      stratum_sample_ids <- unique(stratum_data_subset$sample_id)
      stratum_weights <- weight_by_sample[weight_by_sample$sample_id %in% stratum_sample_ids, ]
      sample_summary <- merge(sample_summary, stratum_weights, by = "sample_id")

      sample_summary$expected_weight_kg <- sample_summary$catch_density_kg_km2 * sample_summary$sample_area_km2
      sample_summary$upweight_factor <- sample_summary$expected_weight_kg / sample_summary$observed_weight_kg

      stratum_area <- strata_data$stratum_area_km2[strata_data$stratum == stratum][1]
      stratum_sample_area <- sum(sample_summary$sample_area_km2)
      stratum_upweight <- stratum_area / stratum_sample_area
    }

    # Create lookup table for upweight factors and merge
    upweight_lookup <- sample_summary[, c("sample_id", "upweight_factor")]
    stratum_data_weighted <- merge(stratum_data_subset, upweight_lookup, by = "sample_id", sort = FALSE)
    stratum_data_weighted$total_upweight <- stratum_data_weighted$upweight_factor * stratum_upweight

    # Calculate weighted contributions for each sex category
    stratum_data_weighted$male_weighted <- stratum_data_weighted$male * stratum_data_weighted$total_upweight
    stratum_data_weighted$female_weighted <- stratum_data_weighted$female * stratum_data_weighted$total_upweight
    stratum_data_weighted$unsexed_weighted <- stratum_data_weighted$unsexed * stratum_data_weighted$total_upweight
    stratum_data_weighted$total_weighted <- stratum_data_weighted$total * stratum_data_weighted$total_upweight

    # Filter to valid age range
    valid_age_filter <- stratum_data_weighted$age_binned >= min(ages) &
      stratum_data_weighted$age_binned <= max(ages)
    valid_ages_data <- stratum_data_weighted[valid_age_filter, ]

    if (nrow(valid_ages_data) > 0) {
      # Aggregate by binned age
      age_aggregated <- aggregate(
        cbind(male_weighted, female_weighted, unsexed_weighted, total_weighted) ~ age_binned,
        data = valid_ages_data, sum
      )

      # Update results array using vectorized indexing
      age_indices <- match(age_aggregated$age_binned, ages)
      valid_indices <- !is.na(age_indices)

      if (any(valid_indices)) {
        valid_age_indices <- age_indices[valid_indices]
        valid_aggregated <- age_aggregated[valid_indices, ]

        age_comp[valid_age_indices, "male", s] <- valid_aggregated$male_weighted
        age_comp[valid_age_indices, "female", s] <- valid_aggregated$female_weighted
        age_comp[valid_age_indices, "unsexed", s] <- valid_aggregated$unsexed_weighted
        age_comp[valid_age_indices, "total", s] <- valid_aggregated$total_weighted
      }
    }

    # Handle plus group: fish with ages beyond max
    if (plus_group_age) {
      max_age_idx <- n_ages
      plus_filter <- stratum_data_weighted$age_binned > max(ages) &
        !(stratum_data_weighted$age_binned %in% ages)
      # Note: age_binned was already capped to max(ages) above,
      # so plus group fish are already included in the main aggregation
    }

    # Handle minus group: fish with ages below min
    if (minus_group_age) {
      # Note: age_binned was already floored to min(ages) above,
      # so minus group fish are already included in the main aggregation
    }
  }

  return(age_comp)
}
