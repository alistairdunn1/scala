# Scaled Length Frequency Calculator with Bootstrap
# Simplified version based on the catch-at-age package

#' Calculate scaled length frequencies with bootstrap uncertainty
#' 
#' @param fish_data Data frame with columns: stratum, sample_id, length, count, 
#'                  sample_weight_kg, total_catch_weight_kg
#' @param strata_data Data frame with columns: stratum, stratum_total_catch_kg
#' @param length_range Vector of min and max lengths to include
#' @param bootstraps Number of bootstrap iterations (default 300)
#' @param plus_group Logical, combine lengths >= max length (default FALSE)
#' @param minus_group Logical, combine lengths <= min length (default FALSE)
#' 
#' @return List containing scaled length frequencies, CVs, and bootstrap results
calculate_scaled_length_frequencies <- function(fish_data, 
                                               strata_data,
                                               length_range = c(min(fish_data$length), max(fish_data$length)),
                                               bootstraps = 300,
                                               plus_group = FALSE,
                                               minus_group = FALSE) {
  
  # Validate inputs
  required_fish_cols <- c("stratum", "sample_id", "length", "count", 
                         "sample_weight_kg", "total_catch_weight_kg")
  required_strata_cols <- c("stratum", "stratum_total_catch_kg")
  
  if (!all(required_fish_cols %in% names(fish_data))) {
    stop("fish_data missing required columns: ", 
         paste(setdiff(required_fish_cols, names(fish_data)), collapse = ", "))
  }
  
  if (!all(required_strata_cols %in% names(strata_data))) {
    stop("strata_data missing required columns: ", 
         paste(setdiff(required_strata_cols, names(strata_data)), collapse = ", "))
  }
  
  # Set up length bins
  lengths <- length_range[1]:length_range[2]
  n_lengths <- length(lengths)
  strata_names <- unique(fish_data$stratum)
  n_strata <- length(strata_names)
  
  # Calculate main length frequency
  cat("Calculating scaled length frequencies...\n")
  main_lf <- calculate_lf_result(fish_data, strata_data, lengths, plus_group, minus_group)
  
  # Calculate pooled results across all strata
  pooled_lf <- apply(main_lf, 1, sum)
  pooled_proportions <- pooled_lf / sum(pooled_lf)
  
  # Calculate proportions by stratum
  proportions <- main_lf
  for (i in 1:n_strata) {
    stratum_total <- sum(main_lf[, i])
    if (stratum_total > 0) {
      proportions[, i] <- main_lf[, i] / stratum_total
    }
  }
  
  # Bootstrap if requested
  if (bootstraps > 0) {
    cat("Running", bootstraps, "bootstrap iterations...\n")
    
    # Set up bootstrap arrays
    lf_bootstraps <- array(0, dim = c(n_lengths, n_strata, bootstraps))
    dimnames(lf_bootstraps) <- list(lengths, strata_names, 1:bootstraps)
    
    # Run bootstrap iterations
    for (b in 1:bootstraps) {
      if (b %% 50 == 0) cat("Bootstrap iteration", b, "of", bootstraps, "\n")
      
      # Resample and calculate
      boot_data <- resample_fish_data(fish_data)
      boot_lf <- calculate_lf_result(boot_data, strata_data, lengths, plus_group, minus_group)
      lf_bootstraps[, , b] <- boot_lf
    }
    
    # Calculate CVs from bootstrap results
    lf_means <- apply(lf_bootstraps, c(1, 2), mean)
    lf_sds <- apply(lf_bootstraps, c(1, 2), sd)
    lf_cvs <- ifelse(lf_means > 0, lf_sds / lf_means, 0)
    
    # Pooled bootstrap results
    pooled_bootstraps <- apply(lf_bootstraps, c(1, 3), sum)
    pooled_mean <- apply(pooled_bootstraps, 1, mean)
    pooled_sd <- apply(pooled_bootstraps, 1, sd)
    pooled_cv <- ifelse(pooled_mean > 0, pooled_sd / pooled_mean, 0)
    
    # Bootstrap proportions
    prop_bootstraps <- lf_bootstraps
    for (b in 1:bootstraps) {
      for (i in 1:n_strata) {
        stratum_total <- sum(lf_bootstraps[, i, b])
        if (stratum_total > 0) {
          prop_bootstraps[, i, b] <- lf_bootstraps[, i, b] / stratum_total
        }
      }
    }
    
    prop_means <- apply(prop_bootstraps, c(1, 2), mean)
    prop_sds <- apply(prop_bootstraps, c(1, 2), sd)
    prop_cvs <- ifelse(prop_means > 0, prop_sds / prop_means, 0)
    
    # Pooled proportion bootstrap
    pooled_prop_bootstraps <- pooled_bootstraps
    for (b in 1:bootstraps) {
      total <- sum(pooled_bootstraps[, b])
      if (total > 0) {
        pooled_prop_bootstraps[, b] <- pooled_bootstraps[, b] / total
      }
    }
    
    pooled_prop_mean <- apply(pooled_prop_bootstraps, 1, mean)
    pooled_prop_sd <- apply(pooled_prop_bootstraps, 1, sd)
    pooled_prop_cv <- ifelse(pooled_prop_mean > 0, pooled_prop_sd / pooled_prop_mean, 0)
    
  } else {
    lf_bootstraps <- NA
    lf_cvs <- NA
    prop_cvs <- NA
    pooled_cv <- NA
    pooled_prop_cv <- NA
  }
  
  # Format results
  results <- list(
    # Main results
    length_frequency = main_lf,
    proportions = proportions,
    pooled_length_frequency = pooled_lf,
    pooled_proportions = pooled_proportions,
    
    # Uncertainty estimates
    lf_cvs = lf_cvs,
    proportions_cvs = prop_cvs,
    pooled_lf_cv = pooled_cv,
    pooled_proportions_cv = pooled_prop_cv,
    
    # Bootstrap results
    lf_bootstraps = lf_bootstraps,
    
    # Metadata
    lengths = lengths,
    strata_names = strata_names,
    n_bootstraps = bootstraps,
    plus_group = plus_group,
    minus_group = minus_group
  )
  
  class(results) <- "scaled_length_frequency"
  return(results)
}

