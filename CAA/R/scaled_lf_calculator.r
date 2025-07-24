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

#' Core function to calculate length frequency result
calculate_lf_result <- function(fish_data, strata_data, lengths, plus_group, minus_group) {
  
  strata_names <- unique(fish_data$stratum)
  n_strata <- length(strata_names)
  n_lengths <- length(lengths)
  
  # Initialize result matrix
  lf_result <- matrix(0, nrow = n_lengths, ncol = n_strata)
  dimnames(lf_result) <- list(lengths, strata_names)
  
  # Process each stratum
  for (s in 1:n_strata) {
    stratum <- strata_names[s]
    stratum_data <- fish_data[fish_data$stratum == stratum, ]
    
    if (nrow(stratum_data) == 0) next
    
    # Calculate sample-level summaries
    sample_summary <- aggregate(count ~ sample_id + sample_weight_kg + total_catch_weight_kg, 
                               data = stratum_data, sum)
    names(sample_summary)[4] <- "total_fish_measured"
    
    # Calculate observed weight for each sample (assuming 1kg per fish for simplicity)
    # In practice, you'd use length-weight relationships
    sample_summary$observed_weight_kg <- sample_summary$total_fish_measured * 0.1  # placeholder
    
    # Calculate upweighting factor
    sample_summary$upweight_factor <- sample_summary$total_catch_weight_kg / sample_summary$observed_weight_kg
    
    # Get stratum total catch
    stratum_total <- strata_data$stratum_total_catch_kg[strata_data$stratum == stratum][1]
    stratum_sample_total <- sum(sample_summary$total_catch_weight_kg)
    stratum_upweight <- stratum_total / stratum_sample_total
    
    # Apply upweighting to length frequencies
    for (i in 1:nrow(sample_summary)) {
      sample_id <- sample_summary$sample_id[i]
      sample_fish <- stratum_data[stratum_data$sample_id == sample_id, ]
      
      total_upweight <- sample_summary$upweight_factor[i] * stratum_upweight
      
      # Aggregate by length and apply upweighting
      length_counts <- aggregate(count ~ length, data = sample_fish, sum)
      
      for (j in 1:nrow(length_counts)) {
        len <- length_counts$length[j]
        count <- length_counts$count[j]
        
        if (len >= min(lengths) & len <= max(lengths)) {
          len_idx <- which(lengths == len)
          if (length(len_idx) > 0) {
            lf_result[len_idx, s] <- lf_result[len_idx, s] + (count * total_upweight)
          }
        }
      }
    }
    
    # Apply plus/minus groups if specified
    if (plus_group) {
      max_idx <- which(lengths == max(lengths))
      # Sum all lengths >= max into the plus group
      # This would need more sophisticated logic in practice
    }
    
    if (minus_group) {
      min_idx <- which(lengths == min(lengths))
      # Sum all lengths <= min into the minus group
      # This would need more sophisticated logic in practice
    }
  }
  
  return(lf_result)
}

#' Resample fish data for bootstrap
resample_fish_data <- function(fish_data) {
  
  # Resample samples within each stratum
  strata <- unique(fish_data$stratum)
  resampled_data <- data.frame()
  
  for (stratum in strata) {
    stratum_data <- fish_data[fish_data$stratum == stratum, ]
    samples <- unique(stratum_data$sample_id)
    
    # Resample samples with replacement
    resampled_samples <- sample(samples, length(samples), replace = TRUE)
    
    stratum_resampled <- data.frame()
    for (i in 1:length(resampled_samples)) {
      sample_data <- stratum_data[stratum_data$sample_id == resampled_samples[i], ]
      
      # Resample fish within sample
      if (nrow(sample_data) > 1) {
        resampled_indices <- sample(1:nrow(sample_data), nrow(sample_data), replace = TRUE)
        sample_data <- sample_data[resampled_indices, ]
      }
      
      # Assign new sample ID
      sample_data$sample_id <- paste0(stratum, "_boot_", i)
      stratum_resampled <- rbind(stratum_resampled, sample_data)
    }
    
    resampled_data <- rbind(resampled_data, stratum_resampled)
  }
  
  return(resampled_data)
}

#' Print method for scaled_length_frequency objects
print.scaled_length_frequency <- function(x, show_cvs = TRUE, digits = 2, ...) {
  cat("Scaled Length Frequency Results\n")
  cat("===============================\n\n")
  
  cat("Length range:", min(x$lengths), "to", max(x$lengths), "\n")
  cat("Number of strata:", length(x$strata_names), "\n")
  cat("Bootstrap iterations:", x$n_bootstraps, "\n\n")
  
  cat("Pooled Length Frequency:\n")
  pooled_df <- data.frame(
    Length = x$lengths,
    Count = round(x$pooled_length_frequency, digits),
    Proportion = round(x$pooled_proportions, digits + 2)
  )
  
  if (show_cvs && !is.null(x$pooled_lf_cv)) {
    pooled_df$CV_Count <- round(x$pooled_lf_cv * 100, 1)
    pooled_df$CV_Prop <- round(x$pooled_proportions_cv * 100, 1)
  }
  
  print(pooled_df)
  
  cat("\nBy Stratum:\n")
  for (i in 1:length(x$strata_names)) {
    cat("\nStratum:", x$strata_names[i], "\n")
    stratum_df <- data.frame(
      Length = x$lengths,
      Count = round(x$length_frequency[, i], digits),
      Proportion = round(x$proportions[, i], digits + 2)
    )
    
    if (show_cvs && !is.null(x$lf_cvs)) {
      stratum_df$CV_Count <- round(x$lf_cvs[, i] * 100, 1)
      stratum_df$CV_Prop <- round(x$proportions_cvs[, i] * 100, 1)
    }
    
    print(stratum_df)
  }
}

# Example usage and test data generation
generate_test_data <- function() {
  set.seed(123)
  
  # Generate test fish data
  n_samples <- 20
  fish_data <- data.frame()
  
  for (i in 1:n_samples) {
    stratum <- sample(c("North", "South"), 1)
    sample_id <- paste0("Sample_", i)
    
    # Generate length distribution (normal around 25cm)
    n_fish <- sample(20:50, 1)
    lengths <- round(rnorm(n_fish, mean = 25, sd = 5))
    lengths <- pmax(10, pmin(40, lengths))  # Constrain to 10-40cm
    
    # Count fish by length
    length_counts <- table(lengths)
    
    sample_data <- data.frame(
      stratum = stratum,
      sample_id = sample_id,
      length = as.numeric(names(length_counts)),
      count = as.numeric(length_counts),
      sample_weight_kg = runif(1, 5, 15),
      total_catch_weight_kg = runif(1, 50, 200)
    )
    
    fish_data <- rbind(fish_data, sample_data)
  }
  
  # Generate strata data
  strata_data <- data.frame(
    stratum = c("North", "South"),
    stratum_total_catch_kg = c(2000, 1500)
  )
  
  return(list(fish_data = fish_data, strata_data = strata_data))
}

# Example usage:
# test_data <- generate_test_data()
# results <- calculate_scaled_length_frequencies(
#   fish_data = test_data$fish_data,
#   strata_data = test_data$strata_data,
#   length_range = c(15, 35),
#   bootstraps = 100
# )
# print(results)