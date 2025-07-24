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

