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

