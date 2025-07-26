#' Resample Fish Data for Bootstrap Analysis
#'
#' Resamples fish data for bootstrap uncertainty estimation by resampling samples within each stratum.
#'
#' @param fish_data Data frame with fish observation data including columns: stratum, sample_id, length, male, female, unsexed, total
#'
#' @return Resampled fish data frame with the same structure as input
#'
#' @details
#' The function performs bootstrap resampling by:
#' 1. Identifying unique samples within each stratum
#' 2. Resampling samples with replacement within each stratum
#' 3. Returning the resampled data while preserving the original data structure
#'
#' This resampling approach maintains the hierarchical structure of the data (strata > samples > fish observations)
#' which is important for proper uncertainty estimation in the length composition calculations.
#'
#' @seealso \code{\link{calculate_length_compositions}} for the main analysis function that uses this resampling,
#'   \code{\link{generate_test_data}} for creating test data to resample
#' @export
resample_fish_data <- function(fish_data) {
  # Optimized resampling using base R functions
  strata <- unique(fish_data$stratum)

  # Process each stratum
  resampled_list <- lapply(strata, function(stratum) {
    stratum_data <- fish_data[fish_data$stratum == stratum, ]

    if (nrow(stratum_data) == 0) {
      return(data.frame())
    } # Handle empty strata

    unique_samples <- unique(stratum_data$sample_id)
    n_samples <- length(unique_samples)

    # Sample with replacement
    resampled_sample_ids <- sample(unique_samples, n_samples, replace = TRUE)

    # Create resampled data for this stratum
    stratum_resampled_list <- lapply(seq_along(resampled_sample_ids), function(i) {
      sample_data <- stratum_data[stratum_data$sample_id == resampled_sample_ids[i], ]

      if (nrow(sample_data) == 0) {
        return(data.frame())
      } # Handle empty samples

      sample_data$sample_id <- paste0(stratum, "_resample_", i)
      sample_data
    })

    # Filter out empty data frames and combine
    valid_samples <- stratum_resampled_list[sapply(stratum_resampled_list, nrow) > 0]
    if (length(valid_samples) > 0) {
      do.call(rbind, valid_samples)
    } else {
      data.frame()
    }
  })

  # Filter out empty data frames and combine
  valid_strata <- resampled_list[sapply(resampled_list, nrow) > 0]
  if (length(valid_strata) > 0) {
    do.call(rbind, valid_strata)
  } else {
    data.frame()
  }
}
