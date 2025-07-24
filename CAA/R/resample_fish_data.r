##' Resample Fish Data for Bootstrap (Sex-Aware)
##'
##' Resamples fish data for bootstrap analysis by resampling samples within each stratum and fish within each sample, with replacement. Sex columns are preserved.
##'
##' @param fish_data A data frame containing fish measurement data with columns such as stratum, sample_id, length, male, female, unsexed, total, sample_weight_kg, total_catch_weight_kg
##'
##' @return A data frame of resampled fish data with new sample IDs for each bootstrap replicate.
##'
##' @details
##' The function performs two-stage resampling:
##'   \enumerate{
##'     \item Samples are resampled with replacement within each stratum.
##'     \item Fish within each sample are resampled with replacement if more than one fish is present.
##'   }
##' New sample IDs are assigned in the format "stratum_boot_i" for each bootstrap replicate. Sex columns are retained.
##'
##' @examples
##' \dontrun{
##' fish_data <- data.frame(
##'   stratum = c("A", "A", "B", "B"),
##'   sample_id = c(1, 1, 2, 2),
##'   length = c(20, 25, 22, 28),
##'   male = c(2, 1, 3, 2),
##'   female = c(2, 1, 2, 1),
##'   unsexed = c(1, 1, 2, 1),
##'   total = c(5, 3, 7, 4)
##' )
##' resampled <- resample_fish_data(fish_data)
##' }
##'
##' @export
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
    for (i in seq_along(resampled_samples)) {
      sample_data <- stratum_data[stratum_data$sample_id == resampled_samples[i], ]

      # Resample fish within sample
      if (nrow(sample_data) > 1) {
        resampled_indices <- sample(seq_len(nrow(sample_data)), nrow(sample_data), replace = TRUE)
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
