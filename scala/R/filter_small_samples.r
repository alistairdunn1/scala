#' Filter Small Samples from Fisheries Data
#'
#' Creates a filtered version of fisheries data by excluding specified samples
#' identified by the sample size evaluation. This simplified approach removes
#' samples that are explicitly marked for exclusion.
#'
#' @param fish_data A data frame containing fisheries sampling data
#' @param samples_to_exclude A data frame with sample_id and stratum columns
#'   identifying samples to exclude (typically from \code{evaluate_sample_sizes()}).
#'   Common usage: \code{evaluation$non_representative_small_samples} or
#'   \code{evaluation$all_small_samples}
#'
#' @return A list containing:
#'   \describe{
#'     \item{filtered_fish_data}{Filtered fish data with specified samples removed}
#'     \item{excluded_samples}{Data frame of excluded samples}
#'     \item{filter_summary}{Summary of filtering impact}
#'   }
#'
#' @details
#' This function provides a simple filtering mechanism based on explicitly
#' identified samples to exclude. The function includes input validation to
#' ensure proper usage and provide helpful error messages.
#'
#' Use with caution as excluding samples can:
#' \itemize{
#'   \item Improve bootstrap stability and reduce uncertainty estimates
#'   \item Reduce spatial or temporal coverage of the analysis
#'   \item Introduce bias if excluded samples are not randomly distributed
#'   \item Reduce total sample size significantly
#' }
#'
#' \strong{Best Practice}: Always run analyses both with and without filtering
#' to assess sensitivity of results to sample exclusion.
#'
#' @examples
#' \dontrun{
#' # Generate test data and evaluate sample sizes
#' test_data <- generate_test_data()
#' evaluation <- evaluate_sample_sizes(test_data$fish_data)
#'
#' # Filter using non-representative small samples from evaluation
#' filtered_data <- filter_small_samples(
#'   test_data$fish_data,
#'   evaluation$non_representative_small_samples
#' )
#'
#' # Or filter using all small samples
#' filtered_data_all <- filter_small_samples(
#'   test_data$fish_data,
#'   evaluation$all_small_samples
#' )
#'
#' # Compare original vs filtered data
#' print("Original data summary:")
#' print(summary(test_data$fish_data))
#' print("Filtered data summary:")
#' print(summary(filtered_data$filtered_fish_data))
#' print(filtered_data$filter_summary)
#' }
#'
#' @seealso \code{\link{evaluate_sample_sizes}} for sample size evaluation
#'
#' @export
filter_small_samples <- function(fish_data, samples_to_exclude) {
  # Validate inputs
  if (!is.data.frame(samples_to_exclude)) {
    stop(
      "samples_to_exclude must be a data frame. Did you mean to use:\n",
      "  filter_small_samples(fish_data, evaluation$non_representative_small_samples)\n",
      "  or\n",
      "  filter_small_samples(fish_data, evaluation$all_small_samples)?"
    )
  }

  if (!all(c("sample_id", "stratum") %in% colnames(samples_to_exclude))) {
    stop("samples_to_exclude must contain 'sample_id' and 'stratum' columns")
  }

  if (nrow(samples_to_exclude) == 0) {
    message("No samples to exclude. Returning original data.")
    return(list(
      filtered_fish_data = fish_data,
      excluded_samples = samples_to_exclude,
      filter_summary = data.frame(
        metric = c("Original Fish", "Excluded Fish", "Remaining Fish", "% Fish Retained"),
        value = c(nrow(fish_data), 0, nrow(fish_data), 100)
      )
    ))
  }

  # Create exclusion key
  samples_to_exclude$exclude_key <- paste(samples_to_exclude$sample_id, samples_to_exclude$stratum, sep = "_")
  fish_data$exclude_key <- paste(fish_data$sample_id, fish_data$stratum, sep = "_")

  # Filter data
  excluded_fish <- fish_data[fish_data$exclude_key %in% samples_to_exclude$exclude_key, ]
  filtered_fish_data <- fish_data[!fish_data$exclude_key %in% samples_to_exclude$exclude_key, ]

  # Remove the temporary key column
  excluded_fish$exclude_key <- NULL
  filtered_fish_data$exclude_key <- NULL

  # Calculate summary statistics
  original_fish <- nrow(fish_data)
  excluded_fish_count <- nrow(excluded_fish)
  remaining_fish <- nrow(filtered_fish_data)
  pct_retained <- round(100 * remaining_fish / original_fish, 1)

  # Create filter summary
  filter_summary <- data.frame(
    metric = c(
      "Original Fish",
      "Excluded Fish",
      "Remaining Fish",
      "% Fish Retained",
      "Excluded Samples",
      "Excluded Strata"
    ),
    value = c(
      original_fish,
      excluded_fish_count,
      remaining_fish,
      pct_retained,
      length(unique(samples_to_exclude$sample_id)),
      length(unique(samples_to_exclude$stratum))
    )
  )

  return(list(
    filtered_fish_data = filtered_fish_data,
    excluded_samples = excluded_fish,
    filter_summary = filter_summary
  ))
}
