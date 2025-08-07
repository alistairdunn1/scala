# Global variables for R CMD check
utils::globalVariables(c("total", "sample_category", "sample_id", "stratum"))

#' @title Plot Sample Size Distribution by Strata
#' @description Creates a stacked histogram showing the distribution of sample sizes by strata,
#'   with bars categorized by sample adequacy: non-representative small samples,
#'   representative small samples, and adequate samples.
#' @param evaluation_result A list returned by \code{\link{evaluate_sample_sizes}}
#'   containing sample size evaluation results.
#' @param bin_width Numeric, width of bins for the histogram (default: 5 fish).
#' @param max_fish Numeric, maximum number of fish to display on x-axis.
#'   If NULL (default), uses the maximum observed sample size.
#' @param min_fish Numeric, minimum number of fish to display on x-axis (default: 0).
#' @param show_thresholds Logical, whether to show vertical lines indicating
#'   minimum sample size thresholds (default: TRUE).
#' @return A ggplot object showing the stacked histogram of sample sizes by strata.
#' @details
#' This function visualises the output from \code{\link{evaluate_sample_sizes}} as a
#' stacked histogram, making it easy to see:
#' \itemize{
#'   \item The distribution of sample sizes across strata
#'   \item Which samples fall below minimum thresholds
#'   \item The representativeness of small samples (when weight data is available)
#'   \item The relative frequency of different sample categories
#' }
#'
#' The plot uses three categories:
#' \itemize{
#'   \item \strong{Non-representative Small}: Small samples with poor representativeness (red)
#'   \item \strong{Representative Small}: Small samples that are highly representative (amber)
#'   \item \strong{Adequate}: Samples meeting minimum fish count thresholds (green)
#' }
#'
#' When sample weight data is not available, all small samples are grouped together
#' as "Small Samples" since representativeness cannot be assessed.
#'
#' @examples
#' \dontrun{
#' # Generate test data and evaluate sample sizes
#' test_data <- generate_test_data()
#' evaluation <- evaluate_sample_sizes(test_data$fish_data)
#'
#' # Create sample size distribution plot
#' plot_sample_size_distribution(evaluation)
#'
#' # Customize the plot
#' plot_sample_size_distribution(
#'   evaluation,
#'   bin_width = 10,
#'   max_fish = 100
#' )
#'
#' # Plot without threshold lines
#' plot_sample_size_distribution(evaluation, show_thresholds = FALSE)
#' }
#' @seealso \code{\link{evaluate_sample_sizes}} for evaluating sample adequacy
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline scale_fill_manual
#'   facet_wrap labs theme_minimal theme element_text guides guide_legend
#' @importFrom dplyr mutate case_when
#' @export
plot_sample_size_distribution <- function(evaluation_result,
                                          bin_width = 5,
                                          max_fish = NULL,
                                          min_fish = 0,
                                          show_thresholds = TRUE) {
  # Check if ggplot2 and dplyr are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it with: install.packages('ggplot2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data manipulation. Please install it with: install.packages('dplyr')")
  }

  # Validate input
  if (!is.list(evaluation_result) || !all(c("sample_sizes", "all_small_samples") %in% names(evaluation_result))) {
    stop("evaluation_result must be a list returned by evaluate_sample_sizes()")
  }

  # Extract data
  sample_sizes <- evaluation_result$sample_sizes
  all_small_samples <- evaluation_result$all_small_samples
  representative_small <- evaluation_result$representative_small_samples
  non_representative_small <- evaluation_result$non_representative_small_samples

  # Check if we have representativeness information
  has_representativeness <- nrow(representative_small) > 0 || nrow(non_representative_small) > 0

  # Create sample category classification
  if (has_representativeness) {
    # When we have representativeness data, use three categories
    sample_data <- sample_sizes %>%
      dplyr::mutate(
        sample_category = dplyr::case_when(
          sample_id %in% non_representative_small$sample_id ~ "Insufficient",
          sample_id %in% representative_small$sample_id ~ "Low but representative",
          TRUE ~ "Sufficient"
        )
      )

    # Set factor levels for proper ordering in legend
    sample_data$sample_category <- factor(
      sample_data$sample_category,
      levels = c("Insufficient", "Low but representative", "Sufficient")
    )
  } else {
    # When no representativeness data, use two categories
    sample_data <- sample_sizes %>%
      dplyr::mutate(
        sample_category = dplyr::case_when(
          sample_id %in% all_small_samples$sample_id ~ "Low number",
          TRUE ~ "Sufficient"
        )
      )

    # Set factor levels
    sample_data$sample_category <- factor(
      sample_data$sample_category,
      levels = c("Low number", "Sufficient")
    )
  }

  # Set max_fish if not provided
  if (is.null(max_fish)) {
    max_fish <- max(sample_data$total, na.rm = TRUE)
  }

  # Filter data to display range
  plot_data <- sample_data[sample_data$total >= min_fish & sample_data$total <= max_fish, ]

  # Create the plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = total, fill = sample_category)) +
    ggplot2::geom_histogram(binwidth = bin_width, colour = "black", alpha = 0.8, size = 0.3) +
    ggplot2::facet_wrap(~stratum, scales = "fixed") +
    ggplot2::labs(
      x = "Number of fish per sample",
      y = "Frequency (number of samples)",
    )

  # Add threshold lines if requested
  if (show_thresholds && length(all_small_samples) > 0) {
    # Get the minimum fish threshold from the small samples
    # (samples below this threshold are considered small)
    min_sufficient <- min(sample_data$total[sample_data$sample_category %in% c("Sufficient")], na.rm = TRUE)
    if (is.finite(min_sufficient)) {
      p <- p + ggplot2::geom_vline(
        xintercept = min_sufficient,
        linetype = "dashed",
        colour = "darkgray",
        size = 0.8,
        alpha = 0.7
      )
    }
  }

  return(p)
}
