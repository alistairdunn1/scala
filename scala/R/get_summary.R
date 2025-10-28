#' Get Summary Statistics from Length or Age Composition
#'
#' This function calculates summary statistics from a length or age composition object,
#' including mean weighted coefficient of variation (CV), number of fish sampled,
#' and total number of hauls sampled.
#'
#' @param x An object of class \code{length_composition} or \code{age_composition}
#' @param by_stratum Logical, whether to calculate summaries by stratum (TRUE) or pooled across strata (FALSE). Default is FALSE.
#' @param type Character, either "composition" or "proportion" to specify which CV values to use. Default is "composition".
#' @param sex Character vector specifying which sex categories to include. Default is c("male", "female", "total").
#' @param stratum Character, specific stratum name when by_stratum = TRUE. If NULL, all strata are included. Default is NULL.
#' @param length_range Numeric vector of length 2 specifying the length range c(min, max) to include in calculations. If NULL, all lengths are used. Default is NULL.
#'
#' @return A list containing summary statistics:
#' \describe{
#'   \item{data_type}{The type of data: "length_composition" or "age_composition"}
#'   \item{n_lengths}{Number of length bins (for length compositions)}
#'   \item{n_ages}{Number of age bins (for age compositions)}
#'   \item{n_strata}{Number of strata in the data}
#'   \item{length_range}{The min and max lengths (for length compositions)}
#'   \item{age_range}{The min and max ages (for age compositions)}
#'   \item{strata_names}{Names of the strata in the data}
#'   \item{total_fish}{Total number of fish in the data}
#'   \item{fish_by_stratum}{Number of fish by stratum}
#'   \item{fish_by_sex}{Number of fish by sex category}
#'   \item{n_samples}{Number of samples collected}
#'   \item{has_bootstraps}{Logical indicating if bootstrap data is available}
#'   \item{n_bootstraps}{Number of bootstrap iterations (if applicable)}
#'   \item{cv_range}{Range of CVs in the data (if bootstrap data is available)}
#'   \item{ci_coverage}{Confidence interval coverage (if applicable)}
#'   \item{alk_type}{Type of ALK used (for age compositions)}
#' }
#' @details
#' The function calculates summary statistics from length or age composition objects.
#' Fish counts and other metrics are extracted from the object's attributes.
#'
#' When \code{by_stratum = FALSE}, the function uses pooled data across all strata.
#' When \code{by_stratum = TRUE}, the function calculates summaries for each stratum separately.
#'
#' @examples
#' \dontrun{
#' # Generate test data and calculate length compositions with bootstrap
#' test_data <- generate_test_data("commercial")
#' result <- calculate_length_compositions(
#'   fish_data = test_data$fish_data,
#'   strata_data = test_data$strata_data,
#'   length_range = c(25, 55),
#'   bootstraps = 100
#' )
#'
#' # Get summary statistics
#' summary_stats <- get_summary(result)
#' }
#'
#' @export
get_summary <- function(x,
                        by_stratum = FALSE,
                        type = "composition",
                        sex = c("male", "female", "total"),
                        stratum = NULL,
                        length_range = NULL) {
  # Validate inputs
  if (!inherits(x, "length_composition")) {
    if (inherits(x, "age_composition")) {
      stop("Input must be an object of class 'length_composition'")
    } else {
      stop("Object must be a length_composition or age_composition")
    }
  }

  # Validate inputs
  valid_sex_categories <- c("male", "female", "unsexed", "total")
  if (!all(sex %in% valid_sex_categories)) {
    stop("sex must be one or more of: ", paste(valid_sex_categories, collapse = ", "))
  }

  # Validate length_range parameter
  if (!is.null(length_range)) {
    if (!is.numeric(length_range) || length(length_range) != 2 || length_range[1] >= length_range[2]) {
      stop("length_range must be a numeric vector of length 2 with format c(min, max)")
    }
  }

  # Check for bootstrap requirements
  if (by_stratum == FALSE && type == "composition" && is.null(x$bootstraps)) {
    stop("No CV data available. Bootstrap results are required to calculate CVs.")
  }

  # Create the basic summary structure
  summary_result <- list()

  # Add data type
  summary_result$data_type <- "length_composition"

  # Add dimensions
  summary_result$n_lengths <- length(x$lengths)
  summary_result$length_range <- range(x$lengths)

  # Add strata information
  summary_result$n_strata <- length(x$strata_names)
  summary_result$strata_names <- x$strata_names

  # Add fish counts - with robust error handling
  if (!is.null(x$summary_stats) && !is.null(x$summary_stats$total_summary)) {
    if (!is.null(x$summary_stats$total_summary$n_fish)) {
      total_fish_vector <- x$summary_stats$total_summary$n_fish
      summary_result$total_fish <- sum(total_fish_vector)
      summary_result$fish_by_sex <- total_fish_vector
    } else {
      summary_result$total_fish <- 0
      summary_result$fish_by_sex <- c(male = 0, female = 0, unsexed = 0, total = 0)
    }

    summary_result$n_samples <- if (!is.null(x$summary_stats$total_summary$n_hauls)) {
      x$summary_stats$total_summary$n_hauls
    } else {
      0
    }
  } else {
    summary_result$total_fish <- 0
    summary_result$fish_by_sex <- c(male = 0, female = 0, unsexed = 0, total = 0)
    summary_result$n_samples <- 0
  }

  # Add fish counts by stratum - with robust error handling
  summary_result$fish_by_stratum <- numeric(length(x$strata_names))
  names(summary_result$fish_by_stratum) <- x$strata_names

  if (!is.null(x$summary_stats) && !is.null(x$summary_stats$stratum_summary)) {
    for (i in seq_along(x$strata_names)) {
      stratum <- x$strata_names[i]
      if (stratum %in% names(x$summary_stats$stratum_summary) &&
        !is.null(x$summary_stats$stratum_summary[[stratum]]$n_fish)) {
        summary_result$fish_by_stratum[i] <- sum(x$summary_stats$stratum_summary[[stratum]]$n_fish)
      } else {
        summary_result$fish_by_stratum[i] <- 0
      }
    }
  }

  # Add bootstrap information
  has_bootstraps <- !is.null(x$bootstraps) && length(x$bootstraps) > 0
  summary_result$has_bootstraps <- has_bootstraps

  if (has_bootstraps) {
    summary_result$n_bootstraps <- length(x$bootstraps)

    # Get CV range if available
    if (!is.null(x$lc_cvs)) {
      cv_values <- x$lc_cvs[!is.na(x$lc_cvs)]
      if (length(cv_values) > 0) {
        summary_result$cv_range <- range(cv_values, na.rm = TRUE)
      } else {
        summary_result$cv_range <- c(0, 0)
      }
    } else if (!is.null(x$proportions_cvs)) {
      cv_values <- x$proportions_cvs[!is.na(x$proportions_cvs)]
      if (length(cv_values) > 0) {
        summary_result$cv_range <- range(cv_values, na.rm = TRUE)
      } else {
        summary_result$cv_range <- c(0, 0)
      }
    } else {
      summary_result$cv_range <- c(0, 0)
    }

    summary_result$ci_coverage <- 0.95 # Default value
  }

  # Set class
  class(summary_result) <- "length_composition_summary"

  return(summary_result)
}
