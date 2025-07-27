#' Get Summary Statistics from Length Composition
#'
#' This function calculates summary statistics from a length composition object, 
#' including mean weighted coefficient of variation (CV), number of fish sampled,
#' and total number of hauls sampled.
#'
#' @param x An object of class \code{length_composition}
#' @param by_stratum Logical, whether to calculate summaries by stratum (TRUE) or pooled across strata (FALSE). Default is FALSE.
#' @param type Character, either "composition" or "proportion" to specify which CV values to use. Default is "composition".
#' @param sex Character vector specifying which sex categories to include. Default is c("male", "female", "total").
#' @param stratum Character, specific stratum name when by_stratum = TRUE. If NULL, all strata are included. Default is NULL.
#' @param length_range Numeric vector of length 2 specifying the length range c(min, max) to include in calculations. If NULL, all lengths are used. Default is NULL.
#'
#' @return A list containing summary statistics:
#' \describe{
#'   \item{mean_weighted_cv}{Mean weighted coefficient of variation}
#'   \item{n_fish}{Number of fish sampled by sex category}
#'   \item{total_fish}{Total number of fish sampled}
#'   \item{n_hauls}{Total number of hauls/samples}
#' }
#' @details 
#' The function calculates the mean weighted CV using the formula:
#' \deqn{CV_{weighted} = \frac{\sum_{i} w_i \times CV_i}{\sum_{i} w_i}}
#' where \eqn{w_i} are the composition values (weights) and \eqn{CV_i} are the 
#' coefficient of variation values.
#' 
#' Fish counts and haul counts are extracted from pre-computed summary statistics
#' stored during the length composition calculations. When \code{length_range} is 
#' specified, a warning is issued as fish counts represent the full range used in
#' the original calculation.
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
#' # Get summary statistics for pooled data
#' summary_stats <- get_summary(result)
#' 
#' # Get summary statistics by stratum
#' summary_by_stratum <- get_summary(result, by_stratum = TRUE)
#' 
#' # Get summary statistics for proportions
#' summary_prop <- get_summary(result, type = "proportion")
#' 
#' # Get summary statistics for specific sex categories
#' summary_total <- get_summary(result, sex = "total")
#' 
#' # Get summary statistics for specific length range
#' summary_range <- get_summary(result, length_range = c(30, 50))
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
    stop("Input must be an object of class 'length_composition'")
  }
  
  # Use pmatch for partial matching of type parameter
  type_options <- c("composition", "proportion")
  type <- type_options[pmatch(type, type_options)]
  if (is.na(type)) {
    stop("type must be either 'composition' or 'proportion'")
  }
  
  # Validate sex parameter
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
  
  # Check if CV data is available
  if (by_stratum) {
    cv_data <- if (type == "composition") x$lc_cvs else x$proportions_cvs
    comp_data <- if (type == "composition") x$length_composition else x$proportions
  } else {
    cv_data <- if (type == "composition") x$pooled_lc_cv else x$pooled_proportions_cv
    comp_data <- if (type == "composition") x$pooled_length_composition else x$pooled_proportions
  }
  
  if (is.null(cv_data) || all(is.na(cv_data))) {
    stop("No CV data available. Bootstrap results are required to calculate CVs.")
  }
  
  if (is.null(comp_data)) {
    stop("No composition data available.")
  }
  
  # Helper function to calculate weighted CV for a subset of data
  calculate_weighted_cv <- function(cv_subset, comp_subset) {
    # Remove NA values and ensure both arrays have same dimensions
    valid_indices <- !is.na(cv_subset) & !is.na(comp_subset) & comp_subset > 0
    
    if (sum(valid_indices) == 0) {
      return(NA)
    }
    
    cv_values <- cv_subset[valid_indices]
    weights <- comp_subset[valid_indices]
    
    # Calculate weighted mean CV
    weighted_cv <- sum(weights * cv_values) / sum(weights)
    return(weighted_cv)
  }
  
  # Helper function to calculate fish counts from pre-computed summary statistics
  calculate_fish_counts <- function(summary_stats, length_range = NULL, strata_filter = NULL) {
    if (is.null(strata_filter)) {
      # Use total summary for pooled data
      fish_counts <- summary_stats$total_summary$n_fish
      n_hauls <- summary_stats$total_summary$n_hauls
    } else {
      # Use specific stratum summary
      if (strata_filter %in% names(summary_stats$stratum_summary)) {
        fish_counts <- summary_stats$stratum_summary[[strata_filter]]$n_fish
        n_hauls <- summary_stats$stratum_summary[[strata_filter]]$n_hauls
      } else {
        # Handle case where stratum doesn't exist
        fish_counts <- c(male = 0, female = 0, unsexed = 0, total = 0)
        n_hauls <- 0
      }
    }
    
    # Note: length_range filtering would require storing length-specific summaries
    # For now, we'll use the full range counts and add a warning if length_range is specified
    if (!is.null(length_range)) {
      warning("Fish counts are reported for the full length range used in calculate_length_compositions, not the specified length_range")
    }
    
    return(list(
      n_fish = fish_counts,
      n_hauls = n_hauls
    ))
  }
  
  # Process data based on by_stratum parameter
  if (!by_stratum) {
    # Pooled data - matrices (length × sex)
    
    # Get length bins and filter by length_range if specified
    length_bins <- as.numeric(rownames(comp_data))
    if (!is.null(length_range)) {
      length_indices <- which(length_bins >= length_range[1] & length_bins <= length_range[2])
      if (length(length_indices) == 0) {
        stop("No data available for the specified length_range")
      }
      cv_data <- cv_data[length_indices, , drop = FALSE]
      comp_data <- comp_data[length_indices, , drop = FALSE]
    }
    
    # Filter by sex categories
    available_sex <- colnames(comp_data)
    sex_to_use <- intersect(sex, available_sex)
    if (length(sex_to_use) == 0) {
      stop("None of the specified sex categories are available in the data")
    }
    
    # Calculate weighted CVs
    cv_results <- numeric(length(sex_to_use))
    names(cv_results) <- sex_to_use
    
    for (s in sex_to_use) {
      cv_subset <- cv_data[, s]
      comp_subset <- comp_data[, s]
      cv_results[s] <- calculate_weighted_cv(cv_subset, comp_subset)
    }
    
    # Calculate fish counts and haul counts from original data
    fish_counts <- calculate_fish_counts(x$summary_stats, length_range)
    
    # Filter fish counts by requested sex categories
    fish_counts_filtered <- fish_counts$n_fish[sex_to_use]
    
    # Return comprehensive summary
    return(list(
      mean_weighted_cv = if (length(cv_results) == 1) as.numeric(cv_results) else cv_results,
      n_fish = fish_counts_filtered,
      total_fish = fish_counts$n_fish["total"],
      n_hauls = fish_counts$n_hauls
    ))
    
  } else {
    # By-stratum data - 3D arrays (length × sex × stratum)
    
    # Get available strata
    available_strata <- dimnames(comp_data)[[3]]
    if (!is.null(stratum)) {
      if (!stratum %in% available_strata) {
        stop("Specified stratum '", stratum, "' not found in data. Available strata: ", 
             paste(available_strata, collapse = ", "))
      }
      strata_to_use <- stratum
    } else {
      strata_to_use <- available_strata
    }
    
    # Get length bins and filter by length_range if specified
    length_bins <- as.numeric(dimnames(comp_data)[[1]])
    if (!is.null(length_range)) {
      length_indices <- which(length_bins >= length_range[1] & length_bins <= length_range[2])
      if (length(length_indices) == 0) {
        stop("No data available for the specified length_range")
      }
      cv_data <- cv_data[length_indices, , , drop = FALSE]
      comp_data <- comp_data[length_indices, , , drop = FALSE]
    }
    
    # Filter by sex categories
    available_sex <- dimnames(comp_data)[[2]]
    sex_to_use <- intersect(sex, available_sex)
    if (length(sex_to_use) == 0) {
      stop("None of the specified sex categories are available in the data")
    }
    
    # Create results structure for CVs
    cv_results <- array(NA, dim = c(length(strata_to_use), length(sex_to_use)),
                        dimnames = list(strata_to_use, sex_to_use))
    
    for (st in strata_to_use) {
      for (s in sex_to_use) {
        cv_subset <- cv_data[, s, st]
        comp_subset <- comp_data[, s, st]
        cv_results[st, s] <- calculate_weighted_cv(cv_subset, comp_subset)
      }
    }
    
    # Calculate fish counts and haul counts by stratum
    fish_haul_summary <- list()
    
    for (st in strata_to_use) {
      fish_counts <- calculate_fish_counts(x$summary_stats, length_range, st)
      fish_counts_filtered <- fish_counts$n_fish[sex_to_use]
      
      fish_haul_summary[[st]] <- list(
        n_fish = fish_counts_filtered,
        total_fish = fish_counts$n_fish["total"],
        n_hauls = fish_counts$n_hauls
      )
    }
    
    # Simplify CV results structure based on dimensions
    if (length(strata_to_use) == 1 && length(sex_to_use) == 1) {
      cv_final <- as.numeric(cv_results[1, 1])
      fish_final <- fish_haul_summary[[1]]
    } else if (length(strata_to_use) == 1) {
      cv_final <- as.numeric(cv_results[1, ])
      names(cv_final) <- sex_to_use
      fish_final <- fish_haul_summary[[1]]
    } else if (length(sex_to_use) == 1) {
      cv_final <- as.numeric(cv_results[, 1])
      names(cv_final) <- strata_to_use
      fish_final <- fish_haul_summary
    } else {
      cv_final <- cv_results
      fish_final <- fish_haul_summary
    }
    
    # Return comprehensive summary
    return(list(
      mean_weighted_cv = cv_final,
      fish_haul_summary = fish_final
    ))
  }
}
