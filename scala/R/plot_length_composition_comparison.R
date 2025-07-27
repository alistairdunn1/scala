#' Plot multiple length composition objects in a multi-panel layout
#'
#' Creates a multi-panel ggplot visualization where each row represents a different
#' length composition result and columns show sex categories.
#'
#' @param x List of objects of class \code{length_composition}
#' @param names Character vector of names for each list element (optional). If NULL, uses list names or creates default names.
#' @param by_stratum Logical, whether to plot by stratum (TRUE) or pooled across strata (FALSE). Default is FALSE.
#' @param stratum Character, specific stratum name to plot when by_stratum = TRUE. If NULL, uses first stratum. Ignored when by_stratum = FALSE.
#' @param show_CIs Logical, whether to show 95 percent confidence interval ribbons if bootstrap results are available. Default is TRUE.
#' @param type Character, either "composition" or "proportion" (or partial matches). Default is "composition".
#' @param length_bin_size Numeric, size of length bins in cm for aggregating data (e.g., 2, 5). If NULL (default), no binning is performed.
#' @param unsexed Logical, whether to include unsexed fish category in the plot. Default is FALSE.
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object with faceted layout (rows = list elements, columns = sex categories)
#'
#' @examples
#' \dontrun{
#' # Create multiple results
#' result1 <- calculate_length_compositions(...)
#' result2 <- calculate_length_compositions(...)
#' results_list <- list("Scenario A" = result1, "Scenario B" = result2)
#' 
#' # Plot comparison
#' plot_length_composition_comparison(results_list)
#' 
#' # Plot specific stratum comparison
#' plot_length_composition_comparison(results_list, by_stratum = TRUE, stratum = "North")
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon facet_grid labs ylim
#' @importFrom tools toTitleCase
#' @export
plot_length_composition_comparison <- function(x,
                                               names = NULL,
                                               by_stratum = FALSE,
                                               stratum = NULL,
                                               show_CIs = TRUE,
                                               type = "composition",
                                               length_bin_size = NULL,
                                               unsexed = FALSE,
                                               ...) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it with: install.packages('ggplot2')")
  }

  # Validate inputs
  if (!is.list(x) || length(x) == 0) {
    stop("Input must be a non-empty list of length_composition objects")
  }
  
  # Check that all elements are length_composition objects
  for (i in seq_along(x)) {
    if (!inherits(x[[i]], "length_composition")) {
      stop(paste("Element", i, "is not a length_composition object"))
    }
  }

  # Set up names for list elements
  if (is.null(names)) {
    if (!is.null(names(x))) {
      element_names <- names(x)
    } else {
      element_names <- paste("Result", seq_along(x))
    }
  } else {
    if (length(names) != length(x)) {
      stop("Length of 'names' must match length of input list")
    }
    element_names <- names
  }

  # Use pmatch for partial matching of type parameter
  type_options <- c("composition", "proportion")
  type <- type_options[pmatch(type, type_options)]
  if (is.na(type)) {
    stop("type must be either 'composition' or 'proportion'")
  }

  # Validate length_bin_size parameter
  if (!is.null(length_bin_size)) {
    if (!is.numeric(length_bin_size) || length(length_bin_size) != 1 || length_bin_size <= 0) {
      stop("length_bin_size must be a positive numeric value")
    }
  }

  # Helper function to extract data from a single length_composition object
  extract_composition_data <- function(lc_obj, element_name) {
    
    # Determine which dataset to use based on by_stratum
    if (!by_stratum) {
      # Use pooled data across all strata
      if (!is.null(lc_obj$pooled_length_composition)) {
        # Bootstrap results available
        comp_data <- lc_obj$pooled_length_composition
        ci_lower <- lc_obj$pooled_lc_ci_lower
        ci_upper <- lc_obj$pooled_lc_ci_upper
        if (type == "proportion") {
          comp_data <- lc_obj$pooled_proportions
          ci_lower <- lc_obj$pooled_proportions_ci_lower
          ci_upper <- lc_obj$pooled_proportions_ci_upper
        }
      } else {
        # Simple results (no bootstrap)
        comp_data <- apply(lc_obj$length_compositions, c(1, 2), sum) # Sum across strata
        ci_lower <- NULL
        ci_upper <- NULL
        if (type == "proportion") {
          total_counts <- sum(comp_data[, "total"])
          comp_data <- comp_data / total_counts
        }
      }
      
      # Apply length binning if requested
      lengths_vec <- as.numeric(rownames(comp_data))
      if (!is.null(length_bin_size)) {
        # Use the same aggregate_length_bins function from plot.length_composition
        bin_result <- aggregate_length_bins_helper(comp_data, lengths_vec, length_bin_size)
        comp_data <- bin_result$data
        lengths_vec <- bin_result$lengths
        
        # Apply binning to confidence intervals if available
        if (!is.null(ci_lower) && !is.null(ci_upper)) {
          ci_lower_bin <- aggregate_length_bins_helper(ci_lower, as.numeric(rownames(ci_lower)), length_bin_size)
          ci_upper_bin <- aggregate_length_bins_helper(ci_upper, as.numeric(rownames(ci_upper)), length_bin_size)
          ci_lower <- ci_lower_bin$data
          ci_upper <- ci_upper_bin$data
        }
      }
      
    } else {
      # Use by-stratum data
      if (!is.null(lc_obj$length_composition)) {
        # Bootstrap results available
        comp_data_3d <- lc_obj$length_composition
        ci_lower_3d <- lc_obj$lc_ci_lower
        ci_upper_3d <- lc_obj$lc_ci_upper
        if (type == "proportion") {
          comp_data_3d <- lc_obj$proportions
          ci_lower_3d <- lc_obj$proportions_ci_lower
          ci_upper_3d <- lc_obj$proportions_ci_upper
        }
      } else {
        # Simple results (no bootstrap)
        comp_data_3d <- lc_obj$length_compositions
        ci_lower_3d <- NULL
        ci_upper_3d <- NULL
        if (type == "proportion") {
          # Convert to proportions
          comp_data_prop <- comp_data_3d
          n_strata <- dim(comp_data_3d)[3]
          for (s in seq_len(n_strata)) {
            total_count <- sum(comp_data_3d[, "total", s])
            if (total_count > 0) {
              comp_data_prop[, , s] <- comp_data_3d[, , s] / total_count
            }
          }
          comp_data_3d <- comp_data_prop
        }
      }
      
      # Select specific stratum
      available_strata <- dimnames(comp_data_3d)[[3]]
      if (is.null(stratum)) {
        selected_stratum <- available_strata[1]
        warning(paste("No stratum specified, using first available stratum:", selected_stratum))
      } else {
        if (!stratum %in% available_strata) {
          stop(paste("Stratum '", stratum, "' not found. Available strata:", paste(available_strata, collapse = ", ")))
        }
        selected_stratum <- stratum
      }
      
      # Extract data for selected stratum
      comp_data <- comp_data_3d[, , selected_stratum]
      ci_lower <- if (!is.null(ci_lower_3d)) ci_lower_3d[, , selected_stratum] else NULL
      ci_upper <- if (!is.null(ci_upper_3d)) ci_upper_3d[, , selected_stratum] else NULL
      
      # Apply length binning if requested
      lengths_vec <- as.numeric(dimnames(comp_data_3d)[[1]])
      if (!is.null(length_bin_size)) {
        bin_result <- aggregate_length_bins_helper(comp_data, lengths_vec, length_bin_size)
        comp_data <- bin_result$data
        lengths_vec <- bin_result$lengths
        
        # Apply binning to confidence intervals if available
        if (!is.null(ci_lower) && !is.null(ci_upper)) {
          ci_lower_bin <- aggregate_length_bins_helper(ci_lower, lengths_vec, length_bin_size)
          ci_upper_bin <- aggregate_length_bins_helper(ci_upper, lengths_vec, length_bin_size)
          ci_lower <- ci_lower_bin$data
          ci_upper <- ci_upper_bin$data
        }
      }
    }

    # Define sex categories to include based on unsexed parameter
    all_sex_categories <- colnames(comp_data)
    # Filter out non-sex categories like "composition"
    valid_sex_categories <- all_sex_categories[all_sex_categories %in% c("male", "female", "unsexed", "total")]
    
    sex_categories <- if (unsexed) {
      valid_sex_categories
    } else {
      valid_sex_categories[valid_sex_categories != "unsexed"]
    }

    # Convert to data frame for ggplot
    plot_data_list <- list()

    for (sex_name in sex_categories) {
      if (sex_name %in% colnames(comp_data)) {
        sex_data <- data.frame(
          length_bin = lengths_vec,
          composition = comp_data[, sex_name],
          sex = tools::toTitleCase(as.character(sex_name)),
          element = element_name,
          stringsAsFactors = FALSE
        )

        # Add confidence intervals if available
        if (!is.null(ci_lower) && !is.null(ci_upper)) {
          sex_data$ci_lower <- ci_lower[, sex_name]
          sex_data$ci_upper <- ci_upper[, sex_name]
        }

        plot_data_list[[sex_name]] <- sex_data
      }
    }

    return(do.call(rbind, plot_data_list))
  }

  # Helper function for length binning (simplified version)
  aggregate_length_bins_helper <- function(data_array, lengths_vec, bin_size) {
    if (is.null(bin_size) || length(lengths_vec) == 0 || all(is.na(lengths_vec))) {
      return(list(data = data_array, lengths = lengths_vec))
    }

    min_length <- min(lengths_vec, na.rm = TRUE)
    max_length <- max(lengths_vec, na.rm = TRUE)
    
    if (!is.finite(min_length) || !is.finite(max_length)) {
      return(list(data = data_array, lengths = lengths_vec))
    }
    
    bin_breaks <- seq(from = min_length, to = max_length + bin_size, by = bin_size)
    bin_centers <- bin_breaks[-length(bin_breaks)] + bin_size / 2
    length_bins <- cut(lengths_vec, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
    bin_indices <- as.numeric(length_bins)

    if (is.matrix(data_array)) {
      aggregated_data <- matrix(0, nrow = length(bin_centers), ncol = ncol(data_array))
      rownames(aggregated_data) <- as.character(bin_centers)
      colnames(aggregated_data) <- colnames(data_array)
      
      for (i in seq_along(lengths_vec)) {
        if (!is.na(bin_indices[i])) {
          aggregated_data[bin_indices[i], ] <- aggregated_data[bin_indices[i], ] + data_array[i, ]
        }
      }
    } else {
      return(list(data = data_array, lengths = lengths_vec))
    }

    return(list(data = aggregated_data, lengths = bin_centers))
  }

  # Extract data from all length_composition objects
  all_data <- list()
  for (i in seq_along(x)) {
    all_data[[i]] <- extract_composition_data(x[[i]], element_names[i])
  }

  # Combine all data
  plot_data <- do.call(rbind, all_data)

  if (nrow(plot_data) == 0) {
    stop("No composition data available for plotting.")
  }

  # Set up the basic plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = length_bin, y = composition, colour = sex))

  # Add uncertainty ribbons if requested and available
  if (show_CIs && ("ci_lower" %in% names(plot_data)) && ("ci_upper" %in% names(plot_data))) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = sex),
      alpha = 0.3,
      colour = NA
    )
  }

  # Add lines
  p <- p + ggplot2::geom_line()

  # Set up faceting: rows = elements, columns = sex
  p <- p + ggplot2::ylim(0, NA) + ggplot2::facet_grid(element ~ sex, scales = "free_y")

  # Add labels and theme
  y_label <- if (type == "composition") "Scaled length composition" else "Scaled length proportions"
  
  # Update x-axis label if binning is used
  x_label <- if (!is.null(length_bin_size)) {
    paste0("Length bins (", length_bin_size, " cm)")
  } else {
    "Length (cm)"
  }
  
  # Add stratum info to title if applicable
  title_suffix <- if (by_stratum && !is.null(stratum)) {
    paste(" - Stratum:", tools::toTitleCase(stratum))
  } else if (by_stratum) {
    " - By Stratum"
  } else {
    " - Pooled"
  }

  p <- p +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      colour = "Sex",
      fill = "Sex",
      title = paste("Length Composition Comparison", title_suffix)
    )

  return(p)
}
