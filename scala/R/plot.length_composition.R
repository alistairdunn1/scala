# Global variables for R CMD check
utils::globalVariables(c("length_bin", "composition", "sex", "ci_lower", "ci_upper", "stratum"))

#' Plot method for length composition objects
#'
#' Creates ggplot visualizations of length composition results.
#'
#' @param x An object of class \code{length_composition}
#' @param by_stratum Logical, whether to plot by stratum (TRUE) or total across strata (FALSE). Default is FALSE.
#' @param stratum Character, name of a specific stratum to plot. If provided, only that stratum is plotted (overrides by_stratum). Default is NULL.
#' @param show_CIs Logical, whether to show 95 percent confidence interval ribbons if bootstrap results are available. Default is TRUE.
#' @param type Character, either "composition" or "proportion" (or partial matches). Default is "composition".
#' @param length_bin_size Numeric, size of length bins in cm for aggregating data (e.g., 2, 5). If NULL (default), no binning is performed.
#' @param unsexed Logical, whether to include unsexed fish category in the plot. Default is FALSE.
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_colour_manual scale_fill_manual facet_grid labs ylim
#' @importFrom tools toTitleCase
#' @export
plot.length_composition <- function(x,
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
  if (!inherits(x, "length_composition")) {
    stop("Input must be an object of class 'length_composition'")
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

  # Validate stratum parameter
  if (!is.null(stratum)) {
    if (!is.character(stratum) || length(stratum) != 1) {
      stop("stratum must be a single character string")
    }
    # Check if stratum exists in the data
    available_strata <- NULL
    if (!is.null(x$length_composition)) {
      available_strata <- dimnames(x$length_composition)[[3]]
    } else if (!is.null(x$length_compositions)) {
      available_strata <- dimnames(x$length_compositions)[[3]]
    }

    if (!is.null(available_strata) && !stratum %in% available_strata) {
      stop(paste("stratum '", stratum, "' not found. Available strata:", paste(available_strata, collapse = ", ")))
    }
  }

  # Helper function to aggregate length data into bins
  aggregate_length_bins <- function(data_array, lengths_vec, bin_size) {
    if (is.null(bin_size)) {
      return(list(data = data_array, lengths = lengths_vec))
    }

    # Check if lengths_vec is valid
    if (length(lengths_vec) == 0 || all(is.na(lengths_vec))) {
      warning("No valid length data available for binning")
      return(list(data = data_array, lengths = lengths_vec))
    }

    # Create length bins
    min_length <- min(lengths_vec, na.rm = TRUE)
    max_length <- max(lengths_vec, na.rm = TRUE)

    if (!is.finite(min_length) || !is.finite(max_length)) {
      warning("Invalid length range for binning")
      return(list(data = data_array, lengths = lengths_vec))
    }

    bin_breaks <- seq(from = min_length, to = max_length + bin_size, by = bin_size)
    bin_centers <- bin_breaks[-length(bin_breaks)] + bin_size / 2

    # Assign each length to a bin
    length_bins <- cut(lengths_vec, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)
    bin_indices <- as.numeric(length_bins)

    # Handle data aggregation based on array dimensions
    if (is.matrix(data_array)) {
      # 2D matrix (length x sex)
      aggregated_data <- matrix(0, nrow = length(bin_centers), ncol = ncol(data_array))
      rownames(aggregated_data) <- as.character(bin_centers)
      colnames(aggregated_data) <- colnames(data_array)

      for (i in seq_along(lengths_vec)) {
        if (!is.na(bin_indices[i])) {
          aggregated_data[bin_indices[i], ] <- aggregated_data[bin_indices[i], ] + data_array[i, ]
        }
      }
    } else {
      # 3D array (length x sex x stratum)
      aggregated_data <- array(0, dim = c(length(bin_centers), dim(data_array)[2], dim(data_array)[3]))
      dimnames(aggregated_data) <- list(
        as.character(bin_centers),
        dimnames(data_array)[[2]],
        dimnames(data_array)[[3]]
      )

      for (i in seq_along(lengths_vec)) {
        if (!is.na(bin_indices[i])) {
          aggregated_data[bin_indices[i], , ] <- aggregated_data[bin_indices[i], , ] + data_array[i, , ]
        }
      }
    }

    return(list(data = aggregated_data, lengths = bin_centers))
  }

  # Determine which dataset to use based on by_stratum and stratum parameters
  if (!is.null(stratum)) {
    # Plot a specific stratum only
    if (!is.null(x$length_composition)) {
      # Bootstrap results available
      comp_data <- x$length_composition[, , stratum, drop = FALSE]
      ci_lower <- x$lc_ci_lower[, , stratum, drop = FALSE]
      ci_upper <- x$lc_ci_upper[, , stratum, drop = FALSE]
      if (type == "proportion") {
        comp_data <- x$proportions[, , stratum, drop = FALSE]
        ci_lower <- x$proportions_ci_lower[, , stratum, drop = FALSE]
        ci_upper <- x$proportions_ci_upper[, , stratum, drop = FALSE]
      }

      # Apply length binning if requested
      lengths_vec <- as.numeric(dimnames(comp_data)[[1]])
      bin_result <- aggregate_length_bins(comp_data, lengths_vec, length_bin_size)
      comp_data <- bin_result$data
      lengths_vec <- bin_result$lengths

      # Apply binning to confidence intervals if available
      if (!is.null(ci_lower) && !is.null(ci_upper)) {
        ci_lower_bin <- aggregate_length_bins(ci_lower, as.numeric(dimnames(ci_lower)[[1]]), length_bin_size)
        ci_upper_bin <- aggregate_length_bins(ci_upper, as.numeric(dimnames(ci_upper)[[1]]), length_bin_size)
        ci_lower <- ci_lower_bin$data
        ci_upper <- ci_upper_bin$data
      }
    } else {
      # Simple results (no bootstrap)
      comp_data <- x$length_compositions[, , stratum, drop = FALSE]
      ci_lower <- NULL
      ci_upper <- NULL

      # Apply length binning if requested
      lengths_vec <- as.numeric(dimnames(comp_data)[[1]])
      bin_result <- aggregate_length_bins(comp_data, lengths_vec, length_bin_size)
      comp_data <- bin_result$data
      lengths_vec <- bin_result$lengths

      if (type == "proportion") {
        # Convert to proportions
        total_count <- sum(comp_data[, "total", 1])
        if (total_count > 0) {
          comp_data[, , 1] <- comp_data[, , 1] / total_count
        }
      }
    }

    # Convert to data frame format similar to pooled data (remove stratum dimension)
    comp_data_2d <- comp_data[, , 1]

    # Convert to data frame for ggplot
    plot_data_list <- list()

    # Define sex categories to include based on unsexed parameter
    sex_categories <- if (unsexed) {
      c("male", "female", "unsexed", "total")
    } else {
      c("male", "female", "total")
    }

    for (sex_name in sex_categories) {
      if (sex_name %in% colnames(comp_data_2d)) {
        sex_data <- data.frame(
          length_bin = lengths_vec,
          composition = comp_data_2d[, sex_name],
          sex = tools::toTitleCase(as.character(sex_name)),
          stringsAsFactors = FALSE
        )

        # Add confidence intervals if available
        if (!is.null(ci_lower) && !is.null(ci_upper)) {
          sex_data$ci_lower <- ci_lower[, sex_name, 1]
          sex_data$ci_upper <- ci_upper[, sex_name, 1]
        }

        plot_data_list[[sex_name]] <- sex_data
      }
    }

    plot_data <- do.call(rbind, plot_data_list)
  } else if (!by_stratum) {
    # Use pooled data across all strata
    if (!is.null(x$pooled_length_composition)) {
      # Bootstrap results available
      comp_data <- x$pooled_length_composition
      prop_data <- x$pooled_proportions
      ci_lower <- x$pooled_lc_ci_lower
      ci_upper <- x$pooled_lc_ci_upper
      if (type == "proportion") {
        comp_data <- prop_data
        ci_lower <- x$pooled_proportions_ci_lower
        ci_upper <- x$pooled_proportions_ci_upper
      }

      # Apply length binning if requested
      lengths_vec <- as.numeric(rownames(comp_data))
      bin_result <- aggregate_length_bins(comp_data, lengths_vec, length_bin_size)
      comp_data <- bin_result$data
      lengths_vec <- bin_result$lengths

      # Apply binning to confidence intervals if available
      if (!is.null(ci_lower) && !is.null(ci_upper)) {
        ci_lower_bin <- aggregate_length_bins(ci_lower, as.numeric(rownames(ci_lower)), length_bin_size)
        ci_upper_bin <- aggregate_length_bins(ci_upper, as.numeric(rownames(ci_upper)), length_bin_size)
        ci_lower <- ci_lower_bin$data
        ci_upper <- ci_upper_bin$data
      }
    } else {
      # Simple results (no bootstrap)
      comp_data <- apply(x$length_compositions, c(1, 2), sum) # Sum across strata
      ci_lower <- NULL
      ci_upper <- NULL

      # Apply length binning if requested
      lengths_vec <- as.numeric(rownames(comp_data))
      bin_result <- aggregate_length_bins(comp_data, lengths_vec, length_bin_size)
      comp_data <- bin_result$data
      lengths_vec <- bin_result$lengths

      if (type == "proportion") {
        total_counts <- sum(comp_data[, "total"])
        comp_data <- comp_data / total_counts
      }
    }

    # Convert to data frame for ggplot
    plot_data_list <- list()

    # Define sex categories to include based on unsexed parameter
    sex_categories <- if (unsexed) {
      c("male", "female", "unsexed", "total")
    } else {
      c("male", "female", "total")
    }

    for (sex_name in sex_categories) {
      if (sex_name %in% colnames(comp_data)) {
        sex_data <- data.frame(
          length_bin = lengths_vec,
          composition = comp_data[, sex_name],
          sex = tools::toTitleCase(as.character(sex_name)), # Capitalize sex labels
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

    plot_data <- do.call(rbind, plot_data_list)
  } else {
    # Use by-stratum data
    if (!is.null(x$length_composition)) {
      # Bootstrap results available
      comp_data <- x$length_composition
      ci_lower <- x$lc_ci_lower
      ci_upper <- x$lc_ci_upper
      if (type == "proportion") {
        comp_data <- x$proportions
        ci_lower <- x$proportions_ci_lower
        ci_upper <- x$proportions_ci_upper
      }

      # Apply length binning if requested
      lengths_vec <- as.numeric(dimnames(comp_data)[[1]])
      bin_result <- aggregate_length_bins(comp_data, lengths_vec, length_bin_size)
      comp_data <- bin_result$data
      lengths_vec <- bin_result$lengths

      # Apply binning to confidence intervals if available
      if (!is.null(ci_lower) && !is.null(ci_upper)) {
        ci_lower_bin <- aggregate_length_bins(ci_lower, as.numeric(dimnames(ci_lower)[[1]]), length_bin_size)
        ci_upper_bin <- aggregate_length_bins(ci_upper, as.numeric(dimnames(ci_upper)[[1]]), length_bin_size)
        ci_lower <- ci_lower_bin$data
        ci_upper <- ci_upper_bin$data
      }
    } else {
      # Simple results (no bootstrap)
      comp_data <- x$length_compositions
      ci_lower <- NULL
      ci_upper <- NULL

      # Apply length binning if requested
      lengths_vec <- as.numeric(dimnames(comp_data)[[1]])
      bin_result <- aggregate_length_bins(comp_data, lengths_vec, length_bin_size)
      comp_data <- bin_result$data
      lengths_vec <- bin_result$lengths

      if (type == "proportion") {
        # Convert to proportions
        comp_data_prop <- comp_data
        n_strata <- dim(comp_data)[3]
        for (s in seq_len(n_strata)) {
          total_count <- sum(comp_data[, "total", s])
          if (total_count > 0) {
            comp_data_prop[, , s] <- comp_data[, , s] / total_count
          }
        }
        comp_data <- comp_data_prop
      }
    }

    # Define sex categories to include based on unsexed parameter
    all_sex_categories <- dimnames(comp_data)[[2]]
    # Filter out non-sex categories like "composition"
    valid_sex_categories <- all_sex_categories[all_sex_categories %in% c("male", "female", "unsexed", "total")]

    sex_categories <- if (unsexed) {
      valid_sex_categories
    } else {
      # Filter out 'unsexed' from sex categories
      valid_sex_categories[valid_sex_categories != "unsexed"]
    }

    # Convert 3D array to long format data frame
    length_bins <- as.numeric(dimnames(comp_data)[[1]])
    strata <- dimnames(comp_data)[[3]]

    # Create properly structured data frame
    plot_data <- data.frame()

    for (stratum in strata) {
      for (sex in sex_categories) {
        for (i in seq_along(length_bins)) {
          length_bin <- length_bins[i]
          row_data <- data.frame(
            length_bin = length_bin,
            composition = comp_data[i, sex, stratum],
            sex = sex,
            stratum = stratum,
            stringsAsFactors = FALSE
          )

          # Add confidence intervals if available - use proper indexing
          if (!is.null(ci_lower) && !is.null(ci_upper)) {
            # Check if indices are valid for CI arrays
            if (i <= dim(ci_lower)[1] && sex %in% dimnames(ci_lower)[[2]] && stratum %in% dimnames(ci_lower)[[3]]) {
              row_data$ci_lower <- ci_lower[i, sex, stratum]
              row_data$ci_upper <- ci_upper[i, sex, stratum]
            } else {
              # Set NA if indices are out of bounds
              row_data$ci_lower <- NA
              row_data$ci_upper <- NA
            }
          }

          plot_data <- rbind(plot_data, row_data)
        }
      }
    }

    # Capitalize sex and stratum labels for better presentation
    plot_data$sex <- tools::toTitleCase(as.character(plot_data$sex))
    plot_data$stratum <- tools::toTitleCase(as.character(plot_data$stratum))
  }

  if (nrow(plot_data) == 0) {
    stop("No composition data available for plotting.")
  }

  # Convert composition to proportion if requested
  # (This is now handled above in the data preparation section)

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

  # Set up faceting for by_stratum plots
  if (by_stratum) {
    p <- p + ggplot2::ylim(0, NA) + ggplot2::facet_grid(stratum ~ sex, scales = "fixed")
  } else if (!is.null(stratum)) {
    # For single stratum plots, facet by sex only
    p <- p + ggplot2::ylim(0, NA) + ggplot2::facet_wrap(~sex, scales = "fixed")
  }

  # Add labels and theme
  y_label <- if (type == "composition") "Scaled length composition" else "Scaled length proportions"

  # Update x-axis label if binning is used
  x_label <- if (!is.null(length_bin_size)) {
    paste0("Length bins (", length_bin_size, " cm)")
  } else {
    "Length (cm)"
  }

  p <- p +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      colour = "Sex",
      fill = "Sex"
    )

  return(p)
}
