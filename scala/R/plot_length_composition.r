##' Plot Scaled Length Composition Results
##'
##' Creates ggplot visualizations of scaled length composition results by sex and stratum.
##' Can plot either by-stratum results (faceted) or pooled results across all strata.
##'
##' @param x An object of class \code{scaled_length_composition} as returned by \code{calculate_scaled_length_compositions}
##' @param plot_type Character, either "by_stratum" for faceted plots by stratum, or "pooled" for combined results (default: "pooled")
##' @param show_uncertainty Logical, whether to show error bars based on CV estimates (default: TRUE)
##' @param sex_colors Named vector of colors for each sex category. Default uses colorblind-friendly palette
##' @param title Character, plot title (default: auto-generated based on plot type)
##' @param y_axis Character, either "composition" for absolute numbers or "proportion" for relative proportions (default: "composition")
##'
##' @return A ggplot object
##'
##' @details
##' The function creates bar plots showing length composition distributions by sex category.
##'
##' **Plot Types:**
##' - \code{by_stratum}: Creates faceted plots with one panel per stratum
##' - \code{pooled}: Shows combined results across all strata
##'
##' **Y-axis Options:**
##' - \code{composition}: Shows absolute scaled compositions (number of fish)
##' - \code{proportion}: Shows relative proportions (0-1 scale)
##'
##' Error bars represent ±1 standard error based on bootstrap CV estimates when \code{show_uncertainty = TRUE}.
##'
##' @examples
##' \dontrun{
##' # Generate test data and calculate results
##' test_data <- generate_test_data("commercial")
##' lw_params <- get_default_lw_params()
##'
##' results <- calculate_scaled_length_compositions(
##'   fish_data = test_data$fish_data,
##'   strata_data = test_data$strata_data,
##'   length_range = c(20, 35),
##'   lw_params_male = lw_params$male,
##'   lw_params_female = lw_params$female,
##'   lw_params_unsexed = lw_params$unsexed,
##'   bootstraps = 100
##' )
##'
##' # Plot pooled results
##' plot_length_composition(results, plot_type = "pooled")
##'
##' # Plot by stratum with proportions
##' plot_length_composition(results, plot_type = "by_stratum", y_axis = "proportion")
##'
##' # Custom colors
##' custom_colors <- c("male" = "#1f77b4", "female" = "#ff7f0e",
##'                    "unsexed" = "#2ca02c", "total" = "#d62728")
##' plot_length_composition(results, sex_colors = custom_colors)
##' }
##'
##' @export
plot_length_composition <- function(x,
                                    plot_type = "pooled",
                                    show_uncertainty = TRUE,
                                    sex_colors = NULL,
                                    title = NULL,
                                    y_axis = "composition") {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it with: install.packages('ggplot2')")
  }

  # Validate inputs
  if (!inherits(x, "scaled_length_composition")) {
    stop("Input must be an object of class 'scaled_length_composition'")
  }

  if (!plot_type %in% c("pooled", "by_stratum")) {
    stop("plot_type must be either 'pooled' or 'by_stratum'")
  }

  if (!y_axis %in% c("composition", "proportion")) {
    stop("y_axis must be either 'composition' or 'proportion'")
  }

  # Set default colors (colorblind-friendly palette)
  if (is.null(sex_colors)) {
    sex_colors <- c(
      "male" = "#1f77b4", # Blue
      "female" = "#ff7f0e", # Orange
      "unsexed" = "#2ca02c", # Green
      "total" = "#d62728" # Red
    )
  }

  # Prepare data for plotting
  if (plot_type == "pooled") {
    # Use pooled data
    if (y_axis == "composition") {
      plot_data <- x$pooled_length_composition
      cv_data <- x$pooled_lc_cv
      y_label <- "Scaled Length Composition"
    } else {
      plot_data <- x$pooled_proportions
      cv_data <- x$pooled_proportions_cv
      y_label <- "Proportion"
    }

    # Convert to long format
    plot_df <- data.frame()
    for (sex in colnames(plot_data)) {
      temp_df <- data.frame(
        length = x$lengths,
        composition = plot_data[, sex],
        cv = if (show_uncertainty && !is.null(cv_data)) cv_data[, sex] else NA,
        sex = sex,
        stratum = "Pooled"
      )
      plot_df <- rbind(plot_df, temp_df)
    }
  } else {
    # Use by-stratum data
    if (y_axis == "composition") {
      plot_data <- x$length_composition
      cv_data <- x$lc_cvs
      y_label <- "Scaled Length Composition"
    } else {
      plot_data <- x$proportions
      cv_data <- x$proportions_cvs
      y_label <- "Proportion"
    }

    # Convert 3D array to long format
    plot_df <- data.frame()
    for (s in seq_len(dim(plot_data)[3])) { # strata
      for (sex in dimnames(plot_data)[[2]]) { # sex
        temp_df <- data.frame(
          length = x$lengths,
          composition = plot_data[, sex, s],
          cv = if (show_uncertainty && !is.null(cv_data)) cv_data[, sex, s] else NA,
          sex = sex,
          stratum = x$strata_names[s]
        )
        plot_df <- rbind(plot_df, temp_df)
      }
    }
  }

  # Calculate standard errors for error bars
  if (show_uncertainty && any(!is.na(plot_df$cv))) {
    plot_df$se_lower <- plot_df$composition - (plot_df$composition * plot_df$cv)
    plot_df$se_upper <- plot_df$composition + (plot_df$composition * plot_df$cv)
    # Don't allow negative values
    plot_df$se_lower <- pmax(plot_df$se_lower, 0)
  } else {
    plot_df$se_lower <- plot_df$composition
    plot_df$se_upper <- plot_df$composition
    show_uncertainty <- FALSE
  }

  # Filter out zero compositions for cleaner plots
  plot_df <- plot_df[plot_df$composition > 0, ]

  # Create plot title if not provided
  if (is.null(title)) {
    scaling_type <- if (!is.null(x$scaling_type)) {
      ifelse(x$scaling_type == "weight", "Weight-based", "Density-based")
    } else {
      ""
    }

    if (plot_type == "pooled") {
      title <- paste("Pooled Length Composition by Sex", scaling_type)
    } else {
      title <- paste("Length Composition by Sex and Stratum", scaling_type)
    }
  }

  # Create the plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = "length", y = "composition", fill = "sex")) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::scale_fill_manual(values = sex_colors, name = "Sex") +
    ggplot2::labs(
      title = title,
      x = "Length (cm)",
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 11, face = "bold")
    )

  # Add error bars if requested
  if (show_uncertainty) {
    # Create a mapping for the error bars using strings to avoid CMD check issues
    p <- p + ggplot2::geom_errorbar(
      data = plot_df,
      ggplot2::aes_string(x = "length", ymin = "se_lower", ymax = "se_upper", group = "sex"),
      position = ggplot2::position_dodge(width = 0.9),
      width = 0.5,
      alpha = 0.7
    )
  }

  # Add faceting for by-stratum plots
  if (plot_type == "by_stratum") {
    p <- p + ggplot2::facet_wrap(~stratum, scales = "free_y")
  }

  # Add scaling information as subtitle if available
  if (!is.null(x$scaling_type)) {
    scaling_info <- ifelse(x$scaling_type == "weight",
      "Commercial fisheries (weight-based scaling)",
      "Research survey (density-based scaling)"
    )
    p <- p + ggplot2::labs(subtitle = scaling_info)
  }

  return(p)
}
