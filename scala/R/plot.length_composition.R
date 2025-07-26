#' Plot method for length composition objects
#'
#' Creates ggplot visualizations of length composition results.
#'
#' @param x An object of class \code{length_composition}
#' @param by_stratum Logical, whether to plot by stratum (TRUE) or total across strata (FALSE). Default is FALSE.
#' @param show_uncertainty Logical, whether to show 95 percent confidence interval ribbons if bootstrap results are available. Default is TRUE.
#' @param sex_colours Named vector of colours for each sex category. If NULL, uses default colourblind-friendly palette.
#' @param type Character, either "composition" or "proportion" (or partial matches). Default is "composition".
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object
#'
#' @export
plot.length_composition <- function(x,
                                    by_stratum = FALSE,
                                    show_uncertainty = TRUE,
                                    sex_colours = NULL,
                                    type = "composition",
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

  # Set default colours (colourblind-friendly palette)
  if (is.null(sex_colours)) {
    sex_colours <- c(
      "male" = "#1f77b4", # Blue
      "female" = "#ff7f0e", # Orange
      "unsexed" = "#2ca02c", # Green
      "total" = "#d62728" # Red
    )
  }

  # Determine which dataset to use based on by_stratum
  if (!by_stratum) {
    plot_data <- x$totals
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      stop("No total composition data available. Ensure calculate_length_compositions was run with appropriate data.")
    }
  } else {
    plot_data <- x$by_stratum
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      stop("No by-stratum composition data available. Ensure calculate_length_compositions was run with stratum data.")
    }
  }

  # Convert composition to proportion if requested
  if (type == "proportion") {
    # Calculate proportions within each sex and (if applicable) stratum
    if (!by_stratum) {
      plot_data <- plot_data %>%
        dplyr::group_by(sex) %>%
        dplyr::mutate(
          composition = composition / sum(composition, na.rm = TRUE),
          ci_lower = ci_lower / sum(x$totals$composition[x$totals$sex == sex[1]], na.rm = TRUE),
          ci_upper = ci_upper / sum(x$totals$composition[x$totals$sex == sex[1]], na.rm = TRUE)
        ) %>%
        dplyr::ungroup()
    } else {
      plot_data <- plot_data %>%
        dplyr::group_by(stratum, sex) %>%
        dplyr::mutate(
          composition = composition / sum(composition, na.rm = TRUE),
          ci_lower = ci_lower / sum(composition, na.rm = TRUE),
          ci_upper = ci_upper / sum(composition, na.rm = TRUE)
        ) %>%
        dplyr::ungroup()
    }
  }

  # Set up the basic plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = length_bin, y = composition, colour = sex))

  # Add uncertainty ribbons if requested and available
  if (show_uncertainty && !is.null(x$bootstrap_results)) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = sex),
      alpha = 0.3,
      colour = NA
    )
  }

  # Add lines
  p <- p + ggplot2::geom_line(size = 0.8)

  # Apply colours
  available_sexes <- unique(plot_data$sex)
  used_colours <- sex_colours[names(sex_colours) %in% available_sexes]

  p <- p +
    ggplot2::scale_colour_manual(values = used_colours) +
    ggplot2::scale_fill_manual(values = used_colours)

  # Set up faceting for by_stratum plots
  if (by_stratum) {
    p <- p + ggplot2::facet_grid(stratum ~ sex, scales = "free_y")
  }

  # Add labels and theme
  y_label <- if (type == "composition") "Scaled Length Composition" else "Proportion"

  p <- p +
    ggplot2::labs(
      x = "Length (cm)",
      y = y_label,
      colour = "Sex",
      fill = "Sex"
    )

  return(p)
}
