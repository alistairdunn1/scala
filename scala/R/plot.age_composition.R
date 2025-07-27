#' Print Method for Age Composition Objects
#'
#' @param x An age_composition object from calculate_age_compositions()
#' @param ... Additional arguments (not used)
#' @export
print.age_composition <- function(x, ...) {
  cat("Age Composition Analysis Results\n")
  cat("==============================\n\n")

  cat("Data Summary:\n")
  cat("  Age range:", min(x$ages), "-", max(x$ages), "\n")
  cat("  Number of strata:", length(x$strata_names), "\n")
  cat("  Strata:", paste(x$strata_names, collapse = ", "), "\n")
  cat("  Scaling type:", x$scaling_type, "\n")
  cat("  Bootstrap iterations:", x$n_bootstraps, "\n")
  cat("\n")

  if (x$n_bootstraps > 0) {
    cat("Uncertainty Estimation: Available (bootstrap-based CVs and 95% CIs)\n")
  } else {
    cat("Uncertainty Estimation: Not available (no bootstrap iterations)\n")
  }
  cat("\n")

  # Show pooled age composition summary
  cat("Pooled Age Composition Summary (across all strata):\n")
  pooled_total <- x$pooled_age_composition[, "total"]
  pooled_props <- x$pooled_age_proportions[, "total"]

  summary_df <- data.frame(
    Age = x$ages,
    Count = round(pooled_total, 1),
    Proportion = round(pooled_props, 4)
  )

  if (x$n_bootstraps > 0) {
    summary_df$CV <- round(x$pooled_age_cv[, "total"], 3)
    summary_df$CI_Lower <- round(x$pooled_age_ci_lower[, "total"], 1)
    summary_df$CI_Upper <- round(x$pooled_age_ci_upper[, "total"], 1)
  }

  print(summary_df, row.names = FALSE)

  cat("\n")
  cat("Use plot(x) to visualize age compositions\n")

  if (x$n_bootstraps > 0) {
    cat("Use calculate_multinomial_n(x) to estimate effective sample sizes\n")
  }
}

#' Plot Method for Age Composition Objects
#'
#' Creates visualizations of age composition data with optional uncertainty ribbons.
#'
#' @param x An age_composition object from calculate_age_compositions()
#' @param type Character, type of plot: "composition" (default) or "proportion"
#' @param strata Character vector of strata to plot. If NULL, plots all strata plus pooled
#' @param sex_categories Character vector of sex categories to include: "male", "female", "unsexed", "total" (default all)
#' @param include_uncertainty Logical, whether to show bootstrap uncertainty ribbons (default TRUE)
#' @param include_pooled Logical, whether to include pooled results (default TRUE)
#' @param age_bins Numeric, bin width for aggregating ages (default 1, no binning)
#' @param ... Additional arguments passed to plotting functions
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon facet_grid labs scale_colour_manual scale_fill_manual
#' @importFrom tools toTitleCase
#' @importFrom rlang .data
plot.age_composition <- function(x,
                                 type = "composition",
                                 strata = NULL,
                                 sex_categories = c("male", "female", "unsexed", "total"),
                                 include_uncertainty = TRUE,
                                 include_pooled = TRUE,
                                 age_bins = 1,
                                 ...) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Install with: install.packages('ggplot2')")
  }

  # Validate plot type
  if (!type %in% c("composition", "proportion")) {
    stop("type must be 'composition' or 'proportion'")
  }

  # Validate sex categories
  valid_sexes <- c("male", "female", "unsexed", "total")
  sex_categories <- sex_categories[sex_categories %in% valid_sexes]
  if (length(sex_categories) == 0) {
    stop("No valid sex categories specified")
  }

  # Determine strata to plot
  if (is.null(strata)) {
    plot_strata <- if (include_pooled) c("Pooled", x$strata_names) else x$strata_names
  } else {
    plot_strata <- strata
    if (include_pooled && !"Pooled" %in% plot_strata) {
      plot_strata <- c("Pooled", plot_strata)
    }
  }

  # Prepare data for plotting
  plot_data <- prepare_age_plot_data(x, type, plot_strata, sex_categories, age_bins)

  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age, y = .data$value, colour = .data$sex, fill = .data$sex))

  # Add uncertainty ribbons if requested and available
  if (include_uncertainty && x$n_bootstraps > 0) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
      alpha = 0.3, colour = NA
    )
  }

  # Add lines
  p <- p + ggplot2::geom_line(size = 1)

  # Add faceting
  p <- p + ggplot2::facet_grid(.data$stratum ~ ., scales = "free_y")

  # Customize appearance
  sex_colors <- c("male" = "#2E86AB", "female" = "#E63946", "unsexed" = "#F77F00", "total" = "#06D6A0")
  p <- p +
    ggplot2::scale_colour_manual(values = sex_colors, name = "Sex") +
    ggplot2::scale_fill_manual(values = sex_colors, name = "Sex") +
    ggplot2::labs(
      x = "Age",
      y = if (type == "composition") "Age Composition" else "Proportion",
      title = paste("Age", tools::toTitleCase(type), "by Stratum and Sex"),
      subtitle = if (include_uncertainty && x$n_bootstraps > 0) {
        paste("Lines show point estimates, ribbons show 95% confidence intervals (", x$n_bootstraps, "bootstraps)")
      } else {
        "Point estimates only"
      }
    )

  return(p)
}

#' Prepare Age Composition Data for Plotting
#'
#' Internal function to prepare age composition data for ggplot2 visualization.
#'
#' @param x Age composition object
#' @param type Plot type ("composition" or "proportion")
#' @param plot_strata Strata to include
#' @param sex_categories Sex categories to include
#' @param age_bins Age binning width
#'
#' @return Data frame ready for plotting
#' @keywords internal
prepare_age_plot_data <- function(x, type, plot_strata, sex_categories, age_bins) {
  plot_data_list <- list()

  for (stratum in plot_strata) {
    for (sex in sex_categories) {
      # Extract appropriate data
      if (stratum == "Pooled") {
        if (type == "composition") {
          values <- x$pooled_age_composition[, sex]
          if (x$n_bootstraps > 0) {
            ci_lower <- x$pooled_age_ci_lower[, sex]
            ci_upper <- x$pooled_age_ci_upper[, sex]
          }
        } else {
          values <- x$pooled_age_proportions[, sex]
          if (x$n_bootstraps > 0) {
            ci_lower <- x$pooled_age_proportions_ci_lower[, sex]
            ci_upper <- x$pooled_age_proportions_ci_upper[, sex]
          }
        }
      } else {
        stratum_idx <- which(x$strata_names == stratum)
        if (type == "composition") {
          values <- x$age_composition[, sex, stratum_idx]
          if (x$n_bootstraps > 0) {
            ci_lower <- x$age_ci_lower[, sex, stratum_idx]
            ci_upper <- x$age_ci_upper[, sex, stratum_idx]
          }
        } else {
          values <- x$age_proportions[, sex, stratum_idx]
          if (x$n_bootstraps > 0) {
            ci_lower <- x$age_proportions_ci_lower[, sex, stratum_idx]
            ci_upper <- x$age_proportions_ci_upper[, sex, stratum_idx]
          }
        }
      }

      # Apply age binning if requested
      ages <- x$ages
      if (age_bins > 1) {
        # Bin ages - simple implementation
        binned_ages <- seq(min(ages), max(ages), by = age_bins)
        binned_values <- numeric(length(binned_ages))
        if (x$n_bootstraps > 0) {
          binned_ci_lower <- numeric(length(binned_ages))
          binned_ci_upper <- numeric(length(binned_ages))
        }

        for (i in seq_along(binned_ages)) {
          age_range <- if (i < length(binned_ages)) {
            (ages >= binned_ages[i]) & (ages < binned_ages[i + 1])
          } else {
            ages >= binned_ages[i]
          }
          binned_values[i] <- sum(values[age_range], na.rm = TRUE)
          if (x$n_bootstraps > 0) {
            binned_ci_lower[i] <- sum(ci_lower[age_range], na.rm = TRUE)
            binned_ci_upper[i] <- sum(ci_upper[age_range], na.rm = TRUE)
          }
        }

        ages <- binned_ages
        values <- binned_values
        if (x$n_bootstraps > 0) {
          ci_lower <- binned_ci_lower
          ci_upper <- binned_ci_upper
        }
      }

      # Create data frame
      df <- data.frame(
        age = ages,
        value = values,
        sex = sex,
        stratum = stratum,
        stringsAsFactors = FALSE
      )

      if (x$n_bootstraps > 0) {
        df$ci_lower <- ci_lower
        df$ci_upper <- ci_upper
      } else {
        df$ci_lower <- df$value
        df$ci_upper <- df$value
      }

      plot_data_list[[paste(stratum, sex, sep = "_")]] <- df
    }
  }

  # Combine all data
  plot_data <- do.call(rbind, plot_data_list)
  rownames(plot_data) <- NULL

  # Set factor levels for proper ordering
  plot_data$stratum <- factor(plot_data$stratum, levels = plot_strata)
  plot_data$sex <- factor(plot_data$sex, levels = c("male", "female", "unsexed", "total"))

  return(plot_data)
}
