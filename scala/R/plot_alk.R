#' Plot alk (Age-Length Key)
#'
#' Creates a visualisation of the age-length key showing
#' the probability distribution of ages for each length, with
#' ages on the x-axis and lengths on the y-axis.
#'
#' @param alk Age-length key data frame
#' @param by_sex Logical, whether to plot by sex (default TRUE)
#' @param type Character, type of plot: "heatmap" for heatmap visualization
#'   or "points" for point plot (default "heatmap")
#' @param rug Logical, whether to add a rug plot on the y-axis showing length distribution (default FALSE)
#' @param raw_data Optional data frame containing raw age-length observations with columns 'age', 'length',
#'   and optionally 'sex'. If provided, a red loess smoothing line will be added to the plot.
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_point geom_rug scale_fill_viridis_c scale_size_continuous labs theme_minimal geom_smooth
#' @importFrom rlang .data
plot_alk <- function(alk, by_sex = TRUE, type = "heatmap", rug = FALSE, raw_data = NULL) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Install with: install.packages('ggplot2')")
  }

  # Match plot type using pmatch
  type <- pmatch(type, c("heatmap", "points"))
  if (is.na(type)) {
    stop("'type' must be 'heatmap' or 'points'")
  }
  type_names <- c("heatmap", "points")
  type <- type_names[type]

  # Handle data structure based on by_sex parameter
  if (by_sex) {
    if (is.list(alk) && !is.data.frame(alk)) {
      # Process each sex dataframe separately to ensure they work with bind_rows
      processed_list <- list()
      for (sex_name in names(alk)) {
        sex_data <- alk[[sex_name]]
        if (!is.null(sex_data) && nrow(sex_data) > 0) {
          # Keep only essential columns
          if (all(c("length", "age", "proportion") %in% names(sex_data))) {
            processed_list[[sex_name]] <- sex_data[, c("length", "age", "proportion")]
          }
        }
      }

      # Now combine the processed data frames
      if (length(processed_list) > 0) {
        alk <- dplyr::bind_rows(processed_list, .id = "sex")
      } else {
        stop("No valid data found in the age-length key")
      }
    }
  } else {
    if (is.list(alk) && !is.data.frame(alk)) {
      # For non-sex specific plotting, combine all data without sex column
      # First process each sex dataframe to ensure consistency
      processed_list <- list()
      for (sex_name in names(alk)) {
        sex_data <- alk[[sex_name]]
        if (!is.null(sex_data) && nrow(sex_data) > 0) {
          # Keep only essential columns
          if (all(c("length", "age", "proportion") %in% names(sex_data))) {
            processed_list[[sex_name]] <- sex_data[, c("length", "age", "proportion")]
          }
        }
      }

      # Now combine the processed data frames
      if (length(processed_list) > 0) {
        alk <- dplyr::bind_rows(processed_list)
      } else {
        stop("No valid data found in the age-length key")
      }
    }
  } # Create base plot
  if (type == "heatmap") {
    p <- ggplot2::ggplot(alk, ggplot2::aes(x = .data$age, y = .data$length, fill = .data$proportion)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(name = "Proportion") +
      ggplot2::labs(
        x = "Age",
        y = "Length"
      )
  } else if (type == "points") {
    # Check if 'n' column exists, if not use proportion for size
    if ("n" %in% names(alk) && any(!is.na(alk$n))) {
      maxN <- max(alk$n, na.rm = TRUE)
      if (maxN == 0 || is.infinite(maxN)) {
        # If all n values are 0 or missing, use proportion instead
        p <- ggplot2::ggplot(alk, ggplot2::aes(x = .data$age, y = .data$length, size = .data$proportion)) +
          ggplot2::geom_point(alpha = 0.4, colour = "royalblue") +
          ggplot2::scale_size_continuous(name = "Proportion", range = c(0.5, 5)) +
          ggplot2::labs(
            x = "Age",
            y = "Length"
          )
      } else {
        p <- ggplot2::ggplot(alk, ggplot2::aes(x = .data$age, y = .data$length, size = .data$n)) +
          ggplot2::geom_point(alpha = 0.4, colour = "royalblue") +
          ggplot2::scale_size_continuous(name = "Number", range = c(0.5, max(6, maxN / 5))) +
          ggplot2::labs(
            x = "Age",
            y = "Length"
          )
      }
    } else {
      # Fall back to using proportion if no 'n' column
      p <- ggplot2::ggplot(alk, ggplot2::aes(x = .data$age, y = .data$length, size = .data$proportion)) +
        ggplot2::geom_point(alpha = 0.4, colour = "royalblue") +
        ggplot2::scale_size_continuous(name = "Proportion", range = c(0.5, 5)) +
        ggplot2::labs(
          x = "Age",
          y = "Length"
        )
    }
  }

  # Add rug plot if requested
  if (rug) {
    # Create data for rug plot - expand lengths by their frequency/proportion
    rug_data <- alk[rep(seq_len(nrow(alk)), times = pmax(1, round(alk$proportion * 100))), ]
    p <- p + ggplot2::geom_rug(
      data = rug_data, ggplot2::aes(y = .data$length),
      inherit.aes = FALSE, alpha = 0.5, sides = "l", colour = "royalblue"
    )
  }

  # Add loess smoothing line if raw data is provided
  if (!is.null(raw_data)) {
    # Validate raw_data has required columns
    if (!all(c("age", "length") %in% names(raw_data))) {
      warning("raw_data must contain 'age' and 'length' columns. Loess line not added.")
    } else {
      # Handle sex-specific data if by_sex is TRUE
      if (by_sex && "sex" %in% names(raw_data)) {
        # Filter raw_data to match the format expected
        loess_data <- raw_data[, c("age", "length", "sex")]
      } else if (!by_sex) {
        # Use raw data without sex column
        loess_data <- raw_data[, c("age", "length")]
      } else {
        # by_sex is TRUE but no sex column in raw_data
        loess_data <- raw_data[, c("age", "length")]
      }
      
      # Add loess smoothing line
      p <- p + ggplot2::geom_smooth(
        data = loess_data,
        ggplot2::aes(x = .data$age, y = .data$length),
        method = "loess",
        se = FALSE,
        colour = "red",
        linewidth = 1,
        inherit.aes = FALSE
      )
    }
  }

  if (by_sex) {
    p <- p + ggplot2::facet_wrap(~ .data$sex)
  }

  return(p)
}
