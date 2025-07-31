rug <- T #' Plot alk (Age-Length Key)
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
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_point geom_rug scale_fill_viridis_c scale_size_continuous labs theme_minimal
#' @importFrom rlang .data
plot_alk <- function(alk, by_sex = TRUE, type = "heatmap", rug = FALSE) {
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
      alk <- dplyr::bind_rows(alk, .id = "sex")
    }
  } else {
    if (is.list(alk) && !is.data.frame(alk)) {
      # For non-sex specific plotting, combine all data without sex column
      alk <- dplyr::bind_rows(alk)
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

  if (by_sex) {
    p <- p + ggplot2::facet_wrap(~ .data$sex)
  }

  return(p)
}
