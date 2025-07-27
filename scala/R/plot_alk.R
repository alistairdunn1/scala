#' Plot alk (Age-Length Key)
#'
#' Creates a heatmap visualization of the age-length key showing
#' the probability distribution of ages for each length.
#'
#' @param alk Age-length key data frame
#' @param by_sex Logical, whether to plot by sex (default TRUE)
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs theme_minimal
#' @importFrom rlang .data
plot_alk <- function(alk, by_sex = TRUE) {
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting. Install with: install.packages('ggplot2')")
  }
  if (by_sex) {
    alk <- dplyr::bind_rows(alk, .id = "sex")
  }

  p <- ggplot2::ggplot(alk, ggplot2::aes(x = .data$length, y = .data$age, fill = .data$proportion)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = "Proportion") +
    ggplot2::labs(
      x = "Length",
      y = "Age"
    )
  if (by_sex) {
    p <- p + ggplot2::facet_wrap(~ .data$sex)
  }

  return(p)
}
