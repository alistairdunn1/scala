#' Print method for scaled_length_frequency objects
print.scaled_length_frequency <- function(x, show_cvs = TRUE, digits = 2, ...) {
  cat("Scaled Length Frequency Results\n")
  cat("===============================\n\n")
  
  cat("Length range:", min(x$lengths), "to", max(x$lengths), "\n")
  cat("Number of strata:", length(x$strata_names), "\n")
  cat("Bootstrap iterations:", x$n_bootstraps, "\n\n")
  
  cat("Pooled Length Frequency:\n")
  pooled_df <- data.frame(
    Length = x$lengths,
    Count = round(x$pooled_length_frequency, digits),
    Proportion = round(x$pooled_proportions, digits + 2)
  )
  
  if (show_cvs && !is.null(x$pooled_lf_cv)) {
    pooled_df$CV_Count <- round(x$pooled_lf_cv * 100, 1)
    pooled_df$CV_Prop <- round(x$pooled_proportions_cv * 100, 1)
  }
  
  print(pooled_df)
  
  cat("\nBy Stratum:\n")
  for (i in 1:length(x$strata_names)) {
    cat("\nStratum:", x$strata_names[i], "\n")
    stratum_df <- data.frame(
      Length = x$lengths,
      Count = round(x$length_frequency[, i], digits),
      Proportion = round(x$proportions[, i], digits + 2)
    )
    
    if (show_cvs && !is.null(x$lf_cvs)) {
      stratum_df$CV_Count <- round(x$lf_cvs[, i] * 100, 1)
      stratum_df$CV_Prop <- round(x$proportions_cvs[, i] * 100, 1)
    }
    
    print(stratum_df)
  }
}

