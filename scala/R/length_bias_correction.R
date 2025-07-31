#' Length Measurement Bias Correction Functions
#'
#' Functions to detect and correct length measurement bias (rounding/heaping)
#' in fisheries data, particularly preference for lengths divisible by 5.
#'
#' @name length_bias_correction
#' @aliases length_bias_correction
#'
#' @importFrom dplyr "%>%" count arrange mutate left_join select rename group_by ungroup
#' @importFrom zoo rollmean
#' @importFrom stats chisq.test ecdf quantile runif
#' @importFrom graphics hist plot abline legend boxplot lines par
#' @importFrom utils globalVariables

# Suppress R CMD check notes for dplyr non-standard evaluation
globalVariables(c(
  "observed_count", "smooth_count", "redistribution_factor",
  "adjustment", "corrected_length", "bias_factor", "proportion",
  "adjusted_proportion", "length", "age"
))

#' Diagnose Length Measurement Bias
#'
#' Detects preference for certain length values (e.g., multiples of 5)
#' in fish measurement data.
#'
#' @param fish_data Data frame containing fish measurements with 'length' column
#' @param modulus Numeric, divisor to check for preference (default 5 for multiples of 5)
#' @param plot_results Logical, whether to create diagnostic plots (default TRUE)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{preference_ratio}: Ratio of mean frequency at preferred vs non-preferred lengths
#'     \item \code{bias_severity}: Character assessment ("None", "Mild", "Moderate", "Strong")
#'     \item \code{preferred_lengths}: Vector of lengths showing preference
#'     \item \code{length_frequencies}: Data frame with length frequencies
#'     \item \code{chi_square_test}: Chi-square test results for uniformity
#'   }
#'
#' @examples
#' \dontrun{
#' test_data <- generate_test_data()
#' bias_check <- diagnose_length_bias(test_data$fish_data)
#' print(bias_check$bias_severity)
#' }
#'
#' @export
diagnose_length_bias <- function(fish_data, modulus = 5, plot_results = TRUE) {
  # Validate input
  if (!"length" %in% names(fish_data)) {
    stop("fish_data must contain a 'length' column")
  }

  # Calculate length frequencies
  length_freq <- table(fish_data$length)
  lengths <- as.numeric(names(length_freq))
  counts <- as.numeric(length_freq)

  # Create frequency data frame
  freq_df <- data.frame(
    length = lengths,
    count = counts,
    is_preferred = (lengths %% modulus == 0)
  )

  # Calculate preference statistics
  preferred_counts <- counts[lengths %% modulus == 0]
  non_preferred_counts <- counts[lengths %% modulus != 0]

  preference_ratio <- ifelse(length(non_preferred_counts) > 0,
    mean(preferred_counts) / mean(non_preferred_counts),
    NA
  )

  # Classify bias severity
  bias_severity <- if (is.na(preference_ratio)) {
    "Unknown"
  } else if (preference_ratio < 1.1) {
    "None"
  } else if (preference_ratio < 1.3) {
    "Mild"
  } else if (preference_ratio < 1.8) {
    "Moderate"
  } else {
    "Strong"
  }

  # Chi-square test for uniformity
  # Test if frequencies deviate from expected uniform distribution
  chi_test <- chisq.test(counts, p = rep(1 / length(counts), length(counts)))

  # Identify specific preferred lengths (outliers)
  mean_count <- mean(counts)
  sd_count <- sd(counts)
  threshold <- mean_count + 1.5 * sd_count
  preferred_lengths <- lengths[counts > threshold & (lengths %% modulus == 0)]

  # Create diagnostic plots
  if (plot_results) {
    par(mfrow = c(2, 2))

    # 1. Length frequency histogram
    hist(fish_data$length,
      breaks = max(20, length(unique(fish_data$length)) / 2),
      main = "Length Frequency Distribution",
      xlab = "Length (cm)", ylab = "Frequency",
      col = "lightblue", border = "white"
    )

    # Highlight preferred lengths
    if (length(preferred_lengths) > 0) {
      abline(v = preferred_lengths, col = "red", lwd = 2, lty = 2)
      legend("topright", "Preferred lengths", col = "red", lty = 2, lwd = 2)
    }

    # 2. Length frequency by modulus
    boxplot(counts ~ I(lengths %% modulus),
      main = paste("Frequency by Modulus", modulus),
      xlab = paste("Length modulus", modulus),
      ylab = "Frequency",
      col = c("lightcoral", rep("lightblue", modulus - 1))
    )

    # 3. Frequency vs length scatter
    plot(lengths, counts,
      type = "h", lwd = 2,
      main = "Length Frequency Pattern",
      xlab = "Length (cm)", ylab = "Frequency",
      col = ifelse(lengths %% modulus == 0, "red", "blue")
    )

    # Add preference threshold line
    abline(h = threshold, col = "orange", lwd = 2, lty = 3)
    legend("topright",
      c("Preferred (mod 5)", "Regular", "Bias threshold"),
      col = c("red", "blue", "orange"),
      lty = c(1, 1, 3), lwd = 2
    )

    # 4. Cumulative distribution
    plot(ecdf(fish_data$length),
      main = "Cumulative Distribution",
      xlab = "Length (cm)", ylab = "Cumulative Probability"
    )

    # Reset plotting layout
    par(mfrow = c(1, 1))
  }

  # Print summary
  cat("Length Measurement Bias Diagnosis\n")
  cat("==================================\n")
  cat("Total fish measured:", nrow(fish_data), "\n")
  cat("Length range:", min(fish_data$length), "-", max(fish_data$length), "cm\n")
  cat("Preference ratio (mod", modulus, "):", round(preference_ratio, 2), "\n")
  cat("Bias severity:", bias_severity, "\n")
  cat("Chi-square p-value:", format.pval(chi_test$p.value), "\n")

  if (length(preferred_lengths) > 0) {
    cat("Specific preferred lengths:", paste(preferred_lengths, collapse = ", "), "\n")
  }

  cat("\nInterpretation:\n")
  cat("- Ratio > 1.5: Strong bias likely\n")
  cat("- Chi-square p < 0.05: Non-uniform distribution\n")
  cat("- Red spikes in plots: Oversampled lengths\n\n")

  # Return results
  list(
    preference_ratio = preference_ratio,
    bias_severity = bias_severity,
    preferred_lengths = preferred_lengths,
    length_frequencies = freq_df,
    chi_square_test = chi_test,
    summary_stats = list(
      total_fish = nrow(fish_data),
      length_range = range(fish_data$length),
      mean_frequency = mean_count,
      sd_frequency = sd_count
    )
  )
}

#' Correct Length Measurement Bias
#'
#' Applies smoothing to reduce the effects of length measurement bias
#' by redistributing fish from over-represented to under-represented lengths.
#'
#' @param fish_data Data frame containing fish measurements with 'length' column
#' @param method Character, correction method: "smooth" (default), "jitter", or "redistribute"
#' @param smoothing_window Numeric, window size for smoothing (default 2)
#' @param max_adjustment Numeric, maximum adjustment factor (default 2.0)
#' @param preserve_total Logical, whether to preserve total number of fish (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return Data frame with corrected length measurements. Original length
#'   is preserved in 'original_length' column.
#'
#' @examples
#' \dontrun{
#' test_data <- generate_test_data()
#' corrected_data <- correct_length_bias(test_data$fish_data, method = "smooth")
#'
#' # Compare distributions
#' par(mfrow = c(1, 2))
#' hist(test_data$fish_data$length, main = "Original")
#' hist(corrected_data$length, main = "Corrected")
#' }
#'
#' @export
correct_length_bias <- function(fish_data,
                                method = "smooth",
                                smoothing_window = 2,
                                max_adjustment = 2.0,
                                preserve_total = TRUE,
                                verbose = TRUE) {
  # Validate inputs
  if (!"length" %in% names(fish_data)) {
    stop("fish_data must contain a 'length' column")
  }

  method <- match.arg(method, c("smooth", "jitter", "redistribute"))

  if (verbose) cat("Applying", method, "correction to length measurements...\n")

  # Store original data
  corrected_data <- fish_data
  corrected_data$original_length <- fish_data$length

  if (method == "smooth") {
    # Method 1: Smoothing approach
    length_counts <- fish_data %>%
      count(length, name = "observed_count") %>%
      arrange(length)

    # Calculate smoothed counts using rolling mean
    length_counts <- length_counts %>%
      mutate(
        # Apply rolling mean smoothing
        smooth_count = zoo::rollmean(observed_count,
          k = smoothing_window * 2 + 1,
          fill = "extend",
          align = "center"
        ),
        # Calculate redistribution factor
        redistribution_factor = smooth_count / observed_count,
        # Cap extreme adjustments
        redistribution_factor = pmax(
          1 / max_adjustment,
          pmin(max_adjustment, redistribution_factor)
        )
      )

    # Apply length adjustments
    corrected_data <- corrected_data %>%
      left_join(length_counts %>% select(length, redistribution_factor),
        by = "length"
      ) %>%
      mutate(
        # Add small random adjustment proportional to correction needed
        adjustment = (redistribution_factor - 1) * runif(n(), -0.4, 0.4),
        corrected_length = length + adjustment,
        # Round to integer lengths
        corrected_length = round(corrected_length)
      ) %>%
      select(-redistribution_factor, -adjustment) %>%
      rename(length = corrected_length)
  } else if (method == "jitter") {
    # Method 2: Add small random jitter to break up heaping
    corrected_data$length <- corrected_data$length +
      runif(nrow(corrected_data), -0.4, 0.4)
    corrected_data$length <- round(corrected_data$length)
  } else if (method == "redistribute") {
    # Method 3: Systematic redistribution
    modulus <- 5
    preferred_fish <- corrected_data[corrected_data$length %% modulus == 0, ]
    regular_fish <- corrected_data[corrected_data$length %% modulus != 0, ]

    # Randomly redistribute some preferred fish to adjacent lengths
    if (nrow(preferred_fish) > 0) {
      redistribute_prop <- 0.3 # Redistribute 30% of preferred fish
      n_redistribute <- round(nrow(preferred_fish) * redistribute_prop)

      if (n_redistribute > 0) {
        redistribute_idx <- sample(nrow(preferred_fish), n_redistribute)

        # Adjust lengths by +/-1 or +/-2
        adjustments <- sample(c(-2, -1, 1, 2), n_redistribute, replace = TRUE)
        preferred_fish$length[redistribute_idx] <-
          preferred_fish$length[redistribute_idx] + adjustments

        corrected_data <- rbind(preferred_fish, regular_fish)
      }
    }
  }

  # Ensure all lengths are positive and within reasonable bounds
  corrected_data$length <- pmax(1, corrected_data$length)

  # Preserve total count if requested
  if (preserve_total && nrow(corrected_data) != nrow(fish_data)) {
    warning("Total fish count changed during correction")
  }

  # Report changes
  if (verbose) {
    original_range <- range(fish_data$length)
    corrected_range <- range(corrected_data$length)

    cat("Correction summary:\n")
    cat("- Original range:", original_range[1], "-", original_range[2], "cm\n")
    cat("- Corrected range:", corrected_range[1], "-", corrected_range[2], "cm\n")
    cat("- Total fish:", nrow(corrected_data), "\n")

    # Check if bias was reduced
    original_bias <- diagnose_length_bias(fish_data, plot_results = FALSE)
    corrected_bias <- diagnose_length_bias(corrected_data, plot_results = FALSE)

    cat(
      "- Preference ratio: ", round(original_bias$preference_ratio, 2),
      " -> ", round(corrected_bias$preference_ratio, 2), "\n"
    )
    cat(
      "- Bias severity: ", original_bias$bias_severity,
      " -> ", corrected_bias$bias_severity, "\n"
    )
  }

  return(corrected_data)
}

#' Create Bias-Adjusted Age-Length Key
#'
#' Adjusts age-length key proportions to account for known length measurement bias.
#'
#' @param age_length_key Data frame with columns: length, age, proportion
#' @param bias_factors Data frame with columns: length, bias_factor
#'   (bias_factor > 1 = over-measured, < 1 = under-measured)
#' @param modulus Numeric, modulus for automatic bias factor calculation (default 5)
#' @param bias_strength Numeric, strength of automatic bias (default 1.5)
#'
#' @return Adjusted age-length key with corrected proportions
#'
#' @examples
#' \dontrun{
#' # Create age-length key
#' alk <- generate_age_length_key(length_range = c(20, 40), age_range = c(1, 8))
#'
#' # Create bias pattern (multiples of 5 over-represented)
#' bias_pattern <- data.frame(
#'   length = 20:40,
#'   bias_factor = ifelse((20:40) %% 5 == 0, 1.5, 1.0)
#' )
#'
#' # Adjust age-length key
#' adjusted_alk <- adjust_age_length_key_bias(alk, bias_pattern)
#' }
#'
#' @export
adjust_age_length_key_bias <- function(age_length_key,
                                       bias_factors = NULL,
                                       modulus = 5,
                                       bias_strength = 1.5) {
  # Validate input
  required_cols <- c("length", "age", "proportion")
  if (!all(required_cols %in% names(age_length_key))) {
    stop("age_length_key must contain columns: ", paste(required_cols, collapse = ", "))
  }

  # Create automatic bias factors if not provided
  if (is.null(bias_factors)) {
    unique_lengths <- unique(age_length_key$length)
    bias_factors <- data.frame(
      length = unique_lengths,
      bias_factor = ifelse(unique_lengths %% modulus == 0, bias_strength, 1.0)
    )
  }

  # Apply bias adjustment
  adjusted_alk <- age_length_key %>%
    left_join(bias_factors, by = "length") %>%
    mutate(
      bias_factor = ifelse(is.na(bias_factor), 1.0, bias_factor),
      # Adjust proportions (higher bias_factor = reduce proportion)
      adjusted_proportion = proportion / bias_factor
    ) %>%
    group_by(length) %>%
    mutate(
      # Renormalize proportions to sum to 1 for each length
      adjusted_proportion = adjusted_proportion / sum(adjusted_proportion)
    ) %>%
    ungroup() %>%
    select(length, age, proportion = adjusted_proportion)

  return(adjusted_alk)
}

#' Compare Length Distributions
#'
#' Creates side-by-side comparison plots of original vs corrected length distributions.
#'
#' @param original_data Data frame with original fish data
#' @param corrected_data Data frame with corrected fish data
#' @param title Character, plot title (default "Length Distribution Comparison")
#'
#' @return NULL (creates plots)
#'
#' @examples
#' \dontrun{
#' test_data <- generate_test_data()
#' corrected_data <- correct_length_bias(test_data$fish_data)
#' compare_length_distributions(test_data$fish_data, corrected_data)
#' }
#'
#' @export
compare_length_distributions <- function(original_data, corrected_data,
                                         title = "Length Distribution Comparison") {
  par(mfrow = c(2, 2))

  # 1. Histograms
  hist(original_data$length,
    breaks = 20,
    main = "Original Distribution",
    xlab = "Length (cm)", col = "lightcoral", border = "white"
  )

  hist(corrected_data$length,
    breaks = 20,
    main = "Corrected Distribution",
    xlab = "Length (cm)", col = "lightblue", border = "white"
  )

  # 2. Frequency comparison
  orig_freq <- table(original_data$length)
  corr_freq <- table(corrected_data$length)

  # Align frequency tables
  all_lengths <- sort(unique(c(names(orig_freq), names(corr_freq))))
  orig_aligned <- rep(0, length(all_lengths))
  corr_aligned <- rep(0, length(all_lengths))

  orig_aligned[all_lengths %in% names(orig_freq)] <- orig_freq[all_lengths[all_lengths %in% names(orig_freq)]]
  corr_aligned[all_lengths %in% names(corr_freq)] <- corr_freq[all_lengths[all_lengths %in% names(corr_freq)]]

  plot(as.numeric(all_lengths), orig_aligned,
    type = "h", lwd = 3, col = "red",
    main = "Frequency Comparison", xlab = "Length (cm)", ylab = "Frequency"
  )
  lines(as.numeric(all_lengths), corr_aligned, type = "h", lwd = 2, col = "blue")
  legend("topright", c("Original", "Corrected"), col = c("red", "blue"), lwd = c(3, 2))

  # 3. Cumulative distributions
  plot(ecdf(original_data$length),
    col = "red", lwd = 2,
    main = "Cumulative Distributions", xlab = "Length (cm)"
  )
  lines(ecdf(corrected_data$length), col = "blue", lwd = 2)
  legend("bottomright", c("Original", "Corrected"), col = c("red", "blue"), lwd = 2)

  par(mfrow = c(1, 1))
}
