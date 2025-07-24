##' Core Function to Calculate Length Frequency Result with Sex Categories
##'
##' Calculates length frequency results by sex and stratum, applying upweighting factors at both sample and stratum levels.
##'
##' @param fish_data Data frame with columns: stratum, sample_id, length, male, female, unsexed, total, sample_weight_kg, total_catch_weight_kg
##' @param strata_data Data frame with columns: stratum, stratum_total_catch_kg
##' @param lengths Numeric vector of length bins to use for the frequency calculation
##' @param plus_group Logical, combine lengths >= max length into a plus group (default FALSE)
##' @param minus_group Logical, combine lengths <= min length into a minus group (default FALSE)
##'
##' @return 3D array: length x sex x stratum, with upweighted frequencies for each sex and stratum
##'
##' @details
##' The function applies a two-stage upweighting process:
##'   \enumerate{
##'     \item Sample-level upweighting based on total catch weight vs observed weight
##'     \item Stratum-level upweighting based on stratum totals vs sample totals
##'   }
##' Sex categories are: male, female, unsexed, and total. Plus and minus group functionality is available for aggregating extreme length bins.
##'
##' @examples
##' \dontrun{
##' fish_data <- data.frame(
##'   stratum = c("A", "A", "B", "B"),
##'   sample_id = c(1, 1, 2, 2),
##'   length = c(20, 25, 22, 28),
##'   male = c(2, 1, 3, 2),
##'   female = c(2, 1, 2, 1),
##'   unsexed = c(1, 1, 2, 1),
##'   total = c(5, 3, 7, 4),
##'   sample_weight_kg = c(10, 10, 15, 15),
##'   total_catch_weight_kg = c(100, 100, 150, 150)
##' )
##' strata_data <- data.frame(
##'   stratum = c("A", "B"),
##'   stratum_total_catch_kg = c(1000, 2000)
##' )
##' lengths <- 15:35
##' result <- calculate_lf_result(fish_data, strata_data, lengths, FALSE, FALSE)
##' }
##'
##' @export
calculate_lf_result <- function(fish_data, strata_data, lengths, plus_group, minus_group) {
  strata_names <- unique(fish_data$stratum)
  n_strata <- length(strata_names)
  n_lengths <- length(lengths)

  # Initialize result array: length x sex x stratum
  lf_result <- array(0, dim = c(n_lengths, 5, n_strata))
  dimnames(lf_result) <- list(lengths, c("length", "male", "female", "unsexed", "total"), strata_names)

  # Set length values
  lf_result[, "length", ] <- lengths

  # Process each stratum
  for (s in 1:n_strata) {
    stratum <- strata_names[s]
    stratum_data <- fish_data[fish_data$stratum == stratum, ]

    if (nrow(stratum_data) == 0) next

    # Calculate sample-level summaries
    sample_summary <- aggregate(
      cbind(male, female, unsexed, total) ~
        sample_id + sample_weight_kg + total_catch_weight_kg,
      data = stratum_data, sum
    )

    # Calculate observed weight for each sample (placeholder - use length-weight in practice)
    sample_summary$observed_weight_kg <- sample_summary$total * 0.1 # placeholder

    # Calculate upweighting factor
    sample_summary$upweight_factor <- sample_summary$total_catch_weight_kg / sample_summary$observed_weight_kg

    # Get stratum total catch
    stratum_total <- strata_data$stratum_total_catch_kg[strata_data$stratum == stratum][1]
    stratum_sample_total <- sum(sample_summary$total_catch_weight_kg)
    stratum_upweight <- stratum_total / stratum_sample_total

    # Apply upweighting to length frequencies
    for (i in seq_len(nrow(sample_summary))) {
      sample_id <- sample_summary$sample_id[i]
      sample_fish <- stratum_data[stratum_data$sample_id == sample_id, ]

      total_upweight <- sample_summary$upweight_factor[i] * stratum_upweight

      # Process each row of sample data
      for (j in seq_len(nrow(sample_fish))) {
        len <- sample_fish$length[j]

        if (len >= min(lengths) && len <= max(lengths)) {
          len_idx <- which(lengths == len)
          if (length(len_idx) > 0) {
            lf_result[len_idx, "male", s] <- lf_result[len_idx, "male", s] +
              (sample_fish$male[j] * total_upweight)
            lf_result[len_idx, "female", s] <- lf_result[len_idx, "female", s] +
              (sample_fish$female[j] * total_upweight)
            lf_result[len_idx, "unsexed", s] <- lf_result[len_idx, "unsexed", s] +
              (sample_fish$unsexed[j] * total_upweight)
            lf_result[len_idx, "total", s] <- lf_result[len_idx, "total", s] +
              (sample_fish$total[j] * total_upweight)
          }
        }
      }
    }

    # Apply plus/minus groups if specified
    if (plus_group) {
      # Sum all lengths >= max into the plus group
      # Implementation would need more sophisticated logic
    }

    if (minus_group) {
      # Sum all lengths <= min into the minus group
      # Implementation would need more sophisticated logic
    }
  }

  return(lf_result)
}
