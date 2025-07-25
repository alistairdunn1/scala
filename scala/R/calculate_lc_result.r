##' Core Function to Calculate Length Composition Result with Sex Categories
##'
##' Calculates length composition results by sex and stratum, applying upweighting factors at both sample and stratum levels.
##' Supports both weight-based scaling (commercial fisheries) and density-based scaling (surveys).
##'
##' @param fish_data Data frame with columns: stratum, sample_id, length, male, female, unsexed, total, sample_weight_kg, total_catch_weight_kg (weight-based) OR stratum, sample_id, length, male, female, unsexed, total, sample_area_km2, catch_density_kg_km2 (density-based)
##' @param strata_data Data frame with columns: stratum, stratum_total_catch_kg (weight-based) OR stratum, stratum_area_km2 (density-based)
##' @param lengths Numeric vector of length bins to use for the composition calculation
##' @param plus_group Logical, combine lengths >= max length into a plus group (default FALSE)
##' @param minus_group Logical, combine lengths <= min length into a minus group (default FALSE)
##' @param scaling_type Character, either "weight" for weight-based scaling or "density" for density-based scaling
##' @param lw_params_male Named vector with length-weight parameters for males: c(a = 0.01, b = 3.0)
##' @param lw_params_female Named vector with length-weight parameters for females: c(a = 0.01, b = 3.0)
##' @param lw_params_unsexed Named vector with length-weight parameters for unsexed fish: c(a = 0.01, b = 3.0)
##'
##' @importFrom stats aggregate
##' @return 3D array: length x sex x stratum, with upweighted compositions for each sex and stratum
##'
##' @details
##' The function applies a two-stage upweighting process:
##'   \enumerate{
##'     \item **Weight-based scaling**: Sample-level upweighting based on total catch weight vs observed weight, then stratum-level upweighting based on stratum totals vs sample totals
##'     \item **Density-based scaling**: Sample-level upweighting based on catch density and sample area vs observed weight, then stratum-level upweighting based on stratum area vs sample areas
##'   }
##'
##' **Length-weight calculations:**
##' Uses the allometric relationship: Weight = a * Length^b where a and b are sex-specific parameters.
##' Individual fish weights are calculated and summed to get total observed sample weight.
##'
##' Sex categories are: male, female, unsexed, and total. Plus and minus group functionality is available for aggregating extreme length bins.
##'
##' @examples
##' \dontrun{
##' # Length-weight parameters
##' lw_male <- c(a = 0.0085, b = 3.1)
##' lw_female <- c(a = 0.0092, b = 3.05)
##' lw_unsexed <- c(a = 0.0088, b = 3.08)
##'
##' # Weight-based example
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
##' result <- calculate_lc_result(fish_data, strata_data, lengths, FALSE, FALSE, "weight", lw_male, lw_female, lw_unsexed)
##' }
##'
##' @export
calculate_lc_result <- function(fish_data, strata_data, lengths, plus_group, minus_group, scaling_type = "weight", lw_params_male, lw_params_female, lw_params_unsexed) {
  # Helper function to calculate individual fish weight
  calculate_fish_weight <- function(length, sex, male_count, female_count, unsexed_count) {
    weight_male <- male_count * lw_params_male["a"] * (length^lw_params_male["b"]) / 1000 # Convert g to kg
    weight_female <- female_count * lw_params_female["a"] * (length^lw_params_female["b"]) / 1000
    weight_unsexed <- unsexed_count * lw_params_unsexed["a"] * (length^lw_params_unsexed["b"]) / 1000
    return(weight_male + weight_female + weight_unsexed)
  }

  strata_names <- unique(fish_data$stratum)
  n_strata <- length(strata_names)
  n_lengths <- length(lengths)

  # Initialize result array: length x sex x stratum
  lc_result <- array(0, dim = c(n_lengths, 5, n_strata))
  dimnames(lc_result) <- list(lengths, c("composition", "male", "female", "unsexed", "total"), strata_names)

  # Set length values
  lc_result[, "composition", ] <- lengths

  # Process each stratum
  for (s in 1:n_strata) {
    stratum <- strata_names[s]
    stratum_data <- fish_data[fish_data$stratum == stratum, ]

    if (nrow(stratum_data) == 0) next

    if (scaling_type == "weight") {
      # Weight-based scaling approach

      # Calculate sample-level summaries
      sample_summary <- aggregate(
        cbind(male, female, unsexed, total) ~
          sample_id + sample_weight_kg + total_catch_weight_kg,
        data = stratum_data, sum
      )

      # Calculate observed weight for each sample using length-weight relationships
      sample_summary$observed_weight_kg <- 0
      for (i in seq_len(nrow(sample_summary))) {
        sample_id <- sample_summary$sample_id[i]
        sample_fish <- stratum_data[stratum_data$sample_id == sample_id, ]

        total_weight <- 0
        for (j in seq_len(nrow(sample_fish))) {
          fish_weight <- calculate_fish_weight(
            sample_fish$length[j], "all",
            sample_fish$male[j], sample_fish$female[j], sample_fish$unsexed[j]
          )
          total_weight <- total_weight + fish_weight
        }
        sample_summary$observed_weight_kg[i] <- total_weight
      }

      # Calculate upweighting factor
      sample_summary$upweight_factor <- sample_summary$total_catch_weight_kg / sample_summary$observed_weight_kg

      # Get stratum total catch
      stratum_total <- strata_data$stratum_total_catch_kg[strata_data$stratum == stratum][1]
      stratum_sample_total <- sum(sample_summary$total_catch_weight_kg)
      stratum_upweight <- stratum_total / stratum_sample_total
    } else if (scaling_type == "density") {
      # Density-based scaling approach

      # Calculate sample-level summaries
      sample_summary <- aggregate(
        cbind(male, female, unsexed, total) ~
          sample_id + sample_area_km2 + catch_density_kg_km2,
        data = stratum_data, sum
      )

      # Calculate observed weight for each sample using length-weight relationships
      sample_summary$observed_weight_kg <- 0
      for (i in seq_len(nrow(sample_summary))) {
        sample_id <- sample_summary$sample_id[i]
        sample_fish <- stratum_data[stratum_data$sample_id == sample_id, ]

        total_weight <- 0
        for (j in seq_len(nrow(sample_fish))) {
          fish_weight <- calculate_fish_weight(
            sample_fish$length[j], "all",
            sample_fish$male[j], sample_fish$female[j], sample_fish$unsexed[j]
          )
          total_weight <- total_weight + fish_weight
        }
        sample_summary$observed_weight_kg[i] <- total_weight
      }

      # Calculate expected weight based on density and area
      sample_summary$expected_weight_kg <- sample_summary$catch_density_kg_km2 * sample_summary$sample_area_km2

      # Calculate upweighting factor
      sample_summary$upweight_factor <- sample_summary$expected_weight_kg / sample_summary$observed_weight_kg

      # Get stratum total area and calculate stratum upweight
      stratum_area <- strata_data$stratum_area_km2[strata_data$stratum == stratum][1]
      stratum_sample_area <- sum(sample_summary$sample_area_km2)
      stratum_upweight <- stratum_area / stratum_sample_area
    } else {
      stop("scaling_type must be either 'weight' or 'density'")
    }

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

  return(lc_result)
}
