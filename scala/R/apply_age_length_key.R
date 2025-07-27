#' Apply Age-Length Key to Length Compositions
#'
#' Internal function to convert length compositions to age compositions using age-length key.
#' Supports both single keys (applied to all sexes) and sex-specific keys.
#' Optimized implementation using vectorized operations and pre-computed matrices.
#'
#' @param length_comp 3D array of length compositions (length x sex x stratum)
#' @param age_length_key Either a single data frame or a named list of sex-specific data frames
#' @param ages Vector of age bins
#' @param lengths Vector of length bins
#' @param plus_group_age Logical, whether to apply plus group for ages
#' @param minus_group_age Logical, whether to apply minus group for ages
#' @param sex_specific_keys Logical, whether sex-specific keys are being used
#'
#' @return 3D array of age compositions (age x sex x stratum)
#' @keywords internal
apply_age_length_key <- function(length_comp, age_length_key, ages, lengths,
                                 plus_group_age = FALSE, minus_group_age = FALSE,
                                 sex_specific_keys = FALSE) {
  n_ages <- length(ages)
  n_lengths <- length(lengths)
  n_strata <- dim(length_comp)[3]
  sex_categories <- c("composition", "male", "female", "unsexed", "total")

  # Initialize age composition array
  age_comp <- array(0, dim = c(n_ages, 5, n_strata))
  dimnames(age_comp) <- list(ages, sex_categories, dimnames(length_comp)[[3]])

  # Set age values in first column
  age_comp[, "composition", ] <- ages

  # Helper function to create optimized ALK matrix from key data
  create_alk_matrix <- function(key_data) {
    # Pre-allocate matrix with proper dimensions
    alk_matrix <- matrix(0, nrow = n_lengths, ncol = n_ages)

    # Create index mappings (much faster than string matching)
    length_indices <- match(key_data$length, lengths)
    age_indices <- match(key_data$age, ages)

    # Filter valid indices
    valid_rows <- !is.na(length_indices) & !is.na(age_indices)

    if (any(valid_rows)) {
      # Vectorized assignment using matrix indexing
      matrix_indices <- cbind(length_indices[valid_rows], age_indices[valid_rows])
      alk_matrix[matrix_indices] <- key_data$proportion[valid_rows]
    }

    return(alk_matrix)
  }

  # Apply ALK to length compositions using vectorized operations
  apply_alk_vectorized <- function(alk_matrix, length_data) {
    # Matrix multiplication: length_data %*% alk_matrix
    # This replaces the nested loops with a single vectorized operation
    return(as.vector(length_data %*% alk_matrix))
  }

  if (sex_specific_keys) {
    # Pre-compute all ALK matrices for sex-specific keys
    alk_matrices <- vector("list", length = 3)
    names(alk_matrices) <- c("male", "female", "unsexed")

    for (sex in c("male", "female", "unsexed")) {
      if (sex %in% names(age_length_key)) {
        alk_matrices[[sex]] <- create_alk_matrix(age_length_key[[sex]])
      }
    }

    # Apply sex-specific keys using vectorized operations
    for (s in 1:n_strata) {
      for (sex_idx in 2:4) { # male, female, unsexed
        sex_name <- sex_categories[sex_idx]

        if (!is.null(alk_matrices[[sex_name]])) {
          # Extract length composition for this sex and stratum
          length_data <- length_comp[, sex_name, s]

          # Apply ALK using vectorized matrix multiplication
          age_result <- apply_alk_vectorized(alk_matrices[[sex_name]], length_data)

          # Assign results
          age_comp[, sex_name, s] <- age_result
        }
      }

      # Calculate total as vectorized sum
      age_comp[, "total", s] <- rowSums(age_comp[, c("male", "female", "unsexed"), s])
    }
  } else {
    # Single age-length key - pre-compute matrix once
    alk_matrix <- create_alk_matrix(age_length_key)

    # Apply to all sex categories and strata using vectorized operations
    for (s in 1:n_strata) {
      # Process all sex categories (except composition) in vectorized manner
      for (sex_idx in 2:5) { # Skip "composition" column
        sex_name <- sex_categories[sex_idx]

        # Extract length composition
        length_data <- length_comp[, sex_name, s]

        # Apply ALK using vectorized operation
        age_result <- apply_alk_vectorized(alk_matrix, length_data)

        # Assign results
        age_comp[, sex_name, s] <- age_result
      }
    }
  }

  # Optimized plus/minus group handling with vectorized operations
  if (plus_group_age || minus_group_age) {
    # Get age range from keys more efficiently
    if (sex_specific_keys) {
      all_ages <- unlist(lapply(
        age_length_key[!sapply(age_length_key, is.null)],
        function(key) range(key$age)
      ))
      key_age_range <- range(all_ages)
    } else {
      key_age_range <- range(age_length_key$age)
    }

    if (plus_group_age && key_age_range[2] > max(ages)) {
      warning("Age-length key contains ages beyond specified age_range. Consider extending age_range or implementing plus group logic.")
    }

    if (minus_group_age && key_age_range[1] < min(ages)) {
      warning("Age-length key contains ages below specified age_range. Consider extending age_range or implementing minus group logic.")
    }
  }

  return(age_comp)
}
