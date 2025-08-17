#' @title Create Complete Age-Length Key
#' @description Creates a complete age-length key that includes all required length and age bins,
#'   with interpolation/extrapolation as needed for missing combinations.
#' @param age_data Data frame containing age-length data with required columns:
#'   'age', 'length', and optionally 'sex'. Each row represents one aged fish.
#' @param lengths Vector of all length bins that will be encountered (required)
#' @param ages Vector of all age bins (required)
#' @param tail_ages Named list specifying length-age pairs for lengths at tails
#'   Format: list(min_lengths = data.frame(length, age), max_lengths = data.frame(length, age))
#' @param min_ages_per_length Minimum number of ages required per length bin (default: 3)
#' @param optimum_ages_per_length Optimum number of ages per length bin (default: 10)
#' @param length_bin_size Numeric value for binning lengths (e.g., 2 for 2cm bins, 5 for 5cm bins).
#'   If NULL (default), uses raw length values without binning.
#' @param interpolate Logical, whether to allow linear interpolation for missing length bins (default: TRUE)
#' @param extrapolate Logical, whether to allow extrapolation for lengths outside the observed range (default: TRUE)
#' @param verbose Logical, whether to print progress messages
#' @return Age-length key object (data.frame for single key, named list for sex-specific keys)
#'   with interpolation information stored as attributes:
#'   \itemize{
#'     \item Standard age-length key format compatible with calculate_age_compositions()
#'     \item \code{attr(*, "interpolation_info")}: Data frame summarising interpolation methods used
#'     \item \code{attr(*, "is_sex_specific")}: Logical indicating if sex-specific keys were used
#'   }
#'
#' @examples
#' \dontrun{
#' # Create age data frame
#' age_data <- data.frame(
#'   age = c(1, 2, 2, 3, 3, 3, 4, 4, 5),
#'   length = c(20, 22, 23, 25, 26, 27, 29, 30, 32),
#'   sex = c("male", "male", "female", "male", "female", "male", "female", "male", "female")
#' )
#'
#' # Create a complete age-length key
#' complete_alk <- create_alk(
#'   age_data = age_data,
#'   lengths = 20:35,
#'   ages = 1:6,
#'   tail_ages = list(
#'     min_lengths = data.frame(length = c(18, 19), age = c(1, 1)),
#'     max_lengths = data.frame(length = c(36, 37), age = c(6, 6))
#'   ),
#'   min_ages_per_length = 5,
#'   optimum_ages_per_length = 200,
#'   length_bin_size = 2, # Use 2cm length bins
#'   interpolate = TRUE, # Allow linear interpolation
#'   extrapolate = FALSE # Disable extrapolation beyond observed range
#' )
#'
#' # Use directly in age composition calculation
#' age_results <- calculate_age_compositions(
#'   x = length_compositions,
#'   age_length_key = complete_alk
#' )
#'
#' # Check interpolation information
#' print(attr(complete_alk, "interpolation_info"))
#' }
#'
#' @importFrom stats aggregate ave
#' @export
create_alk <- function(age_data,
                       lengths,
                       ages,
                       tail_ages = NULL,
                       min_ages_per_length = 3,
                       optimum_ages_per_length = 10,
                       length_bin_size = NULL,
                       interpolate = TRUE,
                       extrapolate = TRUE,
                       verbose = TRUE) {
  # Validate inputs
  if (!is.data.frame(age_data)) {
    stop("age_data must be a data frame")
  }

  # Check required columns
  required_cols <- c("age", "length")
  missing_cols <- setdiff(required_cols, names(age_data))
  if (length(missing_cols) > 0) {
    stop("age_data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check if vectors are provided
  if (missing(lengths) || missing(ages)) {
    stop("Both 'lengths' and 'ages' arguments are required")
  }

  if (!is.numeric(lengths) || !is.numeric(ages)) {
    stop("'lengths' and 'ages' must be numeric vectors")
  }

  # Apply length binning if specified
  if (!is.null(length_bin_size)) {
    if (!is.numeric(length_bin_size) || length(length_bin_size) != 1 || length_bin_size <= 0) {
      stop("'length_bin_size' must be a single positive numeric value")
    }

    if (verbose) cat("Binning lengths using", length_bin_size, "cm bins...\n")

    # Bin the lengths in age_data
    age_data$length <- floor(age_data$length / length_bin_size) * length_bin_size

    # Bin the target lengths vector
    lengths <- floor(lengths / length_bin_size) * length_bin_size
    lengths <- sort(unique(lengths))

    # Bin tail_ages lengths if provided
    if (!is.null(tail_ages)) {
      if ("min_lengths" %in% names(tail_ages)) {
        tail_ages$min_lengths$length <- floor(tail_ages$min_lengths$length / length_bin_size) * length_bin_size
      }
      if ("max_lengths" %in% names(tail_ages)) {
        tail_ages$max_lengths$length <- floor(tail_ages$max_lengths$length / length_bin_size) * length_bin_size
      }
    }
  }

  # Validate interpolation/extrapolation parameters
  if (!is.logical(interpolate) || length(interpolate) != 1) {
    stop("'interpolate' must be a single logical value")
  }
  if (!is.logical(extrapolate) || length(extrapolate) != 1) {
    stop("'extrapolate' must be a single logical value")
  }

  if (verbose && (!interpolate || !extrapolate)) {
    if (!interpolate && !extrapolate) {
      cat("Both interpolation and extrapolation disabled - only original data will be used...\n")
    } else if (!interpolate) {
      cat("Linear interpolation disabled - gaps between observed lengths will not be filled...\n")
    } else if (!extrapolate) {
      cat("Extrapolation disabled - lengths outside observed range will not be filled...\n")
    }
  }

  # Determine if sex-specific analysis is needed
  has_sex <- "sex" %in% names(age_data) && !all(is.na(age_data$sex))

  # Create age-length key from raw data
  if (has_sex) {
    if (verbose) cat("Creating sex-specific age-length keys from raw data...\n")

    # Create proportions by sex, length, and age
    age_length_key <- list()

    for (sex_cat in unique(age_data$sex[!is.na(age_data$sex)])) {
      sex_data <- age_data[!is.na(age_data$sex) & age_data$sex == sex_cat, ]

      # Count fish by length and age
      counts <- aggregate(rep(1, nrow(sex_data)),
        by = list(length = sex_data$length, age = sex_data$age),
        FUN = sum
      )
      names(counts)[3] <- "n"

      # Calculate proportions by length
      counts$proportion <- ave(counts$n, counts$length, FUN = function(x) x / sum(x))

      # Keep the columns needed for ALK including n
      age_length_key[[sex_cat]] <- counts[, c("length", "age", "proportion", "n")]
    }

    # Create combined unsexed key (males + females)
    if (length(age_length_key) >= 2) {
      if (verbose) cat("Creating combined male+female age-length key for unsexed fish...\n")

      # Combine all sex-specific data
      combined_data <- do.call(rbind, age_length_key)

      # Aggregate by length and age, summing both proportions and counts
      combined_counts <- aggregate(cbind(proportion, n) ~ length + age, data = combined_data, sum)

      # Normalise proportions to sum to 1 for each length
      combined_counts$proportion <- ave(combined_counts$proportion, combined_counts$length,
        FUN = function(x) x / sum(x)
      )

      age_length_key$unsexed <- combined_counts
    }

    is_sex_specific <- TRUE
  } else {
    if (verbose) cat("Creating combined age-length key from raw data...\n")

    # Count fish by length and age
    counts <- aggregate(rep(1, nrow(age_data)),
      by = list(length = age_data$length, age = age_data$age),
      FUN = sum
    )
    names(counts)[3] <- "n"

    # Calculate proportions by length
    counts$proportion <- ave(counts$n, counts$length, FUN = function(x) x / sum(x))

    # Keep the columns needed for ALK including n
    age_length_key <- counts[, c("length", "age", "proportion", "n")]
    is_sex_specific <- FALSE
  }

  # Validate tail_ages parameter
  if (!is.null(tail_ages)) {
    if (!is.list(tail_ages)) {
      stop("tail_ages must be a named list or NULL")
    }

    valid_names <- c("min_lengths", "max_lengths")
    invalid_names <- setdiff(names(tail_ages), valid_names)
    if (length(invalid_names) > 0) {
      stop("Invalid names in tail_ages: ", paste(invalid_names, collapse = ", "))
    }

    # Validate and filter tail_ages components
    for (param_name in names(tail_ages)) {
      tail_data <- tail_ages[[param_name]]
      if (!is.data.frame(tail_data)) {
        stop("tail_ages$", param_name, " must be a data.frame with columns 'length' and 'age'")
      }

      required_cols <- c("length", "age")
      if (!all(required_cols %in% names(tail_data))) {
        stop("tail_ages$", param_name, " must contain columns: ", paste(required_cols, collapse = ", "))
      }

      # Filter to only include ages within the specified range
      tail_ages[[param_name]] <- tail_data[tail_data$age %in% ages, ]
    }
  }

  # Enhanced interpolation function with comprehensive tracking
  interpolate_complete_alk <- function(key_data, target_lengths, target_ages, tail_ages = NULL,
                                       interpolate = TRUE, extrapolate = TRUE) {
    key_lengths <- unique(key_data$length)
    missing_lengths <- setdiff(target_lengths, key_lengths)

    if (length(missing_lengths) == 0) {
      # Create simple table showing no interpolation needed
      interpolation_table <- data.frame(
        method = "Original key data",
        count = length(key_lengths),
        lengths = paste(range(key_lengths), collapse = "-"),
        stringsAsFactors = FALSE
      )
      return(list(key_data = key_data, interpolation_table = interpolation_table))
    }

    complete_key <- key_data
    key_lengths_sorted <- sort(key_lengths)

    # Track interpolation methods used
    user_specified_lengths <- c()
    linear_interpolation_lengths <- c()
    extrapolation_lengths <- c()

    for (missing_length in missing_lengths) {
      user_specified <- FALSE

      # Check user-specified tail ages first
      if (!is.null(tail_ages)) {
        # Check min_lengths
        if ("min_lengths" %in% names(tail_ages) && missing_length <= min(key_lengths)) {
          min_data <- tail_ages$min_lengths
          matching_row <- min_data[min_data$length == missing_length, ]
          if (nrow(matching_row) > 0) {
            for (i in seq_len(nrow(matching_row))) {
              if (matching_row$age[i] %in% target_ages) {
                new_row <- data.frame(
                  length = missing_length,
                  age = matching_row$age[i],
                  proportion = 1.0,
                  n = 0, # No actual otoliths for user-specified entries
                  stringsAsFactors = FALSE
                )
                complete_key <- rbind(complete_key, new_row)
                user_specified_lengths <- c(user_specified_lengths, missing_length)
                user_specified <- TRUE
                break
              }
            }
          }
        }

        # Check max_lengths
        if (!user_specified && "max_lengths" %in% names(tail_ages) && missing_length >= max(key_lengths)) {
          max_data <- tail_ages$max_lengths
          matching_row <- max_data[max_data$length == missing_length, ]
          if (nrow(matching_row) > 0) {
            for (i in seq_len(nrow(matching_row))) {
              if (matching_row$age[i] %in% target_ages) {
                new_row <- data.frame(
                  length = missing_length,
                  age = matching_row$age[i],
                  proportion = 1.0,
                  n = 0, # No actual otoliths for user-specified entries
                  stringsAsFactors = FALSE
                )
                complete_key <- rbind(complete_key, new_row)
                user_specified_lengths <- c(user_specified_lengths, missing_length)
                user_specified <- TRUE
                break
              }
            }
          }
        }
      }

      # Use interpolation/extrapolation for missing lengths
      if (!user_specified) {
        if (missing_length < min(key_lengths_sorted)) {
          # Extrapolate using smallest length (if allowed)
          if (extrapolate) {
            nearest_length <- min(key_lengths_sorted)
            nearest_data <- key_data[key_data$length == nearest_length, ]
            extrapolation_lengths <- c(extrapolation_lengths, missing_length)

            for (i in seq_len(nrow(nearest_data))) {
              new_row <- data.frame(
                length = missing_length,
                age = nearest_data$age[i],
                proportion = nearest_data$proportion[i],
                n = 0, # No actual otoliths for extrapolated entries
                stringsAsFactors = FALSE
              )
              complete_key <- rbind(complete_key, new_row)
            }
          }
        } else if (missing_length > max(key_lengths_sorted)) {
          # Extrapolate using largest length (if allowed)
          if (extrapolate) {
            nearest_length <- max(key_lengths_sorted)
            nearest_data <- key_data[key_data$length == nearest_length, ]
            extrapolation_lengths <- c(extrapolation_lengths, missing_length)

            for (i in seq_len(nrow(nearest_data))) {
              new_row <- data.frame(
                length = missing_length,
                age = nearest_data$age[i],
                proportion = nearest_data$proportion[i],
                n = 0, # No actual otoliths for extrapolated entries
                stringsAsFactors = FALSE
              )
              complete_key <- rbind(complete_key, new_row)
            }
          }
        } else {
          # Linear interpolation between nearest lengths (if allowed)
          if (interpolate) {
            lower_length <- max(key_lengths_sorted[key_lengths_sorted < missing_length])
            upper_length <- min(key_lengths_sorted[key_lengths_sorted > missing_length])

            lower_data <- key_data[key_data$length == lower_length, ]
            upper_data <- key_data[key_data$length == upper_length, ]
            linear_interpolation_lengths <- c(linear_interpolation_lengths, missing_length)

            # Calculate interpolation weights
            weight_upper <- (missing_length - lower_length) / (upper_length - lower_length)
            weight_lower <- 1 - weight_upper

            # Get all ages present in either dataset
            all_ages <- unique(c(lower_data$age, upper_data$age))
            all_ages <- all_ages[all_ages %in% target_ages]

            for (age in all_ages) {
              lower_prop <- ifelse(age %in% lower_data$age,
                lower_data$proportion[lower_data$age == age], 0
              )
              upper_prop <- ifelse(age %in% upper_data$age,
                upper_data$proportion[upper_data$age == age], 0
              )

              interpolated_prop <- weight_lower * lower_prop + weight_upper * upper_prop

              if (interpolated_prop > 0) {
                new_row <- data.frame(
                  length = missing_length,
                  age = age,
                  proportion = interpolated_prop,
                  n = 0, # No actual otoliths for interpolated entries
                  stringsAsFactors = FALSE
                )
                complete_key <- rbind(complete_key, new_row)
              }
            }
          }
        }
      }
    }

    # Create simple summary table
    methods_used <- data.frame(
      method = character(0),
      count = integer(0),
      lengths = character(0),
      stringsAsFactors = FALSE
    )

    # Add original data
    if (length(key_lengths) > 0) {
      methods_used <- rbind(methods_used, data.frame(
        method = "Original key data",
        count = length(key_lengths),
        lengths = if (length(key_lengths) == 1) {
          as.character(key_lengths[1])
        } else {
          paste(range(key_lengths), collapse = "-")
        },
        stringsAsFactors = FALSE
      ))
    }

    # Add user-specified tails
    if (length(user_specified_lengths) > 0) {
      methods_used <- rbind(methods_used, data.frame(
        method = "User-specified tails",
        count = length(user_specified_lengths),
        lengths = paste(sort(unique(user_specified_lengths)), collapse = ", "),
        stringsAsFactors = FALSE
      ))
    }

    # Add linear interpolation
    if (length(linear_interpolation_lengths) > 0) {
      methods_used <- rbind(methods_used, data.frame(
        method = "Linear interpolation",
        count = length(linear_interpolation_lengths),
        lengths = paste(sort(unique(linear_interpolation_lengths)), collapse = ", "),
        stringsAsFactors = FALSE
      ))
    }

    # Add extrapolation
    if (length(extrapolation_lengths) > 0) {
      methods_used <- rbind(methods_used, data.frame(
        method = "Tail extrapolation",
        count = length(extrapolation_lengths),
        lengths = paste(sort(unique(extrapolation_lengths)), collapse = ", "),
        stringsAsFactors = FALSE
      ))
    }

    return(list(key_data = complete_key, interpolation_table = methods_used))
  }

  # Process keys
  if (is_sex_specific) {
    if (verbose) cat("Creating complete sex-specific age-length keys...\n")

    complete_keys <- list()
    # Combined interpolation table for all sexes
    combined_interpolation_table <- data.frame(
      sex = character(0),
      method = character(0),
      count = integer(0),
      lengths = character(0),
      stringsAsFactors = FALSE
    )

    for (sex in names(age_length_key)) {
      if (verbose) cat("  Processing", sex, "key...\n")

      # Normalise proportions for this sex
      key <- age_length_key[[sex]]
      prop_sums <- aggregate(proportion ~ length, data = key, sum)
      if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
        if (verbose) cat("    Normalising age-length key proportions for", sex, "...\n")
        key$proportion <- ave(key$proportion, key$length, FUN = function(x) x / sum(x))
      }

      # Apply interpolation
      result <- interpolate_complete_alk(key, lengths, ages, tail_ages, interpolate, extrapolate)
      complete_keys[[sex]] <- result$key_data

      # Add sex column to interpolation table and combine
      sex_table <- result$interpolation_table
      sex_table$sex <- sex
      sex_table <- sex_table[, c("sex", "method", "count", "lengths")] # Reorder columns
      combined_interpolation_table <- rbind(combined_interpolation_table, sex_table)
    }

    interpolation_table <- combined_interpolation_table
  } else {
    if (verbose) cat("Creating complete age-length key...\n")

    # Normalise proportions
    prop_sums <- aggregate(proportion ~ length, data = age_length_key, sum)
    if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
      if (verbose) cat("  Normalising age-length key proportions...\n")
      age_length_key$proportion <- ave(age_length_key$proportion, age_length_key$length,
        FUN = function(x) x / sum(x)
      )
    }

    # Apply interpolation
    result <- interpolate_complete_alk(age_length_key, lengths, ages, tail_ages, interpolate, extrapolate)
    complete_keys <- result$key_data
    interpolation_table <- result$interpolation_table
  }

  # Print summary if verbose
  if (verbose) {
    cat("\nAge-Length Key Creation Summary:\n")
    cat("===============================\n")
    cat("Total length bins:", length(lengths), "\n")
    cat("Minimum otoliths per length bin:", min_ages_per_length, "\n")
    cat("Optimum otoliths per length bin:", optimum_ages_per_length, "\n")

    # Create summary table
    if (is_sex_specific) {
      # Calculate metrics for each sex
      summary_table <- data.frame(
        sex = character(0),
        missing_lengths = integer(0),
        interpolation_rate = numeric(0),
        extrapolation_rate = numeric(0),
        current_otoliths = integer(0),
        min_additional_required = integer(0),
        optimum_additional_required = integer(0),
        stringsAsFactors = FALSE
      )

      for (sex in names(age_length_key)) {
        sex_table <- interpolation_table[interpolation_table$sex == sex, ]

        # Calculate missing lengths - always calculate this regardless of interpolation/extrapolation settings
        original_lengths_for_sex <- unique(age_length_key[[sex]]$length)
        total_missing_for_sex <- length(setdiff(lengths, original_lengths_for_sex))

        # Calculate interpolation and extrapolation rates
        interp_rows <- sex_table[sex_table$method == "Linear interpolation", ]
        extrap_rows <- sex_table[sex_table$method == "Tail extrapolation", ]

        interp_count <- if (nrow(interp_rows) > 0) sum(interp_rows$count) else 0
        extrap_count <- if (nrow(extrap_rows) > 0) sum(extrap_rows$count) else 0

        interp_rate <- interp_count / length(lengths) * 100
        extrap_rate <- extrap_count / length(lengths) * 100

        # Calculate otolith requirements based on current age data coverage
        # Count current ages per length for this sex
        if (sex == "unsexed") {
          # For unsexed, use all sexed fish combined (same fish contribute to unsexed key)
          sex_age_data <- age_data[!is.na(age_data$sex), ]
        } else {
          sex_age_data <- age_data[!is.na(age_data$sex) & age_data$sex == sex, ]
        }

        current_counts <- if (nrow(sex_age_data) > 0) {
          table(sex_age_data$length)
        } else {
          integer(0)
        }

        # Calculate current total and additional otoliths needed
        current_total <- sum(current_counts)
        min_needed <- 0
        optimum_needed <- 0

        for (length_bin in lengths) {
          current_count <- if (as.character(length_bin) %in% names(current_counts)) {
            current_counts[as.character(length_bin)]
          } else {
            0
          }

          min_deficit <- max(0, min_ages_per_length - current_count)
          optimum_deficit <- max(0, optimum_ages_per_length - current_count)

          min_needed <- min_needed + min_deficit
          optimum_needed <- optimum_needed + optimum_deficit
        }

        summary_table <- rbind(summary_table, data.frame(
          sex = sex,
          missing_lengths = total_missing_for_sex,
          interpolation_rate = round(interp_rate, 1),
          extrapolation_rate = round(extrap_rate, 1),
          current_otoliths = current_total,
          min_additional_required = min_needed,
          optimum_additional_required = optimum_needed,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      # Single key analysis
      # Calculate missing lengths - always calculate this regardless of interpolation/extrapolation settings
      original_lengths <- unique(age_length_key$length)
      total_missing <- length(setdiff(lengths, original_lengths))

      # Calculate interpolation and extrapolation
      interp_rows <- interpolation_table[interpolation_table$method == "Linear interpolation", ]
      extrap_rows <- interpolation_table[interpolation_table$method == "Tail extrapolation", ]

      interp_count <- if (nrow(interp_rows) > 0) sum(interp_rows$count) else 0
      extrap_count <- if (nrow(extrap_rows) > 0) sum(extrap_rows$count) else 0

      interp_rate <- interp_count / length(lengths) * 100
      extrap_rate <- extrap_count / length(lengths) * 100

      # Calculate otolith requirements
      current_counts <- table(age_data$length)
      current_total <- sum(current_counts)
      min_needed <- 0
      optimum_needed <- 0

      for (length_bin in lengths) {
        current_count <- if (as.character(length_bin) %in% names(current_counts)) {
          current_counts[as.character(length_bin)]
        } else {
          0
        }

        min_deficit <- max(0, min_ages_per_length - current_count)
        optimum_deficit <- max(0, optimum_ages_per_length - current_count)

        min_needed <- min_needed + min_deficit
        optimum_needed <- optimum_needed + optimum_deficit
      }

      summary_table <- data.frame(
        sex = "Combined",
        missing_lengths = total_missing,
        interpolation_rate = round(interp_rate, 1),
        extrapolation_rate = round(extrap_rate, 1),
        current_otoliths = current_total,
        min_additional_required = min_needed,
        optimum_additional_required = optimum_needed,
        stringsAsFactors = FALSE
      )
    }

    # Print the table
    cat("\nSummary Table:\n")
    print(summary_table, row.names = FALSE)
    cat("\n")
  }

  # Attach interpolation info as attribute to the standard age-length key object
  attr(complete_keys, "interpolation_info") <- interpolation_table
  attr(complete_keys, "is_sex_specific") <- is_sex_specific

  # Set the S3 class
  class(complete_keys) <- c("age_length_key", "list")

  return(complete_keys)
}
