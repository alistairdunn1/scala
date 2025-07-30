#' Create Complete Age-Length Key
#'
#' Creates a complete age-length key that includes all required length and age bins,
#' with interpolation/extrapolation as needed for missing combinations.
#'
#' @param age_length_key Either a single data frame or named list of sex-specific keys
#' @param lengths Vector of all length bins that will be encountered
#' @param ages Vector of all age bins
#' @param tail_ages Named list specifying length-age pairs for lengths at tails
#'   Format: list(min_lengths = data.frame(length, age), max_lengths = data.frame(length, age))
#' @param verbose Logical, whether to print progress messages
#'
#' @return Age-length key object (data.frame for single key, named list for sex-specific keys)
#'   with interpolation information stored as attributes:
#'   \itemize{
#'     \item Standard age-length key format compatible with calculate_age_compositions()
#'     \item \code{attr(*, "interpolation_info")}: Data frame summarizing interpolation methods used
#'     \item \code{attr(*, "is_sex_specific")}: Logical indicating if sex-specific keys were used
#'   }
#'
#' @examples
#' \dontrun{
#' # Create a complete age-length key
#' complete_alk <- create_alk(
#'   age_length_key = my_age_key,
#'   lengths = 20:40,
#'   ages = 1:8,
#'   tail_ages = list(
#'     min_lengths = data.frame(length = c(18, 19), age = c(1, 1)),
#'     max_lengths = data.frame(length = c(41, 42), age = c(8, 8))
#'   )
#' )
#'
#' # Use directly in age composition calculation
#' age_results <- calculate_age_compositions(
#'   x = length_compositions,
#'   age_length_key = complete_alk # Now works directly
#' )
#'
#' # Check interpolation information
#' print(attr(complete_alk, "interpolation_info"))
#' }
#'
#' @importFrom stats aggregate ave
#' @export
create_alk <- function(age_length_key,
                       lengths,
                       ages,
                       tail_ages = NULL,
                       verbose = TRUE) {
  # Determine if sex-specific keys are provided
  is_sex_specific <- is.list(age_length_key) && !is.data.frame(age_length_key)

  # Validate inputs
  if (is_sex_specific) {
    required_sexes <- c("male", "female")
    if (!all(required_sexes %in% names(age_length_key))) {
      stop("Sex-specific age_length_key must contain at least 'male' and 'female' elements")
    }
    # Add unsexed key if missing (use male key)
    if (!"unsexed" %in% names(age_length_key)) {
      if (verbose) cat("Using male age-length key for unsexed fish...\n")
      age_length_key$unsexed <- age_length_key$male
    }
  } else {
    # Validate single key format
    required_cols <- c("length", "age", "proportion")
    if (!all(required_cols %in% names(age_length_key))) {
      stop("age_length_key must contain columns: ", paste(required_cols, collapse = ", "))
    }
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
  interpolate_complete_alk <- function(key_data, target_lengths, target_ages, tail_ages = NULL) {
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
          # Extrapolate using smallest length
          nearest_length <- min(key_lengths_sorted)
          nearest_data <- key_data[key_data$length == nearest_length, ]
          extrapolation_lengths <- c(extrapolation_lengths, missing_length)

          for (i in seq_len(nrow(nearest_data))) {
            new_row <- data.frame(
              length = missing_length,
              age = nearest_data$age[i],
              proportion = nearest_data$proportion[i],
              stringsAsFactors = FALSE
            )
            complete_key <- rbind(complete_key, new_row)
          }
        } else if (missing_length > max(key_lengths_sorted)) {
          # Extrapolate using largest length
          nearest_length <- max(key_lengths_sorted)
          nearest_data <- key_data[key_data$length == nearest_length, ]
          extrapolation_lengths <- c(extrapolation_lengths, missing_length)

          for (i in seq_len(nrow(nearest_data))) {
            new_row <- data.frame(
              length = missing_length,
              age = nearest_data$age[i],
              proportion = nearest_data$proportion[i],
              stringsAsFactors = FALSE
            )
            complete_key <- rbind(complete_key, new_row)
          }
        } else {
          # Linear interpolation between nearest lengths
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
                stringsAsFactors = FALSE
              )
              complete_key <- rbind(complete_key, new_row)
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

      # Normalize proportions for this sex
      key <- age_length_key[[sex]]
      prop_sums <- aggregate(proportion ~ length, data = key, sum)
      if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
        if (verbose) cat("    Normalizing age-length key proportions for", sex, "...\n")
        key$proportion <- ave(key$proportion, key$length, FUN = function(x) x / sum(x))
      }

      # Apply interpolation
      result <- interpolate_complete_alk(key, lengths, ages, tail_ages)
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

    # Normalize proportions
    prop_sums <- aggregate(proportion ~ length, data = age_length_key, sum)
    if (any(abs(prop_sums$proportion - 1) > 1e-6)) {
      if (verbose) cat("  Normalizing age-length key proportions...\n")
      age_length_key$proportion <- ave(age_length_key$proportion, age_length_key$length,
        FUN = function(x) x / sum(x)
      )
    }

    # Apply interpolation
    result <- interpolate_complete_alk(age_length_key, lengths, ages, tail_ages)
    complete_keys <- result$key_data
    interpolation_table <- result$interpolation_table
  }

  # Print summary if verbose
  if (verbose) {
    cat("\nAge-Length Key Creation Summary:\n")
    cat("===============================\n")
    cat("Total length bins:", length(lengths), "\n")

    if (is_sex_specific) {
      cat("Key type: Sex-specific (", paste(names(age_length_key), collapse = ", "), ")\n")

      # Calculate missing lengths for each sex from the table
      for (sex in names(age_length_key)) {
        sex_table <- interpolation_table[interpolation_table$sex == sex, ]
        sex_table <- sex_table[sex_table$method != "Original key data", ]
        missing_count <- sum(sex_table$count)
        interpolation_rate <- missing_count / length(lengths) * 100

        cat("  ", toupper(sex), ":\n")
        cat("    Missing lengths:", missing_count, "\n")
        cat("    Interpolation rate:", round(interpolation_rate, 1), "%\n")
      }
    } else {
      # Calculate statistics from the table
      non_original <- interpolation_table[interpolation_table$method != "Original key data", ]
      missing_count <- sum(non_original$count)
      interpolation_rate <- missing_count / length(lengths) * 100

      cat("Key type: Single key\n")
      cat("Missing lengths:", missing_count, "\n")
      cat("Interpolation rate:", round(interpolation_rate, 1), "%\n")
    }

    # Print method-specific information from the table
    if (is_sex_specific) {
      # Check if any user-specified tails were used across all sexes
      user_specified_rows <- interpolation_table[interpolation_table$method == "User-specified tails", ]
      if (nrow(user_specified_rows) > 0) {
        total_user_specified <- sum(user_specified_rows$count)
        cat("User-specified tail ages used for", total_user_specified, "lengths\n")
      }

      # Check for linear interpolation
      interp_rows <- interpolation_table[interpolation_table$method == "Linear interpolation", ]
      if (nrow(interp_rows) > 0) {
        total_interp <- sum(interp_rows$count)
        cat("Linear interpolation used for", total_interp, "lengths\n")
      }

      # Check for extrapolation
      extrap_rows <- interpolation_table[interpolation_table$method == "Tail extrapolation", ]
      if (nrow(extrap_rows) > 0) {
        total_extrap <- sum(extrap_rows$count)
        cat("Tail extrapolation used for", total_extrap, "lengths\n")
      }
    } else {
      # For single key, show each method used
      for (i in seq_len(nrow(interpolation_table))) {
        method_info <- interpolation_table[i, ]
        if (method_info$method != "Original key data") {
          cat(method_info$method, "used for", method_info$count, "lengths\n")
        }
      }
    }
    cat("\n")
  }

  # Attach interpolation info as attribute to the standard age-length key object
  attr(complete_keys, "interpolation_info") <- interpolation_table
  attr(complete_keys, "is_sex_specific") <- is_sex_specific

  return(complete_keys)
}
