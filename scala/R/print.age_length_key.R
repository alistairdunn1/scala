#' Print Method for Age-Length Key
#'
#' @param x An age-length key data frame or list of sex-specific age-length keys
#' @param ... Additional arguments (not used)
#' @export
print.age_length_key <- function(x, ...) {
  cat("*** AGE-LENGTH KEY SUMMARY ***\n")

  # Handle both single keys and sex-specific keys
  if (is.data.frame(x)) {
    # Single age-length key
    cat("Type: Single age-length key\n")
    cat("Rows:", nrow(x), "\n")
    cat("Columns:", ncol(x), "\n")

    # Analyze length coverage
    if ("length" %in% names(x)) {
      lengths_in_key <- unique(x$length)
      length_range <- range(lengths_in_key)
      cat("Length range in key:", length_range[1], "to", length_range[2], "\n")

      # Check for gaps in length coverage
      expected_lengths <- seq(length_range[1], length_range[2])
      missing_lengths <- setdiff(expected_lengths, lengths_in_key)

      if (length(missing_lengths) > 0) {
        cat(
          "WARNING: Missing lengths that would require interpolation:",
          paste(missing_lengths, collapse = ", "), "\n"
        )
      } else {
        cat("Length coverage: Complete (no interpolation needed within range)\n")
      }

      # Check age range
      if ("age" %in% names(x)) {
        age_range <- range(x$age)
        cat("Age range in key:", age_range[1], "to", age_range[2], "\n")
      }
    }
  } else if (is.list(x)) {
    # Sex-specific age-length keys
    cat("Type: Sex-specific age-length keys\n")
    cat("Sex categories:", paste(names(x), collapse = ", "), "\n")

    # Analyze each sex-specific key
    all_missing_lengths <- list()
    all_length_ranges <- list()

    for (sex_name in names(x)) {
      sex_key <- x[[sex_name]]
      if (is.data.frame(sex_key) && "length" %in% names(sex_key)) {
        cat("\n", toupper(sex_name), "key:\n", sep = "")
        cat("  Rows:", nrow(sex_key), "\n")

        lengths_in_key <- unique(sex_key$length)
        length_range <- range(lengths_in_key)
        all_length_ranges[[sex_name]] <- length_range
        cat("  Length range:", length_range[1], "to", length_range[2], "\n")

        # Check for gaps
        expected_lengths <- seq(length_range[1], length_range[2])
        missing_lengths <- setdiff(expected_lengths, lengths_in_key)
        all_missing_lengths[[sex_name]] <- missing_lengths

        if (length(missing_lengths) > 0) {
          cat("  WARNING: Missing lengths:", paste(missing_lengths, collapse = ", "), "\n")
        } else {
          cat("  Length coverage: Complete\n")
        }

        # Check age range
        if ("age" %in% names(sex_key)) {
          age_range <- range(sex_key$age)
          cat("  Age range:", age_range[1], "to", age_range[2], "\n")
        }
      }
    }

    # Overall summary
    cat("\nOVERALL SUMMARY:\n")
    if (length(all_length_ranges) > 0) {
      overall_min <- min(sapply(all_length_ranges, function(r) r[1]))
      overall_max <- max(sapply(all_length_ranges, function(r) r[2]))
      cat("Combined length range:", overall_min, "to", overall_max, "\n")
    }

    # Report any sex with missing lengths
    sexes_with_gaps <- names(all_missing_lengths)[sapply(all_missing_lengths, length) > 0]
    if (length(sexes_with_gaps) > 0) {
      cat("Sex categories requiring interpolation:", paste(sexes_with_gaps, collapse = ", "), "\n")
    } else {
      cat("All sex categories have complete length coverage\n")
    }
  } else {
    cat("Type: Unknown format\n")
    cat("Class:", class(x), "\n")
  }

  invisible(x)
}
