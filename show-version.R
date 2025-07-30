# Show current package version information
# This script displays the current version and date from DESCRIPTION

show_version_info <- function(desc_file = "scala/DESCRIPTION") {
  if (!file.exists(desc_file)) {
    stop("DESCRIPTION file not found: ", desc_file)
  }

  desc_lines <- readLines(desc_file)

  # Find version and date
  version_line <- grep("^Version:", desc_lines, value = TRUE)
  date_line <- grep("^Date:", desc_lines, value = TRUE)

  cat("=== scala Package Version Information ===\n")

  if (length(version_line) > 0) {
    cat(version_line, "\n")
  } else {
    cat("Version: Not found\n")
  }

  if (length(date_line) > 0) {
    cat(date_line, "\n")
  } else {
    cat("Date: Not found\n")
  }

  # Show git commit info if available
  tryCatch(
    {
      git_commit <- system('git log -1 --format="%h - %ci"', intern = TRUE, ignore.stderr = TRUE)
      if (length(git_commit) > 0) {
        cat("Git commit:", git_commit, "\n")
      }
    },
    error = function(e) {
      cat("Git info: Not available\n")
    }
  )

  cat("=========================================\n")
}

# Run if called directly
if (!interactive()) {
  show_version_info()
}
