# =====================================
# Update package version and date from git
# =====================================

# Function to get git commit date and format as YYYY-MM
get_git_version_date <- function() {
  # Initialize with current date as fallback
  commit_date <- Sys.Date()

  # Try to get the date of the last commit
  tryCatch(
    {
      git_date_cmd <- 'git log -1 --format="%ci"'
      git_date_raw <- system(git_date_cmd, intern = TRUE, ignore.stderr = TRUE)
      status <- attr(git_date_raw, "status")

      if (is.null(status) && length(git_date_raw) > 0 && git_date_raw != "") {
        # Parse the git date (format: YYYY-MM-DD HH:MM:SS +ZZOO)
        commit_date <- as.Date(substr(git_date_raw, 1, 10))
      } else {
        warning("Could not get git commit date. Using current date.")
      }
    },
    error = function(e) {
      warning("Error getting git commit date: ", e$message, ". Using current date.")
    }
  )

  # Format as YYYY-MM for version
  version_date <- format(commit_date, "%Y-%m")

  # Format as YYYY-MM-DD for package date
  package_date <- format(commit_date, "%Y-%m-%d")

  return(list(version = version_date, date = package_date))
} # Function to update DESCRIPTION file
update_description <- function(desc_file = "scala/DESCRIPTION") {
  if (!file.exists(desc_file)) {
    stop("DESCRIPTION file not found: ", desc_file)
  }

  # Get git version and date
  git_info <- get_git_version_date()

  cat("Updating package version to:", git_info$version, "\n")
  cat("Updating package date to:", git_info$date, "\n")

  # Read DESCRIPTION file
  desc_lines <- readLines(desc_file)

  # Update Version line
  version_line <- grep("^Version:", desc_lines)
  if (length(version_line) > 0) {
    desc_lines[version_line] <- paste0("Version: ", git_info$version)
  } else {
    # Add Version line after Title if not found
    title_line <- grep("^Title:", desc_lines)
    if (length(title_line) > 0) {
      desc_lines <- c(
        desc_lines[1:title_line],
        paste0("Version: ", git_info$version),
        desc_lines[(title_line + 1):length(desc_lines)]
      )
    }
  }

  # Update or add Date line
  date_line <- grep("^Date:", desc_lines)
  if (length(date_line) > 0) {
    desc_lines[date_line] <- paste0("Date: ", git_info$date)
  } else {
    # Add Date line after Version
    version_line <- grep("^Version:", desc_lines)
    if (length(version_line) > 0) {
      desc_lines <- c(
        desc_lines[1:version_line],
        paste0("Date: ", git_info$date),
        desc_lines[(version_line + 1):length(desc_lines)]
      )
    }
  }

  # Write updated DESCRIPTION file
  writeLines(desc_lines, desc_file)

  cat("DESCRIPTION file updated successfully.\n")
  return(git_info)
}

# Function to check if we're in a git repository
check_git_repo <- function() {
  tryCatch(
    {
      git_check <- system("git rev-parse --git-dir", intern = TRUE, ignore.stderr = TRUE)
      status <- attr(git_check, "status")

      if (is.null(status) && length(git_check) > 0) {
        return(TRUE) # Success, we're in a git repo
      } else {
        return(FALSE) # Failed or not in git repo
      }
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# Main execution
main <- function() {
  cat("=== scala Package Version Updater ===\n")

  # Check if we're in a git repo
  if (!check_git_repo()) {
    cat("Using current date for version...\n")
  }

  # Update DESCRIPTION
  tryCatch(
    {
      git_info <- update_description()
      cat("\nPackage version updated to:", git_info$version, "\n")
      cat("Package date set to:", git_info$date, "\n")
    },
    error = function(e) {
      cat("Error updating DESCRIPTION:", e$message, "\n")
      quit(status = 1)
    }
  )
}

# Run if called directly
if (!interactive()) {
  main()
}
