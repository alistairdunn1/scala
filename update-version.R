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

# Function to update CITATION file
update_citation <- function(citation_file = "scala/inst/CITATION", git_info) {
  if (!file.exists(citation_file)) {
    warning("CITATION file not found: ", citation_file)
    return(invisible(NULL))
  }

  # Extract year from version date
  year <- substr(git_info$date, 1, 4)

  cat("Updating CITATION year to:", year, "\n")
  cat("Updating CITATION version to:", git_info$version, "\n")

  # Read CITATION file
  cit_lines <- readLines(citation_file)

  # Update year line
  year_line <- grep("^\\s*year\\s*=", cit_lines)
  if (length(year_line) > 0) {
    cit_lines[year_line] <- paste0('  year = "', year, '",')
  }

  # Update note line (R package version)
  note_line <- grep("^\\s*note\\s*=", cit_lines)
  if (length(note_line) > 0) {
    cit_lines[note_line] <- paste0('  note = "R package version ', git_info$version, '",')
  }

  # Write updated CITATION file
  writeLines(cit_lines, citation_file)

  cat("CITATION file updated successfully.\n")
}

# Function to update README.md file
update_readme <- function(readme_file = "Readme.md", git_info) {
  if (!file.exists(readme_file)) {
    warning("README file not found: ", readme_file)
    return(invisible(NULL))
  }

  # Extract year from version date
  year <- substr(git_info$date, 1, 4)
  # Version with double dash for shields.io badge (YYYY--MM)
  badge_version <- gsub("-", "--", git_info$version)

  cat("Updating README version to:", git_info$version, "\n")

  # Read README file
  readme_lines <- readLines(readme_file)

  # Update version badge
  badge_pattern <- "version-[0-9]{4}--[0-9]{2}-orange"
  badge_replacement <- paste0("version-", badge_version, "-orange")
  readme_lines <- gsub(badge_pattern, badge_replacement, readme_lines)

  # Update Package Information version
  pkg_info_pattern <- "\\*\\*Version\\*\\*: [0-9]{4}-[0-9]{2}"
  pkg_info_replacement <- paste0("**Version**: ", git_info$version)
  readme_lines <- gsub(pkg_info_pattern, pkg_info_replacement, readme_lines)

  # Update citation line: Dunn, A. (YYYY). scala: ... R package version YYYY-MM.
  cit_pattern <- "Dunn, A\\. \\([0-9]{4}\\)\\. scala:.* R package version [0-9]{4}-[0-9]{2}\\."
  cit_replacement <- paste0("Dunn, A. (", year, "). scala: Scaled catch at length and age composition analyses. R package version ", git_info$version, ".")
  readme_lines <- gsub(cit_pattern, cit_replacement, readme_lines)

  # Write updated README file
  writeLines(readme_lines, readme_file)

  cat("README file updated successfully.\n")
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

  # Update CITATION
  tryCatch(
    {
      update_citation(git_info = git_info)
    },
    error = function(e) {
      cat("Error updating CITATION:", e$message, "\n")
    }
  )

  # Update README
  tryCatch(
    {
      update_readme(git_info = git_info)
    },
    error = function(e) {
      cat("Error updating README:", e$message, "\n")
    }
  )
}

# Run if called directly
if (!interactive()) {
  main()
}
