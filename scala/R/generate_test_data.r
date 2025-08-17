#' Generate test data for length composition analysis
#'
#' Creates synthetic fish data for testing and demonstrating length composition functions.
#' Supports both commercial fisheries (weight-based) and survey (density-based) data types.
#'
#' @param data_type Character, either "commercial" for weight-based data or "survey" for density-based data
#' @return List containing fish_data and strata_data for testing
#'
#' @details
#' The function generates realistic test datasets with:
#' \itemize{
#'   \item Multiple strata (North/South)
#'   \item Sample-based hierarchical structure
#'   \item Sex-specific length distributions
#'   \item Appropriate scaling information (weights or densities)
#' }
#'
#' @seealso
#' \code{\link{calculate_length_compositions}} for analysing the generated data,
#' \code{\link{generate_commercial_test_data}} and \code{\link{generate_survey_test_data}} for specific data types
#'
#' @examples
#' # Generate commercial data
#' commercial_data <- generate_test_data("commercial")
#'
#' # Generate survey data
#' survey_data <- generate_test_data("survey")
#'
#' @importFrom stats rpois runif
#' @export
generate_test_data <- function(data_type = "commercial") {
  if (data_type == "commercial") {
    # Generate commercial fisheries data (weight-based scaling)
    return(generate_commercial_test_data())
  } else if (data_type == "survey") {
    # Generate survey data (density-based scaling)
    return(generate_survey_test_data())
  } else {
    stop("data_type must be either 'commercial' or 'survey'")
  }
}

#' Generate commercial fisheries test data
#' @importFrom stats rpois runif
#' @export
generate_commercial_test_data <- function() {
  set.seed(123)

  # Generate test fish data with sex information
  n_samples <- 15
  fish_data <- data.frame()

  for (i in 1:n_samples) {
    stratum <- sample(c("North", "South"), 1)
    sample_id <- paste0("Sample_", i)

    # Generate length distribution (normal around 25cm)
    n_length_classes <- sample(5:8, 1)
    lengths <- sample(20:35, n_length_classes)

    sample_data <- data.frame(
      stratum = stratum,
      sample_id = sample_id,
      length = lengths,
      male = rpois(n_length_classes, lambda = 8),
      female = rpois(n_length_classes, lambda = 12),
      unsexed = rpois(n_length_classes, lambda = 3),
      sample_weight_kg = runif(1, 8, 18),
      total_catch_weight_kg = runif(1, 80, 250)
    )

    # Calculate total
    sample_data$total <- sample_data$male + sample_data$female + sample_data$unsexed

    fish_data <- rbind(fish_data, sample_data)
  }

  # Generate strata data
  strata_data <- data.frame(
    stratum = c("North", "South"),
    stratum_total_catch_kg = c(3000, 2500)
  )

  return(list(fish_data = fish_data, strata_data = strata_data))
}

#' Generate survey test data
#' @importFrom stats rpois runif
#' @export
generate_survey_test_data <- function() {
  set.seed(456)

  # Generate test fish data for survey (density-based)
  n_samples <- 12
  fish_data <- data.frame()

  for (i in 1:n_samples) {
    stratum <- sample(c("Shallow", "Deep"), 1)
    sample_id <- paste0("Survey_", i)

    # Sample area and catch density
    sample_area <- runif(1, 0.2, 0.8) # km2 swept area
    catch_density <- if (stratum == "Shallow") runif(1, 150, 400) else runif(1, 80, 250) # kg/km2

    n_length_classes <- sample(4:7, 1)
    lengths <- sample(22:38, n_length_classes)

    sample_data <- data.frame(
      stratum = stratum,
      sample_id = sample_id,
      length = lengths,
      male = rpois(n_length_classes, lambda = 6),
      female = rpois(n_length_classes, lambda = 9),
      unsexed = rpois(n_length_classes, lambda = 2),
      sample_area_km2 = sample_area,
      catch_density_kg_km2 = catch_density
    )

    # Calculate total
    sample_data$total <- sample_data$male + sample_data$female + sample_data$unsexed

    fish_data <- rbind(fish_data, sample_data)
  }

  # Generate strata data (total areas)
  strata_data <- data.frame(
    stratum = c("Shallow", "Deep"),
    stratum_area_km2 = c(1200, 1800)
  )

  return(list(fish_data = fish_data, strata_data = strata_data))
}

#' Generate age data for testing ALK functions
#' 
#' Creates synthetic age-at-length data for testing age-length key functions
#' 
#' @param n_samples Number of fish samples to generate
#' @return Data frame with age, length, and sex columns
#' @export
generate_test_age_data <- function(n_samples = 100) {
  set.seed(789)
  
  # Generate realistic age-length relationships
  ages <- sample(1:6, n_samples, replace = TRUE, prob = c(0.1, 0.2, 0.25, 0.25, 0.15, 0.05))
  
  # Length increases with age but with variation
  lengths <- numeric(n_samples)
  for (i in 1:n_samples) {
    mean_length <- 16 + ages[i] * 3.2  # Length increases with age
    lengths[i] <- round(rnorm(1, mean_length, 1.8), 0)
  }
  
  # Ensure reasonable length bounds that fit test expectations
  lengths <- pmax(lengths, 15)
  lengths <- pmin(lengths, 35)
  
  # Assign sex
  sex <- sample(c("male", "female", "unsexed"), n_samples, replace = TRUE, 
                prob = c(0.45, 0.45, 0.1))
  
  data.frame(
    age = ages,
    length = lengths,
    sex = sex,
    stringsAsFactors = FALSE
  )
}
