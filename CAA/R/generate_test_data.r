# Example usage and test data generation for length compositions

##' Get default length-weight parameters for testing
##'
##' @return List of length-weight parameters for males, females, and unsexed fish
##' @keywords internal
get_default_lw_params <- function() {
  # Default parameters based on typical marine fish (e.g., snapper)
  # Weight (g) = a * Length(cm)^b
  list(
    male = c(a = 0.0085, b = 3.10),
    female = c(a = 0.0092, b = 3.05),
    unsexed = c(a = 0.0088, b = 3.08)
  )
}

##' Generate test data for CAA package examples
##'
##' @param data_type Character, either "commercial" for weight-based data or "survey" for density-based data
##' @return List containing fish_data and strata_data for testing
##' @importFrom stats rpois runif
##' @keywords internal
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

##' Generate commercial fisheries test data
##' @importFrom stats rpois runif
##' @keywords internal
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

##' Generate survey test data
##' @importFrom stats rpois runif
##' @keywords internal
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
