#' Generate Sample Age-Length Key
#'
#' Creates a sample age-length key for testing and demonstration purposes.
#' The key uses biological relationships where younger fish are typically
#' found at smaller lengths.
#'
#' @param length_range Numeric vector of min and max lengths (e.g., c(20, 40))
#' @param age_range Numeric vector of min and max ages (e.g., c(1, 8))
#' @param growth_type Character, type of growth relationship: "linear" (default), "vonbert", or "logistic"
#' @param growth_params Named list of growth parameters (see details)
#' @param cv Numeric, coefficient of variation for age-at-length (default 0.3)
#' @param plus_group_age Logical, whether to combine older ages into plus group (default FALSE)
#'
#' @details
#' **Growth type parameters:**
#'
#' For "linear":
#' \itemize{
#'   \item \code{intercept}: Age intercept (default -2)
#'   \item \code{slope}: Age increase per length unit (default 0.25)
#' }
#'
#' For "vonbert" (von Bertalanffy):
#' \itemize{
#'   \item \code{linf}: Asymptotic length (default 60)
#'   \item \code{k}: Growth coefficient (default 0.2)
#'   \item \code{t0}: Age at length 0 (default -1)
#' }
#'
#' For "logistic":
#' \itemize{
#'   \item \code{l50}: Length at 50% probability of maximum age (default 30)
#'   \item \code{l95}: Length at 95% probability of maximum age (default 40)
#' }
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{length}: Length bin
#'     \item \code{age}: Age class
#'     \item \code{proportion}: Proportion of fish of that length in that age class
#'   }
#'
#' @examples
#' # Simple linear age-length relationship
#' alk_linear <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 8),
#'   growth_type = "linear"
#' )
#'
#' # Von Bertalanffy growth
#' alk_vonbert <- generate_age_length_key(
#'   length_range = c(20, 40),
#'   age_range = c(1, 8),
#'   growth_type = "vonbert",
#'   growth_params = list(linf = 50, k = 0.3, t0 = -0.5)
#' )
#'
#' # Check proportions sum to 1 for each length
#' prop_check <- aggregate(proportion ~ length, data = alk_linear, sum)
#' all(abs(prop_check$proportion - 1) < 1e-10)
#'
#' @export
generate_age_length_key <- function(length_range = c(20, 40),
                                    age_range = c(1, 8),
                                    growth_type = "linear",
                                    growth_params = NULL,
                                    cv = 0.3,
                                    plus_group_age = FALSE) {
  message(
    "Generating a simulated age-length key with length range: ",
    paste(length_range, collapse = " to "),
    ", age range: ", paste(age_range, collapse = " to "),
    ", growth type: ", growth_type, ".  Note that this is a simulated key and the method of simulating will not reflect real biological relationships."
  )
  # Validate inputs
  if (length(length_range) != 2 || length_range[1] >= length_range[2]) {
    stop("length_range must be a vector of [min, max] with min < max")
  }

  if (length(age_range) != 2 || age_range[1] >= age_range[2]) {
    stop("age_range must be a vector of [min, max] with min < max")
  }

  if (!growth_type %in% c("linear", "vonbert", "logistic")) {
    stop("growth_type must be 'linear', 'vonbert', or 'logistic'")
  }

  if (cv <= 0 || cv > 1) {
    stop("cv must be between 0 and 1")
  }

  # Set default growth parameters
  if (is.null(growth_params)) {
    growth_params <- switch(growth_type,
      "linear" = list(intercept = -2, slope = 0.25),
      "vonbert" = list(linf = 60, k = 0.2, t0 = -1),
      "logistic" = list(l50 = mean(length_range), l95 = length_range[2] * 0.9)
    )
  }

  # Create length and age vectors
  lengths <- length_range[1]:length_range[2]
  ages <- age_range[1]:age_range[2]

  # Calculate mean age for each length using specified growth model
  mean_ages <- switch(growth_type,
    "linear" = growth_params$intercept + growth_params$slope * lengths,
    "vonbert" = {
      # Inverse von Bertalanffy: age = -log(1 - length/linf) / k + t0
      with(growth_params, -log(pmax(1 - lengths / linf, 0.01)) / k + t0)
    },
    "logistic" = {
      # Logistic growth: age increases logistically with length
      with(growth_params, {
        max_age <- max(ages)
        min_age <- min(ages)
        steepness <- log(19) / (l95 - l50) # 95% / 5% = 19
        min_age + (max_age - min_age) / (1 + exp(-steepness * (lengths - l50)))
      })
    }
  )

  # Ensure mean ages are within reasonable bounds
  mean_ages <- pmax(age_range[1] - 0.5, pmin(age_range[2] + 0.5, mean_ages))

  # Create age-length key
  alk_data <- list()

  for (i in seq_along(lengths)) {
    length_val <- lengths[i]
    mean_age <- mean_ages[i]

    # Calculate standard deviation based on CV
    sd_age <- cv * mean_age

    # Calculate probabilities for each age using normal distribution
    age_probs <- dnorm(ages, mean = mean_age, sd = sd_age)

    # Normalize to sum to 1
    age_probs <- age_probs / sum(age_probs)

    # Create data frame for this length
    length_data <- data.frame(
      length = length_val,
      age = ages,
      proportion = age_probs,
      stringsAsFactors = FALSE
    )

    alk_data[[i]] <- length_data
  }

  # Combine all data
  alk <- do.call(rbind, alk_data)
  rownames(alk) <- NULL

  # Apply plus group if requested
  if (plus_group_age) {
    max_age <- max(ages)
    # Combine all ages >= max_age into the plus group
    plus_rows <- alk$age >= max_age

    # Sum proportions for plus group
    plus_group_data <- aggregate(proportion ~ length,
      data = alk[plus_rows, ],
      sum
    )
    plus_group_data$age <- max_age

    # Remove original plus group ages and add combined plus group
    alk_final <- rbind(
      alk[!plus_rows, ],
      plus_group_data
    )

    # Sort by length then age
    alk_final <- alk_final[order(alk_final$length, alk_final$age), ]
    rownames(alk_final) <- NULL
    alk <- alk_final
  }

  # Validate that proportions sum to 1 for each length (within tolerance)
  prop_sums <- aggregate(proportion ~ length, data = alk, sum)
  if (any(abs(prop_sums$proportion - 1) > 1e-10)) {
    warning("Proportions do not sum exactly to 1 for some lengths (numerical precision)")
  }

  # Set class for S3 methods
  class(alk) <- c("age_length_key", "data.frame")

  return(alk)
}

