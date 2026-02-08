#' @title Assign Ages to Fish Data Using Cohort Model
#' @description Assigns ages to individual fish observations using predictions from a fitted cohort model.
#'   For each fish, age probabilities are calculated based on length, sampling year, and sex,
#'   and a single age is assigned either deterministically (most probable) or stochastically (sampled).
#'
#' @param fish_data Data frame with columns: length, year, and optionally 'sex'.
#'   Additional columns (e.g., stratum, sample_id, counts) will be preserved.
#' @param cohort_model A fitted cohort model object from \code{\link{fit_cohort_alk}}
#' @param method Character, method for age assignment:
#'   \itemize{
#'     \item \code{"mode"}: Assign the most probable age (default)
#'     \item \code{"expected"}: Assign the expected age (weighted mean of probabilities)
#'     \item \code{"random"}: Randomly sample age based on probabilities
#'   }
#' @param seed Integer, random seed for reproducible sampling when method = "random" (default NULL)
#' @param keep_probabilities Logical, whether to keep all age probabilities in output (default FALSE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return Data frame with original columns plus:
#'   \itemize{
#'     \item \code{age}: Assigned age for each observation
#'     \item \code{age_prob_*}: Age probability columns (only if keep_probabilities = TRUE)
#'   }
#'
#' @details
#' This function uses the \code{predict_age} function from a fitted cohort model to obtain
#' age probability distributions for each fish observation. Ages are then assigned according
#' to the specified method:
#'
#' **Mode method**: Assigns the age with highest probability (most likely age)
#'
#' **Expected method**: Assigns the expected value (mean) of the age distribution.
#' This may result in non-integer ages which are rounded to the nearest integer.
#'
#' **Random method**: Stochastically samples an age from the probability distribution.
#' Use this for propagating uncertainty through subsequent analyses.
#'
#' The function handles sex-specific models automatically. If the cohort model was fitted
#' with \code{by_sex = TRUE}, sex information must be present in fish_data.
#'
#' @examples
#' \dontrun{
#' # Fit cohort model on aged subsample
#' aged_data <- data.frame(
#'   age = c(2, 3, 3, 4, 5),
#'   length = c(25, 30, 32, 38, 42),
#'   year = c(2020, 2020, 2021, 2021, 2022),
#'   sex = c("male", "female", "male", "female", "male")
#' )
#' cohort_model <- fit_cohort_alk(aged_data, by_sex = TRUE)
#'
#' # Assign ages to full length dataset
#' length_data <- data.frame(
#'   length = c(26, 31, 29, 35, 40),
#'   year = c(2020, 2021, 2020, 2021, 2022),
#'   sex = c("male", "male", "female", "female", "male"),
#'   stratum = c("A", "A", "B", "B", "A"),
#'   sample_id = c(1, 1, 2, 2, 3)
#' )
#'
#' # Assign ages using most probable age
#' age_assigned <- assign_ages_from_cohort(length_data, cohort_model, method = "mode")
#'
#' # Assign ages stochastically for uncertainty propagation
#' age_assigned_random <- assign_ages_from_cohort(
#'   length_data,
#'   cohort_model,
#'   method = "random",
#'   seed = 123
#' )
#' }
#'
#' @seealso \code{\link{fit_cohort_alk}}, \code{\link{calculate_age_compositions_from_cohort}}
#' @export
assign_ages_from_cohort <- function(fish_data,
                                    cohort_model,
                                    method = c("mode", "expected", "random"),
                                    seed = NULL,
                                    keep_probabilities = FALSE,
                                    verbose = TRUE) {
  # Validate inputs
  if (!is.data.frame(fish_data)) {
    stop("fish_data must be a data frame")
  }

  if (!inherits(cohort_model, "cohort_alk")) {
    stop("cohort_model must be a fitted cohort_alk object from fit_cohort_alk()")
  }

  method <- match.arg(method)

  # Check required columns
  required_cols <- c("length", "year")
  missing_cols <- setdiff(required_cols, names(fish_data))
  if (length(missing_cols) > 0) {
    stop("fish_data must contain columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for sex column if model requires it
  if (cohort_model$by_sex && !"sex" %in% names(fish_data)) {
    stop("Cohort model was fitted with by_sex = TRUE, but fish_data does not contain 'sex' column")
  }

  # Standardize sex to lowercase if present
  if ("sex" %in% names(fish_data)) {
    fish_data$sex <- tolower(fish_data$sex)
  }

  if (verbose) {
    cat("Assigning ages using cohort model...\n")
    cat("Method:", method, "\n")
    cat("Observations:", nrow(fish_data), "\n")
  }

  # Set seed for reproducibility if using random method
  if (method == "random" && !is.null(seed)) {
    set.seed(seed)
  }

  # Get age probabilities for all fish
  if (cohort_model$by_sex) {
    age_probs <- cohort_model$predict_age(
      lengths = fish_data$length,
      sampling_years = fish_data$year,
      sex = fish_data$sex
    )
  } else {
    age_probs <- cohort_model$predict_age(
      lengths = fish_data$length,
      sampling_years = fish_data$year
    )
  }

  # Extract age values from column names (e.g., "age_1" -> 1)
  age_values <- as.numeric(gsub("age_", "", colnames(age_probs)))

  # Check for rows with zero total probability (fish outside model range)
  row_sums <- rowSums(age_probs)
  zero_prob_rows <- which(row_sums == 0 | !is.finite(row_sums))
  if (length(zero_prob_rows) > 0) {
    warning(length(zero_prob_rows), " observation(s) had zero age probability ",
            "(fish may be outside the model's length/year range). ",
            "These will be assigned NA age.")
  }

  # Assign ages based on method
  assigned_ages <- rep(NA_real_, nrow(age_probs))
  valid_rows <- setdiff(seq_len(nrow(age_probs)), zero_prob_rows)

  if (length(valid_rows) > 0) {
    if (method == "mode") {
      # Assign most probable age
      assigned_ages[valid_rows] <- age_values[apply(age_probs[valid_rows, , drop = FALSE], 1, which.max)]
    } else if (method == "expected") {
      # Assign expected age (weighted mean), rounded to nearest integer
      assigned_ages[valid_rows] <- round(rowSums(sweep(
        age_probs[valid_rows, , drop = FALSE], 2, age_values, "*"
      )))
    } else if (method == "random") {
      # Sample age from probability distribution
      # Guard: sample(x) with length-1 numeric x is interpreted as sample(1:x)
      if (length(age_values) == 1) {
        assigned_ages[valid_rows] <- age_values
      } else {
        for (i in valid_rows) {
          assigned_ages[i] <- sample(age_values, size = 1, prob = age_probs[i, ])
        }
      }
    }
  }

  # Add assigned ages to fish_data
  result <- fish_data
  result$age <- assigned_ages

  # Optionally keep probability columns
  if (keep_probabilities) {
    prob_df <- as.data.frame(age_probs)
    names(prob_df) <- paste0("age_prob_", age_values)
    result <- cbind(result, prob_df)
  }

  if (verbose) {
    valid_ages <- assigned_ages[!is.na(assigned_ages)]
    cat("Age assignment complete!\n")
    if (length(valid_ages) > 0) {
      cat("Age range:", min(valid_ages), "to", max(valid_ages), "\n")
      cat("Mean age:", round(mean(valid_ages), 2), "\n")
    }
    if (length(zero_prob_rows) > 0) {
      cat("WARNING:", length(zero_prob_rows), "observations assigned NA age\n")
    }
  }

  return(result)
}
