#' @title Assign Ages to Fish Data Using Otolith Weight Model
#' @description Assigns ages to individual fish observations using predictions from a fitted
#'   weight-age model. For each fish, age probabilities are calculated based on otolith weight
#'   and optionally sex, and a single age is assigned either deterministically (most probable)
#'   or stochastically (sampled from the posterior).
#'
#' @param fish_data Data frame with column 'otolith_weight' (grams) and optionally 'sex'.
#'   Additional columns (e.g., stratum, sample_id, length) will be preserved.
#' @param weight_age_model A fitted weight-age model object from \code{\link{fit_weight_age}}
#' @param method Character, method for age assignment:
#'   \itemize{
#'     \item \code{"random"}: Randomly sample age based on probabilities (default)
#'     \item \code{"mode"}: Assign the most probable age
#'     \item \code{"expected"}: Assign the expected age (weighted mean of probabilities)
#'   }
#' @param seed Integer, random seed for reproducible sampling when method = "random" (default NULL)
#' @param keep_probabilities Logical, whether to keep all age probabilities in output (default FALSE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#' @param ... Additional named arguments passed through to the model's \code{predict_function}.
#'   Required when the model was fitted with \code{additional_terms}.
#'
#' @return Data frame with original columns plus:
#'   \itemize{
#'     \item \code{age}: Assigned age for each observation
#'     \item \code{age_prob_*}: Age probability columns (only if keep_probabilities = TRUE)
#'   }
#'
#' @details
#' This function applies the \code{predict_function} from a fitted weight-age model to obtain
#' age probability distributions for each fish observation. Ages are then assigned according
#' to the specified method:
#'
#' **Mode method**: Assigns the age with highest probability (most likely age)
#'
#' **Expected method**: Assigns the expected value (mean) of the age distribution.
#' This may result in non-integer ages which are rounded to the nearest integer.
#'
#' **Random method**: Stochastically samples an age from the probability distribution.
#' Use this when bootstrapping to propagate model uncertainty through subsequent analyses.
#'
#' Fish with otolith weights outside the model's training range will still receive age
#' predictions (extrapolation), but a warning is issued. Fish with zero total probability
#' (e.g., due to NA weights) will be assigned NA age.
#'
#' @examples
#' \dontrun{
#' # Fit weight-age model on aged subsample
#' aged_data <- data.frame(
#'   age = rep(1:6, each = 30),
#'   weight = c(
#'     rnorm(30, 0.05, 0.01), rnorm(30, 0.12, 0.015),
#'     rnorm(30, 0.20, 0.02), rnorm(30, 0.30, 0.025),
#'     rnorm(30, 0.41, 0.03), rnorm(30, 0.53, 0.035)
#'   ),
#'   sex = rep(c("male", "female"), 90)
#' )
#' wt_model <- fit_weight_age(aged_data, by_sex = TRUE)
#'
#' # Assign ages to full fish dataset
#' fish_data <- data.frame(
#'   otolith_weight = runif(50, 0.03, 0.60),
#'   sex = sample(c("male", "female"), 50, replace = TRUE),
#'   stratum = sample(c("A", "B"), 50, replace = TRUE),
#'   sample_id = rep(1:10, each = 5)
#' )
#'
#' # Assign ages using most probable age
#' fish_aged <- assign_ages_from_weight(fish_data, wt_model, method = "mode")
#'
#' # Assign ages stochastically for uncertainty propagation
#' fish_aged_random <- assign_ages_from_weight(
#'   fish_data, wt_model, method = "random", seed = 42
#' )
#' }
#'
#' @seealso \code{\link{fit_weight_age}}, \code{\link{calculate_age_compositions_from_weight}}
#' @export
assign_ages_from_weight <- function(fish_data,
                                    weight_age_model,
                                    method = c("random", "mode", "expected"),
                                    seed = NULL,
                                    keep_probabilities = FALSE,
                                    verbose = TRUE,
                                    ...) {
  if (!is.data.frame(fish_data)) {
    stop("fish_data must be a data frame")
  }

  if (!inherits(weight_age_model, "weight_age_model")) {
    stop("weight_age_model must be a fitted weight_age_model object from fit_weight_age()")
  }

  method <- match.arg(method)

  if (!"otolith_weight" %in% names(fish_data)) {
    stop("fish_data must contain 'otolith_weight' column")
  }

  if (weight_age_model$by_sex && !"sex" %in% names(fish_data)) {
    stop("weight_age_model was fitted with by_sex = TRUE, but fish_data does not contain 'sex' column")
  }

  if ("sex" %in% names(fish_data)) {
    fish_data$sex <- tolower(fish_data$sex)
  }

  extra_args <- list(...)

  # Warn about out-of-range weights
  wt_range <- weight_age_model$weight_range
  out_of_range <- !is.na(fish_data$otolith_weight) &
    (fish_data$otolith_weight < wt_range[1] | fish_data$otolith_weight > wt_range[2])
  if (any(out_of_range)) {
    warning(
      sum(out_of_range), " fish have otolith weights outside the model's training range [",
      wt_range[1], ", ", wt_range[2], "]. Age predictions will be extrapolated."
    )
  }

  # Determine which rows can be predicted (non-NA weight)
  predict_rows <- which(!is.na(fish_data$otolith_weight))

  if (verbose) {
    cat("Assigning ages using weight-age model...\n")
    cat("Method:", method, "\n")
    cat("Observations:", nrow(fish_data), "(", length(predict_rows), "to predict)\n")
  }

  if (method == "random" && !is.null(seed)) {
    set.seed(seed)
  }

  # Build predict_function arguments
  predict_args <- list(weights = fish_data$otolith_weight[predict_rows])

  if (weight_age_model$by_sex) {
    predict_args$sex <- fish_data$sex[predict_rows]
  }

  # Append additional variables, subsetted to predict_rows
  predict_extra <- lapply(extra_args, function(v) {
    if (length(v) == nrow(fish_data)) v[predict_rows] else v
  })
  predict_args <- c(predict_args, predict_extra)

  age_probs <- do.call(weight_age_model$predict_function, predict_args)

  age_values <- as.numeric(gsub("age_", "", colnames(age_probs)))

  row_sums <- rowSums(age_probs)
  zero_prob_rows <- which(row_sums == 0 | !is.finite(row_sums))
  if (length(zero_prob_rows) > 0) {
    warning(
      length(zero_prob_rows), " observation(s) had zero age probability. ",
      "These will be assigned NA age."
    )
  }

  assigned_ages_subset <- rep(NA_real_, nrow(age_probs))
  valid_rows <- setdiff(seq_len(nrow(age_probs)), zero_prob_rows)

  if (length(valid_rows) > 0) {
    if (method == "mode") {
      assigned_ages_subset[valid_rows] <- age_values[apply(age_probs[valid_rows, , drop = FALSE], 1, which.max)]
    } else if (method == "expected") {
      assigned_ages_subset[valid_rows] <- round(rowSums(sweep(
        age_probs[valid_rows, , drop = FALSE], 2, age_values, "*"
      )))
    } else if (method == "random") {
      if (length(age_values) == 1) {
        assigned_ages_subset[valid_rows] <- age_values
      } else {
        for (i in valid_rows) {
          assigned_ages_subset[i] <- sample(age_values, size = 1, prob = age_probs[i, ])
        }
      }
    }
  }

  assigned_ages <- rep(NA_real_, nrow(fish_data))
  assigned_ages[predict_rows] <- assigned_ages_subset

  result <- fish_data
  result$age <- assigned_ages

  if (keep_probabilities) {
    prob_df_full <- as.data.frame(matrix(NA_real_, nrow = nrow(fish_data), ncol = ncol(age_probs)))
    names(prob_df_full) <- paste0("age_prob_", age_values)
    prob_df_full[predict_rows, ] <- as.data.frame(age_probs)
    result <- cbind(result, prob_df_full)
  }

  if (verbose) {
    valid_ages <- assigned_ages[!is.na(assigned_ages)]
    cat("Age assignment complete!\n")
    if (length(valid_ages) > 0) {
      cat("Age range:", min(valid_ages), "to", max(valid_ages), "\n")
      cat("Mean age:", round(mean(valid_ages), 2), "\n")
    }
    n_na <- sum(is.na(assigned_ages))
    if (n_na > 0) {
      cat("NOTE:", n_na, "observations assigned NA age (missing or zero-probability)\n")
    }
  }

  return(result)
}
