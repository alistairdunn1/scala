#' @title Fit Cohort-Based Age-Length Model using GAM
#' @description Experimental: Fits an ordinal cohort-at-length model using cumulative logit regression
#'   to estimate year classes (cohorts) from length and sampling year. Cohorts are defined as
#'   (sampling_year - age) - age_offset. The model can predict cohorts from length-year observations and
#'   back-calculate ages given sampling year and length.
#'
#' @param cohort_data Data frame with columns: 'age', 'length', 'year', and optionally 'sex'.
#'   Each row represents one aged fish with known sampling year.
#' @param age_offset Numeric offset for year class calculation: YC = (Year - Age) - age_offset (default 1)
#' @param by_sex Logical, whether to fit sex-specific smooth terms (default TRUE)
#' @param k_length Basis dimension for length smooth terms (default -1 for automatic selection)
#' @param k_year Basis dimension for year smooth terms (default -1 for automatic selection)
#' @param method Smoothing parameter estimation method for mgcv (default "REML")
#' @param weights Optional weights for observations (default NULL)
#' @param verbose Logical, whether to print model fitting details (default TRUE)
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{model}: The fitted mgcv::gam model object
#'     \item \code{predict_cohort}: Function(lengths, years, sex) that returns cohort probabilities
#'     \item \code{predict_age}: Function(lengths, sampling_years, sex) that returns age probabilities
#'     \item \code{summary}: Model summary including deviance explained and significance tests
#'     \item \code{by_sex}: Logical indicating whether sex-specific terms were used
#'     \item \code{cohort_levels}: Vector of cohort levels in the model
#'     \item \code{sex_levels}: Vector of sex levels (if applicable)
#'     \item \code{year_range}: Range of years in training data
#'     \item \code{age_offset}: The age offset used in cohort calculation
#'   }
#'
#' @details
#' The function fits an ordinal regression model using the cumulative logit link function
#' to model cohorts (year classes) as a function of length and year. Cohorts are calculated
#' as: cohort = (sampling_year - age) - age_offset.
#'
#' The model structure is:
#'
#' \strong{Without sex effects:}
#' \code{cohort ~ s(length) + s(year)}
#'
#' \strong{With sex effects:}
#' \code{cohort ~ s(length, by = sex) + s(year, by = sex) + sex}
#'
#' The model enables two types of predictions:
#' 1. \strong{Cohort prediction}: Given length and year, estimate cohort probabilities
#' 2. \strong{Age back-calculation}: Given length and sampling year, estimate age probabilities
#'    by converting cohort predictions using: age = sampling_year - cohort - age_offset
#'
#' @examples
#' \dontrun{
#' # Generate cohort data with age, length, year, and sex
#' set.seed(123)
#' n <- 500
#' years <- 2015:2023
#' cohort_data <- data.frame(
#'   year = sample(years, n, replace = TRUE),
#'   age = sample(1:8, n, replace = TRUE),
#'   sex = sample(c("male", "female"), n, replace = TRUE)
#' )
#'
#' # Add length based on age (with growth variation)
#' cohort_data$length <- with(
#'   cohort_data,
#'   20 + age * 4 + ifelse(sex == "female", 2, 0) + rnorm(n, 0, 2)
#' )
#'
#' # Fit cohort model
#' cohort_model <- fit_cohort_alk(cohort_data, by_sex = TRUE, verbose = TRUE)
#'
#' # Predict cohorts for new length-year combinations
#' test_lengths <- c(25, 35, 45)
#' test_years <- c(2020, 2021, 2022)
#' test_sex <- c("male", "female", "male")
#'
#' cohort_probs <- cohort_model$predict_cohort(test_lengths, test_years, test_sex)
#'
#' # Back-calculate ages for length observations from 2023 sampling
#' length_obs <- c(28, 38, 48)
#' sampling_year <- rep(2023, 3)
#' obs_sex <- c("female", "male", "female")
#'
#' age_probs <- cohort_model$predict_age(length_obs, sampling_year, obs_sex)
#' print(age_probs)
#' }
#'
#' @importFrom mgcv gam s
#' @importFrom stats predict model.matrix
#' @seealso \code{\link{fit_ordinal_alk}}, \code{\link{create_alk}}, \code{\link[mgcv]{gam}}
#' @export
fit_cohort_alk <- function(cohort_data, age_offset = 1, by_sex = TRUE, k_length = -1, k_year = -1,
                           method = "REML", weights = NULL, verbose = TRUE) {
  # Check if mgcv is available
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package is required for cohort age-length modeling. Install with: install.packages('mgcv')")
  }

  # Validate required columns
  required_cols <- c("age", "length", "year")
  if (!all(required_cols %in% names(cohort_data))) {
    stop("cohort_data must contain 'age', 'length', and 'year' columns")
  }

  # Check for sex column if by_sex is TRUE
  if (by_sex && !"sex" %in% names(cohort_data)) {
    stop("by_sex = TRUE requires 'sex' column in cohort_data")
  }

  # Validate age_offset
  if (!is.numeric(age_offset) || length(age_offset) != 1) {
    stop("age_offset must be a single numeric value")
  }

  # Calculate cohorts: cohort = (year - age) - age_offset
  cohort_data$cohort <- (cohort_data$year - cohort_data$age) - age_offset

  # Convert cohort to ordered factor then integer for mgcv::ocat
  cohort_data$cohort <- as.ordered(cohort_data$cohort)
  cohort_levels <- levels(cohort_data$cohort)
  cohort_data$cohort <- as.integer(cohort_data$cohort) # required by ocat

  if (verbose) {
    cat("Fitting cohort-based age-length model...\n")
    cat("Age offset:", age_offset, "(YC = (Year - Age) -", age_offset, ")\n")
    cat("Cohort levels:", paste(cohort_levels, collapse = ", "), "\n")
    cat("Length range:", min(cohort_data$length), "to", max(cohort_data$length), "\n")
    cat("Year range:", min(cohort_data$year), "to", max(cohort_data$year), "\n")
  }

  # Handle sex if applicable
  sex_levels <- NULL
  if (by_sex) {
    cohort_data$sex <- as.factor(cohort_data$sex)
    sex_levels <- levels(cohort_data$sex)
    if (verbose) cat("Sex levels:", paste(sex_levels, collapse = ", "), "\n")
  }

  # Determine appropriate k values based on data
  n_unique_lengths <- length(unique(cohort_data$length))
  n_unique_years <- length(unique(cohort_data$year))

  # Set conservative k values for small datasets
  if (k_length <= 0) k_length <- min(10, max(3, floor(n_unique_lengths / 3)))
  if (k_year <= 0) k_year <- min(10, max(3, floor(n_unique_years / 2)))

  if (verbose) {
    cat("Using k =", k_length, "for length terms,", k_year, "for year terms\n")
  }

  # Build model formula
  if (by_sex) {
    formula_parts <- c()

    # Length terms
    formula_parts <- c(formula_parts, paste0("s(length, by = sex, k = ", k_length, ")"))

    # Year terms
    formula_parts <- c(formula_parts, paste0("s(year, by = sex, k = ", k_year, ")"))

    # Add sex main effect
    formula_parts <- c(formula_parts, "sex")

    formula <- as.formula(paste("cohort ~", paste(formula_parts, collapse = " + ")))

    if (verbose) {
      cat("Model formula: cohort ~", paste(formula_parts, collapse = " + "), "\n")
    }
  } else {
    formula_parts <- c()

    # Length terms
    formula_parts <- c(formula_parts, paste0("s(length, k = ", k_length, ")"))

    # Year terms
    formula_parts <- c(formula_parts, paste0("s(year, k = ", k_year, ")"))

    formula <- as.formula(paste("cohort ~", paste(formula_parts, collapse = " + ")))

    if (verbose) {
      cat("Model formula: cohort ~", paste(formula_parts, collapse = " + "), "\n")
    }
  } # Fit the ordinal GAM model
  if (verbose) cat("Fitting GAM with cumulative logit link...\n")

  tryCatch(
    {
      gam_model <- mgcv::gam(
        formula = formula,
        data = cohort_data,
        family = mgcv::ocat(R = length(cohort_levels)), # Ordered categorical
        weights = weights,
        method = method
      )
    },
    error = function(e) {
      stop("Error fitting GAM model: ", e$message)
    }
  )

  if (verbose) {
    cat("Model fitted successfully!\n")
    cat("Deviance explained:", round(summary(gam_model)$dev.expl * 100, 1), "%\n")
  }

  # Store year range for validation
  year_range <- range(cohort_data$year)

  # Create cohort prediction function
  predict_cohort <- function(lengths, years, sex = NULL) {
    # Validate inputs
    if (!is.numeric(lengths) || !is.numeric(years)) {
      stop("lengths and years must be numeric")
    }

    if (length(lengths) != length(years)) {
      stop("lengths and years must have the same length")
    }

    if (by_sex && is.null(sex)) {
      stop("sex must be provided when model was fitted with by_sex = TRUE")
    }

    if (!by_sex && !is.null(sex)) {
      warning("sex provided but model was fitted with by_sex = FALSE. Ignoring sex.")
      sex <- NULL
    }

    # Create prediction data
    if (by_sex) {
      if (length(sex) == 1) {
        sex <- rep(sex, length(lengths))
      } else if (length(sex) != length(lengths)) {
        stop("sex must be either length 1 or same length as lengths and years")
      }

      # Check sex levels
      if (!all(sex %in% sex_levels)) {
        stop("sex values must be in: ", paste(sex_levels, collapse = ", "))
      }

      newdata <- data.frame(
        length = lengths,
        year = years,
        sex = factor(sex, levels = sex_levels)
      )
    } else {
      newdata <- data.frame(
        length = lengths,
        year = years
      )
    }

    # Use the same prediction logic as fit_ordinal_alk
    n_cohorts <- length(cohort_levels)
    n_obs <- nrow(newdata)

    if (n_cohorts == 1) {
      prob_matrix <- matrix(1, nrow = n_obs, ncol = 1)
      colnames(prob_matrix) <- paste0("cohort_", cohort_levels)
      return(prob_matrix)
    }

    # Try response first
    prob_matrix <- NULL
    resp_ok <- FALSE
    resp_try <- try(stats::predict(gam_model, newdata = newdata, type = "response"), silent = TRUE)
    if (!inherits(resp_try, "try-error") && !is.null(resp_try)) {
      if (is.matrix(resp_try) && ncol(resp_try) %in% c(n_cohorts, n_cohorts - 1)) {
        if (ncol(resp_try) == n_cohorts) {
          prob_matrix <- resp_try
          resp_ok <- TRUE
        } else if (ncol(resp_try) == (n_cohorts - 1)) {
          # Interpret as cumulative probs and convert to class probs
          cumprob <- resp_try
          prob_matrix <- matrix(NA_real_, nrow = n_obs, ncol = n_cohorts)
          prob_matrix[, 1] <- cumprob[, 1]
          if (n_cohorts > 2) {
            for (j in 2:(n_cohorts - 1)) prob_matrix[, j] <- cumprob[, j] - cumprob[, j - 1]
          }
          prob_matrix[, n_cohorts] <- 1 - cumprob[, n_cohorts - 1]
          resp_ok <- TRUE
        }
      } else if (is.vector(resp_try) && length(resp_try) == n_obs * n_cohorts) {
        prob_matrix <- matrix(resp_try, nrow = n_obs, ncol = n_cohorts)
        resp_ok <- TRUE
      }
    }

    if (!resp_ok) {
      # Fallback: get cumulative logits and convert
      pred_link <- stats::predict(gam_model, newdata = newdata, type = "link")

      # Coerce to matrix (n_obs x (K-1)) robustly
      if (is.matrix(pred_link)) {
        if (nrow(pred_link) == n_obs && ncol(pred_link) == (n_cohorts - 1)) {
          # as-is
        } else if (nrow(pred_link) == (n_cohorts - 1) && ncol(pred_link) == n_obs) {
          pred_link <- t(pred_link)
        } else if (length(as.numeric(pred_link)) == n_obs * (n_cohorts - 1)) {
          pred_link <- matrix(as.numeric(pred_link), nrow = n_obs, ncol = (n_cohorts - 1))
        } else {
          stop("Unexpected shape from predict(type='link') for ocat family")
        }
      } else {
        v <- as.numeric(pred_link)
        if (length(v) == n_obs * (n_cohorts - 1)) {
          pred_link <- matrix(v, nrow = n_obs, ncol = (n_cohorts - 1))
        } else {
          stop("Could not coerce prediction to matrix: lengths and cohorts mismatch")
        }
      }

      cumprob <- stats::plogis(pred_link) # n_obs x (K-1)
      prob_matrix <- matrix(NA_real_, nrow = n_obs, ncol = n_cohorts)
      prob_matrix[, 1] <- cumprob[, 1]
      if (n_cohorts > 2) {
        for (j in 2:(n_cohorts - 1)) prob_matrix[, j] <- cumprob[, j] - cumprob[, j - 1]
      }
      prob_matrix[, n_cohorts] <- 1 - cumprob[, n_cohorts - 1]
    }

    # Finalize: name, clip, normalize
    colnames(prob_matrix) <- paste0("cohort_", cohort_levels)
    prob_matrix[!is.finite(prob_matrix)] <- 0
    prob_matrix[prob_matrix < 0] <- 0
    rs <- rowSums(prob_matrix)
    keep <- rs > 0 & is.finite(rs)
    if (any(keep)) prob_matrix[keep, ] <- prob_matrix[keep, , drop = FALSE] / rs[keep]

    return(prob_matrix)
  }

  # Create age back-calculation function
  predict_age <- function(lengths, sampling_years, sex = NULL) {
    # Validate inputs
    if (!is.numeric(lengths) || !is.numeric(sampling_years)) {
      stop("lengths and sampling_years must be numeric")
    }

    if (length(lengths) != length(sampling_years)) {
      stop("lengths and sampling_years must have the same length")
    }

    # Get cohort probabilities for each length at each sampling year
    cohort_probs <- predict_cohort(lengths, sampling_years, sex)

    # Convert cohort probabilities to age probabilities
    # age = sampling_year - cohort + 1, so cohort = sampling_year - age + 1
    n_obs <- length(lengths)
    cohort_years <- as.numeric(cohort_levels)

    # Calculate possible ages for each observation
    # age = sampling_year - cohort - age_offset, so cohort = sampling_year - age - age_offset
    age_matrices <- list()
    for (i in seq_len(n_obs)) {
      ages_for_obs <- sampling_years[i] - cohort_years - age_offset
      valid_ages <- ages_for_obs[ages_for_obs > 0] # Only positive ages

      if (length(valid_ages) == 0) {
        warning("No valid ages for observation ", i, " (sampling year ", sampling_years[i], ")")
        next
      }

      # Create age probability vector for this observation
      age_probs <- numeric(max(valid_ages))
      for (j in seq_along(valid_ages)) {
        if (valid_ages[j] <= length(age_probs)) {
          cohort_idx <- which(cohort_years == (sampling_years[i] - valid_ages[j] - age_offset))
          if (length(cohort_idx) == 1) {
            age_probs[valid_ages[j]] <- cohort_probs[i, cohort_idx]
          }
        }
      }

      age_matrices[[i]] <- age_probs
    }

    # Standardize to common age range
    if (length(age_matrices) > 0) {
      max_age <- max(sapply(age_matrices, length))
      age_matrix <- matrix(0, nrow = n_obs, ncol = max_age)

      for (i in seq_len(n_obs)) {
        if (!is.null(age_matrices[[i]])) {
          age_matrix[i, seq_along(age_matrices[[i]])] <- age_matrices[[i]]
        }
      }

      # Remove trailing zero columns
      last_nonzero <- max(which(colSums(age_matrix) > 0))
      if (last_nonzero < ncol(age_matrix)) {
        age_matrix <- age_matrix[, seq_len(last_nonzero), drop = FALSE]
      }

      colnames(age_matrix) <- paste0("age_", seq_len(ncol(age_matrix)))
    } else {
      age_matrix <- matrix(0, nrow = n_obs, ncol = 1)
      colnames(age_matrix) <- "age_1"
    }

    return(age_matrix)
  }

  # Create model summary
  model_summary <- list(
    deviance_explained = summary(gam_model)$dev.expl,
    aic = AIC(gam_model),
    n_observations = nrow(cohort_data),
    edf = sum(gam_model$edf),
    smooth_terms = summary(gam_model)$s.table
  )

  if (verbose) {
    cat("\nModel Summary:\n")
    cat("Observations:", model_summary$n_observations, "\n")
    cat("AIC:", round(model_summary$aic, 1), "\n")
    cat("Effective degrees of freedom:", round(model_summary$edf, 1), "\n")
  }

  # Return results
  result <- list(
    model = gam_model,
    predict_cohort = predict_cohort,
    predict_age = predict_age,
    summary = model_summary,
    by_sex = by_sex,
    cohort_levels = cohort_levels,
    sex_levels = sex_levels,
    year_range = year_range,
    age_offset = age_offset
  )

  class(result) <- "cohort_alk"
  return(result)
}

#' Print method for cohort_alk objects
#' @param x A cohort_alk object
#' @param ... Additional arguments (ignored)
#' @export
print.cohort_alk <- function(x, ...) {
  cat("Cohort-Based Age-at-Length Model (GAM)\n")
  cat("=====================================\n\n")

  cat("Model specification:\n")
  if (x$by_sex) {
    cat("  Formula: cohort ~ s(length, by = sex) + s(year, by = sex) + sex\n")
    cat("  Sex levels:", paste(x$sex_levels, collapse = ", "), "\n")
  } else {
    cat("  Formula: cohort ~ s(length) + s(year)\n")
  }
  cat("  Age offset:", x$age_offset, "(YC = (Year - Age) -", x$age_offset, ")\n")
  cat("  Cohort levels:", paste(x$cohort_levels, collapse = ", "), "\n")
  cat("  Year range:", paste(x$year_range, collapse = " - "), "\n")
  cat("  Family: Ordered categorical (cumulative logit)\n\n")

  cat("Model fit:\n")
  cat("  Observations:", x$summary$n_observations, "\n")
  cat("  Deviance explained:", round(x$summary$deviance_explained * 100, 1), "%\n")
  cat("  AIC:", round(x$summary$aic, 1), "\n")
  cat("  Effective df:", round(x$summary$edf, 1), "\n\n")

  cat("Functions available:\n")
  cat("  predict_cohort(lengths, years, sex) - Predict cohort probabilities\n")
  cat("  predict_age(lengths, sampling_years, sex) - Back-calculate age probabilities\n")
}
