#' @title Fit Cohort-Based Age-Length Model using GAM
#' @description Fits an ordinal cohort-at-length model using cumulative logit regression
#'   to estimate year classes (cohorts) from length and sampling year. Cohorts are defined as
#'   (sampling_year - age) - age_offset. The model can predict cohorts from length-year observations and
#'   back-calculate ages given sampling year and length.
#'
#' @param cohort_data Data frame with columns: 'age', 'length', 'year', and optionally 'sex'.
#'   Each row represents one aged fish with a positive integer age, a finite positive length and an integer sampling year
#' @param alk_data Alternative name for cohort_data, for compatibility with other functions
#' @param age_offset Non-negative integer offset for year class calculation: YC = (Year - Age) - age_offset (default 1)
#' @param by_sex Logical, whether to fit sex-specific smooth terms (default TRUE)
#' @param k_length Basis dimension for length smooth terms (default -1 for automatic selection)
#' @param k_year Basis dimension for year smooth terms (default -1 for automatic selection)
#' @param additional_terms Character vector of additional GAM formula terms to include in the model (default NULL).
#'   Each element should be a valid mgcv smooth term as a character string (e.g., "te(lat, long)", "s(day_of_year, bs = 'cc')").
#'   When by_sex = TRUE, these terms will automatically be fitted with 'by = sex' interactions.
#' @param select Logical, whether to add an extra penalty to each smooth term allowing
#'   terms to be penalised to zero (variable selection). Recommended for models with
#'   multiple smooth terms (default TRUE). See \code{\link[mgcv]{gam}} for details.
#' @param gamma Numeric multiplier for the effective degrees of freedom in the smoothing
#'   parameter selection criterion. Values > 1 (e.g., 1.4) produce smoother models and
#'   help guard against overfitting (default 1.4, following Wood 2006 recommendation).
#' @param method Smoothing parameter estimation method, either "REML" or "ML"
#' @param weights Optional weights for observations (default NULL)
#' @param verbose Logical, whether to print model fitting details (default TRUE)
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{model}: The fitted mgcv::gam model object
#'     \item \code{predict_cohort}: Function(lengths, years, sex) that returns cohort probabilities
#'     \item \code{predict_age}: Function(lengths, sampling_years, sex) that returns age probabilities
#'     \item \code{model_summary}: Model summary including deviance explained and significance tests
#'     \item \code{by_sex}: Logical indicating whether sex-specific terms were used
#'     \item \code{cohorts}: Vector of cohort levels in the model
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
#' Fitting and prediction condition on positive integer ages in each sampling year.
#' A cohort is admissible when sampling_year - cohort - age_offset is at least one.
#' The likelihood divides the observed cohort probability by the total probability
#' of admissible cohorts. Smooth coefficients, cohort cut-points and smoothing
#' parameters are estimated under this conditional likelihood. Age zero is outside
#' the sampling support of this method.
#'
#' Cohort and age predictions sum to one for each fish. Inadmissible cohorts have
#' probability zero. Prediction stops when no fitted cohort implies a positive age.
#' The fitted GAM supports stats::predict(model, newdata, type = "response")
#' with the same conditional probabilities. The returned age_support metadata
#' records the minimum age and conditioning rule.

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
#' @importFrom stats predict model.matrix AIC as.formula
#' @seealso \code{\link{fit_ordinal_alk}}, \code{\link{create_alk}}, \code{\link[mgcv]{gam}}
#' @export
fit_cohort_alk <- function(cohort_data = NULL, alk_data = NULL, age_offset = 1, by_sex = TRUE,
                           k_length = -1, k_year = -1, additional_terms = NULL,
                           select = TRUE, gamma = 1.4,
                           method = "REML", weights = NULL, verbose = TRUE) {
  # Handle alternative parameter name
  if (is.null(cohort_data) && !is.null(alk_data)) {
    cohort_data <- alk_data
  }
  # Check if mgcv is available
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package is required for cohort age-length modelling. Install with: install.packages('mgcv')")
  }

  # Validate input type first
  if (!is.data.frame(cohort_data)) {
    stop("alk_data must be a data frame")
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

  # Validate additional_terms
  if (!is.null(additional_terms)) {
    if (!is.character(additional_terms)) {
      stop("additional_terms must be a character vector")
    }
  }

  # Standardise sex categories to lowercase to avoid case sensitivity issues
  if ("sex" %in% names(cohort_data)) {
    cohort_data$sex <- tolower(cohort_data$sex)
  }

  # Validate the observed support before constructing the likelihood.
  if (!is.numeric(age_offset) || length(age_offset) != 1L ||
      !is.finite(age_offset) || age_offset < 0 || age_offset != floor(age_offset)) {
    stop("age_offset must be a finite non-negative integer.")
  }
  if (length(method) != 1L || is.na(method) || !method %in% c("REML", "ML")) {
    stop("method must be REML or ML for the conditional cohort likelihood.")
  }
  if (nrow(cohort_data) < 2L) stop("At least two aged observations are required.")
  for (variable in c("age", "length", "year")) {
    if (!is.numeric(cohort_data[[variable]]) || any(!is.finite(cohort_data[[variable]]))) {
      stop(variable, " must contain finite numeric values.")
    }
  }
  if (any(cohort_data$age < 1 | cohort_data$age != floor(cohort_data$age))) {
    stop("age must contain positive integers.")
  }
  if (any(cohort_data$year != floor(cohort_data$year))) stop("year must contain integers.")
  if (any(cohort_data$length <= 0)) stop("length must contain positive values.")
  if (by_sex && (anyNA(cohort_data$sex) || any(!nzchar(cohort_data$sex)))) {
    stop("sex must be observed for every aged fish.")
  }
  if (!is.null(weights) && (!is.numeric(weights) || length(weights) != nrow(cohort_data) ||
      any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0))) {
    stop("weights must be finite non-negative observation weights with a positive total.")
  }

  # Calculate cohorts: cohort = (year - age) - age_offset
  cohort_data$cohort <- (cohort_data$year - cohort_data$age) - age_offset

  # Convert cohort to ordered factor then integer for mgcv::ocat
  cohort_data$cohort <- as.ordered(cohort_data$cohort)
  cohort_levels <- levels(cohort_data$cohort)
  cohort_data$cohort <- as.integer(cohort_data$cohort)
  if (length(cohort_levels) < 2L) stop("At least two observed cohorts are required.")
  valid_last <- findInterval(cohort_data$year - age_offset - 1, as.numeric(cohort_levels))

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
  # For small datasets with few unique years, restrict k_year more aggressively
  if (k_length <= 0) k_length <- min(10, max(3, floor(n_unique_lengths / 3)))
  if (k_year <= 0) {
    if (n_unique_years <= 3) {
      k_year <- min(n_unique_years - 1, 2) # For test data with few years
    } else {
      k_year <- min(10, max(3, floor(n_unique_years / 2)))
    }
  }

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

    # Add additional terms with by = sex interaction
    if (!is.null(additional_terms)) {
      for (term in additional_terms) {
        # Check if term already has 'by =' specification
        if (grepl("by\\s*=", term)) {
          formula_parts <- c(formula_parts, term)
        } else {
          # Insert 'by = sex' before the closing parenthesis
          modified_term <- sub("\\)\\s*$", ", by = sex)", term)
          formula_parts <- c(formula_parts, modified_term)
        }
      }
    }

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

    # Add additional terms as-is
    if (!is.null(additional_terms)) {
      formula_parts <- c(formula_parts, additional_terms)
    }

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
        family = cohort_age_family(length(cohort_levels), valid_last),
        weights = weights,
        method = method,
        select = select,
        gamma = gamma,
        na.action = stats::na.fail
      )
    },
    error = function(e) {
      stop("Error fitting GAM model: ", e$message)
    }
  )

  if (!isTRUE(gam_model$converged) || any(!is.finite(stats::coef(gam_model))) ||
      (!is.null(gam_model$outer.info$conv) && gam_model$outer.info$conv != "full convergence")) {
    stop("Conditional cohort model did not converge to a finite solution.")
  }
  gam_model$cohorts <- as.numeric(cohort_levels)
  gam_model$age_offset <- age_offset
  gam_model$age_support <- list(minimum_age = 1L, conditional = TRUE)
  class(gam_model) <- c("cohort_gam", class(gam_model))

  if (verbose) {
    cat("Model fitted successfully!\n")
    cat("Deviance explained:", round(summary(gam_model)$dev.expl * 100, 1), "%\n")
  }

  # Store year range and unique training years for validation
  year_range <- range(cohort_data$year)
  training_years <- sort(unique(cohort_data$year))

  # Create cohort prediction function
  predict_cohort <- function(lengths, years, sex = NULL, ...) {
    # Capture additional arguments for spatial/temporal variables
    extra_args <- list(...)

    # Validate inputs
    if (!is.numeric(lengths) || !is.numeric(years)) {
      stop("lengths and years must be numeric")
    }

    if (length(lengths) != length(years)) {
      stop("lengths and years must have the same length")
    }
    if (!length(lengths) || any(!is.finite(lengths)) || any(lengths <= 0)) {
      stop("lengths must contain finite positive values.")
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

    # Add any additional variables from extra_args to newdata
    if (length(extra_args) > 0) {
      for (var_name in names(extra_args)) {
        newdata[[var_name]] <- extra_args[[var_name]]
      }
    }

    stats::predict(gam_model, newdata = newdata, type = "response")
  }

  # Create age back-calculation function
  predict_age <- function(lengths, sampling_years, sex = NULL, ...) {
    # Validate inputs
    if (!is.numeric(lengths) || !is.numeric(sampling_years)) {
      stop("lengths and sampling_years must be numeric")
    }

    # Handle case where sampling_years is a single value (as expected by tests)
    if (length(sampling_years) == 1 && length(lengths) > 1) {
      sampling_years <- rep(sampling_years, length(lengths))
    }

    if (length(lengths) != length(sampling_years)) {
      stop("lengths and sampling_years must have the same length")
    }

    # Get cohort probabilities for each length at each sampling year
    # Pass through any additional spatial/temporal arguments
    cohort_probs <- predict_cohort(lengths, sampling_years, sex, ...)

    # Map each admissible cohort probability to its positive integer age.
    cohort_years <- as.numeric(cohort_levels)
    maximum_age <- max(sampling_years - min(cohort_years) - age_offset)
    age_matrix <- matrix(0, length(lengths), maximum_age)
    for (i in seq_along(lengths)) {
      ages <- sampling_years[i] - cohort_years - age_offset
      valid <- ages >= 1
      age_matrix[i, ages[valid]] <- cohort_probs[i, valid]
    }
    colnames(age_matrix) <- paste0("age_", seq_len(ncol(age_matrix)))
    age_matrix
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
    model_summary = model_summary, # renamed from 'summary' to 'model_summary' to match tests
    deviance_explained = model_summary$deviance_explained * 100, # extract and convert to percentage
    by_sex = by_sex,
    cohorts = as.numeric(cohort_levels), # renamed from 'cohort_levels' to 'cohorts' to match tests
    sex_levels = sex_levels,
    year_range = year_range,
    training_years = training_years,
    age_offset = age_offset,
    age_support = gam_model$age_support,
    additional_terms = additional_terms
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
  cat("  Cohort levels:", paste(x$cohorts, collapse = ", "), "\n")
  cat("  Year range:", paste(x$year_range, collapse = " - "), "\n")
  cat("  Training years:", paste(x$training_years, collapse = ", "), "\n")
  cat("  Family: Ordered categorical conditional on positive integer ages\n\n")

  cat("Model fit:\n")
  cat("  Observations:", x$model_summary$n_observations, "\n")
  cat("  Deviance explained:", round(x$deviance_explained, 1), "%\n")
  # Safely print AIC if it exists and is numeric
  tryCatch(
    {
      if (!is.null(x$model_summary) && !is.null(x$model_summary$aic) && is.numeric(x$model_summary$aic)) {
        cat("  AIC:", round(x$model_summary$aic, 1), "\n")
      } else if (!is.null(x$summary) && !is.null(x$summary$aic) && is.numeric(x$summary$aic)) {
        cat("  AIC:", round(x$summary$aic, 1), "\n")
      }
    },
    error = function(e) {
      # Silently ignore errors with AIC formatting
    }
  )
  cat("  Effective df:", round(x$model_summary$edf, 1), "\n\n")

  cat("Functions available:\n")
  cat("  predict_cohort(lengths, years, sex) - Predict cohort probabilities\n")
  cat("  predict_age(lengths, sampling_years, sex) - Back-calculate age probabilities\n")
}
