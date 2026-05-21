#' @title Fit Ordinal Age-at-Weight Model using GAM
#'
#' @description Experimental: Fits an ordinal age-at-weight model using cumulative logit regression with smooth terms
#'   for otolith weight, optionally by sex. Returns a prediction function that can be used to predict
#'   age probabilities for given otolith weights.
#'
#' @param weight_age_data Data frame with columns: 'weight' (otolith weight), 'age', and optionally 'sex'.
#'   Each row represents one aged fish with its measured otolith weight.
#' @param by_sex Logical, whether to fit sex-specific smooth terms (default TRUE)
#' @param k Basis dimension for smooth terms (default -1 for automatic selection)
#' @param additional_terms Character vector of additional GAM formula terms to include in the model (default NULL).
#'   Each element should be a valid mgcv smooth term as a character string (e.g., "s(depth, bs = 'cr')").
#'   When by_sex = TRUE, these terms will automatically be fitted with 'by = sex' interactions.
#' @param select Logical, whether to add an extra penalty to each smooth term allowing
#'   terms to be penalized to zero (variable selection). Recommended for models with
#'   multiple smooth terms (default TRUE). See \code{\link[mgcv]{gam}} for details.
#' @param gamma Numeric multiplier for the effective degrees of freedom in the smoothing
#'   parameter selection criterion. Values > 1 (e.g., 1.4) produce smoother models and
#'   help guard against overfitting (default 1.4, following Wood 2006 recommendation).
#' @param method Smoothing parameter estimation method for mgcv (default "REML")
#' @param weights Optional weights for observations (default NULL)
#' @param verbose Logical, whether to print model fitting details (default TRUE)
#'
#' @return A list of class \code{"weight_age_model"} containing:
#'   \itemize{
#'     \item \code{model}: The fitted mgcv::gam model object
#'     \item \code{predict_function}: Function that takes weights (and optionally sex) and returns age probabilities
#'     \item \code{model_summary}: Model summary including deviance explained and significance tests
#'     \item \code{deviance_explained}: Deviance explained as a percentage
#'     \item \code{by_sex}: Logical indicating whether sex-specific terms were used
#'     \item \code{ages}: Vector of age levels in the model
#'     \item \code{sex_levels}: Vector of sex levels (if applicable)
#'     \item \code{weight_range}: Numeric vector of (min, max) otolith weight in training data
#'   }
#'
#' @details
#' The function fits an ordinal regression model using the cumulative logit link function,
#' which is appropriate for ordered age categories that typically increase with otolith weight.
#' The model structure is:
#'
#' \strong{Without sex effects:}
#' \code{age ~ s(weight)}
#'
#' \strong{With sex effects:}
#' \code{age ~ s(weight, by = sex) + sex}
#'
#' The cumulative logit model estimates the probability that age <= k for each age level k,
#' which naturally respects the ordinal nature of age data.
#'
#' The returned prediction function can be used directly with
#' \code{\link{assign_ages_from_weight}} to assign ages to fish based on their otolith weights.
#'
#' @examples
#' \dontrun{
#' # Generate test weight-age data
#' weight_age_data <- data.frame(
#'   age = rep(1:8, each = 50),
#'   weight = c(
#'     rnorm(50, 0.05, 0.01), rnorm(50, 0.10, 0.015),
#'     rnorm(50, 0.17, 0.02), rnorm(50, 0.25, 0.025),
#'     rnorm(50, 0.34, 0.03), rnorm(50, 0.44, 0.035),
#'     rnorm(50, 0.55, 0.04), rnorm(50, 0.67, 0.045)
#'   ),
#'   sex = rep(c("male", "female"), 200)
#' )
#'
#' # Fit ordinal age-at-weight model
#' wt_model <- fit_weight_age(weight_age_data, by_sex = TRUE, verbose = TRUE)
#'
#' # Use the prediction function
#' test_weights <- seq(0.05, 0.70, by = 0.05)
#' test_sex <- rep("male", length(test_weights))
#' age_probs <- wt_model$predict_function(test_weights, test_sex)
#' }
#'
#' @importFrom mgcv gam s
#' @importFrom stats predict AIC as.formula
#' @seealso \code{\link{assign_ages_from_weight}}, \code{\link{calculate_age_compositions_from_weight}},
#'   \code{\link{fit_ordinal_alk}}, \code{\link[mgcv]{gam}}
#' @export

fit_weight_age <- function(weight_age_data, by_sex = TRUE, k = -1, additional_terms = NULL,
                           select = TRUE, gamma = 1.4,
                           method = "REML", weights = NULL, verbose = TRUE) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package is required for ordinal age-at-weight modeling. Install with: install.packages('mgcv')")
  }

  if (!is.data.frame(weight_age_data)) {
    stop("weight_age_data must be a data frame")
  }

  required_cols <- c("age", "weight")
  if (!all(required_cols %in% names(weight_age_data))) {
    stop("weight_age_data must contain 'age' and 'weight' columns")
  }

  if (by_sex && !"sex" %in% names(weight_age_data)) {
    stop("by_sex = TRUE requires 'sex' column in weight_age_data")
  }

  if (!is.null(additional_terms) && !is.character(additional_terms)) {
    stop("additional_terms must be a character vector")
  }

  if ("sex" %in% names(weight_age_data)) {
    weight_age_data$sex <- tolower(weight_age_data$sex)
  }

  weight_age_data$age <- as.ordered(weight_age_data$age)
  age_levels <- levels(weight_age_data$age)
  weight_age_data$age <- as.integer(weight_age_data$age)

  if (verbose) {
    cat("Fitting ordinal age-at-weight model...\n")
    cat("Age levels:", paste(age_levels, collapse = ", "), "\n")
    cat("Weight range:", min(weight_age_data$weight), "to", max(weight_age_data$weight), "\n")
  }

  sex_levels <- NULL
  if (by_sex) {
    weight_age_data$sex <- as.factor(weight_age_data$sex)
    sex_levels <- levels(weight_age_data$sex)
    if (verbose) cat("Sex levels:", paste(sex_levels, collapse = ", "), "\n")
  }

  if (k <= 0) {
    n_unique_weights <- length(unique(weight_age_data$weight))
    k <- min(10, max(3, floor(n_unique_weights / 3)))
  }

  if (verbose) cat("Using k =", k, "for weight terms\n")

  if (by_sex) {
    formula_parts <- paste0("s(weight, by = sex, k = ", k, ")")

    if (!is.null(additional_terms)) {
      for (term in additional_terms) {
        if (grepl("by\\s*=", term)) {
          formula_parts <- c(formula_parts, term)
        } else {
          formula_parts <- c(formula_parts, sub("\\)\\s*$", ", by = sex)", term))
        }
      }
    }

    formula_parts <- c(formula_parts, "sex")
    formula <- as.formula(paste("age ~", paste(formula_parts, collapse = " + ")))
    if (verbose) cat("Model formula: age ~", paste(formula_parts, collapse = " + "), "\n")
  } else {
    formula_parts <- paste0("s(weight, k = ", k, ")")

    if (!is.null(additional_terms)) {
      formula_parts <- c(formula_parts, additional_terms)
    }

    formula <- as.formula(paste("age ~", paste(formula_parts, collapse = " + ")))
    if (verbose) cat("Model formula: age ~", paste(formula_parts, collapse = " + "), "\n")
  }

  if (verbose) cat("Fitting GAM with cumulative logit link...\n")

  tryCatch(
    {
      gam_model <- mgcv::gam(
        formula = formula,
        data = weight_age_data,
        family = mgcv::ocat(R = length(age_levels)),
        weights = weights,
        method = method,
        select = select,
        gamma = gamma,
        control = mgcv::gam.control(maxit = 200, trace = FALSE)
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

  predict_function <- function(weights, sex = NULL, ...) {
    extra_args <- list(...)

    if (!is.numeric(weights)) {
      stop("weights must be numeric")
    }

    if (by_sex && is.null(sex)) {
      stop("sex must be provided when model was fitted with by_sex = TRUE")
    }

    if (!by_sex && !is.null(sex)) {
      warning("sex provided but model was fitted with by_sex = FALSE. Ignoring sex.")
      sex <- NULL
    }

    if (by_sex) {
      if (length(sex) == 1) {
        sex <- rep(sex, length(weights))
      } else if (length(sex) != length(weights)) {
        stop("sex must be either length 1 or same length as weights")
      }

      if (!all(sex %in% sex_levels)) {
        stop("sex values must be in: ", paste(sex_levels, collapse = ", "))
      }

      newdata <- data.frame(
        weight = weights,
        sex = factor(sex, levels = sex_levels)
      )
    } else {
      newdata <- data.frame(weight = weights)
    }

    if (length(extra_args) > 0) {
      for (var_name in names(extra_args)) {
        newdata[[var_name]] <- extra_args[[var_name]]
      }
    }

    n_ages <- length(age_levels)
    n_obs <- nrow(newdata)

    if (n_ages == 1) {
      prob_matrix <- matrix(1, nrow = n_obs, ncol = 1)
      colnames(prob_matrix) <- paste0("age_", age_levels)
      return(prob_matrix)
    }

    prob_matrix <- NULL
    resp_ok <- FALSE
    resp_try <- try(stats::predict(gam_model, newdata = newdata, type = "response"), silent = TRUE)
    if (!inherits(resp_try, "try-error") && !is.null(resp_try)) {
      if (is.matrix(resp_try) && ncol(resp_try) %in% c(n_ages, n_ages - 1)) {
        if (ncol(resp_try) == n_ages) {
          prob_matrix <- resp_try
          resp_ok <- TRUE
        } else if (ncol(resp_try) == (n_ages - 1)) {
          cumprob <- resp_try
          prob_matrix <- matrix(NA_real_, nrow = n_obs, ncol = n_ages)
          prob_matrix[, 1] <- cumprob[, 1]
          if (n_ages > 2) {
            for (j in 2:(n_ages - 1)) prob_matrix[, j] <- cumprob[, j] - cumprob[, j - 1]
          }
          prob_matrix[, n_ages] <- 1 - cumprob[, n_ages - 1]
          resp_ok <- TRUE
        }
      } else if (is.vector(resp_try) && length(resp_try) == n_obs * n_ages) {
        prob_matrix <- matrix(resp_try, nrow = n_obs, ncol = n_ages)
        resp_ok <- TRUE
      }
    }

    if (!resp_ok) {
      pred_link <- stats::predict(gam_model, newdata = newdata, type = "link")

      if (is.matrix(pred_link)) {
        if (nrow(pred_link) == n_obs && ncol(pred_link) == (n_ages - 1)) {
          # as-is
        } else if (nrow(pred_link) == (n_ages - 1) && ncol(pred_link) == n_obs) {
          pred_link <- t(pred_link)
        } else if (length(as.numeric(pred_link)) == n_obs * (n_ages - 1)) {
          pred_link <- matrix(as.numeric(pred_link), nrow = n_obs, ncol = (n_ages - 1))
        } else {
          stop("Unexpected shape from predict(type='link') for ocat family")
        }
      } else {
        v <- as.numeric(pred_link)
        if (length(v) == n_obs * (n_ages - 1)) {
          pred_link <- matrix(v, nrow = n_obs, ncol = (n_ages - 1))
        } else {
          stop("Could not coerce prediction to matrix: weights and ages mismatch")
        }
      }

      cumprob <- stats::plogis(pred_link)
      prob_matrix <- matrix(NA_real_, nrow = n_obs, ncol = n_ages)
      prob_matrix[, 1] <- cumprob[, 1]
      if (n_ages > 2) {
        for (j in 2:(n_ages - 1)) prob_matrix[, j] <- cumprob[, j] - cumprob[, j - 1]
      }
      prob_matrix[, n_ages] <- 1 - cumprob[, n_ages - 1]
    }

    colnames(prob_matrix) <- paste0("age_", age_levels)
    prob_matrix[!is.finite(prob_matrix)] <- 0
    prob_matrix[prob_matrix < 0] <- 0
    rs <- rowSums(prob_matrix)
    keep <- rs > 0 & is.finite(rs)
    if (any(keep)) prob_matrix[keep, ] <- prob_matrix[keep, , drop = FALSE] / rs[keep]

    return(prob_matrix)
  }

  model_summary <- list(
    deviance_explained = summary(gam_model)$dev.expl,
    aic = AIC(gam_model),
    n_observations = nrow(weight_age_data),
    edf = sum(gam_model$edf),
    smooth_terms = summary(gam_model)$s.table
  )

  if (verbose) {
    cat("\nModel Summary:\n")
    cat("Observations:", model_summary$n_observations, "\n")
    cat("AIC:", round(model_summary$aic, 1), "\n")
    cat("Effective degrees of freedom:", round(model_summary$edf, 1), "\n")
  }

  ages_numeric <- as.numeric(age_levels)
  ages_numeric <- pmin(ages_numeric, 20)

  result <- list(
    model = gam_model,
    predict_function = predict_function,
    model_summary = model_summary,
    deviance_explained = model_summary$deviance_explained * 100,
    by_sex = by_sex,
    ages = ages_numeric,
    sex_levels = sex_levels,
    weight_range = range(weight_age_data$weight),
    additional_terms = additional_terms
  )

  class(result) <- "weight_age_model"
  return(result)
}

#' Print method for weight_age_model objects
#' @param x A weight_age_model object
#' @param ... Additional arguments (ignored)
#' @export
print.weight_age_model <- function(x, ...) {
  cat("Ordinal Age-at-Weight Model (GAM)\n")
  cat("==================================\n\n")

  cat("Model specification:\n")
  if (x$by_sex) {
    cat("  Formula: age ~ s(weight, by = sex) + sex\n")
    cat("  Sex levels:", paste(x$sex_levels, collapse = ", "), "\n")
  } else {
    cat("  Formula: age ~ s(weight)\n")
  }
  cat("  Age levels:", paste(x$ages, collapse = ", "), "\n")
  cat("  Weight range:", x$weight_range[1], "to", x$weight_range[2], "\n")
  cat("  Family: Ordered categorical (cumulative logit)\n\n")

  cat("Model fit:\n")
  cat("  Observations:", x$model_summary$n_observations, "\n")
  cat("  Deviance explained:", round(x$deviance_explained, 1), "%\n")
  cat("  AIC:", round(x$model_summary$aic, 1), "\n")
  cat("  Effective df:", round(x$model_summary$edf, 1), "\n\n")

  cat("Use predict_function(weights, sex) to generate age probabilities\n")
}
