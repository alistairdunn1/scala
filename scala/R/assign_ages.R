#' Assign ages using a fitted model or an age-length key
#'
#' @description Assigns one age to each fish from the probabilities supplied by a
#'   cohort, ordinal, multinomial or otolith-weight model, or a traditional
#'   age-length key. The same assignment rules are used for each probability source.
#' @param fish_data Data frame with one row per fish and the prediction covariates
#'   required by the model; other columns and row order are retained
#' @param model Fitted object from \code{fit_cohort_alk}, \code{fit_ordinal_alk},
#'   \code{fit_multinomial_alk} or \code{fit_weight_age}, or an age-length key from
#'   \code{create_alk}; a data frame with length, age and proportion columns or a
#'   named list of sex-specific key data frames is also accepted
#' @param method Assignment rule: \code{"random"} samples an age from its
#'   probabilities, \code{"mode"} selects the most probable age, and
#'   \code{"expected"} rounds the probability-weighted mean age
#' @param predict_missing Logical indicating whether a year-dependent model may
#'   predict years absent from its training data; FALSE assigns NA in those years
#' @param seed Optional non-negative integer random seed for random assignment
#' @param keep_probabilities Logical indicating whether to include age_prob_N columns
#' @param verbose Logical indicating whether assignment counts are printed
#' @param length_bin_size Optional positive bin width for traditional keys, using
#'   the binning convention of \code{create_alk}; NULL requires exact length matches
#' @param ... Additional named prediction covariates, each of length one or
#'   nrow(fish_data); these take precedence over corresponding fish_data columns
#' @return Data frame with original columns, an assigned age column replacing any
#'   existing age column, optional age_prob_N columns and a plus_group attribute
#'   when the fitted model has a plus group
#' @details
#' Length-based fitted models use their predict_age helper. A year column is
#' required for cohort models and for direct-age models containing a year effect.
#' Direct-age models without a year effect predict all rows, irrespective of year.
#' Sex is required for sex-specific models or keys. Additional fitted covariates
#' are read from fish_data or supplied through named arguments.
#'
#' With predict_missing = TRUE, prediction remains subject to the fitted model's
#' support: annual factors require fitted year levels, and cohort plus groups
#' require years at or after the first training year. Otolith-weight models use
#' otolith_weight and warn when weights lie outside the fitting range.
#' Missing otolith weights receive NA ages and probabilities; other non-finite
#' or non-positive primary prediction inputs stop with an error.
#'
#' Traditional keys are applied by exact length-bin and, where applicable, sex
#' matching. Supply length_bin_size if fish lengths require binning. Even integer
#' bin widths use floor((length - 1) / width) * width + 1; other widths use
#' floor(length / width) * width. No interpolation or extrapolation is performed
#' during assignment. Prepare those probabilities with create_alk. Keys have no
#' implicit year or area matching: apply the appropriate key to each data subset.
#'
#' Each row represents one fish. A count column is retained but does not expand a
#' row into multiple fish. Probabilities must be finite, non-negative and sum to
#' one on positive integer ages. Missing required covariates, unmatched key bins
#' or sexes and invalid probabilities stop with an error. Rows excluded by the
#' explicit predict_missing rule receive NA ages and NA probabilities. If no rows
#' are predicted, no probability columns are added.
#'
#' Mode ties select the youngest tied age. Expected ages use R's round function
#' and can lie in gaps between observed categories. Random assignment preserves
#' the fitted mixture in expectation; modal and expected assignments do not.
#' Random assignment represents conditional age-assignment variability, not
#' uncertainty in fitted coefficients or key proportions. The latter requires
#' refitting or drawing model or key parameters in the uncertainty analysis.
#'
#' A fitted plus group is retained: its label P represents ages P and older.
#' An expected age computed using this label is a mean of capped ages, not the
#' mean actual age. No plus-group threshold is selected during assignment.
#' A supplied seed sets the random-number generator state for random assignment.
#' @examples
#' key <- data.frame(length = c(100, 100, 120, 120),
#'   age = c(5, 10, 5, 10), proportion = c(0.8, 0.2, 0.3, 0.7))
#' fish <- data.frame(length = c(100, 120), sample_id = c(1, 2))
#' assign_ages(fish, key, method = "random", seed = 42, verbose = FALSE)
#' \dontrun{
#' model <- fit_multinomial_alk(aged_fish, k_length = 10, k_year = 10)
#' assigned <- assign_ages(length_data, model, seed = 42)
#' }
#' @seealso \code{\link{assign_ages_from_cohort}}, \code{\link{assign_ages_from_weight}},
#'   \code{\link{fit_cohort_alk}}, \code{\link{fit_ordinal_alk}},
#'   \code{\link{fit_multinomial_alk}}, \code{\link{create_alk}}
#' @export
assign_ages <- function(fish_data, model, method = c("random", "mode", "expected"),
                        predict_missing = FALSE, seed = NULL,
                        keep_probabilities = FALSE, verbose = TRUE,
                        length_bin_size = NULL, ...) {
  if (!is.data.frame(fish_data)) stop("fish_data must be a data frame.")
  method <- match.arg(method)
  for (flag in list(predict_missing, keep_probabilities, verbose)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Logical options must be TRUE or FALSE.")
  }
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
      seed < 0 || seed > .Machine$integer.max || seed != floor(seed))) {
    stop("seed must be NULL or one non-negative integer.")
  }
  fitted <- inherits(model, c("cohort_alk", "ordinal_alk", "multinomial_alk", "weight_age_model"))
  if (!fitted && !is.data.frame(model) && !is.list(model)) stop("Unsupported age model or age-length key.")
  if (!is.null(length_bin_size) && (fitted || !is.numeric(length_bin_size) ||
      length(length_bin_size) != 1L || !is.finite(length_bin_size) || length_bin_size <= 0)) {
    stop("length_bin_size must be a positive number and is only used with traditional keys.")
  }
  extra <- list(...)
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))) ||
      anyDuplicated(names(extra)) || any(names(extra) %in% c("length", "lengths", "year", "years",
        "sampling_years", "sex", "weights", "otolith_weight")))) {
    stop("Additional covariates must have unique names and cannot replace primary prediction inputs.")
  }
  n <- nrow(fish_data)
  for (name in names(extra)) {
    if (!length(extra[[name]]) %in% c(1L, n)) stop(name, " must have length one or nrow(fish_data).")
  }
  result <- fish_data
  result$age <- rep(NA_real_, n)
  attr(result, "plus_group") <- if (fitted) model$plus_group else NULL
  if (!n) return(result)
  rows <- seq_len(n)
  if (fitted) {
    is_weight <- inherits(model, "weight_age_model")
    primary <- if (is_weight) "otolith_weight" else "length"
    if (!primary %in% names(fish_data)) stop("fish_data must contain ", primary, ".")
    values <- fish_data[[primary]]
    observed <- if (is_weight) !is.na(values) else rep(TRUE, n)
    if (!is.numeric(values) || any(!is.finite(values[observed])) || any(values[observed] <= 0)) {
      stop(primary, " must contain finite positive values.")
    }
    if (is_weight) rows <- which(observed)
    variables <- character(0)
    if (!is.null(model$model$formula)) {
      formulas <- model$model$formula
      if (inherits(formulas, "formula")) formulas <- list(formulas)
      variables <- unique(unlist(lapply(formulas, function(f) {
        all.vars(stats::delete.response(stats::terms(f)))
      })))
    }
    if (length(model$additional_terms)) variables <- union(variables,
      unique(unlist(lapply(model$additional_terms, function(term) all.vars(str2lang(term))))))
    year_used <- !is_weight && (inherits(model, "cohort_alk") || "year" %in% variables ||
      length(model$training_years) > 0)
    if (year_used) {
      years <- fish_data$year
      if (!is.numeric(years) || length(years) != n || any(!is.finite(years)) || any(years != floor(years))) {
        stop("fish_data must contain finite integer year values.")
      }
      if (!predict_missing) {
        if (!length(model$training_years)) stop("The year-dependent model has no training_years metadata.")
        rows <- which(years %in% model$training_years)
      }
    }
    if (isTRUE(model$by_sex)) {
      if (!"sex" %in% names(fish_data) || anyNA(fish_data$sex)) stop("Sex-specific models require an observed 'sex' column.")
    }
    covariates <- setdiff(variables, c("length", "year", "sex", "weight", "otolith_weight"))
    unknown <- setdiff(names(extra), covariates)
    if (length(unknown)) stop("Unused prediction covariates: ", paste(unknown, collapse = ", "))
    for (name in covariates) {
      if (!name %in% names(extra)) {
        if (!name %in% names(fish_data)) stop("Missing prediction covariate: ", name)
        extra[[name]] <- fish_data[[name]]
      }
    }
    if (!length(rows)) return(result)
    args <- if (is_weight) list(weights = values[rows]) else list(lengths = values[rows])
    if (year_used) args$sampling_years <- years[rows]
    if (isTRUE(model$by_sex)) args$sex <- tolower(as.character(fish_data$sex[rows]))
    for (name in names(extra)) {
      value <- extra[[name]]
      if (anyNA(value) || (is.numeric(value) && any(!is.finite(value)))) {
        stop(name, " must contain observed finite prediction values.")
      }
      args[[name]] <- if (length(value) == n) value[rows] else rep(value, length(rows))
    }
    if (is_weight && length(model$weight_range) == 2L &&
        any(values[rows] < model$weight_range[1] | values[rows] > model$weight_range[2])) {
      warning("Otolith weights outside the model's training range require extrapolation.")
    }
    predictor <- if (is_weight) model$predict_function else model$predict_age
    if (!is.function(predictor)) stop("The fitted model has no supported prediction function.")
    probability <- do.call(predictor, args)
  } else {
    if (length(extra)) stop("Additional prediction covariates are not used with a traditional key.")
    probability <- alk_assignment_probabilities(fish_data, model, length_bin_size)
  }
  if (!is.matrix(probability) || !is.numeric(probability) ||
      nrow(probability) != length(rows) || !ncol(probability) ||
      is.null(colnames(probability)) || any(!grepl("^age_[0-9]+$", colnames(probability)))) {
    stop("Age predictions must be a numeric matrix with one row per fish and age_N columns.")
  }
  ages <- as.numeric(sub("^age_", "", colnames(probability)))
  if (anyDuplicated(ages) || any(!is.finite(ages)) || any(ages < 1) ||
      any(!is.finite(probability)) || any(probability < 0) || any(abs(rowSums(probability) - 1) > 1e-8)) {
    stop("Age probabilities must be finite, non-negative and normalised on unique positive integer ages.")
  }
  order <- order(ages)
  ages <- ages[order]
  probability <- probability[, order, drop = FALSE]
  if (method == "mode") result$age[rows] <- ages[apply(probability, 1L, which.max)]
  if (method == "expected") result$age[rows] <- round(as.vector(probability %*% ages))
  if (method == "random") {
    if (!is.null(seed)) set.seed(seed)
    result$age[rows] <- vapply(seq_len(nrow(probability)), function(i) {
      if (length(ages) == 1L) ages else ages[sample.int(length(ages), 1L, prob = probability[i, ])]
    }, numeric(1))
  }
  if (keep_probabilities) {
    names <- paste0("age_prob_", ages)
    if (any(names %in% names(result))) stop("fish_data already contains age probability output columns.")
    for (j in seq_along(ages)) {
      result[[names[j]]] <- rep(NA_real_, n)
      result[[names[j]]][rows] <- probability[, j]
    }
  }
  if (verbose) cat("Assigned ages to", length(rows), "of", n, "fish using", method, "assignment.\n")
  result
}

# Read an existing key without constructing probabilities for unmatched bins.
alk_assignment_probabilities <- function(fish_data, key, length_bin_size) {
  lengths <- fish_data$length
  if (!is.numeric(lengths) || length(lengths) != nrow(fish_data) ||
      any(!is.finite(lengths)) || any(lengths <= 0)) stop("fish_data must contain finite positive length values.")
  if (!is.null(length_bin_size)) {
    offset <- if (length_bin_size %% 2 == 0) 1 else 0
    lengths <- floor((lengths - offset) / length_bin_size) * length_bin_size + offset
  }
  columns <- c("length", "age", "proportion")
  if (all(columns %in% names(key)) && all(vapply(key[columns], is.numeric, logical(1)))) {
    keys <- list(combined = as.data.frame(unclass(key)))
    sexes <- rep("combined", length(lengths))
  } else {
    if (!is.list(key) || !length(key) || is.null(names(key)) || any(!nzchar(names(key))) ||
        anyDuplicated(tolower(names(key))) || !all(vapply(key, is.data.frame, logical(1)))) {
      stop("A key must have length, age and proportion columns or named sex-specific key data frames.")
    }
    keys <- key
    names(keys) <- tolower(names(keys))
    if (!"sex" %in% names(fish_data) || anyNA(fish_data$sex)) stop("Sex-specific keys require observed sex values.")
    sexes <- tolower(as.character(fish_data$sex))
    if (any(!sexes %in% names(keys))) stop("No age-length key is available for a requested sex.")
  }
  for (part in keys) {
    if (!all(columns %in% names(part)) || !all(vapply(part[columns], is.numeric, logical(1))) ||
        !nrow(part) || any(!is.finite(as.matrix(part[columns]))) ||
        any(part$age < 1 | part$age != floor(part$age)) || any(part$proportion < 0) ||
        anyDuplicated(part[c("length", "age")])) stop("Invalid or duplicated age-length key entries.")
    totals <- tapply(part$proportion, part$length, sum)
    if (any(abs(totals - 1) > 1e-8)) stop("Age-length key probabilities must sum to one within each length bin.")
  }
  ages <- sort(unique(unlist(lapply(keys, function(part) part$age))))
  probability <- matrix(0, length(lengths), length(ages), dimnames = list(NULL, paste0("age_", ages)))
  for (sex in unique(sexes)) {
    part <- keys[[sex]]
    rows <- which(sexes == sex)
    bins <- unique(part$length)
    index <- match(lengths[rows], bins)
    if (anyNA(index)) stop("No age-length key entry for one or more fish length bins; supply the matching length_bin_size or prepare a complete key.")
    table <- matrix(0, length(bins), length(ages))
    table[cbind(match(part$length, bins), match(part$age, ages))] <- part$proportion
    probability[rows, ] <- table[index, , drop = FALSE]
  }
  probability
}
