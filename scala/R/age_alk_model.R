fit_age_alk_model <- function(alk_data, by_sex = TRUE, k = -1, additional_terms = NULL,
                            select = TRUE, gamma = 1.4, method = "REML",
                            weights = NULL, verbose = TRUE,
                            k_length = NULL, k_year = NULL, model_type = "ordinal", reference_age = NULL, k_supplied = FALSE,
                            plus_group = NULL) {
  if (!requireNamespace("mgcv", quietly = TRUE)) stop("The mgcv package is required.")
  if (is.list(alk_data) && !is.data.frame(alk_data)) {
    if (!length(alk_data) || is.null(names(alk_data)) ||
        !all(names(alk_data) %in% c("male", "female", "unsexed"))) {
      stop("alk_data must be a data frame or a named sex-specific list.")
    }
    alk_data <- dplyr::bind_rows(alk_data, .id = "sex")
  }
  if (!is.data.frame(alk_data)) stop("alk_data must be a data frame.")
  if (!is.logical(by_sex) || length(by_sex) != 1L || is.na(by_sex)) {
    stop("by_sex must be TRUE or FALSE.")
  }
  required <- c("age", "length", if (by_sex) "sex", if (!is.null(k_year)) "year")
  if (!all(required %in% names(alk_data))) {
    stop("alk_data must contain: ", paste(required, collapse = ", "))
  }
  if (nrow(alk_data) < 2L) stop("At least two aged observations are required.")
  for (variable in c("age", "length", if ("year" %in% names(alk_data)) "year")) {
    if (!is.numeric(alk_data[[variable]]) || any(!is.finite(alk_data[[variable]]))) {
      stop(variable, " must contain finite numeric values.")
    }
  }
  if (any(alk_data$age < 1 | alk_data$age != floor(alk_data$age))) {
    stop("age must contain positive integers.")
  }
  validate_alk_plus_group(plus_group)
  if (!is.null(plus_group)) alk_data$age <- pmin(alk_data$age, plus_group)
  if (any(alk_data$length <= 0)) stop("length must contain positive values.")
  if ("year" %in% names(alk_data) && any(alk_data$year != floor(alk_data$year))) {
    stop("year must contain integers.")
  }
  if (length(method) != 1L || is.na(method) || !method %in% c("REML", "ML")) {
    stop("method must be REML or ML.")
  }
  if (model_type == "multinomial" && method != "REML") {
    stop("The multinomial GAM backend supports method = 'REML'; ML is not supported.")
  }
  if (!is.logical(select) || length(select) != 1L || is.na(select)) stop("select must be TRUE or FALSE.")
  if (!is.numeric(gamma) || length(gamma) != 1L || !is.finite(gamma) || gamma <= 0) {
    stop("gamma must be a finite positive number.")
  }
  if (!is.null(weights) && (!is.numeric(weights) || length(weights) != nrow(alk_data) ||
      any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0))) {
    stop("weights must be finite non-negative observation weights with a positive total.")
  }
  if (!is.null(additional_terms) && (!is.character(additional_terms) ||
      anyNA(additional_terms) || any(!nzchar(additional_terms)))) {
    stop("additional_terms must be a character vector of non-empty terms.")
  }
  if (!is.null(k_length) && k_supplied && !isTRUE(all.equal(k, k_length))) {
    stop("k and k_length specify the same setting and must agree when both are supplied.")
  }
  if (is.null(k_length)) k_length <- k
  for (value in list(k_length, k_year)) {
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value != floor(value))) stop("Basis dimensions must be finite integers.")
  }
  if (k_length <= 0) k_length <- min(10, max(3, floor(length(unique(alk_data$length)) / 3)))
  if (!is.null(k_year) && k_year <= 0) {
    n_years <- length(unique(alk_data$year))
    k_year <- if (n_years <= 3) min(n_years - 1, 2) else min(10, max(3, floor(n_years / 2)))
  }
  ages <- sort(unique(alk_data$age))
  if (length(ages) < 2L) stop("At least two observed ages are required.")
  model_ages <- ages
  if (model_type == "multinomial") {
    class_weights <- vapply(ages, function(age) {
      if (is.null(weights)) sum(alk_data$age == age) else sum(weights[alk_data$age == age])
    }, numeric(1))
    if (any(class_weights <= 0)) stop("Every modelled age must have positive total observation weight.")
    if (is.null(reference_age)) reference_age <- ages[which.max(class_weights)]
    if (!is.numeric(reference_age) || length(reference_age) != 1L ||
        !is.finite(reference_age) || !reference_age %in% ages) {
      stop("reference_age must be one observed positive integer age.")
    }
    model_ages <- c(reference_age, setdiff(ages, reference_age))
    alk_data$age <- match(alk_data$age, model_ages) - 1L
  } else {
    alk_data$age <- match(alk_data$age, ages)
  }
  sex_levels <- NULL
  if ("sex" %in% names(alk_data)) alk_data$sex <- tolower(alk_data$sex)
  if (by_sex) {
    if (anyNA(alk_data$sex) || any(!nzchar(alk_data$sex))) stop("sex must be observed for every aged fish.")
    alk_data$sex <- factor(alk_data$sex)
    sex_levels <- levels(alk_data$sex)
  }
  by_term <- if (by_sex) ", by = sex" else ""
  terms <- sprintf("s(length%s, k = %d)", by_term, k_length)
  if (!is.null(k_year)) terms <- c(terms, sprintf("s(year%s, k = %d)", by_term, k_year))
  for (term in additional_terms) {
    term <- alk_sex_term(term, by_sex)
    terms <- c(terms, term)
  }
  if (by_sex) terms <- c(terms, "sex")
  formula <- stats::as.formula(paste("age ~", paste(terms, collapse = " + ")))
  if (model_type == "multinomial") {
    formula <- c(list(formula), rep(list(stats::as.formula(
      paste("~", paste(terms, collapse = " + ")))), length(ages) - 2L))
    family <- age_multinomial_family(length(ages) - 1L)
  } else {
    family <- mgcv::ocat(R = length(ages))
  }
  if (verbose) print(formula)
  if (model_type == "ordinal" && length(ages) == 2L) {
    base_dd <- family$Dd
    # Supply the binary logistic derivatives when there are no free cut-points.
    family$Dd <- function(y, mu, theta, wt = NULL, level = 0) {
      if (is.null(wt)) wt <- rep(1, length(y))
      out <- base_dd(y, mu, theta, wt, level)
      f <- stats::plogis(-1 - mu)
      q <- stats::plogis(mu + 1)
      if (level > 0) out$Dmu3 <- -2 * wt * f * q * (1 - 2 * f)
      if (level > 1) out$Dmu4 <- 2 * wt * f * q * (1 - 6 * f + 6 * f^2)
      out
    }
  }
  gam_model <- mgcv::gam(formula, data = alk_data, family = family,
    weights = weights, method = method, select = select, gamma = gamma,
    optimizer = if (model_type == "multinomial") c("outer", "bfgs") else c("outer", "newton"),
    na.action = stats::na.fail)
  if (model_type == "multinomial" && length(gam_model$xlevels) &&
      all(vapply(gam_model$xlevels, is.list, logical(1)))) {
    # All category predictors use the same terms and fitted factor levels.
    gam_model$xlevels <- gam_model$xlevels[[1L]]
  }
  inner_ok <- if (model_type == "multinomial") length(gam_model$warn) == 0L else isTRUE(gam_model$converged)
  if (!inner_ok || any(!is.finite(stats::coef(gam_model))) ||
      (!is.null(gam_model$outer.info$conv) && gam_model$outer.info$conv != "full convergence")) {
    stop(model_type, " age model did not converge to a finite solution.")
  }
  prediction_variables <- unique(unlist(lapply(if (is.list(formula)) formula else list(formula),
    function(f) all.vars(stats::delete.response(stats::terms(f))))))
  predict_function <- function(lengths, sex = NULL, ...) {
    if (!is.numeric(lengths) || !length(lengths) || any(!is.finite(lengths)) || any(lengths <= 0)) {
      stop("lengths must contain finite positive values.")
    }
    n <- length(lengths)
    recycle <- function(value, name) {
      if (!length(value) %in% c(1L, n) || anyNA(value) ||
          (is.numeric(value) && any(!is.finite(value)))) {
        stop(name, " must contain observed values of length one or the same length as lengths.")
      }
      rep(value, length.out = n)
    }
    newdata <- data.frame(length = lengths)
    if (by_sex) {
      if (is.null(sex)) stop("sex must be provided when model was fitted with by_sex = TRUE.")
      sex <- tolower(recycle(sex, "sex"))
      if (any(!sex %in% sex_levels)) stop("sex values must be in: ", paste(sex_levels, collapse = ", "))
      newdata$sex <- factor(sex, levels = sex_levels)
    } else if (!is.null(sex)) {
      warning("sex provided but model was fitted with by_sex = FALSE. Ignoring sex.")
    }
    extra <- list(...)
    if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))) ||
        anyDuplicated(names(extra)) || any(names(extra) %in% c("length", "sex")))) {
      stop("Additional prediction covariates must have unique names and cannot replace length or sex.")
    }
    for (name in names(extra)) newdata[[name]] <- recycle(extra[[name]], name)
    absent <- setdiff(prediction_variables, names(newdata))
    if (length(absent)) stop("Missing prediction covariates: ", paste(absent, collapse = ", "))
    if ("year" %in% names(newdata) && (!is.numeric(newdata$year) ||
        any(newdata$year != floor(newdata$year)))) stop("year must contain finite integers.")
    eta <- stats::predict(gam_model, newdata = newdata, type = "link", na.action = stats::na.fail)
    if (model_type == "multinomial") {
      if (length(ages) == 2L) eta <- matrix(as.numeric(eta), ncol = 1L)
      if (!is.matrix(eta) || !identical(dim(eta), c(n, length(ages) - 1L)) || any(!is.finite(eta))) {
        stop("Multinomial predictions must contain finite category-specific linear predictors.")
      }
      logits <- cbind(0, eta)
      mass <- exp(logits - apply(logits, 1L, max))
      probability <- (mass / rowSums(mass))[, match(ages, model_ages), drop = FALSE]
    } else {
      probability <- cohort_probabilities(as.numeric(eta), gam_model$family$getTheta(TRUE), rep(length(ages), n))
    }
    if (any(!is.finite(probability)) || any(probability < 0) ||
        any(abs(rowSums(probability) - 1) > 1e-8)) stop("Age probabilities are not finite and normalised.")
    colnames(probability) <- paste0("age_", ages)
    probability
  }
  predict_age <- function(lengths, sampling_years = NULL, sex = NULL, ...) {
    extra <- list(...)
    if (!is.null(sampling_years)) {
      if ("year" %in% names(extra)) stop("Supply sampling_years or year, not both.")
      extra$year <- sampling_years
    }
    p <- do.call(predict_function, c(list(lengths = lengths, sex = sex), extra))
    maximum_age <- if (is.null(plus_group)) max(ages) else plus_group
    output <- matrix(0, nrow(p), maximum_age,
      dimnames = list(NULL, paste0("age_", seq_len(maximum_age))))
    output[, ages] <- p
    output
  }
  model_summary <- list(deviance_explained = summary(gam_model)$dev.expl,
    aic = stats::AIC(gam_model), n_observations = nrow(alk_data),
    edf = sum(gam_model$edf), smooth_terms = summary(gam_model)$s.table)
  year_used <- "year" %in% prediction_variables
  result <- list(model = gam_model, predict_function = predict_function, predict_age = predict_age,
    model_summary = model_summary, deviance_explained = model_summary$deviance_explained * 100,
    by_sex = by_sex, ages = ages, age_levels = ages, sex_levels = sex_levels,
    additional_terms = additional_terms, k_length = k_length, k_year = k_year,
    year_range = if (year_used) range(alk_data$year) else NULL,
    training_years = if (year_used) sort(unique(alk_data$year)) else NULL,
    age_support = list(minimum_age = 1L, observed_ages = ages),
    response_type = model_type, plus_group = plus_group)
  if (model_type == "multinomial") {
    result$reference_age <- reference_age
    result$model_ages <- model_ages
  }
  class(result) <- paste0(model_type, "_alk")
  result
}

