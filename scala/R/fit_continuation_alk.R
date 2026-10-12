#' Fit a continuation-ratio age-at-length model
#'
#' @description Fits age-specific conditional logits and reconstructs a complete
#'   age distribution. Optional random-walk effects follow cohorts through time.
#' @param alk_data Data frame containing positive integer age, positive length,
#'   sex when by_sex is TRUE, and integer year for temporal effects; a named
#'   sex-specific list of data frames is also accepted
#' @param by_sex Whether conditional intercepts and smooths differ by sex
#' @param k_length Length basis dimension (at least three), or NULL for no length effect
#' @param k_year Year basis dimension (at least three), or NULL for no year smooth
#' @param cohort_rw Whether to include sex-specific age-year random effects with
#'   first-order random-walk penalties along cohorts
#' @param age_offset Integer offset defining cohort as year minus age minus offset
#' @param additional_terms Character vector of additional GAM terms, shared across
#'   age steps unless an explicit by = cr_group is supplied in a smooth
#' @param select Whether to apply additional smooth-selection penalties
#' @param gamma Positive smoothing criterion multiplier
#' @param method Smoothing estimation method, REML or ML for gam; fREML for bam
#' @param engine Fitting backend: gam or the discretised large-data bam backend
#' @param age_pooling Whether to pool smooth length/year responses across age
#'   steps using tensor smooths, and shrink sex-specific age intercept deviations
#' @param sex_pooling Whether age-step intercept deviations shrink towards a shared
#'   age baseline when age_pooling is TRUE; FALSE uses separate smooth age
#'   baselines and penalised age deviations within each sex
#' @param weights Non-negative observation weights, with positive weight at every age
#' @param plus_group Optional integer age at which older observations are pooled
#' @param verbose Whether to print the fitted formula
#' @return A continuation_alk object with model, predict_function, predict_age,
#'   predict_conditional, model_summary, age support and cohort-effect metadata
#' @details
#' At each modelled age a below the oldest category, the conditional probability
#' is q[a] = P(A = a | A >= a, covariates). A fish contributes a failure at every
#' younger step and a success at its observed age, unless it is in the last
#' category. The last category receives the remaining probability. The expanded
#' Bernoulli likelihood equals the individual categorical likelihood; rows are
#' not independent fish for resampling purposes. Resample original fish or sets.
#'
#' Intercepts and length/year smooths are specific to each age-sex step. Smooths
#' of the same covariate share a smoothing parameter across steps. This does not
#' impose a common regression curve or cumulative proportional odds.
#'
#' With cohort_rw = TRUE, deviations u[a,y,sex] have separate estimated penalties
#' for successive differences u[a+1,y+1,sex] - u[a,y,sex] and for the initial
#' deviation of each cohort. Thus initial cohort deviations are shrunk towards
#' zero, rather than treated as unrestricted fixed effects. The age-specific
#' intercept remains outside this penalty. These are conditional composition
#' effects, not survival, recruitment or mortality estimates. This option requires
#' consecutive observed ages, at least two years and at least two conditional
#' age steps (age_pooling permits gaps). It cannot be combined with k_year: these are alternative temporal
#' specifications. Predictions are restricted to the fitted year range, including
#' internal missing years; no random-walk forecasting is performed.
#'
#' With age_pooling = TRUE, age intercepts remain unrestricted but length/year
#' smooths borrow information across age steps. Sex deviations in age intercepts
#' are penalised towards zero; individual age-sex groups need not contain both
#' outcomes. Empty ages still receive zero probability. Cohort increments across
#' gaps between observed ages have variance proportional to the elapsed years.
#' With sex_pooling = FALSE, separate smooth age baselines and penalised age
#' deviations are estimated for each sex. These age deviations shrink towards
#' the same-sex baseline, not the other sex. Length/year surfaces remain separate
#' by sex under both specifications. Within-sex smoothing and shared-intercept
#' pooling therefore impose different assumptions on sparse age categories.
#' This is an explicit pooling specification, not a fallback for a failed fit.
#'
#' For static models, unobserved ages receive zero probability. A plus group
#' denotes capped ages; it is not interpreted as an exact age. Predictions retain
#' the age_N column convention used by assign_ages. predict_function returns
#' observed categories, whereas predict_age includes zero columns for gaps.
#' predict_conditional returns the conditional probabilities before conversion.
#' Coefficient uncertainty is not propagated by random assignment.
#'
#' The implementation uses an expanded binomial mgcv GAM. The bam backend
#' discretises covariates and constructs random effects as a factor smooth to
#' avoid storing a dense observation-by-cohort-cell matrix. It uses fast REML
#' and one numerical thread. The default gam backend uses a dense design.
#' Memory grows with fish, age steps and (for cohort_rw) age-year cells. It should
#' be evaluated on a representative subset before large fits. Failed fits and
#' invalid inputs stop with an error; missing categories are not imputed.
#' @examples
#' \dontrun{
#' fit <- fit_continuation_alk(aged_fish, k_length = 5, cohort_rw = TRUE)
#' p <- fit$predict_age(c(100, 120), sampling_years = 2025, sex = "female")
#' assigned <- assign_ages(length_data, fit, seed = 7)
#' }
#' @seealso \code{\link{fit_multinomial_alk}}, \code{\link{fit_cohort_alk}},
#'   \code{\link{assign_ages}}, \code{\link{calculate_age_compositions_from_model}}
#' @export
fit_continuation_alk <- function(alk_data, by_sex = TRUE, k_length = 5L,
                                 k_year = NULL, cohort_rw = FALSE, age_offset = 1L,
                                 additional_terms = NULL, select = TRUE,
                                 gamma = 1.4, method = "REML", weights = NULL,
                                 plus_group = NULL, verbose = TRUE, engine = c("gam", "bam"), age_pooling = FALSE, sex_pooling = TRUE) {
  engine <- match.arg(engine)
  if (engine == "bam" && missing(method)) method <- "fREML"
  for (flag in list(by_sex, cohort_rw, select, verbose, age_pooling, sex_pooling)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Flags must be TRUE or FALSE.")
  }
  if (is.list(alk_data) && !is.data.frame(alk_data)) {
    if (!length(alk_data) || is.null(names(alk_data)) || anyDuplicated(names(alk_data)) ||
        !all(names(alk_data) %in% c("male", "female", "unsexed"))) stop("Invalid sex-specific data list.")
    alk_data <- dplyr::bind_rows(alk_data, .id = "sex")
  }
  temporal <- cohort_rw || !is.null(k_year)
  required <- c("age", "length", if (by_sex) "sex", if (temporal) "year")
  if (!is.data.frame(alk_data) || !all(required %in% names(alk_data)) || nrow(alk_data) < 2L) {
    stop("alk_data must contain at least two fish and columns: ", paste(required, collapse = ", "))
  }
  if (any(grepl("^cr_", names(alk_data)))) stop("Input names beginning cr_ are reserved.")
  for (name in c("age", "length", if ("year" %in% names(alk_data)) "year")) {
    x <- alk_data[[name]]
    if (!is.numeric(x) || any(!is.finite(x))) stop(name, " must be finite numeric values.")
    if (name != "length" && any(x != floor(x))) stop(name, " must contain integers.")
  }
  if (any(alk_data$age < 1) || any(alk_data$length <= 0)) stop("Age and length must be positive.")
  validate_alk_plus_group(plus_group)
  if (!is.null(plus_group)) alk_data$age <- pmin(alk_data$age, plus_group)
  for (k in list(k_length, k_year)) if (!is.null(k) &&
      (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k < 3 || k != floor(k))) {
    stop("Basis dimensions must be NULL or integers of at least three.")
  }
  if (!is.numeric(age_offset) || length(age_offset) != 1L || !is.finite(age_offset) ||
      age_offset != floor(age_offset)) stop("age_offset must be one finite integer.")
  if (cohort_rw && !is.null(k_year)) stop("Choose cohort_rw or k_year, not both.")
  if (length(method) != 1L || is.na(method) || !method %in% if (engine == "gam") c("REML", "ML") else "fREML") stop("Use REML/ML for gam or fREML for bam.")
  if (!is.numeric(gamma) || length(gamma) != 1L || !is.finite(gamma) || gamma <= 0) stop("gamma must be positive.")
  if (is.null(weights)) weights <- rep(1, nrow(alk_data))
  if (!is.numeric(weights) || length(weights) != nrow(alk_data) || any(!is.finite(weights)) ||
      any(weights < 0) || !any(weights > 0)) stop("Invalid observation weights.")
  if (!is.null(additional_terms) && (!is.character(additional_terms) || anyNA(additional_terms) ||
      any(!nzchar(additional_terms)))) stop("additional_terms must be non-empty character terms.")
  ages <- sort(unique(alk_data$age))
  if (length(ages) < 2L) stop("At least two observed ages are required.")
  steps <- utils::head(ages, -1L)
  sex_levels <- if (by_sex) sort(unique(tolower(as.character(alk_data$sex)))) else "pooled"
  if (by_sex && (anyNA(alk_data$sex) || any(!nzchar(sex_levels)))) stop("Sex must be observed.")
  if (by_sex) alk_data$sex <- factor(tolower(as.character(alk_data$sex)), levels = sex_levels)
  years <- if (temporal) seq.int(min(alk_data$year), max(alk_data$year)) else NULL
  if (cohort_rw && (length(steps) < 2L || length(years) < 2L || (!age_pooling && any(diff(ages) != 1L)))) {
    stop("cohort_rw requires consecutive ages, at least two conditional steps and two years.")
  }
  group_levels <- as.vector(outer(steps, sex_levels, paste, sep = ":"))
  # Each original observation contributes only to its eligible conditional steps.
  parts <- lapply(steps, function(a) {
    i <- which(alk_data$age >= a & weights > 0)
    x <- alk_data[i, , drop = FALSE]
    x$cr_response <- as.integer(x$age == a)
    x$cr_step <- a
    x$cr_weight <- weights[i]
    x$cr_sex <- if (by_sex) as.character(x$sex) else "pooled"
    x$cr_group <- factor(paste(a, x$cr_sex, sep = ":"), levels = group_levels)
    x
  })
  expanded <- dplyr::bind_rows(parts)
  check_groups <- if (age_pooling) as.character(steps) else group_levels
  for (g in check_groups) {
    y <- expanded$cr_response[if (age_pooling) expanded$cr_step == as.numeric(g) else expanded$cr_group == g]
    if (!all(c(0, 1) %in% y)) stop("Conditional step ", g, " requires positive-weight fish both at and above that age.")
  }
  terms <- if (length(group_levels) == 1L) "1" else "0 + cr_group"
  if (!is.null(k_length)) terms <- c(terms, sprintf("s(length, by = cr_group, k = %d, id = 'cr_length')", k_length))
  if (!is.null(k_year)) terms <- c(terms, sprintf("s(year, by = cr_group, k = %d, id = 'cr_year')", k_year))
  if (length(group_levels) == 1L) terms <- gsub("by = cr_group, ", "", terms, fixed = TRUE)
  if (age_pooling) {
    terms <- if (length(steps) == 1L) "1" else "0 + cr_age"
    if (by_sex && length(sex_levels) > 1L) terms <- c(terms, "sex", "s(cr_group, bs = 're')")
    if ((!is.null(k_length) || !is.null(k_year)) && length(steps) < 3L) stop("Pooled smooths require at least three conditional steps.")
    by <- if (by_sex && length(sex_levels) > 1L) ", by = sex" else ""
    if (!sex_pooling && by_sex && length(sex_levels) > 1L) {
      if (length(steps) < 3L) stop("Within-sex age pooling requires three conditional steps.")
      terms <- c("sex", sprintf("s(cr_step, by = sex, k = %d)", min(10L, length(steps))),
        "s(cr_age, by = sex, bs = 're')")
    }
    if (!is.null(k_length)) terms <- c(terms, sprintf("te(length, cr_step, k = c(%d, %d)%s)", k_length, min(5L, length(steps)), by))
    if (!is.null(k_year)) terms <- c(terms, sprintf("te(year, cr_step, k = c(%d, %d)%s)", k_year, min(5L, length(steps)), by))
  }
  terms <- c(terms, additional_terms)
  external <- unique(unlist(lapply(additional_terms, function(x) all.vars(str2lang(x)))))
  if (any(grepl("^cr_", setdiff(external, "cr_group"))) || "age" %in% external) {
    stop("Additional terms cannot use response age or internal variables other than cr_group.")
  }
  variables <- unique(c("length", if (by_sex) "sex", if (temporal) "year", setdiff(external, "cr_group")))
  if (!all(variables %in% names(alk_data))) stop("Missing covariates: ", paste(setdiff(variables, names(alk_data)), collapse = ", "))
  temporal <- "year" %in% variables
  grid <- penalties <- NULL
  if (cohort_rw) {
    grid <- expand.grid(age = steps, year = years, sex = sex_levels, stringsAsFactors = FALSE)
    grid$cohort <- grid$year - grid$age - age_offset
    keys <- paste(grid$age, grid$year, grid$sex, sep = ":")
    preceding_age <- c(NA_real_, utils::head(steps, -1L))[match(grid$age, steps)]
    interval <- grid$age - preceding_age
    parent <- match(paste(preceding_age, grid$year - interval, grid$sex, sep = ":"), keys)
    linked <- which(!is.na(parent))
    difference <- matrix(0, length(linked), nrow(grid))
    difference[cbind(seq_along(linked), linked)] <- 1 / sqrt(interval[linked])
    difference[cbind(seq_along(linked), parent[linked])] <- -1 / sqrt(interval[linked])
    penalties <- list(increments = crossprod(difference), initial = diag(as.numeric(is.na(parent))))
    if (engine == "bam") {
      cr_rw_xt <- list(S = unname(penalties), rank = c(length(linked), sum(is.na(parent))))
      terms <- c(terms, "s(cr_cell, bs = 're', xt = cr_rw_xt)")
    } else terms <- c(terms, "cr_rw")
  }
  prepare <- function(x) {
    x$cr_age <- factor(x$cr_step, levels = steps)
    x$cr_group <- factor(paste(x$cr_step, if (by_sex) as.character(x$sex) else "pooled", sep = ":"), levels = group_levels)
    if (cohort_rw) {
      index <- match(paste(x$cr_step, x$year, if (by_sex) as.character(x$sex) else "pooled", sep = ":"), keys)
      if (anyNA(index)) stop("Cohort random-walk predictions require years within the fitted range.")
      if (engine == "bam") {
        x$cr_cell <- factor(keys[index], levels = keys)
      } else {
        z <- matrix(0, nrow(x), nrow(grid))
        z[cbind(seq_len(nrow(x)), index)] <- 1
        x$cr_rw <- I(z)
      }
    }
    x
  }
  expanded <- prepare(expanded)
  formula <- stats::as.formula(paste("cr_response ~", paste(terms, collapse = " + ")))
  if (verbose) print(formula)
  model <- if (engine == "bam") mgcv::bam(formula, data = expanded,
    family = stats::binomial(), weights = expanded$cr_weight, method = "fREML",
    select = select, gamma = gamma, discrete = TRUE, nthreads = 1L,
    drop.unused.levels = FALSE, na.action = stats::na.fail) else
    mgcv::gam(formula, data = expanded, family = stats::binomial(),
    weights = expanded$cr_weight, method = method, select = select, gamma = gamma,
    paraPen = if (cohort_rw) list(cr_rw = unname(penalties)) else NULL,
    na.action = stats::na.fail)
  if (!isTRUE(model$converged) || any(!is.finite(stats::coef(model))) ||
      (!is.null(model$outer.info$conv) && model$outer.info$conv != "full convergence")) {
    stop("Continuation-ratio fit did not converge to a finite solution.")
  }
  predict_conditional <- function(lengths, sampling_years = NULL, sex = NULL, ...) {
    if (!is.numeric(lengths) || !length(lengths) || any(!is.finite(lengths)) || any(lengths <= 0)) stop("lengths must be finite and positive.")
    n <- length(lengths)
    recycle <- function(x, label) {
      if (!length(x) %in% c(1L, n) || anyNA(x) || (is.numeric(x) && any(!is.finite(x)))) stop("Invalid prediction covariate: ", label)
      rep(x, length.out = n)
    }
    x <- data.frame(length = lengths)
    if (by_sex) {
      sx <- tolower(as.character(recycle(sex, "sex")))
      if (any(!sx %in% sex_levels)) stop("Unknown sex level.")
      x$sex <- factor(sx, levels = sex_levels)
    }
    extra <- list(...)
    if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))) || anyDuplicated(names(extra)))) stop("Prediction covariates must have unique names.")
    if (!is.null(sampling_years)) {
      if ("year" %in% names(extra)) stop("Supply sampling_years or year, not both.")
      extra$year <- sampling_years
    }
    if (any(!names(extra) %in% setdiff(variables, c("length", "sex")))) stop("Unused prediction covariates.")
    for (name in names(extra)) x[[name]] <- recycle(extra[[name]], name)
    if (length(setdiff(variables, names(x)))) stop("Missing prediction covariates: ", paste(setdiff(variables, names(x)), collapse = ", "))
    if (temporal && (!is.numeric(x$year) || any(x$year != floor(x$year)))) stop("Prediction years must be finite integers.")
    q <- vapply(steps, function(a) {
      z <- x; z$cr_step <- a
      as.numeric(stats::predict(model, newdata = prepare(z), type = "response", na.action = stats::na.fail))
    }, numeric(n))
    q <- matrix(q, nrow = n, ncol = length(steps), dimnames = list(NULL, paste0("age_", steps)))
    if (any(!is.finite(q)) || any(q < 0 | q > 1)) stop("Invalid conditional probabilities.")
    q
  }
  predict_function <- function(lengths, sex = NULL, ...) {
    q <- predict_conditional(lengths, sex = sex, ...)
    remaining <- rep(1, nrow(q))
    p <- matrix(0, nrow(q), length(ages), dimnames = list(NULL, paste0("age_", ages)))
    for (j in seq_along(steps)) {
      p[, j] <- remaining * q[, j]
      remaining <- remaining * (1 - q[, j])
    }
    p[, length(ages)] <- remaining
    if (any(!is.finite(p)) || any(p < 0) || any(abs(rowSums(p) - 1) > 1e-8)) stop("Invalid age probabilities.")
    p
  }
  predict_age <- function(lengths, sampling_years = NULL, sex = NULL, ...) {
    p <- predict_function(lengths, sex = sex, sampling_years = sampling_years, ...)
    maximum <- if (is.null(plus_group)) max(ages) else plus_group
    out <- matrix(0, nrow(p), maximum, dimnames = list(NULL, paste0("age_", seq_len(maximum))))
    out[, ages] <- p
    out
  }
  result <- list(model = model, predict_function = predict_function,
    predict_age = predict_age, predict_conditional = predict_conditional,
    prediction_variables = variables, by_sex = by_sex, sex_levels = if (by_sex) sex_levels else NULL,
    ages = ages, age_levels = ages, plus_group = plus_group, additional_terms = additional_terms,
    k_length = k_length, k_year = k_year, cohort_rw = cohort_rw, age_offset = age_offset,
    cohort_grid = grid, cohort_penalties = penalties, engine = engine, age_pooling = age_pooling, sex_pooling = sex_pooling,
    training_years = if (temporal) sort(unique(alk_data$year)) else NULL,
    year_range = if (temporal) range(alk_data$year) else NULL,
    age_support = list(minimum_age = 1L, observed_ages = ages), response_type = "continuation",
    model_summary = list(n_observations = nrow(alk_data), n_expanded = nrow(expanded),
      aic = stats::AIC(model), edf = sum(model$edf)))
  class(result) <- "continuation_alk"
  result
}

#' Print a continuation-ratio age-at-length model
#' @param x A continuation_alk object
#' @param ... Additional arguments
#' @export
print.continuation_alk <- function(x, ...) {
  cat("Continuation-ratio age-at-length model\n")
  cat("Age categories:", paste(x$ages, collapse = ", "), "\n")
  cat("Fish:", x$model_summary$n_observations, "; conditional rows:", x$model_summary$n_expanded, "\n")
  cat("Cohort random walk:", x$cohort_rw, "\n")
  invisible(x)
}
