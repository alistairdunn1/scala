#' Fit a multinomial age-at-length model
#'
#' @description Fits a penalised multinomial-logit model with age-specific smooth
#' relationships to length and optionally year, sex and other covariates.
#' @inheritParams fit_ordinal_alk
#' @param k_length Integer basis dimension for the length smooth; non-positive
#'   values select the automatic rule documented in \code{\link{fit_ordinal_alk}}
#' @param reference_age Observed age category after grouping used as the reference;
#'   \code{NULL} selects the age with the largest total observation weight,
#'   breaking ties by smaller age
#' @param method Smoothing parameter estimation method; \code{"REML"} is supported by the
#'   multinomial GAM backend and other methods stop with an error
#' @return A multinomial_alk list with model, predict_function, predict_age,
#'   model_summary, deviance_explained (percentage), ages, age_levels, sex_levels,
#'   by_sex, additional_terms, k_length, k_year, year_range, training_years,
#'   age_support, response_type, plus_group, reference_age and model_ages
#' @details
#' Each non-reference age has its own log-odds predictor relative to the reference
#' age. Predictors use the same requested terms, but their coefficients and
#' smoothing parameters are estimated separately. A softmax transformation gives
#' probabilities summing to one. The model does not impose shared ordinal
#' cut-points or a monotonic relationship between age and length.
#'
#' k_length and k_year have the same basis-dimension meanings as in
#' fit_ordinal_alk and fit_cohort_alk. Supplying k_year adds age-specific year
#' smooths; NULL omits the built-in year smooth. Automatic basis dimensions
#' follow the rules documented in \code{\link{fit_ordinal_alk}}. With
#' by_sex = TRUE, each non-reference age has sex-specific smooths
#' and a sex main effect. Smoothness is estimated using method; select adds
#' null-space penalties and gamma controls the smoothing criterion. Observation
#' weights are likelihood weights, not catch totals.
#' Smoothing parameters are estimated by outer BFGS optimisation of the REML
#' criterion, retaining the requested gamma and smooth-selection penalties.
#'
#' Additional smooth terms receive by = sex when requested; parametric terms
#' are used as supplied. For separate year effects, omit k_year and supply
#' factor(year), or sex:factor(year) for sex-specific annual effects, through
#' additional_terms. Length-year interactions can also be supplied explicitly.
#'
#' Ages must be positive integers. At least two observed categories must remain
#' after grouping, and each must have positive total weight. Age labels retain
#' their values, including gaps. predict_function(lengths, sex = NULL, ...) returns
#' observed-age columns in ascending age order. predict_age(lengths,
#' sampling_years = NULL, sex = NULL, ...) returns columns age_1 through the
#' maximum observed age, or through P when a plus group is specified, with zeros
#' for unobserved categories. Sampling years are required when year is a fitted
#' covariate. A scalar sampling year is recycled; other prediction covariates must
#' have length one or match lengths. No age_offset applies. Covariate validation
#' and missing-value handling are shared with fit_ordinal_alk. Invalid inputs, failed convergence
#' and invalid predictions stop with an error.
#'
#' With \code{plus_group = P}, ages at or above P are pooled as P before fitting.
#' The final category represents P and older, and its prediction is their combined
#' probability. Choose P for each analysis; no threshold is selected automatically.
#' For example, \code{plus_group = 50} represents 50+, but 50 is not a default.
#' The model does not estimate the distribution within the plus group. Treating
#' its label as an exact age yields a mean of capped ages, not the mean actual age.
#'
#' The reference age and model_ages record the mapping to mgcv categories, with
#' the reference first. Predictions from the returned helpers are reordered by
#' actual age. Smoothing penalties are applied to reference-category log odds,
#' so changing the reference can change a penalised fit. There is no smoothing
#' across age categories. Sparse ages require scrutiny, and the number of
#' coefficients grows with the number of ages. The mgcv multinomial backend can
#' be computationally demanding for many ages; ages are not grouped automatically.
#'
#' \strong{Sparse-category and convergence caution:} Category-specific predictors
#' estimate more parameters than a shared ordinal predictor. Sparse age categories,
#' limited coverage across years or areas, and covariates that nearly separate age
#' categories can produce weakly determined estimates or convergence failures.
#' An optional plus group can reduce the number of sparse older-age categories,
#' but does not guarantee convergence or reliable predictions. Choose its threshold
#' for each analysis and assess sensitivity to grouping and model complexity.
#' Check category counts, covariate coverage and held-out predictions. Numerical
#' convergence alone does not establish that age-specific effects are well estimated.
#' A fit that fails the convergence checks stops with an error.
#'
#' A traditional ALK estimates separate age mixtures within sampled length bins.
#' This model instead shares information through smooth length, year and spatial
#' relationships while allowing category-specific probabilities. Aged samples
#' selected by length support conditional age probabilities when selection within
#' the modelled covariate strata is independent of age. Catch compositions still
#' require representative length data and the appropriate catch expansion.
#' @examples
#' \dontrun{
#' model <- fit_multinomial_alk(aged_fish, k_length = 10, k_year = 10,
#'   additional_terms = "te(long, lat, k = c(6, 6))")
#' probability <- model$predict_age(c(100, 120), 2025, "female",
#'   long = c(180, 181), lat = c(-70, -71))
#' }
#' @seealso \code{\link{fit_ordinal_alk}}, \code{\link{fit_cohort_alk}},
#'   \code{\link[mgcv]{multinom}}
#' @export
fit_multinomial_alk <- function(alk_data, by_sex = TRUE, k_length = -1,
                                k_year = NULL, additional_terms = NULL,
                                select = TRUE, gamma = 1.4, method = "REML",
                                weights = NULL, verbose = TRUE, reference_age = NULL,
                                plus_group = NULL) {
  fit_age_alk_model(alk_data = alk_data, by_sex = by_sex,
    k_length = k_length, k_year = k_year, additional_terms = additional_terms,
    select = select, gamma = gamma, method = method, weights = weights,
    verbose = verbose, model_type = "multinomial", reference_age = reference_age,
    plus_group = plus_group)
}

#' Print a multinomial age-at-length model
#' @param x A multinomial_alk object
#' @param ... Additional arguments
#' @export
print.multinomial_alk <- function(x, ...) {
  cat("Multinomial age-at-length model (category-specific log odds)\n")
  cat("Reference age:", x$reference_age, "\n")
  cat("Age categories:", paste(x$ages, collapse = ", "), "\n")
  cat("Observations:", x$model_summary$n_observations, "\n")
  cat("AIC:", round(x$model_summary$aic, 1), "\n")
  cat("Effective degrees of freedom:", round(x$model_summary$edf, 1), "\n")
  invisible(x)
}
