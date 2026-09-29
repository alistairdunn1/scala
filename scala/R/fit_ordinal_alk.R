#' Fit an ordinal age-at-length model
#'
#' @description Fits a cumulative-logit model for positive integer age categories,
#'   with smooth effects of length and optionally year, sex and other covariates.
#' @param alk_data Data frame with one row per aged fish, positive integer
#'   \code{age}, finite positive \code{length}, and \code{sex} when
#'   \code{by_sex = TRUE}; integer \code{year} and other covariates when included
#'   in the model, or a named list of sex-specific data frames
#' @param by_sex Logical indicating sex-specific smooths and a sex main effect
#' @param k Alias for \code{k_length}; both must agree if supplied together
#' @param additional_terms Character vector of additional GAM terms; smooth terms
#'   receive \code{by = sex} when \code{by_sex = TRUE} unless a by variable is
#'   already specified; parametric terms are used as supplied
#' @param select Logical indicating extra penalties on smooth null spaces,
#'   allowing complete smooth terms to shrink towards zero
#' @param gamma Multiplier of effective degrees of freedom in the smoothing
#'   criterion, with larger values favouring smoother fits
#' @param method Smoothing parameter estimation method, either \code{"REML"}
#'   or \code{"ML"}
#' @param weights Optional finite non-negative observation likelihood weights,
#'   one per input row, with a positive total; \code{NULL} gives equal weights
#' @param verbose Logical indicating whether fitting details are printed
#' @param k_length Integer basis dimension for the length smooth; non-positive
#'   values select the automatic rule described in Details; \code{NULL} uses \code{k}
#' @param k_year Integer basis dimension for the year smooth; non-positive values
#'   select the automatic rule described in Details; \code{NULL} omits the
#'   built-in year smooth
#' @param plus_group Optional positive integer defining an inclusive upper-age
#'   category; \code{NULL} (the default) applies no plus group
#' @return An ordinal_alk list containing model, predict_function, predict_age,
#'   model_summary, deviance_explained (percentage), ages, age_levels, sex_levels,
#'   by_sex, additional_terms, k_length, k_year, year_range, training_years,
#'   age_support, response_type and plus_group
#' @details
#' The model estimates ordered age categories directly. Its response is age,
#' whereas fit_cohort_alk estimates cohort and converts cohort to age using the
#' sampling year. Both use a cumulative-logit link, sex-specific smooths when
#' requested, and the same meanings for select, gamma, method and weights.
#' Basis dimensions limit smooth complexity; smoothing parameters are estimated.
#' The automatic length basis dimension is min(10, max(3, floor(n_length / 3))),
#' where n_length is the number of distinct lengths. The automatic year dimension
#' is min(n_year - 1, 2) for at most three distinct years, and
#' min(10, max(3, floor(n_year / 2))) otherwise. The data must support the
#' requested smooths; mgcv may increase dimensions below its minimum.
#'
#' Supplying k_year adds a year smooth. With by_sex = TRUE the resulting predictor
#' is age ~ s(length, by = sex) + s(year, by = sex) + sex, plus additional terms.
#' A year smooth is optional for direct-age models. For a matched cohort comparison,
#' specify the same k_length, k_year, additional_terms and fitting settings.
#' fit_multinomial_alk uses the same age categories and prediction interfaces,
#' with age-specific log odds rather than shared ordinal cut-points.
#'
#' Ages must be positive integers and at least two distinct categories must remain
#' after grouping. Age categories are the distinct observed ages after grouping.
#' Missing intermediate ages are not estimated categories. The model assigns
#' probability only to these positive ages, in fitting and prediction.
#' Missing fitting covariates, failed convergence and invalid predictions stop
#' with an error. Observation weights are likelihood weights, not catch totals.
#'
#' With \code{plus_group = P}, ages at or above P are pooled as P before fitting.
#' The final category represents P and older, and its prediction is their combined
#' probability. Choose P for each analysis; no threshold is selected automatically.
#' For example, \code{plus_group = 50} represents 50+, but 50 is not a default.
#' The model does not estimate the distribution within the plus group. Treating
#' its label as an exact age yields a mean of capped ages, not the mean actual age.
#'
#' predict_function(lengths, sex = NULL, ...) returns one column per observed age,
#' named age_N. Supply year and other fitted covariates through named arguments.
#' predict_age(lengths, sampling_years = NULL, sex = NULL, ...) returns columns
#' age_1 through the maximum observed age, or through P when a plus group is
#' specified, with zero for unobserved categories.
#' A scalar sampling year is recycled; other prediction covariates must have
#' length one or match lengths. Sampling years are required when year is a fitted
#' covariate. Predictions outside sampled years retain the fitted model's
#' extrapolation assumptions. No age_offset applies to a direct-age response.
#'
#' Aged samples selected by length support conditional age probabilities when
#' sampling within modelled covariate strata is independent of age. Catch age
#' compositions require representative length data and appropriate catch expansion.
#' @examples
#' \dontrun{
#' direct <- fit_ordinal_alk(aged_fish, k_length = 10, k_year = 10,
#'   additional_terms = "te(long, lat, k = c(6, 6))")
#' probabilities <- direct$predict_age(c(100, 120), 2025, "female",
#'   long = c(180, 181), lat = c(-70, -71))
#' }
#' @seealso \code{\link{fit_cohort_alk}}, \code{\link{fit_multinomial_alk}}, \code{\link[mgcv]{gam}}
#' @importFrom mgcv gam s
#' @importFrom stats predict model.matrix AIC as.formula
#' @export
fit_ordinal_alk <- function(alk_data, by_sex = TRUE, k = -1, additional_terms = NULL,
                            select = TRUE, gamma = 1.4, method = "REML",
                            weights = NULL, verbose = TRUE,
                            k_length = NULL, k_year = NULL, plus_group = NULL) {
  fit_age_alk_model(alk_data = alk_data, by_sex = by_sex, k = k,
    additional_terms = additional_terms, select = select, gamma = gamma,
    method = method, weights = weights, verbose = verbose,
    k_length = k_length, k_year = k_year, model_type = "ordinal",
    k_supplied = !missing(k), plus_group = plus_group)
}
#' Print an ordinal age-at-length model
#' @param x An ordinal_alk object
#' @param ... Additional arguments
#' @export
print.ordinal_alk <- function(x, ...) {
  cat("Ordinal age-at-length model (cumulative logit)\n")
  print(stats::formula(x$model))
  cat("Age categories:", paste(x$ages, collapse = ", "), "\n")
  cat("Observations:", x$model_summary$n_observations, "\n")
  cat("AIC:", round(x$model_summary$aic, 1), "\n")
  cat("Effective degrees of freedom:", round(x$model_summary$edf, 1), "\n")
  invisible(x)
}
