#' Assign ages using a multinomial model
#'
#' @description Assigns ages through the shared \code{\link{assign_ages}}
#'   implementation, with model-class validation.
#' @inheritParams assign_ages
#' @param multinomial_model Fitted model from \code{\link{fit_multinomial_alk}}
#' @return Data frame returned by \code{\link{assign_ages}}
#' @details Assignment rules, probability validation, covariate handling and
#'   uncertainty interpretation follow \code{\link{assign_ages}}.
#' @seealso \code{\link{assign_ages}}, \code{\link{fit_multinomial_alk}}
#' @export
assign_ages_from_multinomial <- function(fish_data, multinomial_model,
    method = c("random", "mode", "expected"), predict_missing = FALSE, seed = NULL,
    keep_probabilities = FALSE, verbose = TRUE, ...) {
  if (!inherits(multinomial_model, "multinomial_alk")) {
    stop("multinomial_model must be a fitted multinomial_alk object from fit_multinomial_alk().")
  }
  assign_ages(fish_data, multinomial_model, method = method,
    predict_missing = predict_missing, seed = seed,
    keep_probabilities = keep_probabilities, verbose = verbose, ...)
}