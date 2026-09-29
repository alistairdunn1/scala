#' Assign ages using a weight model
#'
#' @description Assigns ages through the shared \code{\link{assign_ages}}
#'   implementation, with model-class validation.
#' @inheritParams assign_ages
#' @param weight_age_model Fitted model from \code{\link{fit_weight_age}}
#' @return Data frame returned by \code{\link{assign_ages}}
#' @details Assignment rules, probability validation, covariate handling and
#'   uncertainty interpretation follow \code{\link{assign_ages}}.
#' @seealso \code{\link{assign_ages}}, \code{\link{fit_weight_age}}
#' @export
assign_ages_from_weight <- function(fish_data, weight_age_model,
    method = c("random", "mode", "expected"), seed = NULL,
    keep_probabilities = FALSE, verbose = TRUE, ...) {
  if (!inherits(weight_age_model, "weight_age_model")) {
    stop("weight_age_model must be a fitted weight_age_model object from fit_weight_age().")
  }
  assign_ages(fish_data, weight_age_model, method = method,
    seed = seed,
    keep_probabilities = keep_probabilities, verbose = verbose, ...)
}