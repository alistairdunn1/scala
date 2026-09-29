#' Assign ages using an ordinal model
#'
#' @description Assigns ages through the shared \code{\link{assign_ages}}
#'   implementation, with model-class validation.
#' @inheritParams assign_ages
#' @param ordinal_model Fitted model from \code{\link{fit_ordinal_alk}}
#' @return Data frame returned by \code{\link{assign_ages}}
#' @details Assignment rules, probability validation, covariate handling and
#'   uncertainty interpretation follow \code{\link{assign_ages}}.
#' @seealso \code{\link{assign_ages}}, \code{\link{fit_ordinal_alk}}
#' @export
assign_ages_from_ordinal <- function(fish_data, ordinal_model,
    method = c("random", "mode", "expected"), predict_missing = FALSE, seed = NULL,
    keep_probabilities = FALSE, verbose = TRUE, ...) {
  if (!inherits(ordinal_model, "ordinal_alk")) {
    stop("ordinal_model must be a fitted ordinal_alk object from fit_ordinal_alk().")
  }
  assign_ages(fish_data, ordinal_model, method = method,
    predict_missing = predict_missing, seed = seed,
    keep_probabilities = keep_probabilities, verbose = verbose, ...)
}
