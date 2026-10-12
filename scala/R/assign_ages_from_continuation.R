#' Assign ages using a continuation-ratio model
#'
#' @description Assigns ages through the shared \code{\link{assign_ages}}
#'   implementation, with model-class validation.
#' @inheritParams assign_ages
#' @param continuation_model Fitted model from \code{\link{fit_continuation_alk}}
#' @return Data frame returned by \code{\link{assign_ages}}
#' @details Assignment rules, probability validation, covariate handling and
#'   uncertainty interpretation follow \code{\link{assign_ages}}.
#' @seealso \code{\link{assign_ages}}, \code{\link{fit_continuation_alk}}
#' @export
assign_ages_from_continuation <- function(fish_data, continuation_model,
    method = c("random", "mode", "expected"), predict_missing = FALSE, seed = NULL,
    keep_probabilities = FALSE, verbose = TRUE, ...) {
  if (!inherits(continuation_model, "continuation_alk")) {
    stop("continuation_model must be a fitted continuation_alk object from fit_continuation_alk().")
  }
  assign_ages(fish_data, continuation_model, method = method,
    predict_missing = predict_missing, seed = seed,
    keep_probabilities = keep_probabilities, verbose = verbose, ...)
}