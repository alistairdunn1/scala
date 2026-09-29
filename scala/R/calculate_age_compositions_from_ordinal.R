#' Calculate age compositions from an ordinal model
#'
#' @description Calculates age compositions through the shared model-assignment
#'   and bootstrap implementation.
#' @inheritParams calculate_age_compositions_from_model
#' @param ordinal_model Optional fitted model from \code{\link{fit_ordinal_alk}}
#' @return List returned by \code{\link{calculate_age_compositions_from_model}}
#' @details Point compositions use the supplied ages. Optional bootstrap assignment
#'   uses fixed fitted probabilities; coefficient uncertainty requires a separate
#'   refitting or parameter-draw procedure.
#' @seealso \code{\link{calculate_age_compositions_from_model}}, \code{\link{assign_ages}}
#' @export
calculate_age_compositions_from_ordinal <- function(fish_data, strata_data,
    age_range, lw_params_male, lw_params_female, lw_params_unsexed,
    bootstraps = 300, plus_group_age = TRUE, minus_group_age = FALSE,
    ordinal_model = NULL, verbose = TRUE) {
  if (!is.null(ordinal_model) && !inherits(ordinal_model, "ordinal_alk")) {
    stop("ordinal_model must be a fitted ordinal_alk object from fit_ordinal_alk().")
  }
  calculate_age_compositions_from_model(fish_data, strata_data, age_range,
    lw_params_male, lw_params_female, lw_params_unsexed,
    bootstraps = bootstraps, plus_group_age = plus_group_age,
    minus_group_age = minus_group_age, model = ordinal_model, verbose = verbose)
}
