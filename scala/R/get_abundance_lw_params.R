#' Get abundance-based length-weight parameters
#'
#' Returns length-weight parameters that effectively disable weight scaling,
#' providing abundance (count) calculations instead of weight-based scaling.
#'
#' @return List containing length-weight parameters for males, females, and unsexed fish.
#'   Each element is a named vector with components:
#'   \itemize{
#'     \item \code{a}: Scaling parameter (set to 1)
#'     \item \code{b}: Exponent parameter (set to 0)
#'   }
#'
#' @details
#' This function returns parameters where a=1 and b=0 for all sex categories,
#' which results in the length-weight relationship: Weight = 1 * Length^0 = 1.
#' This effectively provides a constant weight of 1 for all fish regardless of length,
#' making the scaling process abundance-based rather than biomass-based.
#'
#' This is particularly useful when:
#' \itemize{
#'   \item Working with abundance indices rather than biomass estimates
#'   \item Length-weight relationships are unknown or unreliable
#'   \item You want to scale by fish counts rather than estimated weights
#'   \item Conducting sensitivity analyses comparing abundance vs. biomass scaling
#' }
#'
#' @seealso \code{\link{get_default_lw_params}} for biomass-based length-weight parameters
#'
#' @examples
#' # Get abundance-based parameters
#' abundance_params <- get_abundance_lw_params()
#'
#' # Use in length composition calculation for abundance scaling
#' \dontrun{
#' results <- calculate_length_compositions_bootstrap(
#'   fish_data = your_fish_data,
#'   strata_data = your_strata_data,
#'   lw_params_male = abundance_params$male,
#'   lw_params_female = abundance_params$female,
#'   lw_params_unsexed = abundance_params$unsexed,
#'   bootstraps = 300
#' )
#' }
#'
#' @export
get_abundance_lw_params <- function() {
  # Parameters for abundance calculations (no weight scaling)
  # Weight = 1 * Length^0 = 1 (constant)
  list(
    male = c(a = 1, b = 0),
    female = c(a = 1, b = 0),
    unsexed = c(a = 1, b = 0)
  )
}
