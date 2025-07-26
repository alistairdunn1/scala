#' Get default length-weight parameters for Ross Sea toothfish
#'
#' Returns species-specific length-weight parameters for Ross Sea toothfish
#' (Dissostichus mawsoni) with sex-specific growth characteristics.
#'
#' @return List containing length-weight parameters for males, females, and unsexed fish.
#'   Each element is a named vector with components:
#'   \itemize{
#'     \item \code{a}: Scaling parameter (species and sex-specific)
#'     \item \code{b}: Exponent parameter (species and sex-specific)
#'   }
#'
#' @details
#' Default parameters are based on Ross Sea toothfish (Dissostichus mawsoni).
#' The length-weight relationship follows the allometric equation:
#' Weight (g) = a × Length(cm)^b
#'
#' Parameters are sex-specific to account for sexual dimorphism in growth:
#' \itemize{
#'   \item \strong{Males}: a = 1.247e-05, b = 2.990
#'   \item \strong{Females}: a = 7.361e-06, b = 3.105
#'   \item \strong{Unsexed}: a = 9.916e-06, b = 3.048 (averaged parameters)
#' }
#'
#' These parameters are provided for testing and demonstration purposes.
#' For real analyses, use species-specific parameters appropriate to your study.
#'
#' @note These are example parameters for Ross Sea toothfish. For other species,
#' obtain appropriate length-weight parameters from published literature or
#' species-specific studies.
#'
#' @seealso
#' \code{\link{get_abundance_lw_params}} for abundance-based parameters,
#' \code{\link{calculate_length_compositions}} for usage in scaling calculations
#'
#' @examples
#' # Get default parameters
#' lw_params <- get_default_lw_params()
#'
#' # Extract individual sex parameters
#' male_params <- lw_params$male
#' female_params <- lw_params$female
#'
#' # Use in length composition calculation
#' \dontrun{
#' results <- calculate_length_compositions_bootstrap(
#'   fish_data = your_fish_data,
#'   strata_data = your_strata_data,
#'   lw_params_male = lw_params$male,
#'   lw_params_female = lw_params$female,
#'   lw_params_unsexed = lw_params$unsexed,
#'   bootstraps = 300
#' )
#' }
#'
#' @export
get_default_lw_params <- function() {
  # Ross Sea toothfish parameters
  # Weight (g) = a * Length(cm)^b
  list(
    male = c(a = 1.247e-005, b = 2.990),
    female = c(a = 7.361e-006, b = 3.105),
    unsexed = c(a = 9.9155e-006, b = 3.0475)
  )
}
