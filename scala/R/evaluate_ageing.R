# Global variables for R CMD check
utils::globalVariables(c(
  "length", "age", "proportion", "recommended_ageing",
  "ageing_deficit", "priority", "current_aged", "composition",
  "male", "female", "unsexed", "total_composition", "composition_prop",
  "composition_prop_scaled", "sex_category", ".", "total_deficit",
  "in_priority_range", "high_abundance"
))

#' Evaluate Ageing Needs for Age-Length Keys
#'
#' Analyses length composition data to identify where additional age-length key data
#' is needed for robust age composition analysis. Provides recommendations for
#' targeted ageing efforts based on the importance of different length classes
#' in the population and current ageing coverage.
#'
#' @param length_comp_results A length_composition object from
#'   \code{\link{calculate_length_compositions}}
#' @param age_length_key Either a single data frame or a named list of sex-specific
#'   data frames containing age-length key data with columns: length, age, proportion.
#'   Each row should represent one age-length-sex combination.
#' @param target_fish_per_length Numeric, target number of fish to age per length class.
#'   If a single value, applied uniformly. If a function, applied to each length
#'   (default: 10, or a function prioritising higher abundance length classes if NULL)
#' @param min_fish_per_length Numeric, minimum number of fish that should be aged
#'   per length class (default: 3)
#' @param priority_length_range Numeric vector of length 2, specifying the length
#'   range that should receive highest ageing priority (default: middle 60% of
#'   length distribution)
#' @param sex_specific Logical, whether to analyse sex-specific ageing needs
#'   (default: TRUE if sex-specific keys provided)
#' @param create_plot Logical, whether to create diagnostic plots (default: TRUE)
#'
#' @return A list containing:
#'   \describe{
#'     \item{ageing_summary}{Data frame summarising ageing needs by length and sex}
#'     \item{priority_lengths}{Vector of length classes needing immediate attention}
#'     \item{recommendations}{Character vector with specific ageing recommendations}
#'     \item{coverage_summary}{Summary statistics of current ageing coverage}
#'     \item{diagnostic_plot}{ggplot object showing ageing gaps (if create_plot = TRUE)}
#'   }
#'
#' @details
#' This function evaluates the adequacy of age-length key coverage by:
#' \itemize{
#'   \item Analysing the length composition to identify important length classes
#'   \item Counting the number of age classes aged for each length (one row per age-length combination)
#'   \item Comparing current ageing effort with recommended targets
#'   \item Prioritising ageing needs based on length class importance and gaps
#'   \item Providing targeted recommendations for additional ageing
#' }
#'
#' **Target Ageing Strategy:**
#' \itemize{
#'   \item \strong{High priority}: Middle-aged and older fish (main reproductive classes)
#'   \item \strong{Medium priority}: Younger fish in significant numbers
#'   \item \strong{Lower priority}: Very small/large fish with low abundance
#' }
#'
#' The default target function prioritizes:
#' \itemize{
#'   \item 10 age classes for lengths in the priority range with high relative abundance (>50% of max)
#'   \item 8 age classes for lengths in the priority range (middle 60% of distribution)
#'   \item 7 age classes for lengths with high relative abundance (>50% of max composition)
#'   \item 5 age classes for lengths with moderate relative abundance (>20% of max composition)
#'   \item 3+ age classes minimum for all other length classes present
#' }
#'
#' **Note:** Composition proportions are rescaled so the maximum proportion becomes 1.0,
#' making priority decisions independent of the number of length bins in the analysis.
#'
#' @examples
#' \dontrun{
#' # Calculate length compositions
#' lc_results <- calculate_length_compositions(
#'   fish_data = my_fish_data,
#'   strata_data = my_strata_data,
#'   length_range = c(20, 60),
#'   bootstraps = 100
#' )
#'
#' # Load existing age-length key
#' my_alk <- read.csv("my_age_length_key.csv")
#'
#' # Evaluate ageing needs
#' ageing_diagnosis <- evaluate_ageing(
#'   length_comp_results = lc_results,
#'   age_length_key = my_alk
#' )
#'
#' # View recommendations
#' cat("Ageing Recommendations:\\n")
#' for (rec in ageing_diagnosis$recommendations) {
#'   cat("-", rec, "\\n")
#' }
#'
#' # View diagnostic plot
#' print(ageing_diagnosis$diagnostic_plot)
#'
#' # Get priority lengths for immediate attention
#' print(ageing_diagnosis$priority_lengths)
#'
#' # Custom target function emphasising larger fish
#' custom_target <- function(length, max_length) {
#'   ifelse(length > 0.7 * max_length, 30,
#'     ifelse(length > 0.5 * max_length, 20, 10)
#'   )
#' }
#'
#' ageing_diagnosis_custom <- evaluate_ageing(
#'   length_comp_results = lc_results,
#'   age_length_key = my_alk,
#'   target_fish_per_length = custom_target
#' )
#' }
#'
#' @seealso \code{\link{create_alk}} for creating complete age-length keys,
#'   \code{\link{calculate_length_compositions}} for length composition analysis
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_hline facet_wrap labs
#' @importFrom dplyr mutate case_when group_by summarise left_join arrange desc
#'   filter n slice ungroup
#' @export
evaluate_ageing <- function(length_comp_results,
                           age_length_key,
                           target_fish_per_length = 10,
                           min_fish_per_length = 3,
                           priority_length_range = NULL,
                           sex_specific = NULL,
                           create_plot = TRUE) {
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE) && create_plot) {
    warning("ggplot2 not available. Setting create_plot = FALSE")
    create_plot <- FALSE
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required for this function. Please install it with: install.packages('dplyr')")
  }

  # Validate inputs
  if (!inherits(length_comp_results, "length_composition")) {
    stop("length_comp_results must be a length_composition object from calculate_length_compositions()")
  }

  # Validate and standardise age_length_key format
  if (is.matrix(age_length_key)) {
    age_length_key <- as.data.frame(age_length_key)
  }

  if (is.data.frame(age_length_key)) {
    # Single data frame provided - no need to do anything
  } else if (is.list(age_length_key)) {
    # List of keys provided, ensure each is a data frame
    for (i in seq_along(age_length_key)) {
      if (is.matrix(age_length_key[[i]])) {
        age_length_key[[i]] <- as.data.frame(age_length_key[[i]])
      }
    }
  } else {
    # Convert other types to data frame
    age_length_key <- as.data.frame(age_length_key)
  }

  # Extract length composition data (pooled across strata)
  pooled_comp <- length_comp_results$pooled_length_composition
  lengths <- length_comp_results$lengths

  # Convert matrix to data frame for easier manipulation
  comp_data <- data.frame(
    length = lengths,
    composition = pooled_comp[, "composition"],
    male = pooled_comp[, "male"],
    female = pooled_comp[, "female"],
    unsexed = pooled_comp[, "unsexed"]
  )

  # Determine if we have sex-specific keys
  is_sex_specific <- is.list(age_length_key) &&
    any(c("male", "female", "unsexed") %in% names(age_length_key))

  if (is.null(sex_specific)) {
    sex_specific <- is_sex_specific
  }

  # Set priority length range if not provided (middle 60% of distribution)
  if (is.null(priority_length_range)) {
    length_20th <- quantile(lengths, 0.2, na.rm = TRUE)
    length_80th <- quantile(lengths, 0.8, na.rm = TRUE)
    priority_length_range <- c(length_20th, length_80th)
  }

  # Define default target function if not provided
  if (is.null(target_fish_per_length)) {
    target_fish_per_length <- function(length_val, composition_prop_scaled_val, max_length_val) {
      # Priority based on length position and rescaled abundance (0-1 scale)
      in_priority_range <- length_val >= priority_length_range[1] &
        length_val <= priority_length_range[2]
      high_abundance <- composition_prop_scaled_val > 0.5 # > 50% of maximum composition

      dplyr::case_when(
        in_priority_range & high_abundance ~ 10, # High priority + abundant
        in_priority_range ~ 8, # High priority range
        high_abundance ~ 7, # High abundance
        composition_prop_scaled_val > 0.2 ~ 5, # Moderate abundance (> 20% of max)
        TRUE ~ max(min_fish_per_length, 3) # Minimum for all others
      )
    }
  }

  # Calculate total composition by length for prioritisation
  grouped_comp <- dplyr::group_by(comp_data, length)
  comp_summary <- dplyr::summarise(grouped_comp,
    total_composition = sum(male + female + unsexed, na.rm = TRUE),
    .groups = "drop"
  )
  comp_summary <- dplyr::mutate(comp_summary,
    composition_prop = total_composition / sum(total_composition, na.rm = TRUE),
    # Rescale proportions so maximum becomes 1.0
    composition_prop_scaled = composition_prop / max(composition_prop, na.rm = TRUE)
  )

  # Function to analyse ageing coverage for a single key
  analyse_single_key <- function(key_data, key_name = "combined") {
    # Ensure key_data is a data frame
    if (!is.data.frame(key_data)) {
      key_data <- as.data.frame(key_data)
    }

    # Get current ageing coverage by length
    # Count number of age classes aged for each length (assuming one row per age-length combination)
    current_coverage <- key_data %>%
      dplyr::group_by(length) %>%
      dplyr::summarise(
        current_aged = dplyr::n(), # Number of age classes aged for this length
        .groups = "drop"
      )

    # Merge with composition data and calculate targets
    ageing_analysis <- comp_summary %>%
      dplyr::left_join(current_coverage, by = "length") %>%
      dplyr::mutate(
        current_aged = ifelse(is.na(current_aged), 0, current_aged)
      )

    # Calculate recommended ageing targets
    if (is.function(target_fish_per_length)) {
      # Apply function to each row using rescaled proportions
      ageing_analysis$recommended_ageing <- mapply(
        target_fish_per_length,
        ageing_analysis$length,
        ageing_analysis$composition_prop_scaled,
        max(ageing_analysis$length)
      )
    } else {
      ageing_analysis$recommended_ageing <- target_fish_per_length
    }

    # Complete the analysis
    ageing_analysis <- ageing_analysis %>%
      dplyr::mutate(
        ageing_deficit = pmax(0, recommended_ageing - current_aged),
        priority = dplyr::case_when(
          ageing_deficit >= 15 & composition_prop_scaled > 0.7 ~ "High",
          ageing_deficit >= 10 | (ageing_deficit >= 5 & composition_prop_scaled > 0.5) ~ "Medium",
          ageing_deficit > 0 ~ "Low",
          TRUE ~ "Adequate"
        ),
        sex_category = key_name
      )

    return(ageing_analysis)
  }

  # Analyse ageing needs
  if (sex_specific && is_sex_specific) {
    # Analyse each sex category separately
    ageing_analyses <- list()

    for (sex in c("male", "female", "unsexed")) {
      if (sex %in% names(age_length_key) && !is.null(age_length_key[[sex]])) {
        ageing_analyses[[sex]] <- analyse_single_key(age_length_key[[sex]], sex)
      }
    }

    # Combine all analyses
    ageing_summary <- do.call(rbind, ageing_analyses)
  } else {
    # Single combined analysis
    if (is.list(age_length_key) && !is.data.frame(age_length_key)) {
      # If list provided but sex_specific = FALSE, combine all keys
      combined_key <- do.call(rbind, age_length_key)
      # Remove any duplicate length-age combinations (keep first occurrence)
      combined_key <- combined_key %>%
        dplyr::group_by(length, age) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    } else {
      combined_key <- age_length_key
    }

    ageing_summary <- analyse_single_key(combined_key, "combined")
  }

  # Generate recommendations
  recommendations <- character(0)

  # Priority lengths needing immediate attention
  priority_lengths <- ageing_summary %>%
    dplyr::filter(priority %in% c("High", "Medium")) %>%
    dplyr::arrange(dplyr::desc(ageing_deficit * composition_prop)) %>%
    .$length

  # Calculate summary statistics
  total_deficit <- sum(ageing_summary$ageing_deficit, na.rm = TRUE)
  high_priority_deficit <- sum(ageing_summary$ageing_deficit[ageing_summary$priority == "High"], na.rm = TRUE)
  coverage_pct <- round(100 * sum(ageing_summary$current_aged > 0) / nrow(ageing_summary), 1)

  # Generate specific recommendations
  if (total_deficit == 0) {
    recommendations <- "Age-length key coverage appears adequate for all length classes."
  } else {
    recommendations <- c(
      sprintf(
        "Total additional ageing recommended: %d fish across %d length classes",
        total_deficit, sum(ageing_summary$ageing_deficit > 0)
      ),
      sprintf(
        "High priority ageing needs: %d fish for lengths with major composition gaps",
        high_priority_deficit
      )
    )

    # Length-specific recommendations
    if (length(priority_lengths) > 0) {
      priority_summary <- ageing_summary %>%
        dplyr::filter(length %in% priority_lengths[seq_len(min(5, length(priority_lengths)))]) %>%
        dplyr::arrange(dplyr::desc(ageing_deficit * composition_prop))

      recommendations <- c(
        recommendations,
        "Top priority lengths for additional ageing:",
        sprintf(
          "  Length %s: %d additional fish needed (%.1f%% of composition)",
          priority_summary$length,
          priority_summary$ageing_deficit,
          priority_summary$composition_prop * 100
        )
      )
    }

    # Strategic recommendations
    if (coverage_pct < 50) {
      recommendations <- c(
        recommendations,
        "Consider systematic ageing across the full length range - current coverage is limited"
      )
    }

    if (sex_specific) {
      sex_deficits <- ageing_summary %>%
        dplyr::group_by(sex_category) %>%
        dplyr::summarise(total_deficit = sum(ageing_deficit), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(total_deficit))

      if (max(sex_deficits$total_deficit) > 0) {
        recommendations <- c(
          recommendations,
          sprintf(
            "Focus ageing efforts on: %s",
            paste(sex_deficits$sex_category[sex_deficits$total_deficit > 0],
              collapse = ", "
            )
          )
        )
      }
    }
  }

  # Create diagnostic plot
  diagnostic_plot <- NULL
  if (create_plot) {
    # Prepare data for plotting
    plot_data <- ageing_summary %>%
      dplyr::mutate(
        priority = factor(priority, levels = c("Adequate", "Low", "Medium", "High"))
      )

    # Create plot
    diagnostic_plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = length, y = ageing_deficit, fill = priority)
    ) +
      ggplot2::geom_col(alpha = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "solid", colour = "black", linewidth = 0.5) +
      ggplot2::labs(x = "Length (cm)", y = "Additional Fish Needed for Ageing")

    # Add faceting for sex-specific analysis
    if (sex_specific && length(unique(ageing_summary$sex_category)) > 1) {
      diagnostic_plot <- diagnostic_plot +
        ggplot2::facet_wrap(~sex_category, scales = "fixed")
    }
  }

  # Coverage summary statistics
  coverage_summary <- list(
    total_length_classes = nrow(ageing_summary),
    length_classes_with_ageing = sum(ageing_summary$current_aged > 0),
    coverage_percentage = coverage_pct,
    total_fish_currently_aged = sum(ageing_summary$current_aged, na.rm = TRUE),
    total_additional_ageing_needed = total_deficit,
    high_priority_classes = sum(ageing_summary$priority == "High"),
    medium_priority_classes = sum(ageing_summary$priority == "Medium")
  )

  return(list(
    ageing_summary = ageing_summary,
    priority_lengths = priority_lengths,
    recommendations = recommendations,
    coverage_summary = coverage_summary,
    diagnostic_plot = diagnostic_plot
  ))
}
