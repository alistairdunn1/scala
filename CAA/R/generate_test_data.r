# Example usage and test data generation
generate_test_data <- function() {
  set.seed(123)
  
  # Generate test fish data with sex information
  n_samples <- 15
  fish_data <- data.frame()
  
  for (i in 1:n_samples) {
    stratum <- sample(c("North", "South"), 1)
    sample_id <- paste0("Sample_", i)
    
    # Generate length distribution (normal around 25cm)
    n_length_classes <- sample(5:8, 1)
    lengths <- sample(20:35, n_length_classes)
    
    sample_data <- data.frame(
      stratum = stratum,
      sample_id = sample_id,
      length = lengths,
      male = rpois(n_length_classes, lambda = 8),
      female = rpois(n_length_classes, lambda = 12),
      unsexed = rpois(n_length_classes, lambda = 3),
      sample_weight_kg = runif(1, 8, 18),
      total_catch_weight_kg = runif(1, 80, 250)
    )
    
    # Calculate total
    sample_data$total <- sample_data$male + sample_data$female + sample_data$unsexed
    
    fish_data <- rbind(fish_data, sample_data)
  }
  
  # Generate strata data
  strata_data <- data.frame(
    stratum = c("North", "South"),
    stratum_total_catch_kg = c(3000, 2500)
  )
  
  return(list(fish_data = fish_data, strata_data = strata_data))
}

# Example usage:
# test_data <- generate_test_data()
# results <- calculate_scaled_length_frequencies(
#   fish_data = test_data$fish_data,
#   strata_data = test_data$strata_data,
#   length_range = c(18, 38),
#   bootstraps = 100
# )
# print(results)