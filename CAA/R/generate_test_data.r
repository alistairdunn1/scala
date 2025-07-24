# Example usage and test data generation
generate_test_data <- function() {
  set.seed(123)
  
  # Generate test fish data
  n_samples <- 20
  fish_data <- data.frame()
  
  for (i in 1:n_samples) {
    stratum <- sample(c("North", "South"), 1)
    sample_id <- paste0("Sample_", i)
    
    # Generate length distribution (normal around 25cm)
    n_fish <- sample(20:50, 1)
    lengths <- round(rnorm(n_fish, mean = 25, sd = 5))
    lengths <- pmax(10, pmin(40, lengths))  # Constrain to 10-40cm
    
    # Count fish by length
    length_counts <- table(lengths)
    
    sample_data <- data.frame(
      stratum = stratum,
      sample_id = sample_id,
      length = as.numeric(names(length_counts)),
      count = as.numeric(length_counts),
      sample_weight_kg = runif(1, 5, 15),
      total_catch_weight_kg = runif(1, 50, 200)
    )
    
    fish_data <- rbind(fish_data, sample_data)
  }
  
  # Generate strata data
  strata_data <- data.frame(
    stratum = c("North", "South"),
    stratum_total_catch_kg = c(2000, 1500)
  )
  
  return(list(fish_data = fish_data, strata_data = strata_data))
}
# Example usage:
# test_data <- generate_test_data()
# results <- calculate_scaled_length_frequencies(
#   fish_data = test_data$fish_data,
#   strata_data = test_data$strata_data,
#   length_range = c(15, 35),
#   bootstraps = 100
# )
# print(results)