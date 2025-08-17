test_that("calculate_age_compositions basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  # First get length compositions
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )
  
  # Create simple ALK
  simple_alk <- data.frame(
    length = rep(15:35, each = 5),
    age = rep(1:5, times = length(15:35)),
    proportion = rep(c(0.1, 0.2, 0.4, 0.2, 0.1), times = length(15:35))
  )
  
  # Calculate age compositions
  age_result <- calculate_age_compositions(
    x = lc_result,
    age_length_key = simple_alk,
    age_range = c(1, 5)
  )
  
  # Test return structure
  expect_s3_class(age_result, "age_composition")
  expect_type(age_result, "list")
  
  expected_names <- c("age_composition", "age_proportions", "ages", 
                     "strata_names", "n_bootstraps", "plus_group_age", 
                     "minus_group_age", "has_sex_data", "scaling_type", 
                     "age_length_key")
  expect_true(all(expected_names %in% names(age_result)))
  
  # Test composition column contains ages (not rescaled values)
  expect_equal(as.numeric(age_result$age_composition[, "composition", 1]), age_result$ages)
  expect_equal(as.numeric(age_result$age_proportions[, "composition", 1]), age_result$ages)
  
  # Test pooled proportions composition column
  expect_equal(as.numeric(age_result$pooled_age_proportions[, "composition"]), age_result$ages)
  
  # Check ages are in correct range
  expect_true(all(age_result$ages >= 1 & age_result$ages <= 5))
  
  # Check proportions sum correctly - the total column should sum to 1
  # Individual sex columns are proportions of the total population
  for (s in 1:dim(age_result$age_proportions)[3]) {
    total_prop_sum <- sum(age_result$age_proportions[, "total", s])
    expect_true(abs(total_prop_sum - 1) < 1e-6 || total_prop_sum == 0, 
                info = paste("Total proportions in stratum", s, "sum to", total_prop_sum))
    
    # Individual sex proportions should sum to the total proportion
    male_sum <- sum(age_result$age_proportions[, "male", s])
    female_sum <- sum(age_result$age_proportions[, "female", s])
    unsexed_sum <- sum(age_result$age_proportions[, "unsexed", s])
    combined_sum <- male_sum + female_sum + unsexed_sum
    
    expect_true(abs(combined_sum - total_prop_sum) < 1e-6,
                info = paste("Combined sex proportions", combined_sum, 
                            "should equal total", total_prop_sum, "in stratum", s))
  }
})

test_that("calculate_age_compositions with sex-specific ALK", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )
  
  # Create sex-specific ALK
  sex_specific_alk <- list(
    male = data.frame(
      length = rep(15:35, each = 3),
      age = rep(1:3, times = length(15:35)),
      proportion = rep(c(0.3, 0.5, 0.2), times = length(15:35))
    ),
    female = data.frame(
      length = rep(15:35, each = 4),
      age = rep(1:4, times = length(15:35)),
      proportion = rep(c(0.2, 0.3, 0.3, 0.2), times = length(15:35))
    ),
    unsexed = data.frame(
      length = rep(15:35, each = 3),
      age = rep(1:3, times = length(15:35)),
      proportion = rep(c(0.4, 0.4, 0.2), times = length(15:35))
    )
  )
  
  age_result <- calculate_age_compositions(
    x = lc_result,
    age_length_key = sex_specific_alk,
    age_range = c(1, 4)
  )
  
  expect_true(age_result$sex_specific_keys)
  expect_equal(age_result$ages, 1:4)
  
  # Check composition columns still contain ages
  expect_equal(as.numeric(age_result$age_composition[, "composition", 1]), age_result$ages)
  expect_equal(as.numeric(age_result$age_proportions[, "composition", 1]), age_result$ages)
})

test_that("calculate_age_compositions parameter validation", {
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )
  
  # Invalid ALK format
  expect_error(
    calculate_age_compositions(
      x = lc_result,
      age_length_key = "invalid",
      age_range = c(1, 5)
    )
  )
  
  # Missing required columns in ALK
  bad_alk <- data.frame(length = 15:35, bad_column = 1:21)
  expect_error(
    calculate_age_compositions(
      x = lc_result,
      age_length_key = bad_alk,
      age_range = c(1, 5)
    )
  )
})

test_that("calculate_age_compositions plus/minus groups for ages", {
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  lc_result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )
  
  simple_alk <- data.frame(
    length = rep(15:35, each = 5),
    age = rep(1:5, times = length(15:35)),
    proportion = rep(c(0.1, 0.2, 0.4, 0.2, 0.1), times = length(15:35))
  )
  
  # Test age plus group
  age_result_plus <- calculate_age_compositions(
    x = lc_result,
    age_length_key = simple_alk,
    age_range = c(1, 5),
    plus_group_age = TRUE
  )
  
  expect_true(age_result_plus$plus_group_age)
  
  # Test age minus group
  age_result_minus <- calculate_age_compositions(
    x = lc_result,
    age_length_key = simple_alk,
    age_range = c(1, 5),
    minus_group_age = TRUE
  )
  
  expect_true(age_result_minus$minus_group_age)
})
