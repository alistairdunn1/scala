test_that("calculate_length_compositions basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )
  
  # Test return structure
  expect_s3_class(result, "length_composition")
  expect_type(result, "list")
  
  expected_names <- c("length_compositions", "lengths", "strata_names", 
                     "n_bootstraps", "plus_group", "minus_group", 
                     "has_sex_data", "scaling_type")
  expect_true(all(expected_names %in% names(result)))
  
  # Test array dimensions and structure
  expect_true(is.array(result$length_compositions))
  expect_equal(length(dim(result$length_compositions)), 3)
  
  # Check composition column contains lengths
  expect_equal(as.numeric(result$length_compositions[, "composition", 1]), result$lengths)
  
  # Check lengths are in correct range
  expect_true(all(result$lengths >= 15 & result$lengths <= 35))
  
  # Check no negative values
  expect_true(all(result$length_compositions >= 0))
})

test_that("calculate_length_compositions with bootstraps", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  result <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 10
  )
  
  # Test bootstrap results are included
  bootstrap_names <- c("lc_cvs", "lc_ci_lower", "lc_ci_upper", 
                      "pooled_length_composition", "pooled_proportions")
  expect_true(all(bootstrap_names %in% names(result)))
  
  # Test CVs are reasonable (between 0 and some reasonable upper bound)
  if (!all(is.na(result$lc_cvs))) {
    expect_true(all(result$lc_cvs >= 0, na.rm = TRUE))
    expect_true(all(result$lc_cvs <= 10, na.rm = TRUE)) # CVs shouldn't be crazy high
  }
})

test_that("calculate_length_compositions parameter validation", {
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  # Missing length-weight parameters
  expect_error(
    calculate_length_compositions(
      fish_data = test_data$fish_data,
      strata_data = test_data$strata_data,
      length_range = c(15, 35)
    ),
    "Length-weight parameters are required"
  )
  
  # Invalid length range
  expect_error(
    calculate_length_compositions(
      fish_data = test_data$fish_data,
      strata_data = test_data$strata_data,
      length_range = c(35, 15), # Wrong order
      lw_params_male = lw_params$male,
      lw_params_female = lw_params$female,
      lw_params_unsexed = lw_params$unsexed
    )
  )
})

test_that("calculate_length_compositions plus/minus groups", {
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()
  
  # Test plus group
  result_plus <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    plus_group = TRUE,
    bootstraps = 0
  )
  
  expect_true(result_plus$plus_group)
  
  # Test minus group
  result_minus <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    minus_group = TRUE,
    bootstraps = 0
  )
  
  expect_true(result_minus$minus_group)
})
