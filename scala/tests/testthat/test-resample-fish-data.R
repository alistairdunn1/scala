test_that("resample_fish_data basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Test basic resampling
  resampled <- resample_fish_data(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    n_resamples = 1
  )
  
  expect_type(resampled, "list")
  expect_length(resampled, 1)
  
  # Check structure of resampled data
  resampled_data <- resampled[[1]]
  expect_s3_class(resampled_data, "data.frame")
  expect_true(all(names(test_data$fish_data) %in% names(resampled_data)))
  
  # Should have same or similar number of rows (with replacement)
  expect_true(nrow(resampled_data) > 0)
  
  # All strata should be represented
  original_strata <- unique(test_data$fish_data$stratum)
  resampled_strata <- unique(resampled_data$stratum)
  expect_true(all(original_strata %in% resampled_strata))
})

test_that("resample_fish_data multiple resamples", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Test multiple resamples
  n_resamples <- 5
  resampled <- resample_fish_data(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    n_resamples = n_resamples
  )
  
  expect_length(resampled, n_resamples)
  
  # Each resample should be different (usually)
  for (i in 1:n_resamples) {
    expect_s3_class(resampled[[i]], "data.frame")
    expect_true(nrow(resampled[[i]]) > 0)
  }
  
  # Test reproducibility with same seed
  set.seed(123)
  resampled2 <- resample_fish_data(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    n_resamples = n_resamples
  )
  
  expect_identical(resampled, resampled2)
})

test_that("resample_fish_data preserves data structure", {
  set.seed(123)
  test_data <- generate_test_data()
  
  resampled <- resample_fish_data(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    n_resamples = 3
  )
  
  for (i in 1:3) {
    resampled_data <- resampled[[i]]
    
    # Check column types are preserved
    expect_true(is.numeric(resampled_data$length))
    expect_true(is.numeric(resampled_data$male))
    expect_true(is.numeric(resampled_data$female))
    expect_true(is.numeric(resampled_data$unsexed))
    
    # Check values are reasonable
    expect_true(all(resampled_data$length > 0))
    expect_true(all(resampled_data$male >= 0))
    expect_true(all(resampled_data$female >= 0))
    expect_true(all(resampled_data$unsexed >= 0))
    
    # Check stratum values are valid
    original_strata <- unique(test_data$fish_data$stratum)
    expect_true(all(resampled_data$stratum %in% original_strata))
  }
})

test_that("resample_fish_data parameter validation", {
  test_data <- generate_test_data()
  
  # Invalid fish_data
  expect_error(
    resample_fish_data(
      fish_data = "invalid",
      strata_data = test_data$strata_data,
      n_resamples = 1
    ),
    "fish_data must be a data frame"
  )
  
  # Invalid strata_data
  expect_error(
    resample_fish_data(
      fish_data = test_data$fish_data,
      strata_data = "invalid",
      n_resamples = 1
    ),
    "strata_data must be a data frame"
  )
  
  # Invalid n_resamples
  expect_error(
    resample_fish_data(
      fish_data = test_data$fish_data,
      strata_data = test_data$strata_data,
      n_resamples = 0
    )
  )
  
  expect_error(
    resample_fish_data(
      fish_data = test_data$fish_data,
      strata_data = test_data$strata_data,
      n_resamples = -1
    )
  )
})

test_that("resample_fish_data with different data types", {
  # Test with survey data
  survey_data <- generate_survey_test_data()
  
  resampled_survey <- resample_fish_data(
    fish_data = survey_data$fish_data,
    strata_data = survey_data$strata_data,
    n_resamples = 2
  )
  
  expect_length(resampled_survey, 2)
  
  for (i in 1:2) {
    resampled_data <- resampled_survey[[i]]
    
    # Should have survey-specific columns
    expect_true("sample_area_km2" %in% names(resampled_data))
    expect_true("catch_density_kg_km2" %in% names(resampled_data))
    
    # Check values are reasonable
    expect_true(all(resampled_data$sample_area_km2 > 0))
    expect_true(all(resampled_data$catch_density_kg_km2 >= 0))
  }
  
  # Test with commercial data
  commercial_data <- generate_commercial_test_data()
  
  resampled_commercial <- resample_fish_data(
    fish_data = commercial_data$fish_data,
    strata_data = commercial_data$strata_data,
    n_resamples = 2
  )
  
  expect_length(resampled_commercial, 2)
  
  for (i in 1:2) {
    resampled_data <- resampled_commercial[[i]]
    
    # Should have commercial-specific columns
    expect_true("sample_weight_kg" %in% names(resampled_data))
    expect_true("total_catch_weight_kg" %in% names(resampled_data))
    
    # Check values are reasonable
    expect_true(all(resampled_data$sample_weight_kg > 0))
    expect_true(all(resampled_data$total_catch_weight_kg > 0))
  }
})
