test_that("resample_fish_data basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()

  # Test basic resampling
  resampled <- resample_fish_data(test_data$fish_data)

  expect_s3_class(resampled, "data.frame")
  expect_true(all(names(test_data$fish_data) %in% names(resampled)))

  # Should have same or similar number of rows (with replacement)
  expect_true(nrow(resampled) > 0)

  # All strata should be represented
  original_strata <- unique(test_data$fish_data$stratum)
  resampled_strata <- unique(resampled$stratum)
  expect_true(all(original_strata %in% resampled_strata))

  # Total fish count should be similar (Poisson resampling preserves expected total)
  original_total <- sum(test_data$fish_data$total)
  resampled_total <- sum(resampled$total)
  expect_true(abs(resampled_total - original_total) / original_total < 0.5) # Within 50% (reasonable for Poisson)
})

test_that("resample_fish_data multiple resamples", {
  set.seed(123)
  test_data <- generate_test_data()

  # Test multiple resamples by calling function multiple times
  n_resamples <- 5
  resampled_list <- vector("list", n_resamples)

  for (i in 1:n_resamples) {
    resampled_list[[i]] <- resample_fish_data(test_data$fish_data)
  }

  # Each resample should be a data frame
  for (i in 1:n_resamples) {
    expect_s3_class(resampled_list[[i]], "data.frame")
    expect_true(nrow(resampled_list[[i]]) > 0)
  }

  # Test reproducibility with same seed
  set.seed(123)
  resampled1 <- resample_fish_data(test_data$fish_data)
  set.seed(123)
  resampled2 <- resample_fish_data(test_data$fish_data)

  expect_identical(resampled1, resampled2)
})

test_that("resample_fish_data preserves data structure", {
  set.seed(123)
  test_data <- generate_test_data()

  # Test multiple resamples
  resampled_list <- vector("list", 3)
  for (i in 1:3) {
    resampled_list[[i]] <- resample_fish_data(test_data$fish_data)
  }

  for (i in 1:3) {
    resampled_data <- resampled_list[[i]]

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

  # Invalid fish_data - should be data frame
  expect_error(
    resample_fish_data("invalid"),
    "fish_data must be a data frame"
  )

  # Empty data frame
  expect_error(
    resample_fish_data(data.frame()),
    "fish_data must contain required columns"
  )

  # Missing required columns
  invalid_data <- data.frame(length = 1:10, male = 1:10)
  expect_error(
    resample_fish_data(invalid_data),
    "fish_data must contain required columns"
  )
})

test_that("resample_fish_data with different data types", {
  # Test with survey data (if function exists)
  # Note: This test may need to be updated based on available test data functions
  test_data <- generate_test_data()

  # Test that function works with the standard test data
  resampled <- resample_fish_data(test_data$fish_data)

  expect_s3_class(resampled, "data.frame")
  expect_true(nrow(resampled) > 0)

  # Should preserve all original columns
  expect_true(all(names(test_data$fish_data) %in% names(resampled)))
})
