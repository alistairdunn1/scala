test_that("create_alk basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  age_data <- generate_test_age_data()
  
  # Create basic ALK
  alk <- create_alk(
    age_data = age_data,
    lengths = 15:35,
    ages = 1:10,
    verbose = FALSE
  )
  
  # Test return structure
  expect_s3_class(alk, "age_length_key")
  expect_type(alk, "list")
  
  expected_names <- c("male", "female", "unsexed")
  expect_true(all(expected_names %in% names(alk)))
  
  # Test each sex-specific ALK
  for (sex in names(alk)) {
    sex_alk <- alk[[sex]]
    expect_s3_class(sex_alk, "data.frame")
    expect_true(all(c("length", "age", "proportion") %in% names(sex_alk)))
    
    # Check proportions sum to 1 for each length
    length_sums <- aggregate(sex_alk$proportion, by = list(sex_alk$length), sum)
    expect_true(all(abs(length_sums$x - 1) < 1e-10))
    
    # Check all proportions are non-negative
    expect_true(all(sex_alk$proportion >= 0))
    
    # Check lengths and ages are in expected ranges
    expect_true(all(sex_alk$length %in% 15:35))
    expect_true(all(sex_alk$age %in% 1:10))
  }
})

test_that("create_alk with custom parameters", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Test with custom tail ages
  tail_ages <- list(c(15, 1), c(35, 10))
  
  alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    tail_ages = tail_ages,
    interpolate = TRUE,
    extrapolate = TRUE,
    verbose = FALSE
  )
  
  expect_s3_class(alk, "age_length_key")
  
  # Check tail ages are applied - length 15 should have some proportion for age 1
  # and length 35 should have some proportion for age 10
  for (sex in names(alk)) {
    sex_alk <- alk[[sex]]
    length_15_data <- sex_alk[sex_alk$length == 15, ]
    length_35_data <- sex_alk[sex_alk$length == 35, ]
    
    # Should have entries for these lengths
    expect_true(nrow(length_15_data) > 0)
    expect_true(nrow(length_35_data) > 0)
  }
})

test_that("create_alk without interpolation/extrapolation", {
  set.seed(123)
  test_data <- generate_test_data()
  
  alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    interpolate = FALSE,
    extrapolate = FALSE,
    verbose = FALSE
  )
  
  expect_s3_class(alk, "age_length_key")
  
  # Should only have data for lengths that exist in the original data
  original_lengths <- unique(generate_test_age_data()$length)
  
  for (sex in names(alk)) {
    sex_alk <- alk[[sex]]
    # All ALK lengths should be in the original data (within the specified range)
    alk_lengths <- unique(sex_alk$length)
    expect_true(all(alk_lengths %in% original_lengths))
  }
})

test_that("create_alk parameter validation", {
  test_data <- generate_test_data()
  
  # Invalid age_data
  expect_error(
    create_alk(
      age_data = "invalid",
      lengths = 15:35,
      ages = 1:10
    ),
    "age_data must be a data frame"
  )
  
  # Missing required columns
  bad_age_data <- data.frame(bad_col = 1:10, another_bad = 1:10)
  expect_error(
    create_alk(
      age_data = bad_age_data,
      lengths = 15:35,
      ages = 1:10
    )
  )
  
  # Empty lengths or ages
  expect_error(
    create_alk(
      age_data = generate_test_age_data(),
      lengths = numeric(0),
      ages = 1:10
    )
  )
  
  expect_error(
    create_alk(
      age_data = generate_test_age_data(),
      lengths = 15:35,
      ages = numeric(0)
    )
  )
})

test_that("create_alk with length binning", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Test with length binning
  alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = seq(15, 35, by = 2), # Every 2cm
    ages = 1:10,
    length_bin_size = 2,
    verbose = FALSE
  )
  
  expect_s3_class(alk, "age_length_key")
  
  # Check that lengths are properly binned
  for (sex in names(alk)) {
    sex_alk <- alk[[sex]]
    unique_lengths <- unique(sex_alk$length)
    expect_true(all(unique_lengths %% 2 == 1)) # Should be odd numbers (15, 17, 19, etc.)
  }
})
