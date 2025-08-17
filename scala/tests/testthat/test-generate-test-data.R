test_that("generate_test_data creates valid test data", {
  # Test basic generation
  set.seed(123)
  test_data <- generate_test_data()
  
  expect_type(test_data, "list")
  expect_named(test_data, c("fish_data", "strata_data"))
  
  # Test fish_data structure
  fish_data <- test_data$fish_data
  expect_s3_class(fish_data, "data.frame")
  expect_true(all(c("stratum", "sample_id", "length", "male", "female", "unsexed", 
                   "sample_weight_kg", "total_catch_weight_kg") %in% names(fish_data)))
  expect_true(nrow(fish_data) > 0)
  expect_true(all(fish_data$length > 0))
  expect_true(all(fish_data$male >= 0))
  expect_true(all(fish_data$female >= 0))
  expect_true(all(fish_data$unsexed >= 0))
  
  # Test strata_data structure
  strata_data <- test_data$strata_data
  expect_s3_class(strata_data, "data.frame")
  expect_true(all(c("stratum", "stratum_total_catch_kg") %in% names(strata_data)))
  expect_true(nrow(strata_data) > 0)
  expect_true(all(strata_data$stratum_total_catch_kg > 0))
  
  # Test reproducibility
  set.seed(123)
  test_data2 <- generate_test_data()
  expect_identical(test_data$fish_data, test_data2$fish_data)
})

test_that("generate_test_age_data creates valid age data", {
  # Test basic generation
  age_data <- generate_test_age_data()
  
  expect_s3_class(age_data, "data.frame")
  expect_true(all(c("age", "length", "sex") %in% names(age_data)))
  expect_true(nrow(age_data) > 0)
  expect_true(all(age_data$age > 0))
  expect_true(all(age_data$length > 0))
  expect_true(all(age_data$sex %in% c("male", "female", "unsexed")))
  
  # Test parameter specification
  age_data_small <- generate_test_age_data(n_samples = 50)
  expect_equal(nrow(age_data_small), 50)
  
  # Test realistic age-length relationships
  expect_true(cor(age_data$age, age_data$length) > 0.5)  # Should be positively correlated
  
  # Test reproducibility
  age_data2 <- generate_test_age_data()
  expect_identical(age_data, age_data2)
})

test_that("generate_test_data supports different data types", {
  # Test commercial data type (default)
  commercial_data <- generate_test_data("commercial")
  expect_true(all(c("sample_weight_kg", "total_catch_weight_kg") %in% names(commercial_data$fish_data)))
  
  # Test survey data type  
  survey_data <- generate_test_data("survey")
  expect_true(all(c("sample_area_km2", "catch_density_kg_km2") %in% names(survey_data$fish_data)))
  
  # Test invalid data type
  expect_error(generate_test_data("invalid"), "data_type must be either")
})

test_that("generate_commercial_test_data works", {
  commercial_data <- generate_commercial_test_data()
  
  expect_type(commercial_data, "list")
  expect_named(commercial_data, c("fish_data", "strata_data"))
  
  # Test commercial-specific features
  fish_data <- commercial_data$fish_data
  expect_true("sample_weight_kg" %in% names(fish_data))
  expect_true("total_catch_weight_kg" %in% names(fish_data))
  expect_true("stratum_total_catch_kg" %in% names(commercial_data$strata_data))
})

test_that("generate_survey_test_data works", {
  survey_data <- generate_survey_test_data()
  
  expect_type(survey_data, "list")
  expect_named(survey_data, c("fish_data", "strata_data"))
  
  # Test survey-specific features
  fish_data <- survey_data$fish_data
  expect_true("sample_area_km2" %in% names(fish_data))
  expect_true("catch_density_kg_km2" %in% names(fish_data))
  
  strata_data <- survey_data$strata_data
  expect_true("stratum_area_km2" %in% names(strata_data))
})
