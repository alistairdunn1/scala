test_that("fit_ordinal_alk basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Test without sex
  ord_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  expect_type(ord_alk, "list")
  expect_s3_class(ord_alk, "ordinal_alk")
  
  expected_names <- c("model", "predict_function", "by_sex", "ages", 
                     "model_summary", "deviance_explained")
  expect_true(all(expected_names %in% names(ord_alk)))
  
  # Test prediction function
  expect_type(ord_alk$predict_function, "closure")
  
  # Test predictions
  test_lengths <- c(20, 25, 30)
  test_sex <- rep("unsexed", 3)
  
  predictions <- ord_alk$predict_function(test_lengths, test_sex)
  
  expect_true(is.matrix(predictions))
  expect_equal(nrow(predictions), length(test_lengths))
  expect_equal(ncol(predictions), length(ord_alk$ages))
  
  # Check predictions are probabilities (sum to 1, non-negative)
  row_sums <- rowSums(predictions)
  expect_true(all(abs(row_sums - 1) < 1e-10))
  expect_true(all(predictions >= 0))
  
  # Check deviance explained is reasonable
  expect_true(ord_alk$deviance_explained >= 0 && ord_alk$deviance_explained <= 100)
})

test_that("fit_ordinal_alk with sex", {
  set.seed(123)
  test_data <- generate_test_data()
  
  ord_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  expect_true(ord_alk$by_sex)
  
  # Test sex-specific predictions
  test_lengths <- c(20, 25, 30)
  test_sex <- c("male", "female", "male")
  
  predictions <- ord_alk$predict_function(test_lengths, test_sex)
  
  expect_true(is.matrix(predictions))
  expect_equal(nrow(predictions), length(test_lengths))
  
  # Predictions should be different for different sexes (usually)
  male_pred <- ord_alk$predict_function(25, "male")
  female_pred <- ord_alk$predict_function(25, "female")
  
  # They might be the same if the model doesn't detect sex differences,
  # but the function should handle both
  expect_true(is.matrix(male_pred))
  expect_true(is.matrix(female_pred))
})

test_that("fit_ordinal_alk parameter validation", {
  test_data <- generate_test_data()
  
  # Invalid alk_data
  expect_error(
    fit_ordinal_alk(alk_data = "invalid"),
    "alk_data must be a data frame"
  )
  
  # Missing required columns
  bad_data <- data.frame(bad_col = 1:10, another_bad = 1:10)
  expect_error(
    fit_ordinal_alk(alk_data = bad_data)
  )
  
  # Empty data
  empty_data <- data.frame(age = numeric(0), length = numeric(0), sex = character(0))
  expect_error(
    fit_ordinal_alk(alk_data = empty_data)
  )
})

test_that("fit_ordinal_alk prediction edge cases", {
  set.seed(123)
  test_data <- generate_test_data()
  
  ord_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  # Test single length prediction
  single_pred <- ord_alk$predict_function(25, "unsexed")
  expect_true(is.matrix(single_pred))
  expect_equal(nrow(single_pred), 1)
  
  # Test extreme lengths (extrapolation)
  extreme_lengths <- c(5, 50) # Outside typical range
  extreme_pred <- ord_alk$predict_function(extreme_lengths, rep("unsexed", 2))
  expect_true(is.matrix(extreme_pred))
  expect_equal(nrow(extreme_pred), 2)
  expect_true(all(extreme_pred >= 0))
  expect_true(all(abs(rowSums(extreme_pred) - 1) < 1e-10))
})

test_that("fit_ordinal_alk model quality", {
  set.seed(123)
  test_data <- generate_test_data()
  
  ord_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  # Model should explain a reasonable amount of deviance for structured data
  expect_true(ord_alk$deviance_explained > 50) # Should be quite good for test data
  
  # Model summary should be present
  expect_true(!is.null(ord_alk$model_summary))
  
  # Ages should be reasonable
  expect_true(min(ord_alk$ages) >= 1)
  expect_true(max(ord_alk$ages) <= 20) # Reasonable upper bound for fish ages
})
