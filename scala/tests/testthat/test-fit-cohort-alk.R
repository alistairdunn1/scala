test_that("fit_cohort_alk basic functionality", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Add year column to age data if not present
  if (!"year" %in% names(generate_test_age_data())) {
    generate_test_age_data()$year <- sample(2015:2020, nrow(generate_test_age_data()), replace = TRUE)
  }
  
  cohort_alk <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  expect_type(cohort_alk, "list")
  expect_s3_class(cohort_alk, "cohort_alk")
  
  expected_names <- c("model", "predict_cohort", "predict_age", "by_sex", 
                     "cohorts", "age_offset", "model_summary", "deviance_explained")
  expect_true(all(expected_names %in% names(cohort_alk)))
  
  # Test prediction functions
  expect_type(cohort_alk$predict_cohort, "closure")
  expect_type(cohort_alk$predict_age, "closure")
  
  # Test cohort predictions
  test_lengths <- c(20, 25, 30)
  test_years <- c(2018, 2019, 2020)
  test_sex <- rep("unsexed", 3)
  
  cohort_predictions <- cohort_alk$predict_cohort(test_lengths, test_years, test_sex)
  
  expect_true(is.matrix(cohort_predictions))
  expect_equal(nrow(cohort_predictions), length(test_lengths))
  expect_equal(ncol(cohort_predictions), length(cohort_alk$cohorts))
  
  # Check predictions are probabilities
  row_sums <- rowSums(cohort_predictions)
  expect_true(all(abs(row_sums - 1) < 1e-10))
  expect_true(all(cohort_predictions >= 0))
  
  # Test age predictions (back-calculation)
  sampling_year <- 2020
  age_predictions <- cohort_alk$predict_age(test_lengths, sampling_year, test_sex)
  
  expect_true(is.matrix(age_predictions))
  expect_equal(nrow(age_predictions), length(test_lengths))
  
  # Check age predictions are probabilities
  age_row_sums <- rowSums(age_predictions)
  expect_true(all(abs(age_row_sums - 1) < 1e-10))
  expect_true(all(age_predictions >= 0))
})

test_that("fit_cohort_alk with custom age_offset", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Add year column
  if (!"year" %in% names(generate_test_age_data())) {
    generate_test_age_data()$year <- sample(2015:2020, nrow(generate_test_age_data()), replace = TRUE)
  }
  
  # Test different age offsets
  cohort_alk_0 <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = FALSE, age_offset = 0)
  cohort_alk_1 <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = FALSE, age_offset = 1)
  cohort_alk_2 <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = FALSE, age_offset = 2)
  
  expect_equal(cohort_alk_0$age_offset, 0)
  expect_equal(cohort_alk_1$age_offset, 1)
  expect_equal(cohort_alk_2$age_offset, 2)
  
  # Cohort ranges should be different for different offsets
  # Higher offset should give earlier cohorts
  expect_true(min(cohort_alk_2$cohorts) < min(cohort_alk_1$cohorts))
  expect_true(min(cohort_alk_1$cohorts) < min(cohort_alk_0$cohorts))
})

test_that("fit_cohort_alk with sex", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Add year column
  if (!"year" %in% names(generate_test_age_data())) {
    generate_test_age_data()$year <- sample(2015:2020, nrow(generate_test_age_data()), replace = TRUE)
  }
  
  cohort_alk <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  expect_true(cohort_alk$by_sex)
  
  # Test sex-specific predictions
  test_lengths <- c(20, 25, 30)
  test_years <- c(2018, 2019, 2020)
  test_sex <- c("male", "female", "male")
  
  cohort_predictions <- cohort_alk$predict_cohort(test_lengths, test_years, test_sex)
  age_predictions <- cohort_alk$predict_age(test_lengths, 2020, test_sex)
  
  expect_true(is.matrix(cohort_predictions))
  expect_true(is.matrix(age_predictions))
  
  # Predictions should handle different sexes
  male_cohort <- cohort_alk$predict_cohort(25, 2019, "male")
  female_cohort <- cohort_alk$predict_cohort(25, 2019, "female")
  
  expect_true(is.matrix(male_cohort))
  expect_true(is.matrix(female_cohort))
})

test_that("fit_cohort_alk parameter validation", {
  test_data <- generate_test_data()
  
  # Add year column
  if (!"year" %in% names(generate_test_age_data())) {
    generate_test_age_data()$year <- sample(2015:2020, nrow(generate_test_age_data()), replace = TRUE)
  }
  
  # Invalid alk_data
  expect_error(
    fit_cohort_alk(alk_data = "invalid"),
    "alk_data must be a data frame"
  )
  
  # Missing year column
  bad_data <- generate_test_age_data()[, !names(generate_test_age_data()) %in% "year"]
  expect_error(
    fit_cohort_alk(alk_data = bad_data)
  )
  
  # Invalid age_offset
  expect_error(
    fit_cohort_alk(alk_data = generate_test_age_data(), age_offset = -1)
  )
  
  expect_error(
    fit_cohort_alk(alk_data = generate_test_age_data(), age_offset = "invalid")
  )
})

test_that("fit_cohort_alk year class calculation", {
  set.seed(123)
  
  # Create specific test data to verify year class calculation
  test_age_data <- data.frame(
    age = c(3, 4, 5, 3, 4, 5),
    length = c(25, 30, 35, 24, 29, 34),
    sex = rep(c("male", "female"), each = 3),
    year = c(2018, 2018, 2018, 2019, 2019, 2019)
  )
  
  cohort_alk <- fit_cohort_alk(alk_data = test_age_data, by_sex = FALSE, age_offset = 1)
  
  # For age_offset = 1, year classes should be:
  # 2018 - 3 - 1 = 2014
  # 2018 - 4 - 1 = 2013
  # 2018 - 5 - 1 = 2012
  # 2019 - 3 - 1 = 2015
  # 2019 - 4 - 1 = 2014
  # 2019 - 5 - 1 = 2013
  
  expected_cohorts <- c(2012, 2013, 2014, 2015)
  expect_true(all(expected_cohorts %in% cohort_alk$cohorts))
  
  # Test with different offset
  cohort_alk_0 <- fit_cohort_alk(alk_data = test_age_data, by_sex = FALSE, age_offset = 0)
  expected_cohorts_0 <- expected_cohorts + 1 # One year later
  expect_true(all(expected_cohorts_0 %in% cohort_alk_0$cohorts))
})

test_that("fit_cohort_alk model quality", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Add year column
  if (!"year" %in% names(generate_test_age_data())) {
    generate_test_age_data()$year <- sample(2015:2020, nrow(generate_test_age_data()), replace = TRUE)
  }
  
  cohort_alk <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = TRUE)
  
  # Model should explain a reasonable amount of deviance
  expect_true(cohort_alk$deviance_explained > 50)
  
  # Model summary should be present
  expect_true(!is.null(cohort_alk$model_summary))
  
  # Cohorts should be reasonable years
  expect_true(all(cohort_alk$cohorts > 1900))
  expect_true(all(cohort_alk$cohorts < 2030))
})
