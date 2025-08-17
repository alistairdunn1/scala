test_that("print.length_composition works", {
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
  
  # Test that print doesn't error
  expect_output(print(lc_result), "Length Composition Summary")
  expect_output(print(lc_result), "Data Type.*length compositions")
  expect_output(print(lc_result), "Length Range")
  expect_output(print(lc_result), "Strata")
})

test_that("print.age_composition works", {
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
  
  simple_alk <- data.frame(
    length = rep(15:35, each = 5),
    age = rep(1:5, times = length(15:35)),
    proportion = rep(c(0.1, 0.2, 0.4, 0.2, 0.1), times = length(15:35))
  )
  
  age_result <- calculate_age_compositions(
    x = lc_result,
    age_length_key = simple_alk,
    age_range = c(1, 5)
  )
  
  # Test that print doesn't error
  expect_output(print(age_result), "Age Composition Summary")
  expect_output(print(age_result), "Data Type.*age compositions")
  expect_output(print(age_result), "Age Range")
  expect_output(print(age_result), "Strata")
})

test_that("print.age_length_key works", {
  set.seed(123)
  test_data <- generate_test_data()
  
  alk <- create_alk(
    age_data = generate_test_age_data(),
    lengths = 15:35,
    ages = 1:10,
    verbose = FALSE
  )
  
  # Test that print doesn't error
  expect_output(print(alk), "Age-Length Key Summary")
  expect_output(print(alk), "Type.*Complete ALK")
  expect_output(print(alk), "Length Range")
  expect_output(print(alk), "Age Range")
})

test_that("print.ordinal_alk works", {
  set.seed(123)
  test_data <- generate_test_data()
  
  ord_alk <- fit_ordinal_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  # Test that print doesn't error
  expect_output(print(ord_alk), "Ordinal Age-at-Length Model")
  expect_output(print(ord_alk), "Model Type")
  expect_output(print(ord_alk), "Deviance Explained")
  expect_output(print(ord_alk), "Age Range")
})

test_that("print.cohort_alk works", {
  set.seed(123)
  test_data <- generate_test_data()
  
  # Add year column if not present
  if (!"year" %in% names(generate_test_age_data())) {
    generate_test_age_data()$year <- sample(2015:2020, nrow(generate_test_age_data()), replace = TRUE)
  }
  
  cohort_alk <- fit_cohort_alk(alk_data = generate_test_age_data(), by_sex = FALSE)
  
  # Test that print doesn't error
  expect_output(print(cohort_alk), "Cohort Age-at-Length Model")
  expect_output(print(cohort_alk), "Model Type")
  expect_output(print(cohort_alk), "Deviance Explained")
  expect_output(print(cohort_alk), "Age Offset")
  expect_output(print(cohort_alk), "Cohort Range")
})

test_that("print.multinomial_n works", {
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
    bootstraps = 20
  )
  
  mult_n_result <- calculate_multinomial_n(lc_result)
  
  # Test that print doesn't error
  expect_output(print(mult_n_result), "Multinomial Effective Sample Size")
  expect_output(print(mult_n_result), "Input Type")
  expect_output(print(mult_n_result), "Summary Statistics")
})
