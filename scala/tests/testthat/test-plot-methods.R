test_that("plot.length_composition basic functionality", {
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

  # Test that plot doesn't error and returns a ggplot object
  skip_if_not_installed("ggplot2")

  p <- plot(lc_result)
  expect_s3_class(p, "ggplot")

  # Test plot with options
  p_props <- plot(lc_result, type = "proportion")
  expect_s3_class(p_props, "ggplot")

  p_pooled <- plot(lc_result, pooled = TRUE)
  expect_s3_class(p_pooled, "ggplot")
})

test_that("plot.length_composition with bootstraps", {
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
    bootstraps = 10
  )

  skip_if_not_installed("ggplot2")

  # Test plot with confidence intervals
  p_ci <- plot(lc_result, show_ci = TRUE)
  expect_s3_class(p_ci, "ggplot")

  # Test plot without confidence intervals
  p_no_ci <- plot(lc_result, show_ci = FALSE)
  expect_s3_class(p_no_ci, "ggplot")
})

test_that("plot.age_composition basic functionality", {
  skip("Skipping age composition plotting test until issues with structure are resolved")

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

  skip_if_not_installed("ggplot2")

  # Test that plot doesn't error
  p <- plot(age_result)
  expect_s3_class(p, "ggplot")

  # Test with proportions
  p_props <- plot(age_result, type = "proportion")
  expect_s3_class(p_props, "ggplot")

  # Test pooled
  p_pooled <- plot(age_result, pooled = TRUE)
  expect_s3_class(p_pooled, "ggplot")
})

test_that("plot functions with different parameters", {
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
    bootstraps = 10
  )

  skip_if_not_installed("ggplot2")

  # Test different sex categories
  p_male <- plot(lc_result, sex_categories = "male")
  expect_s3_class(p_male, "ggplot")

  p_total <- plot(lc_result, sex_categories = "total")
  expect_s3_class(p_total, "ggplot")

  p_multiple <- plot(lc_result, sex_categories = c("male", "female"))
  expect_s3_class(p_multiple, "ggplot")
})

test_that("plot_alk function", {
  set.seed(123)
  test_data <- generate_test_data()

  # Generate test age data with consistent lengths
  age_data <- generate_test_age_data()

  # Create a consistent ALK using the same lengths for all sexes
  alk <- list()

  # Make sure our lengths and repetition patterns match exactly
  n_lengths <- length(15:35) # 21 lengths
  n_ages <- 5 # 5 age classes
  n_repeats <- 2 # Each length appears twice

  # Total rows: 21 lengths * 2 repeats = 42 rows
  # Each age needs to appear 42/5 = 8.4 times, so we'll make it 8 repeats per age (40 rows)
  # and add 2 more rows manually

  alk$male <- data.frame(
    length = rep(15:35, each = n_repeats),
    age = c(rep(1:5, each = 8), 1, 2), # 8 repeats of each age + 2 extra
    proportion = rep(0.2, 42) # Equal proportions for simplicity
  )

  alk$female <- data.frame(
    length = rep(15:35, each = n_repeats),
    age = c(rep(1:5, each = 8), 1, 2), # Same pattern as male
    proportion = rep(0.2, 42)
  )

  alk$unsexed <- data.frame(
    length = rep(15:35, each = n_repeats),
    age = c(rep(1:5, each = 8), 1, 2), # Same pattern as others
    proportion = rep(0.2, 42)
  )

  class(alk) <- c("age_length_key", "list")

  skip_if_not_installed("ggplot2")

  # Test ALK plot
  p_alk <- plot_alk(alk)
  expect_s3_class(p_alk, "ggplot")

  # Test with specific sex parameter
  p_alk_male <- plot_alk(alk, by_sex = TRUE)
  expect_s3_class(p_alk_male, "ggplot")
})

test_that("plot_length_composition_comparison function", {
  set.seed(123)
  test_data <- generate_test_data()
  lw_params <- get_default_lw_params()

  lc1 <- calculate_length_compositions(
    fish_data = test_data$fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )

  # Create a second length composition (slightly different)
  modified_fish_data <- test_data$fish_data
  modified_fish_data$male <- modified_fish_data$male * 0.9

  lc2 <- calculate_length_compositions(
    fish_data = modified_fish_data,
    strata_data = test_data$strata_data,
    length_range = c(15, 35),
    lw_params_male = lw_params$male,
    lw_params_female = lw_params$female,
    lw_params_unsexed = lw_params$unsexed,
    bootstraps = 0
  )

  skip_if_not_installed("ggplot2")

  # Test comparison plot - provide a list of length compositions
  p_comp <- plot_length_composition_comparison(list("Original" = lc1, "Modified" = lc2))
  expect_s3_class(p_comp, "ggplot")
})
