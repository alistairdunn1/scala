make_weight_model <- function(by_sex = FALSE, seed = 42) {
  set.seed(seed)
  ages <- rep(1:6, each = 50)
  weights <- c(
    rnorm(50, 0.10, 0.04), rnorm(50, 0.23, 0.04),
    rnorm(50, 0.36, 0.05), rnorm(50, 0.49, 0.05),
    rnorm(50, 0.62, 0.06), rnorm(50, 0.75, 0.06)
  )
  weights <- pmax(0.01, weights)
  dat <- data.frame(age = ages, weight = weights, stringsAsFactors = FALSE)
  if (by_sex) dat$sex <- rep(c("male", "female"), length.out = length(ages))
  fit_weight_age(dat, by_sex = by_sex, verbose = FALSE)
}

make_fish_data <- function(n = 30, with_sex = FALSE, seed = 99) {
  set.seed(seed)
  fd <- data.frame(
    otolith_weight = runif(n, 0.05, 0.55),
    length = round(rnorm(n, 25, 5)),
    stratum = sample(c("North", "South"), n, replace = TRUE),
    sample_id = rep(paste0("S", seq_len(ceiling(n / 5))), each = 5)[seq_len(n)],
    male = rpois(n, 4),
    female = rpois(n, 4),
    unsexed = rpois(n, 1),
    sample_weight_kg = runif(n, 8, 18),
    total_catch_weight_kg = runif(n, 80, 250),
    stringsAsFactors = FALSE
  )
  if (with_sex) fd$sex <- sample(c("male", "female"), n, replace = TRUE)
  fd
}

test_that("assign_ages_from_weight basic functionality (mode)", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data(with_sex = FALSE)

  result <- assign_ages_from_weight(fish, model, method = "mode", verbose = FALSE)

  expect_true(is.data.frame(result))
  expect_true("age" %in% names(result))
  expect_equal(nrow(result), nrow(fish))

  # All non-NA weights should give non-NA ages
  expect_true(all(!is.na(result$age)))

  # Ages should be within model's age range
  expect_true(all(result$age >= min(model$ages)))
  expect_true(all(result$age <= max(model$ages)))
})

test_that("assign_ages_from_weight expected method", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data()

  result <- assign_ages_from_weight(fish, model, method = "expected", verbose = FALSE)

  expect_true("age" %in% names(result))
  expect_true(all(!is.na(result$age)))
  # Expected ages are rounded to integers
  expect_true(all(result$age == round(result$age)))
})

test_that("assign_ages_from_weight random method is reproducible with seed", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data()

  r1 <- assign_ages_from_weight(fish, model, method = "random", seed = 7, verbose = FALSE)
  r2 <- assign_ages_from_weight(fish, model, method = "random", seed = 7, verbose = FALSE)

  expect_equal(r1$age, r2$age)
})

test_that("assign_ages_from_weight random method varies without seed", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data(n = 100)

  r1 <- assign_ages_from_weight(fish, model, method = "random", seed = NULL, verbose = FALSE)
  r2 <- assign_ages_from_weight(fish, model, method = "random", seed = NULL, verbose = FALSE)

  # With 100 fish drawn from a distribution, some should differ
  expect_false(identical(r1$age, r2$age))
})

test_that("assign_ages_from_weight with sex model", {
  model <- make_weight_model(by_sex = TRUE)
  fish <- make_fish_data(with_sex = TRUE)

  result <- assign_ages_from_weight(fish, model, method = "mode", verbose = FALSE)

  expect_true("age" %in% names(result))
  expect_true(all(!is.na(result$age)))
})

test_that("assign_ages_from_weight NA otolith_weight gives NA age", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data(n = 10)
  fish$otolith_weight[3] <- NA
  fish$otolith_weight[7] <- NA

  result <- assign_ages_from_weight(fish, model, method = "mode", verbose = FALSE)

  expect_true(is.na(result$age[3]))
  expect_true(is.na(result$age[7]))
  expect_false(anyNA(result$age[-c(3, 7)]))
})

test_that("assign_ages_from_weight keep_probabilities = TRUE", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data(n = 5)

  result <- assign_ages_from_weight(fish, model, method = "mode",
                                    keep_probabilities = TRUE, verbose = FALSE)

  prob_cols <- grep("^age_prob_", names(result), value = TRUE)
  expect_true(length(prob_cols) == length(model$ages))

  # Rows with non-NA weight: probabilities should sum to ~1
  valid <- !is.na(fish$otolith_weight)
  prob_sums <- rowSums(result[valid, prob_cols])
  expect_true(all(abs(prob_sums - 1) < 1e-8))
})

test_that("assign_ages_from_weight preserves original columns", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data(n = 10)

  result <- assign_ages_from_weight(fish, model, method = "mode", verbose = FALSE)

  orig_cols <- names(fish)
  expect_true(all(orig_cols %in% names(result)))
})

test_that("assign_ages_from_weight input validation", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data()

  expect_error(assign_ages_from_weight("not df", model), "must be a data frame")
  expect_error(assign_ages_from_weight(fish, list()), "weight_age_model")

  fish_no_wt <- fish[, setdiff(names(fish), "otolith_weight")]
  expect_error(assign_ages_from_weight(fish_no_wt, model), "otolith_weight")

  model_sex <- make_weight_model(by_sex = TRUE)
  fish_no_sex <- make_fish_data(with_sex = FALSE)
  expect_error(assign_ages_from_weight(fish_no_sex, model_sex), "'sex' column")
})

test_that("assign_ages_from_weight out-of-range weights generate warning", {
  model <- make_weight_model(by_sex = FALSE)
  fish <- make_fish_data(n = 10)
  fish$otolith_weight[1] <- 999  # far out of range

  expect_warning(
    assign_ages_from_weight(fish, model, method = "mode", verbose = FALSE),
    "outside the model's training range"
  )
})
