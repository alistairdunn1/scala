test_that("continuation likelihood reproduces weighted category frequencies", {
  d <- data.frame(age = rep(c(2, 5, 9), c(20, 30, 50)), length = 100)
  w <- ifelse(d$age == 5, 2, 1)
  fit <- fit_continuation_alk(d, by_sex = FALSE, k_length = NULL,
    weights = w, verbose = FALSE)
  p <- fit$predict_function(100)
  expect_equal(as.vector(p), c(20, 60, 50) / 130, tolerance = 1e-6)
  expect_equal(as.vector(fit$predict_conditional(100)), c(20/130, 60/110), tolerance = 1e-6)
  expect_equal(as.numeric(stats::logLik(fit$model)), sum(w * log(p[1, match(d$age, fit$ages)])), tolerance = 1e-6)
  all_ages <- fit$predict_age(c(90, 110))
  expect_equal(rowSums(all_ages), c(1, 1))
  expect_true(all(all_ages[, -c(2, 5, 9)] == 0))
  assigned <- assign_ages(data.frame(length = rep(100, 5000)), fit,
    seed = 12, keep_probabilities = TRUE, verbose = FALSE)
  expect_true(all(assigned$age %in% c(2, 5, 9)))
  expect_equal(as.numeric(table(factor(assigned$age, levels = c(2, 5, 9)))) / 5000,
    c(20, 60, 50) / 130, tolerance = 0.025)
  duplicated <- fit_continuation_alk(d[rep(seq_len(nrow(d)), w), ],
    by_sex = FALSE, k_length = NULL, verbose = FALSE)
  expect_equal(duplicated$predict_age(100), fit$predict_age(100), tolerance = 1e-6)
})

test_that("two-category continuation agrees with a binomial GAM", {
  set.seed(702)
  d <- data.frame(length = runif(400, 60, 140))
  d$age <- ifelse(runif(nrow(d)) < plogis((100 - d$length)/20), 1, 2)
  fit <- fit_continuation_alk(d, by_sex = FALSE, k_length = 4,
    select = FALSE, gamma = 1, verbose = FALSE)
  reference <- mgcv::gam(I(age == 1) ~ s(length, k = 4), family = stats::binomial(),
    data = d, method = "REML")
  x <- data.frame(length = c(70, 100, 130))
  expect_equal(as.vector(fit$predict_age(x$length)[, 1]),
    as.vector(stats::predict(reference, x, type = "response")), tolerance = 1e-5)
})

test_that("sex, temporal covariates and plus groups reach generic assignment", {
  set.seed(703)
  d <- expand.grid(age = 1:5, year = 2010:2015, sex = c("male", "female"), fish = 1:5)
  d$length <- runif(nrow(d), 70, 130)
  d$depth <- runif(nrow(d), 100, 300)
  fit <- fit_continuation_alk(d, k_length = 3, k_year = 3, additional_terms = "depth",
    plus_group = 4, verbose = FALSE)
  fish <- data.frame(length = c(90, 120), year = c(2012, 2014), sex = c("male", "female"), depth = c(150, 250))
  result <- assign_ages(fish, fit, seed = 2, keep_probabilities = TRUE, verbose = FALSE)
  expected <- fit$predict_age(fish$length, fish$year, fish$sex, depth = fish$depth)
  expect_equal(unname(as.matrix(result[paste0("age_prob_", 1:4)])), unname(expected))
  expect_equal(attr(result, "plus_group"), 4)
  expect_error(fit$predict_age(100, 2012, "male"), "Missing prediction")
  expect_error(fit$predict_age(100, 2012, "unknown", depth = 200), "Unknown sex")
  expect_error(fit$predict_age(100, 2012.5, "male", depth = 200), "integer")
})

test_that("cohort random walk links the same cohort and permits changing effects", {
  set.seed(704)
  d <- expand.grid(year = 2010:2015, fish = 1:150)
  d$length <- runif(nrow(d), 80, 120)
  # A pulse follows cohort 2009 and changes amplitude across its observed ages.
  p <- t(vapply(d$year, function(y) {
    x <- rep(1, 5); a <- y - 2009
    if (a %in% 1:5) x[a] <- if (a <= 2) 8 else 3
    x / sum(x)
  }, numeric(5)))
  d$age <- vapply(seq_len(nrow(d)), function(i) sample.int(5, 1, prob = p[i, ]), integer(1))
  fit <- fit_continuation_alk(d, by_sex = FALSE, k_length = NULL,
    cohort_rw = TRUE, age_offset = 0, gamma = 1, verbose = FALSE)
  grid <- fit$cohort_grid
  s <- fit$cohort_penalties$increments
  edges <- which(s < 0, arr.ind = TRUE)
  expect_true(nrow(edges) > 0)
  expect_true(all(grid$cohort[edges[,1]] == grid$cohort[edges[,2]]))
  expect_true(all(abs(grid$year[edges[,1]] - grid$year[edges[,2]]) == 1))
  prediction <- fit$predict_age(rep(100, 6), 2010:2015)
  expect_equal(rowSums(prediction), rep(1, 6))
  expect_gt(prediction[1,1], prediction[5,1] + 0.2)
  expect_gt(prediction[2,2], prediction[5,2] + 0.2)
  expect_error(fit$predict_age(100, 2016), "fitted range")
  assigned <- assign_ages(data.frame(length = 100, year = 2012), fit, verbose = FALSE)
  expect_true(assigned$age %in% 1:5)
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveRDS(fit, path)
  expect_equal(readRDS(path)$predict_age(100, 2012), fit$predict_age(100, 2012))
})

test_that("unsupported continuation inputs stop rather than substitute", {
  d <- data.frame(age = rep(c(1, 3, 4), 10), length = 100, year = rep(2010:2012, 10))
  expect_error(fit_continuation_alk(d, by_sex = FALSE, cohort_rw = TRUE), "consecutive")
  expect_error(fit_continuation_alk(d, by_sex = FALSE, cohort_rw = TRUE, k_year = 3), "not both")
  expect_error(fit_continuation_alk(d, by_sex = FALSE, weights = rep(0,30)), "weights")
  expect_error(fit_continuation_alk(d, by_sex = FALSE, additional_terms = "age"), "response age")
  expect_error(fit_continuation_alk(d, by_sex = FALSE, weights = ifelse(d$age == 3, 0, 1)), "positive-weight")
})

test_that("continuation models participate in composition bootstraps", {
  set.seed(705)
  d <- data.frame(age = rep(c(2, 4, 7), each = 30), length = 100)
  fit <- fit_continuation_alk(d, by_sex = FALSE, k_length = NULL, verbose = FALSE)
  data <- generate_test_data("commercial")
  fish <- assign_ages(data$fish_data, fit, seed = 3, verbose = FALSE)
  result <- calculate_age_compositions_from_model(fish, data$strata_data,
    age_range = c(1, 7), lw_params_male = c(a = .01, b = 3),
    lw_params_female = c(a = .01, b = 3), lw_params_unsexed = c(a = .01, b = 3),
    model = fit, bootstraps = 3, verbose = FALSE)
  expect_equal(result$n_bootstraps, 3)
  expect_true(all(is.finite(result$age_composition)))
  expect_equal(sum(result$pooled_age_proportions[, "total"]), 1)
})

test_that("pooled continuation fits preserve separate sex curves and sparse ages", {
  set.seed(706)
  d <- expand.grid(age = c(2, 3, 4, 6, 7), sex = c("female", "male"), replicate = 1:30)
  d <- d[!(d$sex == "male" & d$age == 6), ]
  d$length <- 45 + d$age * ifelse(d$sex == "male", 8, 15) + stats::rnorm(nrow(d), 0, 9)
  for (pool in c(FALSE, TRUE)) {
    fit <- fit_continuation_alk(d, k_length = 4, age_pooling = TRUE,
      sex_pooling = pool, engine = "bam", gamma = 1.4, verbose = FALSE)
    p <- fit$predict_age(c(90, 90), sex = c("female", "male"))
    expect_equal(rowSums(p), c(1, 1))
    expect_true(all(is.finite(p)))
    expect_equal(unname(p[, 5]), c(0, 0))
    expect_gt(sum(p[2, ] * 1:7), sum(p[1, ] * 1:7))
    expect_identical(fit$sex_pooling, pool)
    expect_equal(sum(vapply(fit$model$smooth, function(x) grepl("te(length", x$label, fixed = TRUE), logical(1))), 2)
  }
})

test_that("bam cohort penalties agree with the dense formulation", {
  set.seed(707)
  d <- expand.grid(age = 1:4, year = 2010:2013, replicate = 1:8)
  d$length <- 100
  a <- fit_continuation_alk(d, by_sex = FALSE, k_length = NULL,
    cohort_rw = TRUE, verbose = FALSE)
  b <- fit_continuation_alk(d, by_sex = FALSE, k_length = NULL,
    cohort_rw = TRUE, engine = "bam", verbose = FALSE)
  expect_equal(a$predict_age(rep(100, 4), 2010:2013),
    b$predict_age(rep(100, 4), 2010:2013), tolerance = 1e-4)
})

test_that("continuation wrappers validate the model and delegate to shared functions", {
  d <- data.frame(age = rep(c(2, 5, 9), c(20, 30, 50)), length = 100)
  fit <- fit_continuation_alk(d, by_sex = FALSE, k_length = NULL, verbose = FALSE)
  fish <- data.frame(length = c(90, 100, 110))
  expect_identical(
    assign_ages_from_continuation(fish, fit, seed = 3, keep_probabilities = TRUE, verbose = FALSE),
    assign_ages(fish, fit, seed = 3, keep_probabilities = TRUE, verbose = FALSE))
  wrong <- structure(list(), class = "ordinal_alk")
  expect_error(assign_ages_from_continuation(fish, wrong), "continuation_alk")
  expect_error(calculate_age_compositions_from_continuation(fish, NULL, c(1, 9),
    NULL, NULL, NULL, continuation_model = wrong), "continuation_alk")
})
