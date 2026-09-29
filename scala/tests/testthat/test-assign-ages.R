assignment_model <- function(class = "ordinal_alk", years = NULL, plus_group = NULL) {
  predictor <- function(lengths, sampling_years = NULL, sex = NULL, ...) {
    matrix(rep(c(0.25, 0.75), each = length(lengths)), ncol = 2,
      dimnames = list(NULL, c("age_2", "age_7")))
  }
  structure(list(predict_age = predictor, by_sex = FALSE, training_years = years,
    plus_group = plus_group), class = class)
}

test_that("assignment wrappers use the shared rules and retain fish records", {
  fish <- data.frame(length = c(80, 100, 120), year = 2015, sex = c("Male", "Female", "Male"),
    sample_id = c("b", "a", "c"), age = 99)
  wrappers <- list(cohort_alk = assign_ages_from_cohort, ordinal_alk = assign_ages_from_ordinal,
    multinomial_alk = assign_ages_from_multinomial)
  for (class in names(wrappers)) {
    model <- assignment_model(class, 2015, 7)
    for (method in c("random", "mode", "expected")) {
      generic <- assign_ages(fish, model, method, seed = 42, keep_probabilities = TRUE, verbose = FALSE)
      wrapper <- wrappers[[class]](fish, model, method, seed = 42, keep_probabilities = TRUE, verbose = FALSE)
      expect_identical(generic, wrapper)
      expect_equal(generic[names(fish)[names(fish) != "age"]], fish[names(fish)[names(fish) != "age"]])
      expect_equal(attr(generic, "plus_group"), 7)
      expect_equal(rowSums(generic[c("age_prob_2", "age_prob_7")]), rep(1, 3))
    }
  }
  expect_equal(assign_ages(fish, assignment_model(), "mode", verbose = FALSE)$age, rep(7, 3))
  expect_equal(assign_ages(fish, assignment_model(), "expected", verbose = FALSE)$age, rep(6, 3))
  attr(fish, "plus_group") <- 7
  expect_null(attr(assign_ages(fish, assignment_model(), verbose = FALSE), "plus_group"))
  expect_error(assign_ages_from_ordinal(fish, assignment_model("multinomial_alk")), "ordinal_alk")
  expect_error(assign_ages_from_multinomial(fish, assignment_model()), "multinomial_alk")
})

test_that("year rules distinguish year-dependent and year-independent models", {
  fish <- data.frame(length = c(80, 90, 100), year = c(2014, 2015, 2016))
  model <- assignment_model(years = 2015)
  result <- assign_ages(fish, model, "mode", keep_probabilities = TRUE, verbose = FALSE)
  expect_equal(result$age, c(NA_real_, 7, NA_real_))
  expect_true(all(is.na(result$age_prob_2[c(1, 3)])))
  expect_equal(assign_ages(fish, model, "mode", predict_missing = TRUE, verbose = FALSE)$age, rep(7, 3))
  expect_equal(assign_ages(fish, assignment_model(), "mode", verbose = FALSE)$age, rep(7, 3))
  expect_equal(assign_ages(fish["length"], assignment_model(), "mode", verbose = FALSE)$age, rep(7, 3))
  expect_true(all(is.na(assign_ages(fish[c(1, 3), ], model, verbose = FALSE)$age)))
  expect_equal(nrow(assign_ages(fish[FALSE, ], model, verbose = FALSE)), 0)
  expect_error(assign_ages(fish["length"], model), "year")
})

test_that("traditional keys use explicit length bins and sex strata", {
  key <- data.frame(length = c(81, 81, 101, 101), age = c(7, 2, 7, 2), proportion = c(.5, .5, .8, .2))
  fish <- data.frame(length = c(82, 102))
  result <- assign_ages(fish, key, "mode", length_bin_size = 2, keep_probabilities = TRUE, verbose = FALSE)
  expect_equal(result$age, c(2, 7))
  expect_equal(result$age_prob_7, c(.5, .8))
  expect_error(assign_ages(fish, key), "length bins")
  expect_error(assign_ages(fish, transform(key, proportion = proportion * 2)), "sum to one")
  expect_error(assign_ages(fish, rbind(key, key[1, ])), "duplicated")
  sex_keys <- list(male = key, female = transform(key, proportion = 1 - proportion))
  fish$sex <- c("MALE", "female")
  expect_equal(assign_ages(fish, sex_keys, "mode", length_bin_size = 2, verbose = FALSE)$age, c(2, 2))
  expect_error(assign_ages(transform(fish, sex = "unknown"), sex_keys), "requested sex")
  single <- data.frame(length = 80, age = 7, proportion = 1)
  expect_equal(assign_ages(data.frame(length = rep(80, 20)), single, seed = 1, verbose = FALSE)$age, rep(7, 20))
  raw <- data.frame(length = rep(c(80, 100), each = 10), age = rep(c(2, 7), 10))
  completed <- create_alk(raw, lengths = c(80, 100), ages = c(2, 7),
    min_ages_per_length = 1, verbose = FALSE)
  expect_equal(assign_ages(data.frame(length = c(80, 100)), completed, "mode", verbose = FALSE)$age, c(2, 2))
})

test_that("invalid probabilities and covariates stop without substitutes", {
  fish <- data.frame(length = c(80, 100))
  for (bad in list(matrix(c(.2, .2), 2, 1, dimnames = list(NULL, "age_7")),
    matrix(c(NA_real_, 1), 2, 1, dimnames = list(NULL, "age_7")),
    matrix(1, 2, 1, dimnames = list(NULL, "age_0")),
    matrix(1, 1, 2, dimnames = list(NULL, c("age_2", "age_7"))))) {
    model <- assignment_model()
    model$predict_age <- function(...) bad
    expect_error(assign_ages(fish, model), "Age prob|Age predict")
  }
  expect_error(assign_ages(fish, assignment_model(), seed = -1), "seed")
  expect_error(assign_ages(fish, assignment_model(), depth = 1:3), "length one")
  expect_error(assign_ages(fish, assignment_model(), length_bin_size = 2), "traditional")
  expect_error(assign_ages(transform(fish, length = NA_real_), assignment_model()), "finite positive")
})

test_that("fitted ordinal multinomial and cohort models predict through assignment", {
  set.seed(724)
  d <- data.frame(length = stats::runif(400, 50, 120), year = sample(2010:2015, 400, TRUE),
    depth = stats::runif(400, 500, 1000))
  d$age <- pmax(1, pmin(8, round((d$length - 40) / 12 + stats::rnorm(400))))
  for (fit in list(fit_ordinal_alk, fit_multinomial_alk, fit_cohort_alk)) {
    model <- fit(d, by_sex = FALSE, k_length = 4, k_year = 4,
      additional_terms = "s(depth, k = 4)", plus_group = 5, verbose = FALSE)
    fish <- d[1:8, ]
    result <- assign_ages(fish, model, "mode", keep_probabilities = TRUE, verbose = FALSE)
    probabilities <- model$predict_age(fish$length, fish$year, depth = fish$depth)
    expect_equal(unname(as.matrix(result[paste0("age_prob_", 1:5)])), unname(probabilities))
    expect_true(all(result$age <= 5 & result$age >= 1))
    expect_equal(attr(result, "plus_group"), 5)
    expect_error(assign_ages(fish[setdiff(names(fish), "depth")], model), "depth")
    expect_equal(assign_ages(fish, model, "mode", depth = 700, verbose = FALSE)$age,
      assign_ages(transform(fish, depth = 700), model, "mode", verbose = FALSE)$age)
  }
})

test_that("composition wrappers use shared bootstrap assignment", {
  set.seed(91)
  data <- generate_test_data("commercial")
  fish <- data$fish_data
  fish$age <- rep(c(2, 7), length.out = nrow(fish))
  settings <- list(fish_data = fish, strata_data = data$strata_data, age_range = c(1, 7),
    lw_params_male = c(a = .01, b = 3), lw_params_female = c(a = .01, b = 3),
    lw_params_unsexed = c(a = .01, b = 3), bootstraps = 2, verbose = FALSE)
  for (kind in c("ordinal", "multinomial")) {
    model <- assignment_model(paste0(kind, "_alk"))
    set.seed(19)
    generic <- do.call(calculate_age_compositions_from_model, c(settings, list(model = model)))
    set.seed(19)
    extra <- stats::setNames(list(model), paste0(kind, "_model"))
    wrapper <- get(paste0("calculate_age_compositions_from_", kind))
    expect_equal(do.call(wrapper, c(settings, extra)), generic)
    expect_equal(generic$n_bootstraps, 2)
  }
})
