# Test for fit_cohort_alk: cohort modeling and age back-calculation

suppressPackageStartupMessages({
  library(dplyr)
})

source("./scala/R/fit_cohort_alk.R")

if (!requireNamespace("mgcv", quietly = TRUE)) {
  cat("SKIP: mgcv not installed; skipping fit_cohort_alk test.\n")
  quit(status = 0)
}

set.seed(42)
# Generate cohort data: year, age, length, sex
n <- 400
years <- 2015:2023
cohort_data <- data.frame(
  year = sample(years, n, replace = TRUE),
  age = sample(1:8, n, replace = TRUE),
  sex = sample(c("male", "female"), n, replace = TRUE)
)

# Add realistic length based on age and sex
cohort_data$length <- with(
  cohort_data,
  20 + age * 4 + ifelse(sex == "female", 2, 0) + rnorm(n, 0, 2)
)

# Fit cohort model
model <- fit_cohort_alk(cohort_data, age_offset = 1, by_sex = TRUE, verbose = FALSE)

# Test cohort prediction
test_lengths <- c(25, 35, 45)
test_years <- c(2020, 2021, 2022)
test_sex <- c("male", "female", "male")

cohort_probs <- model$predict_cohort(test_lengths, test_years, test_sex)

# Test age back-calculation
length_obs <- c(28, 38, 48)
sampling_year <- rep(2023, 3)
obs_sex <- c("female", "male", "female")

age_probs <- model$predict_age(length_obs, sampling_year, obs_sex)

# Checks
ok <- TRUE
fail <- function(msg) {
  cat("FAIL:", msg, "\n")
  ok <<- FALSE
}
pass <- function(msg) {
  cat("PASS:", msg, "\n")
}

# 1) Model fitted successfully
if (inherits(model, "cohort_alk")) pass("model is cohort_alk class") else fail("model class incorrect")

# 2) Cohort predictions valid
if (is.matrix(cohort_probs) && nrow(cohort_probs) == 3) pass("cohort prediction matrix shape OK") else fail("cohort prediction shape wrong")

rs_cohort <- rowSums(cohort_probs)
if (all(abs(rs_cohort - 1) < 1e-6)) pass("cohort probabilities sum to 1") else fail("cohort probabilities don't sum to 1")

if (all(cohort_probs >= 0)) pass("cohort probabilities non-negative") else fail("negative cohort probabilities")

# 3) Age predictions valid
if (is.matrix(age_probs) && nrow(age_probs) == 3) pass("age prediction matrix shape OK") else fail("age prediction shape wrong")

rs_age <- rowSums(age_probs)
if (all(rs_age >= 0)) pass("age probabilities non-negative row sums") else fail("negative age probability row sums")

# 4) Model components present
if (!is.null(model$cohort_levels)) pass("cohort levels present") else fail("cohort levels missing")
if (!is.null(model$year_range)) pass("year range present") else fail("year range missing")

cat("\nRESULT:", if (ok) "ALL CHECKS PASSED" else "SOME CHECKS FAILED", "\n")
