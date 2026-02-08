# Test that extra parameters in fit_cohort_alk flow through to predict_age
# When additional_terms are used (e.g. s(depth)), predict_age must accept
# the extra variable via ... and pass it to predict_cohort -> newdata
library(devtools)
load_all("scala")

set.seed(42)

# ---- 1. Generate data WITH an extra variable (depth) ----
years <- 2016:2022
n <- 600

vb <- list(Linf = 60, K = 0.22, t0 = -0.6)

cohort_data <- data.frame(
  year = sample(years, n, replace = TRUE),
  age = sample(1:8, n,
    replace = TRUE,
    prob = c(0.22, 0.22, 0.18, 0.14, 0.10, 0.06, 0.04, 0.04)
  ),
  sex = sample(c("male", "female"), n, replace = TRUE)
)

# Depth: deeper water tends to have older/larger fish
cohort_data$depth <- 20 + cohort_data$age * 5 + rnorm(n, 0, 5)
cohort_data$depth <- pmax(cohort_data$depth, 5)

# Length from von Bertalanffy + depth effect + noise
cohort_data$length <- with(cohort_data, {
  vb$Linf * (1 - exp(-vb$K * (age - vb$t0))) + depth * 0.05 + rnorm(n, 0, 2)
})
cohort_data$length <- round(pmax(cohort_data$length, 5), 1)

cat("Data generated:", nrow(cohort_data), "rows\n")
cat("Depth range:", round(range(cohort_data$depth), 1), "\n")

# ---- 2. Fit cohort model WITH additional_terms = "s(depth)" ----
cat("\n--- Fitting cohort ALK with additional_terms = s(depth) ---\n")
cohort_model <- fit_cohort_alk(
  cohort_data,
  by_sex = TRUE,
  age_offset = 1,
  additional_terms = "s(depth)",
  verbose = TRUE
)
print(cohort_model)

# ---- 3. Test predict_age WITH depth supplied via ... ----
cat("\n--- Testing predict_age with depth via ... ---\n")

test_lengths <- c(25, 35, 45, 30, 40)
test_years <- c(2020, 2021, 2022, 2020, 2021)
test_sex <- c("male", "female", "male", "female", "male")
test_depth <- c(30, 50, 70, 40, 60)

# This should work: passing depth via ...
age_probs <- tryCatch(
  cohort_model$predict_age(
    lengths        = test_lengths,
    sampling_years = test_years,
    sex            = test_sex,
    depth          = test_depth # <-- extra parameter via ...
  ),
  error = function(e) {
    cat("ERROR in predict_age with depth: ", e$message, "\n")
    NULL
  }
)

if (!is.null(age_probs)) {
  cat("SUCCESS: predict_age returned a", nrow(age_probs), "x", ncol(age_probs), "matrix\n")
  cat("Column names:", colnames(age_probs), "\n")
  cat("Row sums (should be ~1):", round(rowSums(age_probs), 4), "\n")
} else {
  cat("FAILED: predict_age with extra params returned NULL or error\n")
}

# ---- 4. Test predict_cohort WITH depth via ... ----
cat("\n--- Testing predict_cohort with depth via ... ---\n")

cohort_probs <- tryCatch(
  cohort_model$predict_cohort(
    lengths = test_lengths,
    years   = test_years,
    sex     = test_sex,
    depth   = test_depth # <-- extra parameter via ...
  ),
  error = function(e) {
    cat("ERROR in predict_cohort with depth: ", e$message, "\n")
    NULL
  }
)

if (!is.null(cohort_probs)) {
  cat("SUCCESS: predict_cohort returned a", nrow(cohort_probs), "x", ncol(cohort_probs), "matrix\n")
  cat("Row sums (should be ~1):", round(rowSums(cohort_probs), 4), "\n")
} else {
  cat("FAILED: predict_cohort with extra params returned NULL or error\n")
}

# ---- 5. Negative test: predict_age WITHOUT depth should fail ----
cat("\n--- Testing predict_age WITHOUT depth (should fail) ---\n")

age_probs_no_depth <- tryCatch(
  cohort_model$predict_age(
    lengths        = test_lengths,
    sampling_years = test_years,
    sex            = test_sex
    # depth NOT supplied
  ),
  error = function(e) {
    cat("Expected error (depth missing): ", e$message, "\n")
    "error"
  }
)

if (identical(age_probs_no_depth, "error")) {
  cat("CONFIRMED: predict_age correctly fails when depth is not provided\n")
} else {
  cat("NOTE: predict_age did not error without depth (mgcv may have defaulted)\n")
  cat("Result:", nrow(age_probs_no_depth), "x", ncol(age_probs_no_depth), "\n")
}

# ---- 6. Test with model WITHOUT additional_terms (control) ----
cat("\n--- Control: fitting model WITHOUT additional_terms ---\n")
cohort_model_simple <- fit_cohort_alk(
  cohort_data,
  by_sex     = TRUE,
  age_offset = 1,
  verbose    = FALSE
)

age_probs_simple <- tryCatch(
  cohort_model_simple$predict_age(
    lengths        = test_lengths,
    sampling_years = test_years,
    sex            = test_sex
  ),
  error = function(e) {
    cat("ERROR in simple predict_age: ", e$message, "\n")
    NULL
  }
)

if (!is.null(age_probs_simple)) {
  cat("SUCCESS: simple predict_age (no extra params) works fine\n")
  cat("Row sums:", round(rowSums(age_probs_simple), 4), "\n")
} else {
  cat("FAILED: simple predict_age should have worked\n")
}

cat("\n=== All tests completed ===\n")
