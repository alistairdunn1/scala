# Demonstration of fit_cohort_alk functionality

source("./scala/R/fit_cohort_alk.R")

# Generate example data
set.seed(123)
n <- 200
cohort_data <- data.frame(
  year = sample(2018:2023, n, replace = TRUE),
  age = sample(2:7, n, replace = TRUE),
  sex = sample(c("male", "female"), n, replace = TRUE)
)
cohort_data$length <- with(cohort_data, 25 + age * 5 + ifelse(sex == "female", 3, 0) + rnorm(n, 0, 2))

# Show the year class calculation
cohort_data$year_class <- cohort_data$year - cohort_data$age - 1
cat("Example data showing year class calculation (year - age - 1):\n")
print(head(cohort_data, 10))

# Fit model
cat("\n=== Fitting Cohort Model ===\n")
model <- fit_cohort_alk(cohort_data, by_sex = TRUE, verbose = TRUE)

# Example: predict cohorts for lengths 35, 45 in year 2022
cat("\n=== Cohort Predictions ===\n")
cohort_probs <- model$predict_cohort(c(35, 45), c(2022, 2022), c("male", "female"))
cat("Cohort predictions for length 35 (male) and 45 (female) in 2022:\n")
print(round(cohort_probs, 3))

# Example: back-calculate ages for lengths observed in 2023
cat("\n=== Age Back-calculations ===\n")
age_probs <- model$predict_age(c(30, 40, 50), rep(2023, 3), c("male", "female", "male"))
cat("Age back-calculations for lengths 30, 40, 50 observed in 2023:\n")
print(round(age_probs, 3))

cat("\n=== Model Summary ===\n")
print(model)
