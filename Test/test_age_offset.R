# Test fit_cohort_alk with different age_offset values

source("./scala/R/fit_cohort_alk.R")

# Generate example data
set.seed(123)
n <- 150
cohort_data <- data.frame(
  year = sample(2018:2023, n, replace = TRUE),
  age = sample(2:6, n, replace = TRUE),
  sex = sample(c("male", "female"), n, replace = TRUE)
)
cohort_data$length <- with(cohort_data, 25 + age * 5 + ifelse(sex == "female", 3, 0) + rnorm(n, 0, 2))

cat("=== Testing different age_offset values ===\n\n")

# Test with default age_offset = 1
cat("1. Default age_offset = 1:\n")
model1 <- fit_cohort_alk(cohort_data, age_offset = 1, by_sex = TRUE, verbose = TRUE)
cat("Cohort range:", range(as.numeric(model1$cohort_levels)), "\n\n")

# Test with age_offset = 0
cat("2. Age_offset = 0:\n")
model0 <- fit_cohort_alk(cohort_data, age_offset = 0, by_sex = TRUE, verbose = TRUE)
cat("Cohort range:", range(as.numeric(model0$cohort_levels)), "\n\n")

# Test with age_offset = 2
cat("3. Age_offset = 2:\n")
model2 <- fit_cohort_alk(cohort_data, age_offset = 2, by_sex = TRUE, verbose = TRUE)
cat("Cohort range:", range(as.numeric(model2$cohort_levels)), "\n\n")

# Show the effect on specific calculations
cat("=== Effect on specific fish ===\n")
test_fish <- cohort_data[1:3, c("year", "age", "sex")]
cat("Example fish:\n")
print(test_fish)

cat("\nYear class calculations:\n")
cat("Offset = 0:", (test_fish$year - test_fish$age) - 0, "\n")
cat("Offset = 1:", (test_fish$year - test_fish$age) - 1, "\n")
cat("Offset = 2:", (test_fish$year - test_fish$age) - 2, "\n")

# Test age back-calculation with different offsets
cat("\n=== Age back-calculation comparison ===\n")
test_lengths <- c(35, 45)
test_sampling_year <- rep(2023, 2)
test_sex <- c("male", "female")

cat("Age predictions for lengths", paste(test_lengths, collapse = ", "), "in 2023:\n")

ages1 <- model1$predict_age(test_lengths, test_sampling_year, test_sex)
ages0 <- model0$predict_age(test_lengths, test_sampling_year, test_sex)
ages2 <- model2$predict_age(test_lengths, test_sampling_year, test_sex)

cat("Offset=1: Most likely ages =", apply(ages1, 1, function(x) which.max(x)), "\n")
cat("Offset=0: Most likely ages =", apply(ages0, 1, function(x) which.max(x)), "\n")
cat("Offset=2: Most likely ages =", apply(ages2, 1, function(x) which.max(x)), "\n")
