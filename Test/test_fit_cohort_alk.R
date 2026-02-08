# Test fit_cohort_alk: generate sample data with sex, fit model, plot observed vs expected ages
library(devtools)
load_all("scala")

set.seed(42)

# ---- 1. Generate realistic age-length data with sex ----
years <- 2015:2024
n_per_year <- 200
n <- length(years) * n_per_year

# Von Bertalanffy-like growth parameters (differ by sex)
# Females grow larger
vb <- list(
  male   = list(Linf = 55, K = 0.25, t0 = -0.5),
  female = list(Linf = 65, K = 0.20, t0 = -0.8)
)

# Build data year by year with realistic age structure
rows <- vector("list", n)
idx <- 1
for (yr in years) {
  for (i in seq_len(n_per_year)) {
    sex <- sample(c("male", "female"), 1)
    # Age distribution: younger fish more common
    age <- sample(1:10, 1, prob = c(0.20, 0.22, 0.18, 0.14, 0.10, 0.07, 0.04, 0.03, 0.01, 0.01))
    # Von Bertalanffy length with noise
    p <- vb[[sex]]
    mean_len <- p$Linf * (1 - exp(-p$K * (age - p$t0)))
    len <- round(mean_len + rnorm(1, 0, 2.5), 1)
    len <- max(5, len)  # floor at 5 cm

    rows[[idx]] <- data.frame(year = yr, age = age, length = len, sex = sex)
    idx <- idx + 1
  }
}

cohort_data <- do.call(rbind, rows)
cat("Generated", nrow(cohort_data), "observations\n")
cat("Age range:", range(cohort_data$age), "\n")
cat("Length range:", round(range(cohort_data$length), 1), "\n")
cat("Years:", range(cohort_data$year), "\n")
cat("Sex counts:\n"); print(table(cohort_data$sex))

# ---- 2. Fit cohort model ----
cat("\n--- Fitting cohort ALK ---\n")
cohort_model <- fit_cohort_alk(cohort_data, by_sex = TRUE, age_offset = 1, verbose = TRUE)
print(cohort_model)

# ---- 3. Predict ages for training data ----
cat("\n--- Predicting ages ---\n")
age_probs <- cohort_model$predict_age(
  lengths = cohort_data$length,
  sampling_years = cohort_data$year,
  sex = cohort_data$sex
)

# Expected age = weighted mean of age probabilities
age_values <- as.numeric(gsub("age_", "", colnames(age_probs)))
predicted_age <- rowSums(sweep(age_probs, 2, age_values, "*"))

# Mode age = most probable age
predicted_mode <- age_values[apply(age_probs, 1, which.max)]

cohort_data$predicted_age <- predicted_age
cohort_data$predicted_mode <- predicted_mode

# ---- 4. Plot observed vs expected ages ----
cat("\n--- Plotting results ---\n")

png("Test/cohort_alk_diagnostics.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))

# Plot 1: Observed vs Expected age (scatter with jitter)
plot(jitter(cohort_data$age, 0.3), cohort_data$predicted_age,
     xlab = "Observed Age", ylab = "Predicted Age (expected value)",
     main = "Observed vs Predicted Age",
     pch = 16, cex = 0.4, col = rgb(0.2, 0.4, 0.8, 0.3))
abline(0, 1, col = "red", lwd = 2)
legend("topleft", legend = paste("r =", round(cor(cohort_data$age, cohort_data$predicted_age), 3)),
       bty = "n", cex = 1.1)

# Plot 2: Observed vs Mode age (confusion-style)
# Compute proportion table
age_tab <- table(Observed = cohort_data$age, Predicted = cohort_data$predicted_mode)
age_tab_prop <- prop.table(age_tab, margin = 1)  # row proportions
accuracy <- mean(cohort_data$age == cohort_data$predicted_mode)

image(as.numeric(rownames(age_tab_prop)), as.numeric(colnames(age_tab_prop)),
      age_tab_prop, col = hcl.colors(50, "Blues 3", rev = TRUE),
      xlab = "Observed Age", ylab = "Predicted Age (mode)",
      main = paste0("Confusion Matrix (accuracy ", round(accuracy * 100, 1), "%)"))
abline(0, 1, col = "red", lwd = 2)

# Plot 3: Residuals by length, coloured by sex
residuals <- cohort_data$predicted_age - cohort_data$age
cols <- ifelse(cohort_data$sex == "male", rgb(0.2, 0.5, 0.9, 0.3), rgb(0.9, 0.3, 0.3, 0.3))
plot(cohort_data$length, residuals,
     xlab = "Length (cm)", ylab = "Age Residual (predicted - observed)",
     main = "Residuals by Length & Sex",
     pch = 16, cex = 0.4, col = cols)
abline(h = 0, col = "grey40", lty = 2, lwd = 1.5)
legend("topright", legend = c("Male", "Female"), pch = 16,
       col = c(rgb(0.2, 0.5, 0.9, 0.8), rgb(0.9, 0.3, 0.3, 0.8)), bty = "n")

# Plot 4: Mean predicted vs observed age by age class and sex
agg <- aggregate(cbind(predicted_age, age) ~ age + sex, data = cohort_data, mean)
plot(NULL, xlim = range(agg$age), ylim = range(c(agg$predicted_age, agg$age)),
     xlab = "Observed Age", ylab = "Mean Predicted Age",
     main = "Mean Predicted vs Observed by Age & Sex")
abline(0, 1, col = "grey60", lty = 2, lwd = 1.5)
points(agg$age[agg$sex == "male"], agg$predicted_age[agg$sex == "male"],
       pch = 16, col = "steelblue", cex = 1.5)
points(agg$age[agg$sex == "female"], agg$predicted_age[agg$sex == "female"],
       pch = 17, col = "tomato", cex = 1.5)
legend("topleft", legend = c("Male", "Female"), pch = c(16, 17),
       col = c("steelblue", "tomato"), bty = "n")
dev.off()
cat("Plot saved to Test/cohort_alk_diagnostics.png\n")

cat("\nDone. Overall mode accuracy:", round(accuracy * 100, 1), "%\n")
cat("Correlation (expected age):", round(cor(cohort_data$age, cohort_data$predicted_age), 3), "\n")
cat("RMSE:", round(sqrt(mean(residuals^2)), 2), "\n")
