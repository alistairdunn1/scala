multinomial.n <- deriv(logcv ~ log(sqrt(n * P * (1 - P)) / (n * P)), c("n"), function(n, P) NULL)

plot.multinomial <- function(P, cv, r = 0, xlim, ylim, add = F, pch = 16, lty = 1, cex.multiplier = 1) {
  if (missing(xlim)) xlim <- range(P[!is.na(P) & P > 0])
  if (missing(ylim)) ylim <- range(cv[!is.na(cv) & cv > 0])
  index <- (1:length(P))[(!is.na(P) & P > 0) & (!is.na(cv) & cv > 0)]
  P <- P[index]
  P <- P / Sum(P)
  cv <- cv[index]
  if (!add) plot(P, cv, xlab = "Proportion", ylab = "cv", ylim = ylim, xlim = xlim, type = "n", log = "xy", pch = 16, cex = par()$cex * cex.multiplier)
  points(P, cv, pch = pch, cex = par()$cex * cex.multiplier)
  index <- order(P)
  P <- P[index]
  cv <- cv[index]
  # First run
  fit.multinomial <- nls(logcv ~ multinomial.n(n, P), data = data.frame(logcv = log(cv), P), start = list(n = 5), trace = F)
  multinomial.cv <- sqrt(summary(fit.multinomial)$parameters[1] * P * (1 - P)) / (summary(fit.multinomial)$parameters[1] * P)
  if (r > 0) {
    # Second run
    LST <- (multinomial.cv - cv)^2
    index <- ifelse(LST <= quantile(LST, probs = 1 - r), T, F)
    P <- P[index]
    cv <- cv[index]
    fit.multinomial <- nls(logcv ~ multinomial.n(n, P), data = data.frame(logcv = log(cv), P), start = list(n = 5), trace = F)
    multinomial.cv <- sqrt(summary(fit.multinomial)$parameters[1] * P * (1 - P)) / (summary(fit.multinomial)$parameters[1] * P)
  }
  lines(P, multinomial.cv, lty = lty)
  return("n" = floor(as.vector(summary(fit.multinomial)$parameters[1])))
}

calculateN <- function(data = HAK.age, xlim = c(0.0001, 0.3), ylim = c(0.1, 5), r = 0.05, cex.multiplier = 1) {
  N <- rep(NA, length(data))
  for (i in 1:length(data)) {
    total <- data[[i]]$age[, "total.pooled"]
    total.cv <- data[[i]]$age[, "total.pooled.cv"]
    if (Sum(total) != 0) {
      N[i] <- plot.multinomial(total, total.cv, r, xlim = xlim, ylim = ylim, pch = 16, lty = 1, cex.multiplier = cex.multiplier)
      mtext(side = 3, adj = 0, line = -2.2, text = paste(" ", as.character(data[[i]]$year)), cex = par()$cex * cex.multiplier)
    }
  }
  N <- data.frame("label" = unlist(lapply(data, function(x) x$label)), "N" = N)
  return(N)
}
