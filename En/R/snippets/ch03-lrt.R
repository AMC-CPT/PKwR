# Likelihood ratio test: how much -2logL falls in a model given one more parameter.
# Check that under a true null hypothesis the decrease follows chi-square with 1 df.
set.seed(20260828)
dev <- replicate(3000, {
  s  <- rnorm(20, 0, 1)                       # true mean is 0 (null hypothesis true)
  m0 <- -2*sum(dnorm(s, 0,       sd = 1, log = TRUE))   # reduced model
  m1 <- -2*sum(dnorm(s, mean(s), sd = 1, log = TRUE))   # full model
  m0 - m1
})
round(c(mean = mean(dev), var = var(dev),      # chi2(1): mean 1, variance 2
        q95.sim = quantile(dev, 0.95), q95.chisq = qchisq(0.95, 1)), 4)

hist(dev, breaks = 40, freq = FALSE, las = 1, col = "gray92", border = "gray60",
     xlim = c(0, 12), main = "", xlab = expression(paste(Delta, (-2*log*L))))
curve(dchisq(x, 1), 0.05, 12, add = TRUE, lwd = 1.4)
abline(v = 3.84, lty = 2)
