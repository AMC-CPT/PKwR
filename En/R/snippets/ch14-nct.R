# Do by hand the calculation the packages did for us. The power of TOST is the
# probability of passing both one-sided tests; each statistic is noncentral t.
# Noncentrality = (limit - true log difference)/SE; SE = sqrt(MSE/n), n per sequence.
pwrNC <- function(n, CV, GMR = 1, alpha = 0.05, lo = 0.80, hi = 1.25) {
  se <- sqrt(log(1 + CV^2)/n); df <- 2*(n - 1)
  T0 <- qt(1 - alpha, df);     d  <- log(GMR)
  pt(-T0, df, ncp = (log(lo) - d)/se) - pt(T0, df, ncp = (log(hi) - d)/se)
}
ssNC <- function(CV, GMR = 1, power = 0.8) {
  n <- 2; while (pwrNC(n, CV, GMR) <= power) n <- n + 1; n }

# Sample size table for a true ratio of exactly 1 (subjects per sequence)
cvv <- seq(0.20, 0.40, 0.02)
round(t(sapply(cvv, function(v) { k <- ssNC(v)
  c(CV = v, MSE = log(1 + v^2), N.per.seq = k, power = pwrNC(k, v),
    T0 = qt(0.95, 2*(k - 1))) })), 5)

# If the true ratio is not 1, the noncentrality numerators shift by that much.
# Compare hand calculation and packages at this chapter's GMR 0.95, CV 25%, N = 28.
signif(c(hand.nct        = pwrNC(14, 0.25, 0.95),
         PowerTOST.nct   = power.TOST(CV = 0.25, theta0 = 0.95, n = 28,
                                      method = "nct"),
         PowerTOST.exact = power.TOST(CV = 0.25, theta0 = 0.95, n = 28),
         shifted.central = power.TOST(CV = 0.25, theta0 = 0.95, n = 28,
                                      method = "shifted")), 8)

# The approximation fails where the sample is small and the variability large
round(t(sapply(c(8, 12, 20, 40), function(N)
  c(n = N, nct = pwrNC(N/2, 0.40, 0.95),
    exact = power.TOST(CV = 0.40, theta0 = 0.95, n = N),
    shifted = power.TOST(CV = 0.40, theta0 = 0.95, n = N,
                         method = "shifted")))), 5)
