# Likelihood: given a parameter value, returns 'how plausible these data would be'
y <- log(x)                                   # on the log scale it is normal
m2ll <- function(p) -2*sum(dnorm(y, p[1], p[2], log = TRUE))
opt  <- optim(c(4, 0.5), m2ll, method = "L-BFGS-B",
              lower = c(0, 0.01), upper = c(10, 5))
round(c(mu.hat = opt$par[1], sd.hat = opt$par[2], m2LL = opt$value,
        mu.true = mu, sd.true = sg), 4)
# The MLEs equal the sample mean and the sample SD (the one divided by n)
round(c(mean.y = mean(y), sd.n = sqrt(mean((y - mean(y))^2))), 4)

# Look at the shape of the likelihood: sharp means precise, flat means not
mg <- seq(4.4, 4.75, 0.001)
pr <- sapply(mg, function(m) m2ll(c(m, opt$par[2])))
plot(mg, pr - min(pr), type = "l", las = 1, bty = "l", ylim = c(0, 12),
     xlab = expression(mu), ylab = expression(paste(-2, log, L, " (above minimum)")))
abline(h = 3.84, lty = 2); abline(v = opt$par[1], lty = 3)
ci <- range(mg[pr - min(pr) <= 3.84])         # likelihood-based 95% CI
segments(ci[1], 0, ci[2], 0, lwd = 3); round(ci, 4)
