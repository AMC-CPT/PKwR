# The main distribution of this book is the log-normal, not the normal: concentrations
# and parameters are positive and right-skewed. Taking the log makes them normal.
mu <- log(100); sg <- 0.3                     # mean and SD on the log scale
c(geo.mean = exp(mu), arith.mean = exp(mu + sg^2/2),
  median = exp(mu), CV = sqrt(exp(sg^2) - 1))

par(mfrow = c(1, 2), mar = c(4.2, 5.0, 2.4, 0.8))
xg <- seq(20, 260, 0.5)
plot(xg, dlnorm(xg, mu, sg), type = "l", las = 1, bty = "l",
     xlab = "x", ylab = "Density", main = "(a) Log-normal")
abline(v = c(exp(mu), exp(mu + sg^2/2)), lty = c(3, 2))
legend("topright", bty = "n", cex = 0.8, lty = c(3, 2),
       legend = c("Geometric mean = median", "Arithmetic mean"))
plot(log(xg), dnorm(log(xg), mu, sg), type = "l", las = 1, bty = "l",
     xlab = "log x", ylab = "Density", main = "(b) Normal after taking logs")
abline(v = mu, lty = 3)
