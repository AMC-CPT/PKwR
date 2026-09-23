# A CI means not a probability but 'the coverage over repetitions'. Count it directly.
set.seed(20260828)
n <- 12; B <- 2000
cover <- replicate(B, {
  s  <- rnorm(n, mu, sg)                      # repeat, knowing the true mean mu
  ci <- mean(s) + c(-1, 1)*qt(0.975, n - 1)*sd(s)/sqrt(n)
  c(lo = ci[1], hi = ci[2], hit = ci[1] <= mu & mu <= ci[2])
})
round(c(coverage = mean(cover["hit", ]), mean.width = mean(diff(cover[1:2, ]))), 4)

# Plot the first 40 intervals. The ones that miss stand out.
i <- 1:40; hit <- cover["hit", i] == 1
plot(NA, xlim = range(cover[1:2, i]), ylim = c(0, 41), yaxt = "n", las = 1,
     bty = "l", xlab = expression(mu), ylab = "Replicate")
abline(v = mu, lty = 2)
segments(cover[1, i], i, cover[2, i], i, col = ifelse(hit, "gray55", "black"),
         lwd = ifelse(hit, 1, 2))
