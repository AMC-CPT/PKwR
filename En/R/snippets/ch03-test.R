# Two errors and power: error arises where null and alternative distributions overlap
n3 <- 16; se <- sqrt(2/n3); d <- 0.8         # two groups: true diff 0.8, its SE 0.354
cv <- qnorm(0.975)*se                         # two-sided 5% critical value
round(c(SE = se, crit = cv, power = 1 - pnorm(cv, d, se) + pnorm(-cv, d, se)), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
xg <- seq(-1.2, 2.2, 0.005)
plot(xg, dnorm(xg, 0, se), type = "l", las = 1, bty = "l", ylim = c(0, 1.75),
     xlab = "Observed difference", ylab = "Density", main = "(a) Overlap of the two")
lines(xg, dnorm(xg, d, se), lty = 2)
polygon(c(xg[xg > cv], rev(xg[xg > cv])), c(dnorm(xg[xg > cv], 0, se),
        rep(0, sum(xg > cv))), col = "gray80", border = NA)     # type I error
polygon(c(xg[xg < cv], rev(xg[xg < cv])), c(dnorm(xg[xg < cv], d, se),
        rep(0, sum(xg < cv))), density = 12, angle = 45, border = NA)  # type II
abline(v = cv, lty = 3)
legend("topleft", bty = "n", cex = 0.75, lty = c(1, 2),
       legend = c("Null (difference 0)", "Alternative (difference 0.8)"))

ng <- 4:40                                    # power curve against sample size
pw <- sapply(ng, function(k) { s <- sqrt(2/k); c <- qnorm(0.975)*s
                               1 - pnorm(c, d, s) + pnorm(-c, d, s) })
plot(ng, pw, type = "l", las = 1, bty = "l", ylim = c(0, 1),
     xlab = "Sample size per group", ylab = "Power", main = "(b) Power curve")
abline(h = 0.8, lty = 2); abline(v = ng[which.max(pw >= 0.8)], lty = 3)
c(n.for.80pct = ng[which.max(pw >= 0.8)])
