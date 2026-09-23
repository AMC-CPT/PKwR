# Two faces of exponential decay: a curve on the original scale, a line on the log scale
k <- 0.25; A <- 100
c(half.life = log(2)/k, quarter.life = log(4)/k, tau95 = log(20)/k)

# Taking the logarithm gives a straight line with slope -k
tt <- seq(0, 20, 4); yy <- A*exp(-k*tt)
round(rbind(t = tt, y = yy, log.y = log(yy)), 3)
round(diff(log(yy))/diff(tt), 4)              # the slope is -k everywhere

# Any base gives a line. Multiply the common-log slope by log(10) = 2.303 to get -k.
round(c(slope.ln = coef(lm(log(yy) ~ tt))[[2]],
        slope.log10 = coef(lm(log10(yy) ~ tt))[[2]],
        converted = coef(lm(log10(yy) ~ tt))[[2]]*log(10), minus.k = -k), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
tg <- seq(0, 20, 0.05)
plot(tg, A*exp(-k*tg), type = "l", las = 1, bty = "l", xlab = "t", ylab = "y",
     main = "(a) original scale")
abline(v = log(2)/k, lty = 3); text(log(2)/k, 90, "half-life", pos = 4, cex = 0.8)
plot(tg, A*exp(-k*tg), type = "l", log = "y", las = 1, bty = "l",
     xlab = "t", ylab = "y", main = "(b) log scale: straight line")
abline(v = log(2)/k, lty = 3)
