# Every figure in this book uses base R graphics. Three panels show most of the grammar.
set.seed(20260828)
tt <- c(0.5, 1, 2, 4, 6, 8, 12)
obs <- round(decay(tt)*exp(rnorm(length(tt), 0, 0.12)), 2)

# cex: with three panels the default text becomes small
par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8), cex = 0.85)
plot(tt, obs, las = 1, bty = "l", pch = 16, xlab = "t", ylab = "y",
     main = "(a) Points and a curve")         # las: tick label direction, bty: box type
curve(decay(x), 0, 12, add = TRUE, lty = 2)   # add = TRUE overlays on the plot
legend("topright", bty = "n", cex = 0.85, pch = c(16, NA), lty = c(NA, 2),
       legend = c("observed", "model"))

plot(tt, obs, log = "y", las = 1, bty = "l", pch = 16, xlab = "t", ylab = "y",
     main = "(b) Log axis")                   # log = "y": one letter for semilog
curve(decay(x), 0.3, 12, add = TRUE, lty = 2)

ks <- c(0.15, 0.25, 0.40)                     # several curves: matplot is convenient
tg <- seq(0, 12, 0.1)
matplot(tg, sapply(ks, function(k) decay(tg, k = k)), type = "l", lty = 1:3,
        col = 1, las = 1, bty = "l", xlab = "t", ylab = "y", main = "(c) Many curves")
abline(h = 20, lty = 3)                       # reference line
text(1.6, 26, "y = 20", cex = 0.85)
legend("topright", bty = "n", cex = 0.85, lty = 1:3, legend = paste("k =", ks))
