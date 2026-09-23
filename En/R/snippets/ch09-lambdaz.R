# Terminal rate constant: from the last 3 points add points, pick max adjusted R^2
lambdaz <- function(t, C) {
  n <- length(t)
  fit <- sapply(3:(n - 1), function(k) {         # use the last k points
    i <- (n - k + 1):n
    f <- lm(log(C[i]) ~ t[i])
    c(n = k, lambda.z = -coef(f)[[2]], R2adj = summary(f)$adj.r.squared)
  })
  fit[, which.max(fit["R2adj", ])]
}
lz <- lambdaz(dat2$Time, dat2$DV)
round(c(lz, t.half.z = log(2)/lz[["lambda.z"]]), 4)

# Mark the points used and overlay the regression line for a visual check
used <- seq(nrow(dat2) - lz[["n"]] + 1, nrow(dat2))
plot(dat2$Time, dat2$DV, log = "y", las = 1, bty = "l", pch = 1, cex = 0.9,
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
points(dat2$Time[used], dat2$DV[used], pch = 16, cex = 0.9)
C.at <- exp(predict(lm(log(DV) ~ Time, dat2[used, ]),
                    newdata = data.frame(Time = c(0, 48))))
lines(c(0, 48), C.at, lty = 2)
legend("topright", bty = "n", cex = 0.85, pch = c(16, 1),
       legend = c("used for lambda.z", "not used"))
