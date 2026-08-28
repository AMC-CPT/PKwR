e1 <- r1$Est["PE", ]; e2 <- r2$Est["PE", ]
C1hat <- function(t) e1[["C0"]]*exp(-e1[["k"]]*t)
C2hat <- function(t) e2[["A"]]*exp(-e2[["alpha"]]*t) + e2[["B"]]*exp(-e2[["beta"]]*t)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(dat2$Time, dat2$DV, log = "y", las = 1, bty = "l", pch = 16, cex = 0.7,
     xlab = "Time (hr)", ylab = "Concentration (mg/L)", main = "(a) fit")
curve(C2hat(x), 0.02, 48, add = TRUE)
curve(C1hat(x), 0.02, 48, add = TRUE, lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("2-compartment", "1-compartment"))

plot(t, (dat2$DV - C1hat(t))/C1hat(t), las = 1, bty = "l", pch = 1,
     ylim = c(-0.8, 0.8), xlab = "Time (hr)", ylab = "Proportional residual",
     main = "(b) residuals")
points(t, (dat2$DV - C2hat(t))/C2hat(t), pch = 16)
abline(h = 0, lty = 3)
legend("bottomright", bty = "n", cex = 0.85, pch = c(16, 1),
       legend = c("2-compartment", "1-compartment"))
