par(mfrow = c(1, 2), mar = c(4.2, 5.6, 2.4, 0.8))

# (a) log-log scatter plot, fitted power model line, and the slope-1 reference line
plot(AUC ~ Dose, data = pk, log = "xy", las = 1, bty = "l", pch = 1, cex = 0.7,
     xlab = "Dose (mg)", ylab = "", main = "(a) power model")
title(ylab = "AUClast (ug*h/L)", line = 4.2)
D.grid <- exp(seq(log(90), log(1000), 0.02))
lines(D.grid, exp(fixef(fit)[1] + fixef(fit)[2]*log(D.grid)))
lines(D.grid, exp(fixef(fit)[1] +               log(D.grid)), lty = 2)
legend("bottomright", bty = "n", cex = 0.8, lty = c(1, 2),
       legend = c(sprintf("fitted slope = %.3f", fixef(fit)[2]), "slope = 1"))

# (b) dose-normalized AUC: linear only if there is no trend with dose.
boxplot(AUC.dn ~ Dose, data = pk, las = 1, bty = "l", xlab = "Dose (mg)",
        ylab = "", main = "(b) dose-normalized")
title(ylab = "AUClast / Dose", line = 4.2)
