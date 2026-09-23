# Henderson-Hasselbalch: un-ionized fraction as a function of pH
fnon.acid <- function(pH, pKa) 1/(1 + 10^(pH - pKa))    # weak acid
fnon.base <- function(pH, pKa) 1/(1 + 10^(pKa - pH))    # weak base
pH <- seq(1, 9, 0.02)
plot(pH, fnon.acid(pH, 3.5), type = "l", las = 1, bty = "l", ylim = c(0, 1),
     xlab = "pH", ylab = "Un-ionized fraction")
lines(pH, fnon.base(pH, 9.0), lty = 2)
abline(v = c(1.5, 6.5, 7.4), lty = 3)
text(c(1.5, 6.5, 7.4), 1.04, c("stomach", "gut", "plasma"), cex = 0.75, xpd = TRUE)
legend("right", bty = "n", cex = 0.85, lty = 1:2,
       legend = c("weak acid (pKa 3.5)", "weak base (pKa 9.0)"))

# Ion trapping: only un-ionized drug crosses; total conc. builds up on the ionized side
ratio.base <- function(pH1, pH2, pKa) (1 + 10^(pKa - pH1))/(1 + 10^(pKa - pH2))
round(c(milk.vs.plasma  = ratio.base(7.0, 7.4, 8.0),     # milk pH 7.0
        acidic.urine    = ratio.base(5.0, 7.4, 9.0)), 1) # acidic urine pH 5
