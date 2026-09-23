# Therapeutic range when the desired effect and toxicity each follow a sigmoid Emax
C <- seq(0.05, 12, 0.01)
Eff <- sEmax.model(C, Emax = 100, EC50 = 2.2, H =  6)
Tox <- sEmax.model(C, Emax = 120, EC50 = 7.0, H = 10)

plot(C, Eff, type = "l", las = 1, bty = "l", ylim = c(0, 165), xaxt = "n",
     yaxt = "n", xlab = "Concentration", ylab = "Effect or Toxicity")
axis(2, at = seq(0, 120, 20), las = 1)
lines(C, Tox, lty = 2)
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 2), horiz = TRUE,
       legend = c("Desired Effect", "Toxicity"))
segments(c(2.5, 5.0), 0, c(2.5, 5.0), 105, lwd = 0.6)
arrows(2.5, 78, 5.0, 78, code = 3, length = 0.06)
text(3.75, 64, "Therapeutic\nRange", cex = 0.8)

# Concentration range with effect >= 90% and toxicity <= 10%
c(lower.E90 = C[which.max(Eff >= 90)], upper.T10 = C[which.max(Tox >= 12)])
