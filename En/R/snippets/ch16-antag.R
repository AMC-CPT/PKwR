# Competitive antagonism: apparent EC50 grows (1 + Cb/Kb)-fold (same-site competition)
# Noncompetitive antagonism: EC50 is left unchanged and Emax itself is cut
CA    <- 10^seq(-1, 3, 0.005)                     # agonist concentration (log scale)
ratio <- c(0, 1, 4, 9)                            # Cb/Kb: antagonist conc. in Kb units

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CA, Emax.model(CA, 100, 5), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 112), xlab = "Agonist concentration", ylab = "Effect",
     main = "(a) competitive")
for (r in ratio[-1]) lines(CA, Emax.model(CA, 100, 5*(1 + r)), lty = 2)
abline(h = 50, lty = 3)
text(c(5, 10, 25, 50), 58, labels = ratio, cex = 0.75)
text(0.25, 105, "Cb/Kb", cex = 0.75, pos = 4)

plot(CA, Emax.model(CA, 100, 5), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 112), xlab = "Agonist concentration", ylab = "Effect",
     main = "(b) noncompetitive")
for (f in c(0.75, 0.50, 0.25)) lines(CA, f*Emax.model(CA, 100, 5), lty = 2)
text(300, c(100, 75, 50, 25) + 7, labels = c("1", "0.75", "0.5", "0.25"),
     cex = 0.75)
text(0.25, 105, "Emax ratio", cex = 0.75, pos = 4)

# Apparent EC50 read off the curve (where the effect reaches half of the original Emax)
sapply(ratio, function(r) CA[which.max(Emax.model(CA, 100, 5*(1 + r)) >= 50)])
