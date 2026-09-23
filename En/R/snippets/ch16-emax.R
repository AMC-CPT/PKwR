Emax.model  <- function(C, Emax, EC50) Emax*C/(EC50 + C)
sEmax.model <- function(C, Emax, EC50, H) Emax*C^H/(EC50^H + C^H)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
Cl <- seq(0, 20, 0.05)
plot(Cl, Emax.model(Cl, 100, 5), type = "l", las = 1, bty = "l", ylim = c(0, 125),
     xlab = "Concentration", ylab = "Effect", main = "(a) linear scale")
abline(h = 100, lty = 3); segments(0, 50, 5, 50); segments(5, 0, 5, 50)
text(1.6, 112, "Emax", pos = 4, cex = 0.85)
text(6.5,  22, "EC50", pos = 4, cex = 0.85)

Cg <- 10^seq(log10(0.3), 2, 0.005)
plot(Cg, Emax.model(Cg, 100, 5), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 125), xlab = "Concentration", ylab = "Effect",
     main = "(b) log scale")
abline(h = 100, lty = 3); segments(0.3, 50, 5, 50); segments(5, 0, 5, 50)
text(0.55, 112, "Emax", pos = 4, cex = 0.85)
text(8,     22, "EC50", pos = 4, cex = 0.85)
