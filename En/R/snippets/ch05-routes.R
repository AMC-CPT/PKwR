t <- seq(0, 24, 0.05)
par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8))

plot(t, Civ(t, D = 1, V = 1, k = 0.22), type = "l", las = 1, bty = "l",
     xlab = "Time", ylab = "Concentration", main = "(a) IV bolus injection",
     yaxt = "n")
plot(t, Cinf(t, R0 = 0.22, V = 1, k = 0.22), type = "l", las = 1, bty = "l",
     xlab = "Time", ylab = "Concentration", main = "(b) IV constant infusion",
     yaxt = "n")
plot(t, Cpo(t, D = 1, V = 1, k = 0.22, ka = 1.1), type = "l", las = 1, bty = "l",
     xlab = "Time", ylab = "Concentration", main = "(c) Oral administration",
     yaxt = "n")
