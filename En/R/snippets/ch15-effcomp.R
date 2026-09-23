# Effect compartment: dCe/dt = ke0 (Cp - Ce).  Closed form for a 1-compartment IV bolus
Cp <- function(t, C0 = 10, k = 0.2) C0*exp(-k*t)
Ce <- function(t, C0 = 10, k = 0.2, ke0 = 0.05)
  C0*ke0/(ke0 - k)*(exp(-k*t) - exp(-ke0*t))
E  <- function(C, Emax = 100, EC50 = 2) Emax*C/(EC50 + C)

t <- seq(0, 72, 0.05)
round(c(tmax.Ce.grid = t[which.max(Ce(t))],           # time of the effect peak
        tmax.Ce.analytic = log(0.2/0.05)/(0.2 - 0.05)), 2)

par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
plot(t, Cp(t), type = "l", las = 1, bty = "l", xlim = c(0, 48),
     xlab = "Time (hr)", ylab = "Concentration", main = "(a) time course")
lines(t, Ce(t), lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = 1:2,
       legend = c("plasma (Cp)", "effect site (Ce)"))

plot(Cp(t), E(Ce(t)), type = "l", las = 1, bty = "l",
     xlab = "Concentration", ylab = "Effect (%)", main = "(b) hysteresis loop")
lines(Ce(t), E(Ce(t)), lty = 2, lwd = 1.6)
i <- c(41, 401)                                       # direction arrows (t = 2, 20)
arrows(Cp(t)[i], E(Ce(t))[i], Cp(t)[i + 12], E(Ce(t))[i + 12], length = 0.07)
legend(2.6, 27, bty = "n", cex = 0.85, lty = c(1, 2), lwd = c(1, 1.6),
       legend = c("E vs Cp (loop)", "E vs Ce (collapsed)"))
