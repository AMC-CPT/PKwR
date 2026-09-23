# Repeated dosing: accumulation factor after the n-th dose and time since the last dose
acc  <- function(t, k, tau) (1 - exp(-(floor(t/tau)+1)*k*tau))/(1 - exp(-k*tau))
tprm <- function(t, tau) t - floor(t/tau)*tau

# half-life 12 hr, dosing interval 12 hr
k <- log(2)/12; tau <- 12
t <- seq(0, 120, 0.05)

Cb <- 50 * acc(t, k, tau) * exp(-k*tprm(t, tau))              # IV bolus q12h
ka <- 0.25
Co <- 65.02*(acc(t, k, tau)*exp(-k*tprm(t, tau)) -
             acc(t, ka, tau)*exp(-ka*tprm(t, tau)))           # oral q12h
Ci <- 75*(1 - exp(-k*t))                                      # constant infusion

plot(t, Cb, type = "l", las = 1, bty = "l", ylim = c(0, 125),
     xlab = "Time (hr)", ylab = "Concentration")
lines(t, Co, lty = 2)
lines(t, Ci, lty = 3, lwd = 2)
legend("bottomright", bty = "n", cex = 0.85, lty = c(1, 2, 3), lwd = c(1, 1, 2),
       legend = c("IV bolus injection", "Oral administration",
                  "Continuous IV infusion"))
mtext("Half-life = 12 hr", side = 3, line = -1.4, adj = 0.97, cex = 0.85)
