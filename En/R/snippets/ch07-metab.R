# Metabolite: formation is driven by the parent amount, elimination by its own amount.
# Same form as the Bateman equation of oral absorption (formation <-> absorption).
kp <- 0.35; Vp <- 20; Dp <- 200; fm <- 0.6; Vm <- 20   # parent t1/2 = 2 hr
Cp.t <- function(t) Dp/Vp*exp(-kp*t)
Cm.t <- function(t, km)
  fm*kp*Dp/(Vm*(km - kp))*(exp(-kp*t) - exp(-km*t))

km.f <- 4*kp; km.e <- kp/4               # formation-limited / elimination-limited
t <- seq(0.05, 48, 0.05)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(t, Cp.t(t), type = "l", log = "y", las = 1, bty = "l", ylim = c(0.01, 12),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)",
     main = "(a) formation-limited (km > kp)")
lines(t, Cm.t(t, km.f), lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("parent", "metabolite"))

plot(t, Cp.t(t), type = "l", log = "y", las = 1, bty = "l", ylim = c(0.01, 12),
     xlab = "Time (hr)", ylab = "Concentration (mg/L)",
     main = "(b) elimination-limited (km < kp)")
lines(t, Cm.t(t, km.e), lty = 2)

# Terminal slope of the metabolite: follows the parent's kp in (a), its own km in (b)
slope <- function(km, t1, t2) { tt <- seq(t1, t2, 0.5)
  -coef(lm(log(Cm.t(tt, km)) ~ tt))[[2]] }
round(c(kp = kp, slope.a = slope(km.f, 12, 24),
        km.e = km.e, slope.b = slope(km.e, 36, 48)), 4)

# Check analytic AUC ratio AUCm/AUCp = fm CLp/CLm by numerical integration (case (a))
round(c(formula = fm*(kp*Vp)/(km.f*Vm),
        numeric = integrate(Cm.t, 0, Inf, km = km.f)$value /
                  integrate(Cp.t, 0, Inf)$value), 4)
