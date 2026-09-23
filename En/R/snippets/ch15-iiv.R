# Interindividual variability: distance between curves; intraindividual (residual)
# variability: scatter about each curve.  IV bolus, n = 8, log-normal CL, CV about 36%
TVCL <- 4; TVV <- 50; D <- 200                    # typical values (L/hr, L, mg)
CLi  <- TVCL*exp(rnorm(8, 0, 0.35))               # individual clearances
t.ob <- c(0.5, 1, 2, 4, 8, 12, 24)                # sampling times

tt <- seq(0, 24, 0.1)
plot(NA, xlim = c(0, 24), ylim = c(0.05, 6), log = "y", las = 1, bty = "l",
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
for (i in 1:8) {
  ki <- CLi[i]/TVV
  lines(tt, D/TVV*exp(-ki*tt), col = "gray55")
  points(t.ob, D/TVV*exp(-ki*t.ob)*exp(rnorm(7, 0, 0.1)), pch = 16, cex = 0.55)
}
round(sort(CLi), 2)                               # the 8 clearances (L/hr)
