# System with a circadian baseline: kin follows a cosine with a 24-hour period
library(deSolve)
kin0 <- 10; kout.b <- 0.5; amp <- 0.30; tpk <- 6    # amplitude 30%, peak at 06:00
Cp.b <- function(t) ifelse(t < 12, 0, 25*exp(-0.20*(t - 12)))   # dosed at 12:00
kin.f <- function(t) kin0*(1 + amp*cos(2*pi*(t - tpk)/24))
dbase <- function(t, y, p) {                        # the drug inhibits kin
  I <- 0.75*Cp.b(t)/(4 + Cp.b(t))
  list(kin.f(t)*(1 - p[["drug"]]*I) - kout.b*y[1])
}
tb  <- seq(0, 48, 0.1)
R0b <- kin0/kout.b
pbo <- lsoda(c(R = R0b), tb, dbase, c(drug = 0))[, "R"]   # placebo (baseline) curve
act <- lsoda(c(R = R0b), tb, dbase, c(drug = 1))[, "R"]   # active-drug curve

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(tb, pbo, type = "l", lty = 3, las = 1, bty = "l", ylim = c(5, 30),
     xlab = "Time (hr)", ylab = "Response", main = "(a) observed")
lines(tb, act); abline(h = R0b, lty = 2, col = "gray60")
legend("bottomleft", bty = "n", cex = 0.75, lty = c(1, 3, 2),
       col = c(1, 1, "gray60"),
       legend = c("active", "placebo", "constant baseline"))

plot(tb, act - R0b, type = "l", lty = 2, las = 1, bty = "l", ylim = c(-14, 9),
     xlab = "Time (hr)", ylab = "Change from baseline",
     main = "(b) baseline correction")
lines(tb, act - pbo); abline(h = 0, lty = 3)
legend("topright", bty = "n", cex = 0.75, lty = c(1, 2),
       legend = c("vs placebo (correct)", "vs constant (biased)"))

# The baseline moves even before dosing (0-12 h): correcting with a constant baseline
# shows an 'effect' where the drug has none, and distorts the nadir's size and time
i.pre <- tb <= 12; i.pos <- tb > 12
round(c(pre.range = diff(range(pbo[i.pre])),
        nadir.vs.const = min(act[i.pos] - R0b),
        nadir.vs.pbo   = min(act[i.pos] - pbo[i.pos]),
        t.nadir.const  = tb[i.pos][which.min(act[i.pos] - R0b)],
        t.nadir.pbo    = tb[i.pos][which.min(act[i.pos] - pbo[i.pos])]), 3)
