# Phenytoin-style two-point method: solve Vmax and Km from two (daily dose, Css) pairs
# and back-calculate the dose for the target Css. At steady state dosing rate =
# elimination rate, so DR = Vmax*Css/(Km + Css) holds for both pairs.
dr  <- c(300, 400); css <- c(8, 22)                 # mg/day, mg/L
km   <- (dr[2] - dr[1])/(dr[1]/css[1] - dr[2]/css[2])
vmax <- dr[1]*(km + css[1])/css[1]
d.mm   <- vmax*15/(km + 15)                         # dose for target Css 15
d.prop <- dr[1]*15/css[1]                           # proportional-rule answer
round(c(Km = km, Vmax = vmax, dose.MM = d.mm, dose.prop = d.prop), 1)

DR <- seq(0, 480, 1)                                # dose -> Css curve
plot(DR, km*DR/(vmax - DR), type = "l", las = 1, bty = "l",
     xlim = c(0, 600), ylim = c(0, 40),
     xlab = "Dosing rate (mg/day)", ylab = "Css (mg/L)")
abline(v = vmax, lty = 3); text(vmax, 38, "Vmax", pos = 4, cex = 0.8)
points(dr, css, pch = 16)
points(d.mm, 15, pch = 1, cex = 1.3)
points(d.prop, 15, pch = 4, cex = 1.2)
text(600, 11, "proportional\n(no steady state!)", cex = 0.75, adj = 1)
text(d.mm + 18, 18.5, "MM", cex = 0.8)