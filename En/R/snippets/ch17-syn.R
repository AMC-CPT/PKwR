# Quantifying combinations: Loewe additivity regards the two drugs as 'dilutions of
# each other'. A combination giving the same effect is additive if CA/EA + CB/EB = 1;
# adding an interaction term psi gives synergy (psi > 0) and antagonism (psi < 0).
EA <- 4; EB <- 10                                    # conc. giving target effect alone
iso <- function(CA, psi) {                           # isobole of the target effect
  b <- 1 - CA/EA
  EB*b/(1 + psi*CA/EA)
}
CAg <- seq(0, EA, length.out = 200)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CAg, iso(CAg, 0), type = "l", las = 1, bty = "l", ylim = c(0, EB),
     xlab = "Drug A (mg/L)", ylab = "Drug B (mg/L)", main = "(a) isobologram")
lines(CAg, iso(CAg,  3), lty = 2)
lines(CAg, iso(CAg, -0.6), lty = 4)
legend("topright", bty = "n", cex = 0.8, lty = c(2, 1, 4),
       legend = c("synergy", "additivity", "antagonism"))

# Response surface: Emax-model effect of the additive combination (sum in effect units)
Esurf <- function(a, b, psi) {
  z <- a/EA + b/EB + psi*a*b/(EA*EB)                 # total amount in additive units
  100*z/(1 + z)                                      # target effect 50 as reference
}
ag <- seq(0, 8, length.out = 60); bg <- seq(0, 20, length.out = 60)
contour(ag, bg, outer(ag, bg, Esurf, psi = 3), las = 1, bty = "l",
        levels = c(20, 35, 50, 65, 80), labcex = 0.8,
        xlab = "Drug A (mg/L)", ylab = "Drug B (mg/L)",
        main = "(b) response surface (synergy)")
contour(ag, bg, outer(ag, bg, Esurf, psi = 0), levels = 50, lty = 2,
        drawlabels = FALSE, add = TRUE)

# Gain over the additivity reference: effect of mixing half of A and half of B
round(c(A.alone = Esurf(EA, 0, 0), B.alone = Esurf(0, EB, 0),
        half.additive = Esurf(EA/2, EB/2, 0),
        half.synergy  = Esurf(EA/2, EB/2, 3),
        half.antagon  = Esurf(EA/2, EB/2, -0.6)), 2)
