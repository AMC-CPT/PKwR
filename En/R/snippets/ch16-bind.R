# Saturation binding experiment: total = specific (saturable) + nonspecific (linear)
set.seed(20260828)
Lg <- c(0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256)      # free ligand (nM)
Bm.t <- 2.5; Kd.t <- 8; ns.t <- 0.004               # true values
dB <- data.frame(Lg = Lg, DV = round((Bm.t*Lg/(Kd.t + Lg) + ns.t*Lg)*
                                     exp(rnorm(length(Lg), 0, 0.05)), 4))

library(wnl)                                        # nonlinear regression (Chapter 7)
fB <- function(TH) TH[1]*dB$Lg/(TH[2] + dB$Lg) + TH[3]*dB$Lg
rB <- nlr(fB, dB, pNames = c("Bmax", "Kd", "NS"), IE = c(2, 10, 0.01),
          LB = c(0.1, 0.1, 0), UB = c(20, 500, 1), Error = "P")
round(rB$Est[c("PE", "RSE"), ], 4)

# The Scatchard plot (B/L vs B) is not linear until nonspecific binding is removed
Bsp <- dB$DV - rB$Est["PE", "NS"]*dB$Lg             # keep specific binding only
sc  <- coef(lm(I(Bsp/dB$Lg) ~ Bsp))                 # slope = -1/Kd
round(c(Kd.scatchard = -1/sc[[2]], Bmax.scatchard = -sc[[1]]/sc[[2]],
        Kd.nls = rB$Est["PE", "Kd"], Bmax.nls = rB$Est["PE", "Bmax"]), 3)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(dB$Lg, dB$DV, log = "x", las = 1, bty = "l", pch = 16, ylim = c(0, 3.6),
     xlab = "Free ligand (nM)", ylab = "Bound", main = "(a) saturation")
curve(rB$Est["PE", "Bmax"]*x/(rB$Est["PE", "Kd"] + x) +
      rB$Est["PE", "NS"]*x, 0.3, 300, add = TRUE)
curve(rB$Est["PE", "Bmax"]*x/(rB$Est["PE", "Kd"] + x), 0.3, 300,
      add = TRUE, lty = 2)
curve(rB$Est["PE", "NS"]*x, 0.3, 300, add = TRUE, lty = 3)
legend("topleft", bty = "n", cex = 0.75, lty = 1:3,
       legend = c("total", "specific", "nonspecific"))

plot(Bsp, Bsp/dB$Lg, las = 1, bty = "l", pch = 16, xlim = c(0, 2.6),
     xlab = "Bound (specific)", ylab = "Bound / Free", main = "(b) Scatchard")
abline(sc, lty = 2)
points(dB$DV, dB$DV/dB$Lg, pch = 1)                 # bends if NS is not removed
legend("topright", bty = "n", cex = 0.75, pch = c(16, 1),
       legend = c("NS-corrected", "uncorrected"))

# Displacement study: Ki from the IC50 at labeled ligand conc. L* (Cheng-Prusoff)
Lst <- 5                                            # labeled ligand 5 nM
round(c(IC50 = 30, Ki = 30/(1 + Lst/Kd.t)), 3)
