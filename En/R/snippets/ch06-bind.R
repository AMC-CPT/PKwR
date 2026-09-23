# Saturable protein binding: free conc. is dose-proportional, total conc. is not.
Bmax <- 100; Kdb <- 15                            # binding sites (mg/L) and Kd
CLintb <- 2                                       # CLint of low-extraction drug, L/hr
Ctot <- function(Cf) Cf + Bmax*Cf/(Kdb + Cf)      # total = free + bound
R0b   <- c(1, 2, 5, 10, 20, 40)                   # constant infusion rates (mg/hr)
Cfree <- R0b/CLintb                               # steady state: clears free drug only
round(rbind(rate = R0b, C.free = Cfree, C.total = Ctot(Cfree),
            fu = Cfree/Ctot(Cfree),
            total.per.rate = Ctot(Cfree)/R0b,
            free.per.rate = Cfree/R0b), 4)

Rg <- seq(0, 45, 0.1); Cg <- Rg/CLintb
plot(Rg, Ctot(Cg), type = "l", las = 1, bty = "l", xlab = "Infusion rate (mg/hr)",
     ylab = "Steady-state concentration (mg/L)", ylim = c(0, 130))
lines(Rg, Cg, lty = 2)
lines(Rg, Ctot(Cg[Rg == 5])/5*Rg, lty = 3, col = "gray55")   # low-dose proportionality
legend("topleft", bty = "n", cex = 0.85, lty = c(1, 2, 3),
       col = c(1, 1, "gray55"), legend = c("Total", "Unbound", "Low-dose line"))
