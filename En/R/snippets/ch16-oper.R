# Black-Leff operational model: occupancy (Kd) and transduction (tau) are separated.
# tau = Rtot/KE is 'how strongly this tissue amplifies occupancy into response'.
oper <- function(C, Em, Kd, tau) Em*tau*C/(Kd + (1 + tau)*C)

# The two observed quantities have closed forms as functions of tau
tau  <- c(0.1, 0.3, 1, 3, 10, 30)
Kd   <- 100                                    # nM, the same in every column
round(rbind(tau         = tau,
            Emax.obs    = 100*tau/(1 + tau),   # with Em = 100
            EC50        = Kd/(1 + tau),
            EC50.over.Kd = 1/(1 + tau),
            occ.at.EC50 = 1/(2 + tau)), 3)     # occupancy at EC50

# Same Kd: large tau gives a full agonist (high, left), small a partial (low, right)
CC <- 10^seq(0, 4, 0.01)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(CC, oper(CC, 100, Kd, tau[1]), type = "n", log = "x", las = 1, bty = "l",
     ylim = c(0, 108), xlab = "Agonist (nM)", ylab = "Effect",
     main = "(a) one Kd, six tau")
for (i in seq_along(tau)) lines(CC, oper(CC, 100, Kd, tau[i]),
                                lty = if (i %% 2) 1 else 2)
abline(v = Kd, lty = 3)
text(Kd, 5, "Kd", cex = 0.75, pos = 4)

# One curve cannot distinguish (Em, Kd, tau): one degree of freedom remains
p1   <- c(Em = 100, Kd = 100, tau = 3)
Ec   <- p1[["Kd"]]/(1 + p1[["tau"]])           # observed EC50
Eo   <- p1[["Em"]]*p1[["tau"]]/(1 + p1[["tau"]])  # observed Emax
t2   <- 9                                      # raise tau threefold,
p2   <- c(Em = Eo*(1 + t2)/t2,                 # adjust Em and Kd back to match,
          Kd = Ec*(1 + t2), tau = t2)          # and the curves overlap completely
round(p2, 3)
y1 <- oper(CC, p1[["Em"]], p1[["Kd"]], p1[["tau"]])
y2 <- oper(CC, p2[["Em"]], p2[["Kd"]], p2[["tau"]])
round(c(max.abs.diff  = max(abs(y1 - y2)),
        occ.at.EC50.1 = Ec/(p1[["Kd"]] + Ec),
        occ.at.EC50.2 = Ec/(p2[["Kd"]] + Ec)), 6)

# Fitting the Emax model yields EC50 and Emax, but neither Kd nor tau
round(coef(nls(y1 ~ Emx*CC/(E50 + CC), start = c(Emx = 90, E50 = 30),
               control = nls.control(scaleOffset = 1))), 3)

# A partial agonist behaves like an antagonist in the presence of a full agonist
both <- function(A, P, Em = 100, KA = 100, KP = 100, tA = 10, tP = 0.5) {
  a <- A/KA; p <- P/KP
  Em*(tA*a + tP*p)/(1 + a + p + tA*a + tP*p)
}
Pv <- c(0, 100, 1000, 10000)
plot(CC, both(CC, 0), type = "l", log = "x", las = 1, bty = "l",
     ylim = c(0, 108), xlab = "Full agonist (nM)", ylab = "Effect",
     main = "(b) partial agonist added")
for (P in Pv[-1]) lines(CC, both(CC, P), lty = 2)
text(1.5, sapply(Pv, function(P) both(1, P)) + 4, labels = Pv, cex = 0.7)
legend("bottomright", "partial agonist (nM)", bty = "n", cex = 0.75)

# Floor (no full agonist) and ceiling (a large excess of full agonist) together
round(rbind(P = Pv,
            floor = sapply(Pv, function(P) both(0, P)),
            top   = sapply(Pv, function(P) both(1e5, P))), 2)
