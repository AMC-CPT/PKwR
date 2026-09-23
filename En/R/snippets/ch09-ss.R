# In a linear system the steady-state curve is the superposition of single-dose curves
tau <- 12                                      # dosing interval (hr)
Css <- function(t, n = 40) {                   # concentration after n doses
  s <- (0:(n - 1))*tau                         # dosing times
  sapply(t, function(u) sum(C2iv(u - s[s <= u])))
}
tss <- c(0, 5/60, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12)  # last-interval sampling
t40 <- 39*tau                                  # time of the last (40th) dose
Cs  <- Css(t40 + tss)                          # tss=0 post-dose, 12 next pre-dose

# Metrics of one dosing interval
AUCtau  <- auc.ld(tss, Cs)
Cmax.ss <- max(Cs); Cmin.ss <- min(Cs); Cav <- AUCtau/tau
round(c(AUCtau = AUCtau, Cmax.ss = Cmax.ss, Cmin.ss = Cmin.ss, Cav = Cav,
        PTF.pct = (Cmax.ss - Cmin.ss)/Cav*100, swing = (Cmax.ss - Cmin.ss)/Cmin.ss), 4)

# Identity of a linear system: AUC(tau, ss) = AUC(0-inf, single) = D/CL
round(c(AUCtau.ss = AUCtau, AUCinf.single = integrate(C2iv, 0, Inf)$value,
        D.over.CL = D/CL), 4)

# More than one accumulation ratio: one per metric; the 1-cpt formula matches none
round(c(Rac.AUC  = AUCtau/auc.ld(tss, C2iv(tss)),
        Rac.Cmax = Cmax.ss/C2iv(0),
        Rac.Cmin = Cmin.ss/C2iv(tau),
        onecomp  = 1/(1 - exp(-lam*tau))), 4)

# Same with NonCompart: SS = TRUE turns off extrapolation to infinity, CL = D/AUCtau
library(NonCompart)
r.ss <- sNCA(tss, Cs, dose = D, adm = "Bolus", down = "Log", SS = TRUE, tau = tau,
             doseUnit = "mg", concUnit = "mg/L")
round(r.ss[c("CMAX", "TMAX", "CLST", "AUCLST", "LAMZHL", "CLO", "VZO", "VSSO")], 4)

# Partial AUC: specify the intervals directly (cross-check: they must sum to AUCtau)
iA <- data.frame(Name = c("AUC[0-2h]", "AUC[2-12h]"), Start = c(0, 2), End = c(2, 12))
r.p <- sNCA(tss, Cs, dose = D, adm = "Bolus", down = "Log", SS = TRUE, tau = tau,
            iAUC = iA)
round(r.p[c("AUC[0-2h]", "AUC[2-12h]", "AUCLST")], 4)
