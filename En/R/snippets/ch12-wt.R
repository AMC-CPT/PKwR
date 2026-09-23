# Covariate model: V = THETA(2) * (BWT/70)^THETA(4) * exp(eta2)
FGD4 <- deriv(~ DOSE/(TH2*(BWT/70)^TH4*exp(ETA2)) *
                TH1*exp(ETA1)/(TH1*exp(ETA1) - TH3*exp(ETA3)) *
                (exp(-TH3*exp(ETA3)*TIME) - exp(-TH1*exp(ETA1)*TIME)),
              c("ETA1", "ETA2", "ETA3"),
              function.arg = c("TH1", "TH2", "TH3", "TH4", "ETA1", "ETA2",
                               "ETA3", "DOSE", "TIME", "BWT"),
              func = TRUE, hessian = TRUE)
PRED4 <- function(THETA, ETA, DATAi) {
  FGDres <- FGD4(THETA[1], THETA[2], THETA[3], THETA[4],
                 ETA[1], ETA[2], ETA[3],
                 DATAi[, "DOSE"], DATAi[, "TIME"], DATAi[, "BWT"])
  Res <- cbind(FGDres, attr(FGDres, "gradient"),
               attr(H(FGDres, 0, 0), "gradient"))
  colnames(Res) <- c("F", "G1", "G2", "G3", "H1", "H2")
  Res
}
InitStep(DATA, THETAinit = c(2, 50, 0.1, 0.5), OMinit = OMinit,
         SGinit = SGinit, LB = c(0, 0, 0, -5), UB = c(1e6, 1e6, 1e6, 5),
         Pred = PRED4, METHOD = "COND")
r.wt <- EstStep()
round(r.wt[["Final Estimates"]][1:4], 4)

dOFV <- r.foce$Optim$value - r.wt$Optim$value
c(dOFV = round(dOFV, 3), LRT.p = round(1 - pchisq(dOFV, 1), 3))
# Effect size: ratio of V, lightest (54.6 kg) vs heaviest (86.4 kg)
c(V.ratio = round((86.4/54.6)^r.wt[["Final Estimates"]][4], 3))
