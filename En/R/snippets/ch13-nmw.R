# Cross-check with nmw: equivalent to $EST MAXEVAL=0 POSTHOC in NONMEM. Without
# population estimation (EstStep), put the prior into FinalPara, call only PostHocEta.
library(nmw)
e <- get("e", envir = asNamespace("nmw"))
FGD <- deriv(~ DOSE/(TH2*exp(ETA2)) *               # PRED of Chapter 11 as is
               TH1*exp(ETA1)/(TH1*exp(ETA1) - TH3*exp(ETA3)) *
               (exp(-TH3*exp(ETA3)*TIME) - exp(-TH1*exp(ETA1)*TIME)),
             c("ETA1", "ETA2", "ETA3"),
             function.arg = c("TH1", "TH2", "TH3", "ETA1", "ETA2", "ETA3",
                              "DOSE", "TIME"), func = TRUE, hessian = TRUE)
H <- deriv(~ F + F*EPS1 + EPS2, c("EPS1", "EPS2"),
           function.arg = c("F", "EPS1", "EPS2"), func = TRUE)
PRED <- function(THETA, ETA, DATAi) {
  FGDres <- FGD(THETA[1], THETA[2], THETA[3], ETA[1], ETA[2], ETA[3],
                DATAi[, "DOSE"], DATAi[, "TIME"])
  Res <- cbind(FGDres, attr(FGDres, "gradient"),
               attr(H(FGDres, 0, 0), "gradient"))
  colnames(Res) <- c("F", "G1", "G2", "G3", "H1", "H2")
  Res
}

D1 <- data.frame(ID = 1, TIME = t.pt, DV = dv.pt, DOSE = D)
InitStep(D1, THETAinit = TH, OMinit = OM, SGinit = diag(SG),
         LB = rep(0, 3), UB = rep(1e6, 3), Pred = PRED, METHOD = "COND")
e$FinalPara <- c(TH, mat2ltv(OM), SG)               # prior injected, no estimation
round(rbind(optim.5pt = fit5,
            nmw.posthoc = PostHocEta()[1, c("ETA1", "ETA2", "ETA3")]), 4)
