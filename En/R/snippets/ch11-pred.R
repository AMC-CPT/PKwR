# PRED must return the prediction F together with G (dF/d.eta) and H (dY/d.eps).
# R's built-in deriv() builds it by symbolic differentiation (hessian is for LAPL).
FGD <- deriv(~ DOSE/(TH2*exp(ETA2)) * TH1*exp(ETA1)/(TH1*exp(ETA1) - TH3*exp(ETA3)) *
               (exp(-TH3*exp(ETA3)*TIME) - exp(-TH1*exp(ETA1)*TIME)),
             c("ETA1", "ETA2", "ETA3"),
             function.arg = c("TH1", "TH2", "TH3", "ETA1", "ETA2", "ETA3",
                              "DOSE", "TIME"),
             func = TRUE, hessian = TRUE)
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
