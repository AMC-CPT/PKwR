# FOCE-I: only METHOD changes. Each iteration searches for the individual EBEs,
# so it is much slower than FO (a little over a minute on these data).
InitStep(DATA, THETAinit = THETAinit, OMinit = OMinit, SGinit = SGinit,
         LB = rep(0, 3), UB = rep(1e6, 3), Pred = PRED, METHOD = "COND")
r.foce <- EstStep()
FE <- r.foce[["Final Estimates"]]
round(FE, 4)
c(OFV.FO = r.fo$Optim$value, OFV.FOCEI = r.foce$Optim$value)

# Lower-triangular (row-major) vector -> symmetric OMEGA, and interpretable summaries
utri <- matrix(0, 3, 3)
utri[upper.tri(utri, diag = TRUE)] <- FE[4:9]
OM <- utri + t(utri) - diag(diag(utri))
SG <- diag(FE[10:11]); TH <- FE[1:3]
round(100*sqrt(exp(diag(OM)) - 1), 1)         # IIV CV% (log-normal)
round(cov2cor(OM), 3)                         # correlations among the etas
