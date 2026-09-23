r.fo <- EstStep()                       # estimation step (FO finishes in seconds)
round(r.fo[["Final Estimates"]], 4)     # 3 THETA, 6 lower-triangular OMEGA, 2 SIGMA
c(OFV = r.fo$Optim$value)
