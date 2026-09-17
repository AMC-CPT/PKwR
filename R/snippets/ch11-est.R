r.fo <- EstStep()                       # 추정 단계 (FO 는 수 초면 끝난다)
round(r.fo[["Final Estimates"]], 4)     # THETA 3, OMEGA 하삼각 6, SIGMA 2
c(OFV = r.fo$Optim$value)
