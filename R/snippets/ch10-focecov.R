cov.foce <- CovStep()
SE <- cov.foce[["Standard Error"]]
round(rbind(PE = FE, SE = SE, `RSE%` = 100*SE/abs(FE))[, 1:3], 4)  # THETA
ev <- cov.foce[["Eigen Values"]]
c(cond.number = round(max(ev)/min(ev)))
