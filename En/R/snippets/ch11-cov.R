cov.fo <- CovStep()                     # R^-1 S R^-1 sandwich covariance
round(cov.fo[["Standard Error"]], 4)
ev <- cov.fo[["Eigen Values"]]
c(cond.number = max(ev)/min(ev))        # eigenvalue ratio (corr. matrix) = cond. number
