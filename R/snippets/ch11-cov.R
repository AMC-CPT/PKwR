cov.fo <- CovStep()                     # R^-1 S R^-1 sandwich 공분산
round(cov.fo[["Standard Error"]], 4)
ev <- cov.fo[["Eigen Values"]]
c(cond.number = max(ev)/min(ev))        # 상관행렬 고유값 비 = 조건수
