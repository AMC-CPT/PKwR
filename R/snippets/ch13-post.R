# MAP 은 점추정이다. 처방을 결정하려면 사후분포의 폭도 함께 보아야 한다.
library(numDeriv)
Hp <- hessian(mapObj, fit5, t = t.pt, y = dv.pt, D = D)
Vp <- 2*solve(Hp)                                   # 사후 공분산 (4장 식 4.5)
round(rbind(eta.hat = fit5, post.SD = sqrt(diag(Vp)),
            prior.SD = sqrt(diag(OM)),
            shrinkage = 1 - sqrt(diag(Vp)/diag(OM))), 4)

# 이후 16 h 시점 농도의 예측: 사후분포에서 eta 를 뽑아 전파한다
set.seed(20260828)
es <- fit5 + t(chol(Vp)) %*% matrix(rnorm(3*4000), nrow = 3)
c16 <- apply(es, 2, function(e) ipred(e, 16, D))
obs16 <- c16*(1 + rnorm(4000, 0, sqrt(SG[["prop"]]))) +
         rnorm(4000, 0, sqrt(SG[["add"]]))
round(rbind(model = c(point = ipred(fit5, 16, D), quantile(c16, c(.025, .975))),
            observed = c(NA, quantile(obs16, c(.025, .975)))), 3)
