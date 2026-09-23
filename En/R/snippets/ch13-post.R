# MAP is a point estimate. A dosing decision also needs the width of the posterior.
library(numDeriv)
Hp <- hessian(mapObj, fit5, t = t.pt, y = dv.pt, D = D)
Vp <- 2*solve(Hp)                                   # posterior cov. (Ch. 4, Eq. 4.5)
round(rbind(eta.hat = fit5, post.SD = sqrt(diag(Vp)),
            prior.SD = sqrt(diag(OM)),
            shrinkage = 1 - sqrt(diag(Vp)/diag(OM))), 4)

# Predicting the concentration at 16 h: draw eta from the posterior and propagate.
set.seed(20260828)
es <- fit5 + t(chol(Vp)) %*% matrix(rnorm(3*4000), nrow = 3)
c16 <- apply(es, 2, function(e) ipred(e, 16, D))
obs16 <- c16*(1 + rnorm(4000, 0, sqrt(SG[["prop"]]))) +
         rnorm(4000, 0, sqrt(SG[["add"]]))
round(rbind(model = c(point = ipred(fit5, 16, D), quantile(c16, c(.025, .975))),
            observed = c(NA, quantile(obs16, c(.025, .975)))), 3)
