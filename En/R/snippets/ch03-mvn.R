# Correlated multivariate normal deviates by Cholesky: z ~ N(0,I) gives Lz ~ N(0, LL')
Om <- matrix(c(0.09, 0.036, 0.036, 0.04), nrow = 2)   # same form as OMEGA in ch. 11
L  <- t(chol(Om))
set.seed(20260828)
eta <- t(L %*% matrix(rnorm(2*20000), nrow = 2))
round(rbind(target = c(var1 = Om[1, 1], var2 = Om[2, 2], corr = cov2cor(Om)[1, 2]),
            sample = c(var(eta[, 1]), var(eta[, 2]), cor(eta[, 1], eta[, 2]))), 4)

# Read as CVs: interindividual variability 31% and 20%, with a correlation of 0.6
round(c(CV1.pct = 100*sqrt(exp(Om[1, 1]) - 1), CV2.pct = 100*sqrt(exp(Om[2, 2]) - 1),
        corr = cov2cor(Om)[1, 2]), 2)
