# R names distribution functions by role: d density, p cumulative, q quantile, r random
round(c(d = dnorm(1.96), p = pnorm(1.96), q = qnorm(0.975)), 4)
set.seed(20260828); round(rnorm(3), 4)

# The log-normal is the exponential of the normal. Both routes give the same value.
round(c(via.lnorm = plnorm(120, mu, sg), via.norm = pnorm(log(120), mu, sg)), 6)

# All three from the normal: chi2 = sum of squares, t = Z/sqrt(V/k), F = ratio of two
set.seed(20260828); B <- 20000; k1 <- 5; k2 <- 9
Z <- rnorm(B); V1 <- rchisq(B, k1); V2 <- rchisq(B, k2)
qq <- c(0.5, 0.95, 0.975)
round(rbind(t.sim  = quantile(Z/sqrt(V1/k1), qq), t.true = qt(qq, k1),
            F.sim  = quantile((V1/k1)/(V2/k2), qq), F.true = qf(qq, k1, k2)), 3)
