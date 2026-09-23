# Sparse sampling in nonclinical toxicity: a different animal is sacrificed at each
# time point, so there is no individual curve, no individual NCA, only a group mean
set.seed(20260827)
tsp  <- c(0.5, 1, 2, 4, 8, 24); k <- length(tsp)            # 6 time points
nsp  <- 4                                                   # 4 animals per time point
mu   <- C2iv(tsp)                                           # true mean curve
Y    <- sapply(mu, function(m) m*exp(rnorm(nsp, 0, 0.30)))  # between-animal CV 30%
ybar <- colMeans(Y); s2 <- apply(Y, 2, var)

# Trapezoid = linear combination of the means; extract the weights w, variance follows
w <- c((tsp[2] - tsp[1])/2, (tsp[3:k] - tsp[1:(k - 2)])/2, (tsp[k] - tsp[k - 1])/2)
AUC.b <- sum(w*ybar); SE.b <- sqrt(sum(w^2*s2/nsp))         # Bailer (1988)
round(c(w = w), 4)                                          # trapezoidal coefficients
round(c(AUC.trap = auc.lin(tsp, ybar), AUC.w = AUC.b, SE = SE.b), 4)

# Cross-check: does it agree with a bootstrap resampling animals within time points
B  <- 2000
ab <- replicate(B, sum(w*apply(Y, 2, function(z) mean(sample(z, nsp, TRUE)))))
round(c(SE.bailer = SE.b, SE.boot = sd(ab),
        SE.boot.adj = sd(ab)*sqrt(nsp/(nsp - 1))), 4)       # (n-1)/n correction, n=4

round(c(lo.normal = AUC.b - 1.96*SE.b, hi.normal = AUC.b + 1.96*SE.b,
        lo.boot = quantile(ab, 0.025), hi.boot = quantile(ab, 0.975)), 4)

# With a variance we can test: is twice the dose twice the exposure?
Y2 <- sapply(2*mu, function(m) m*exp(rnorm(nsp, 0, 0.30)))
A2 <- sum(w*colMeans(Y2)); S2 <- sqrt(sum(w^2*apply(Y2, 2, var)/nsp))
dn <- A2/2 - AUC.b                                          # dose-normalized difference
se <- sqrt((S2/2)^2 + SE.b^2)
round(c(AUC.low = AUC.b, AUC.high = A2, ratio = A2/AUC.b,
        diff.dn = dn, se.dn = se, z = dn/se, p = 2*pnorm(-abs(dn/se))), 4)
