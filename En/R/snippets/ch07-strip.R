# Stripping: (1) terminal line -> (2) back-extrapolate, subtract -> (3) residual line
ter <- dat2$Time >= 8                            # terminal phase: 6 points
fb  <- lm(log(DV) ~ Time, dat2[ter, ])
B.hat <- exp(coef(fb)[[1]]); beta.hat <- -coef(fb)[[2]]

early <- dat2$Time <= 1.5                        # distribution phase: 5 points
res   <- dat2$DV[early] - B.hat*exp(-beta.hat*dat2$Time[early])   # residuals
fa  <- lm(log(res) ~ dat2$Time[early])
A.hat <- exp(coef(fa)[[1]]); alpha.hat <- -coef(fa)[[2]]

rbind(strip = c(A = A.hat, alpha = alpha.hat, B = B.hat, beta = beta.hat),
      true  = th)
