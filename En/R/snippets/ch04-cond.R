# Condition number = max/min eigenvalue of the estimate correlation matrix. It changes
# with the coordinates even when the model is the same.
kappa <- function(H) { ev <- eigen(cov2cor(solve(H)), symmetric = TRUE)$values
                       max(ev)/min(ev) }
round(sapply(fits, function(o) kappa(o$hessian)), 3) # ch04-repar coordinates, same SSE

# In linear regression the matrix is X'X of the design: coefficients are fitted first,
# then the residual variance is estimated separately, so X'X has no variance term.
X <- model.matrix(~ x, d4)                    # design matrix of log(DV) = b0 + b1*x
round(c(kappa.XtX = kappa(crossprod(X)),      # condition number, coefficients only
        sigma     = summary(lm(log(DV) ~ x, d4))$sigma), 3) # variance estimated later
