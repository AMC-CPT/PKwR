# Matrix: same-type values in rows and columns; the covariances of Chapter 11 are one.
M <- matrix(c(0.09, 0.03, 0.03, 0.04), nrow = 2,
            dimnames = list(c("CL", "V"), c("CL", "V")))
M
c(nrow = nrow(M), det = det(M))
round(solve(M), 3)                            # inverse (used in Chapters 11 and 15)
round(cov2cor(M), 3)                          # covariance -> correlation
round(M %*% c(1, 1), 3)                       # %*% is matrix product, * elementwise

# List: bundles items of different type and length by name; fit functions return one.
fit <- list(par = c(A = 100, k = 0.25), n = 7L, cov = M)
c(one.bracket = class(fit["par"]), two.brackets = class(fit[["par"]]))
fit$par[["k"]]
names(fit)
