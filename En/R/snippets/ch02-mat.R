# Matrix operations: only product, transpose, inverse, determinant, eigenvalue, Cholesky
M <- matrix(c(4, 2, 2, 3), nrow = 2)          # a symmetric matrix
round(M %*% M, 3); round(solve(M), 4)         # product and inverse
c(det = det(M), trace = sum(diag(M)))

# Symmetric positive definite (all eigenvalues > 0): required of a covariance matrix
egn <- eigen(M); round(egn$values, 4)
c(pos.def = all(egn$values > 0),
  cond.number = max(egn$values)/min(egn$values))   # condition number

# Cholesky: factor M = L L'. Used in parameterizations enforcing positive definiteness.
L <- t(chol(M)); round(L, 4); round(L %*% t(L) - M, 12)

# Quadratic form x' M x. The penalty term of the objective function has this form.
xv <- c(1, -2)
c(quad.form = drop(t(xv) %*% M %*% xv), by.hand = 4*1 + 2*2*(1*-2) + 3*4)
