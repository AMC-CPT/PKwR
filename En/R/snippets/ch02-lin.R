# Solve the linear system Ax = b directly with solve(A, b); do not form the inverse
Amat <- matrix(c(2, 1, 1, 3), nrow = 2); bvec <- c(5, 10)
round(rbind(direct = solve(Amat, bvec),
            via.inverse = drop(solve(Amat) %*% bvec)), 6)

# Nearly singular matrix: with a large condition number, a small change in b shakes x
H <- matrix(c(1, 1, 1, 1.0001), nrow = 2)
signif(c(det = det(H), cond = kappa(H, exact = TRUE)), 4)
s1 <- solve(H, c(2, 2.0001)); s2 <- solve(H, c(2, 2.0002))
round(rbind(b1 = s1, b2 = s2, change = s2 - s1), 4)

# LDL' decomposition: detaching the diagonal separates variance (D) from correlation (L)
M <- matrix(c(0.09, 0.03, 0.03, 0.04), nrow = 2)
Lc <- t(chol(M)); Dg <- diag(Lc)^2; Lu <- Lc %*% diag(1/diag(Lc))
round(Lu, 4); signif(Dg, 4)                   # M = L D L'
c(max.abs.diff = max(abs(Lu %*% diag(Dg) %*% t(Lu) - M)))
