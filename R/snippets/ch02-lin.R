# 연립방정식 Ax = b 는 역행렬을 만들지 말고 solve(A, b) 로 바로 푼다
Amat <- matrix(c(2, 1, 1, 3), nrow = 2); bvec <- c(5, 10)
round(rbind(direct = solve(Amat, bvec),
            via.inverse = drop(solve(Amat) %*% bvec)), 6)

# 거의 특이한 행렬: 조건수가 크면 b 의 작은 변화가 해를 크게 흔든다
H <- matrix(c(1, 1, 1, 1.0001), nrow = 2)
signif(c(det = det(H), cond = kappa(H, exact = TRUE)), 4)
s1 <- solve(H, c(2, 2.0001)); s2 <- solve(H, c(2, 2.0002))
round(rbind(b1 = s1, b2 = s2, change = s2 - s1), 4)

# LDL' 분해: 대각을 따로 떼면 분산(D)과 상관(L)을 나누어 모수화할 수 있다
M <- matrix(c(0.09, 0.03, 0.03, 0.04), nrow = 2)
Lc <- t(chol(M)); Dg <- diag(Lc)^2; Lu <- Lc %*% diag(1/diag(Lc))
round(Lu, 4); signif(Dg, 4)                   # M = L D L'
c(max.abs.diff = max(abs(Lu %*% diag(Dg) %*% t(Lu) - M)))
