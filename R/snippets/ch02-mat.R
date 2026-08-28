# 행렬 연산: 이 책이 쓰는 것은 곱, 전치, 역행렬, 행렬식, 고유값, Cholesky 뿐이다
M <- matrix(c(4, 2, 2, 3), nrow = 2)          # 대칭 행렬
round(M %*% M, 3); round(solve(M), 4)         # 곱과 역행렬
c(det = det(M), trace = sum(diag(M)))

# 대칭 양의 정부호: 고유값이 모두 양수. 분산-공분산 행렬의 조건이다.
egn <- eigen(M); round(egn$values, 4)
c(pos.def = all(egn$values > 0),
  cond.number = max(egn$values)/min(egn$values))   # 조건수

# Cholesky: M = L L' 로 쪼갠다. 양정부호를 강제하는 모수화에 쓴다.
L <- t(chol(M)); round(L, 4); round(L %*% t(L) - M, 12)

# 이차형식 x' M x. 목적함수의 벌점항이 이 꼴이다.
xv <- c(1, -2)
c(quad.form = drop(t(xv) %*% M %*% xv), by.hand = 4*1 + 2*2*(1*-2) + 3*4)
