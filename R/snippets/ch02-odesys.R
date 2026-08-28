# 연립 선형 미분방정식 dy/dt = K y. 해는 K 의 고유값이 정하는 지수항의 합이다.
K <- matrix(c(-0.9,  0.3,
               0.6, -0.4), nrow = 2, byrow = TRUE)
ev <- eigen(K); round(ev$values, 4)           # 두 고유값 (음수라야 안정)
y0v <- c(10, 0)
cf  <- solve(ev$vectors, y0v)                 # 초기조건으로 계수를 정한다
ysol <- function(t) ev$vectors %*% (exp(ev$values*t)*cf)
round(t(sapply(c(0, 1, 3, 8), function(s) drop(ysol(s)))), 4)

# 같은 문제를 수치해법으로 풀어 대조한다
library(deSolve)
num <- lsoda(y0v, c(0, 1, 3, 8), function(t, y, p) list(K %*% y), NULL)
round(num[, 2:3], 4)
c(max.abs.diff = max(abs(num[, 2:3] -
    t(sapply(c(0, 1, 3, 8), function(s) drop(ysol(s)))))))

# 상태 하나만 보면 두 지수항의 합이다: y1(t) = a1 exp(l1 t) + a2 exp(l2 t)
round(c(a1 = ev$vectors[1, 1]*cf[1], l1 = ev$values[1],
        a2 = ev$vectors[1, 2]*cf[2], l2 = ev$values[2]), 4)
