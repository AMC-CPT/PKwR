# 조건수 = 추정치 상관행렬의 최대/최소 고유값 비. 같은 모형이라도 좌표를 바꾸면 달라진다.
kappa <- function(H) { ev <- eigen(cov2cor(solve(H)), symmetric = TRUE)$values
                       max(ev)/min(ev) }
round(sapply(fits, function(o) kappa(o$hessian)), 3)  # ch04-repar 의 세 좌표, SSE 는 같다

# 선형회귀에서 조건수를 재는 행렬은 설계행렬의 X'X 다. 계수를 먼저 적합하고,
# 잔차분산은 계수를 고정한 뒤 잔차에서 별도로 구하므로 X'X 에는 분산항이 없다.
X <- model.matrix(~ x, d4)                                   # log(DV) = b0 + b1*x 의 설계행렬
round(c(kappa.XtX = kappa(crossprod(X)),                     # 회귀계수만 든 행렬의 조건수
        sigma     = summary(lm(log(DV) ~ x, d4))$sigma), 3)  # 분산은 그 뒤에 따로 추정된다
