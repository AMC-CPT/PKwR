# 부분 선형성: k 가 정해지면 A 는 닫힌 식으로 나오므로 탐색은 1차원으로 준다
prof.k <- function(kk) {
  z <- exp(-kk*d4$x); a <- sum(z*d4$DV)/sum(z^2)   # A 의 최소제곱해
  c(A = a, SSE = sum((d4$DV - a*z)^2)) }
ok <- optimize(function(kk) prof.k(kk)[["SSE"]], interval = c(0.01, 2))
round(c(k = ok$minimum, prof.k(ok$minimum)), 5)
round(c(A = fit.ols$par[1], k = fit.ols$par[2], SSE = fit.ols$value), 5)

# nls 의 plinear 알고리즘이 이 일을 자동으로 한다 (초기값이 k 하나면 된다)
np <- nls(DV ~ cbind(exp(-k*x)), d4, start = c(k = 0.1), algorithm = "plinear")
round(coef(np), 5)
