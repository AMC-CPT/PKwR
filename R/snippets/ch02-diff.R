# 수치미분과 기호미분. 기호미분은 오차가 없고 도함수 식을 함께 준다.
dg <- deriv(~ A*exp(-k*t), "t", function.arg = c("A", "k", "t"))
res <- dg(A, k, 4)
round(c(symbolic = attr(res, "gradient")[1],
        numeric  = (g(4 + 1e-6) - g(4 - 1e-6))/2e-6,
        by.hand  = -k*A*exp(-k*4)), 8)

# 여러 파라미터로 미분하면 gradient 벡터, 두 번 미분하면 Hessian 행렬이다.
library(numDeriv)
q <- function(v) (v[1] - 2)^2 + 3*(v[2] + 1)^2 + v[1]*v[2]
round(grad(q, c(0, 0)), 6)
round(hessian(q, c(0, 0)), 6)
