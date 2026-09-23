# Numerical and symbolic differentiation; symbolic is exact and also gives the formula
dg <- deriv(~ A*exp(-k*t), "t", function.arg = c("A", "k", "t"))
res <- dg(A, k, 4)
round(c(symbolic = attr(res, "gradient")[1],
        numeric  = (g(4 + 1e-6) - g(4 - 1e-6))/2e-6,
        by.hand  = -k*A*exp(-k*4)), 8)

# Derivatives in several parameters form the gradient; second derivatives, the Hessian
library(numDeriv)
q <- function(v) (v[1] - 2)^2 + 3*(v[2] + 1)^2 + v[1]*v[2]
round(grad(q, c(0, 0)), 6)
round(hessian(q, c(0, 0)), 6)
