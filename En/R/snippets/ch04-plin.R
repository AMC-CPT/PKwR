# Partial linearity: given k, A has a closed form, so the search is one-dimensional
prof.k <- function(kk) {
  z <- exp(-kk*d4$x); a <- sum(z*d4$DV)/sum(z^2)   # least-squares solution for A
  c(A = a, SSE = sum((d4$DV - a*z)^2)) }
ok <- optimize(function(kk) prof.k(kk)[["SSE"]], interval = c(0.01, 2))
round(c(k = ok$minimum, prof.k(ok$minimum)), 5)
round(c(A = fit.ols$par[1], k = fit.ols$par[2], SSE = fit.ols$value), 5)

# The plinear algorithm of nls does this automatically (a start for k alone suffices)
np <- nls(DV ~ cbind(exp(-k*x)), d4, start = c(k = 0.1), algorithm = "plinear")
round(coef(np), 5)
