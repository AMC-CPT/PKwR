# Gauss-Newton: linearize the model at the current estimate, solve linear least squares
fmod <- function(p) p[1]*exp(-p[2]*d4$x)
Jac  <- function(p) cbind(exp(-p[2]*d4$x), -p[1]*d4$x*exp(-p[2]*d4$x))

p <- c(50, 0.1); tr <- NULL
for (it in 1:20) {
  r <- d4$DV - fmod(p); qv <- qr(Jac(p))
  ro <- sqrt(sum(qr.fitted(qv, r)^2)/sum(qr.resid(qv, r)^2))   # relative offset
  tr <- rbind(tr, c(iter = it, A = p[1], k = p[2], SSE = sum(r^2), offset = ro))
  if (ro < 1e-4) break
  del <- qr.coef(qv, r); lam <- 1               # increment
  while (sum((d4$DV - fmod(p + lam*del))^2) > sum(r^2) && lam > 1e-6) lam <- lam/2
  p <- p + lam*del                              # halve the step if SSE would increase
}
round(tr[, c("iter", "A", "k", "SSE")], 5)
signif(tr[, "offset"], 3)                       # converged when this approaches 0
round(coef(nls(DV ~ A*exp(-k*x), d4, start = c(A = 50, k = 0.1))), 5)
