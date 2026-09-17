# Gauss-Newton: 현재 추정치에서 모형을 일차로 펴고(선형화) 선형 최소제곱을 푼다
fmod <- function(p) p[1]*exp(-p[2]*d4$x)
Jac  <- function(p) cbind(exp(-p[2]*d4$x), -p[1]*d4$x*exp(-p[2]*d4$x))

p <- c(50, 0.1); tr <- NULL
for (it in 1:20) {
  r <- d4$DV - fmod(p); qv <- qr(Jac(p))
  ro <- sqrt(sum(qr.fitted(qv, r)^2)/sum(qr.resid(qv, r)^2))   # 상대 오프셋
  tr <- rbind(tr, c(iter = it, A = p[1], k = p[2], SSE = sum(r^2), offset = ro))
  if (ro < 1e-4) break
  del <- qr.coef(qv, r); lam <- 1               # 증분
  while (sum((d4$DV - fmod(p + lam*del))^2) > sum(r^2) && lam > 1e-6) lam <- lam/2
  p <- p + lam*del                              # 발산하면 걸음을 반으로 줄인다
}
round(tr[, c("iter", "A", "k", "SSE")], 5)
signif(tr[, "offset"], 3)                       # 0 에 가까워지면 수렴이다
round(coef(nls(DV ~ A*exp(-k*x), d4, start = c(A = 50, k = 0.1))), 5)
