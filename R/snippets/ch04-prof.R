# 프로파일: 파라미터 하나를 고정하고 나머지를 다시 최적화해 SSE 를 따라간다
pe4 <- fit.ols$par; n4 <- nrow(d4); s2 <- fit.ols$value/(n4 - 2)
V4  <- Jac(pe4); se4 <- sqrt(diag(s2*solve(t(V4) %*% V4)))    # Wald 표준오차
tau <- function(idx, v) {
  o <- optimize(function(u) sse(if (idx == 1) c(v, u) else c(u, v)),
                interval = if (idx == 1) c(0.01, 5) else c(1, 1000))
  sign(v - pe4[idx])*sqrt(max(o$objective - fit.ols$value, 0)/s2) }
dg <- seq(-3, 3, 0.05)                        # delta = (v - 추정치)/SE
tA <- sapply(pe4[1] + se4[1]*dg, function(v) tau(1, v))
tK <- sapply(pe4[2] + se4[2]*dg, function(v) tau(2, v))

tc <- qt(0.975, n4 - 2)                       # |tau| <= t 인 구간이 프로파일 구간
round(rbind(Wald.A    = pe4[1] + c(-1, 1)*tc*se4[1],
            profile.A = range((pe4[1] + se4[1]*dg)[abs(tA) <= tc]),
            Wald.k    = pe4[2] + c(-1, 1)*tc*se4[2],
            profile.k = range((pe4[2] + se4[2]*dg)[abs(tK) <= tc])), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
for (j in 1:2) {
  plot(dg, if (j == 1) tA else tK, type = "l", las = 1, bty = "l",
       ylim = c(-3.2, 3.2), xlab = expression(delta), ylab = expression(tau),
       main = if (j == 1) "(a) A" else "(b) k")
  abline(0, 1, lty = 2); abline(h = c(-tc, tc), lty = 3)
}
