# 수치미분의 단계 크기 h: 크면 절사오차, 작으면 반올림오차. 최적점이 가운데 있다.
truth <- -k*A*exp(-k*4)                       # g'(4) 의 참값
hs  <- 10^seq(-1, -14, by = -0.25)
fwd <- abs(sapply(hs, function(h) (g(4 + h) - g(4))/h) - truth)          # 전진차분
ctr <- abs(sapply(hs, function(h) (g(4 + h) - g(4 - h))/(2*h)) - truth)  # 중심차분
signif(c(best.h.fwd = hs[which.min(fwd)], sqrt.eps = sqrt(.Machine$double.eps),
         best.h.ctr = hs[which.min(ctr)], cube.root.eps = .Machine$double.eps^(1/3),
         min.err.fwd = min(fwd), min.err.ctr = min(ctr)), 3)

par(mar = c(4.2, 5.4, 1.0, 1.0))
plot(hs, fwd, log = "xy", type = "l", las = 1, bty = "l", ylim = c(1e-12, 1e-1),
     xlab = "h", ylab = "절대오차")
lines(hs, ctr, lty = 2)
abline(v = c(sqrt(.Machine$double.eps), .Machine$double.eps^(1/3)), lty = 3,
       col = "gray55")
legend("bottomleft", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("전진차분 (오차 ~ h)", "중심차분 (오차 ~ h^2)"))
