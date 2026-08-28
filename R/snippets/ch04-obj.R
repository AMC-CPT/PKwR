# 목적함수: 파라미터를 넣으면 '자료와 얼마나 안 맞는가'를 돌려주는 함수
sse <- function(p) sum((d4$DV - p[1]*exp(-p[2]*d4$x))^2)
sse(c(100, 0.25)); sse(c(80, 0.25))          # 참값 근처가 더 작다

# 최적화기는 이 함수를 최소화하는 p 를 찾는다
fit.ols <- optim(c(50, 0.1), sse, method = "L-BFGS-B",
                 lower = c(1, 0.01), upper = c(1e3, 5))
round(c(A = fit.ols$par[1], k = fit.ols$par[2], SSE = fit.ols$value,
        n.eval = fit.ols$counts[["function"]]), 4)

# 목적함수의 지형을 직접 본다: 골짜기가 곡선(바나나) 모양으로 휜다
Ag <- seq(60, 150, length.out = 80); kg <- seq(0.15, 0.40, length.out = 80)
Z  <- outer(Ag, kg, Vectorize(function(a, b) sse(c(a, b))))
contour(Ag, kg, log(Z), las = 1, bty = "l", nlevels = 18, drawlabels = FALSE,
        xlab = "A", ylab = "k", main = "log SSE 등고선")
points(A.t, k.t, pch = 3, cex = 1.3); points(fit.ols$par[1], fit.ols$par[2], pch = 16)
legend("topright", bg = "white", box.col = "gray70", cex = 0.8, pch = c(3, 16),
       legend = c("참값", "추정값"))
