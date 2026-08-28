# 이 책의 그림은 전부 R 기본 그래픽이다. 세 판이면 문법이 거의 다 나온다.
set.seed(20260828)
tt <- c(0.5, 1, 2, 4, 6, 8, 12)
obs <- round(decay(tt)*exp(rnorm(length(tt), 0, 0.12)), 2)

par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8))
plot(tt, obs, las = 1, bty = "l", pch = 16, xlab = "t", ylab = "y",
     main = "(a) 점과 곡선")                  # las: 축 눈금 방향, bty: 상자 모양
curve(decay(x), 0, 12, add = TRUE, lty = 2)   # add = TRUE 로 겹쳐 그린다
legend("topright", bty = "n", cex = 0.85, pch = c(16, NA), lty = c(NA, 2),
       legend = c("관측", "모형"))

plot(tt, obs, log = "y", las = 1, bty = "l", pch = 16, xlab = "t", ylab = "y",
     main = "(b) 로그 축")                    # log = "y" 한 글자로 반로그
curve(decay(x), 0.3, 12, add = TRUE, lty = 2)

ks <- c(0.15, 0.25, 0.40)                     # 여러 곡선은 matplot 이 편하다
tg <- seq(0, 12, 0.1)
matplot(tg, sapply(ks, function(k) decay(tg, k = k)), type = "l", lty = 1:3,
        col = 1, las = 1, bty = "l", xlab = "t", ylab = "y", main = "(c) 여러 곡선")
abline(h = 20, lty = 3)                       # 참조선
text(1.6, 26, "y = 20", cex = 0.85)
legend("topright", bty = "n", cex = 0.85, lty = 1:3, legend = paste("k =", ks))
