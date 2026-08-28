# 이 장의 예제: 지수감쇠 y = A exp(-k t). 뒤 장의 농도 곡선이 모두 이 꼴이다.
set.seed(20260828)
A.t <- 100; k.t <- 0.25                       # 참값
tt  <- c(0.5, 1, 2, 3, 4, 6, 8, 12, 16, 24)
d4  <- data.frame(x = tt,
                  DV = round(A.t*exp(-k.t*tt)*exp(rnorm(length(tt), 0, 0.10)), 3))
d4

par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
plot(d4$x, d4$DV, las = 1, bty = "l", pch = 16, ylim = c(0, 110),
     xlab = "x", ylab = "y", main = "(a) 원척도")
curve(A.t*exp(-k.t*x), 0, 24, add = TRUE, lty = 3)
plot(d4$x, d4$DV, log = "y", las = 1, bty = "l", pch = 16,
     xlab = "x", ylab = "y", main = "(b) 반로그 척도")
curve(A.t*exp(-k.t*x), 0, 24, add = TRUE, lty = 3)
