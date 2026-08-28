# 지수감쇠의 두 얼굴: 원척도에서는 곡선, 로그척도에서는 직선
k <- 0.25; A <- 100
c(half.life = log(2)/k, quarter.life = log(4)/k, tau95 = log(20)/k)

# 로그를 취하면 기울기가 -k 인 직선이 된다
tt <- seq(0, 20, 4); yy <- A*exp(-k*tt)
round(rbind(t = tt, y = yy, log.y = log(yy)), 3)
round(diff(log(yy))/diff(tt), 4)              # 기울기는 어디서나 -k

# 밑이 달라도 직선이다. 상용로그의 기울기에 log(10) = 2.303 을 곱하면 -k 가 된다.
round(c(slope.ln = coef(lm(log(yy) ~ tt))[[2]],
        slope.log10 = coef(lm(log10(yy) ~ tt))[[2]],
        converted = coef(lm(log10(yy) ~ tt))[[2]]*log(10), minus.k = -k), 4)

par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 0.8))
tg <- seq(0, 20, 0.05)
plot(tg, A*exp(-k*tg), type = "l", las = 1, bty = "l", xlab = "t", ylab = "y",
     main = "(a) 원척도")
abline(v = log(2)/k, lty = 3); text(log(2)/k, 90, "반감기", pos = 4, cex = 0.8)
plot(tg, A*exp(-k*tg), type = "l", log = "y", las = 1, bty = "l",
     xlab = "t", ylab = "y", main = "(b) 로그척도: 직선")
abline(v = log(2)/k, lty = 3)
