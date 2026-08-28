# 이 책의 주력 분포는 정규가 아니라 로그정규다. 농도도 파라미터도 양수이고
# 오른쪽으로 치우쳐 있기 때문이다. 로그를 취하면 정규가 된다.
mu <- log(100); sg <- 0.3                     # 로그척도의 평균과 표준편차
c(geo.mean = exp(mu), arith.mean = exp(mu + sg^2/2),
  median = exp(mu), CV = sqrt(exp(sg^2) - 1))

par(mfrow = c(1, 2), mar = c(4.2, 5.0, 2.4, 0.8))
xg <- seq(20, 260, 0.5)
plot(xg, dlnorm(xg, mu, sg), type = "l", las = 1, bty = "l",
     xlab = "x", ylab = "밀도", main = "(a) 로그정규")
abline(v = c(exp(mu), exp(mu + sg^2/2)), lty = c(3, 2))
legend("topright", bty = "n", cex = 0.8, lty = c(3, 2),
       legend = c("기하평균 = 중앙값", "산술평균"))
plot(log(xg), dnorm(log(xg), mu, sg), type = "l", las = 1, bty = "l",
     xlab = "log x", ylab = "밀도", main = "(b) 로그를 취하면 정규")
abline(v = mu, lty = 3)
