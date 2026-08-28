# 신뢰구간의 뜻은 확률이 아니라 '반복했을 때의 적중률'이다. 직접 세어 본다.
set.seed(20260828)
n <- 12; B <- 2000
cover <- replicate(B, {
  s  <- rnorm(n, mu, sg)                      # 참 평균 mu 를 아는 채로 반복
  ci <- mean(s) + c(-1, 1)*qt(0.975, n - 1)*sd(s)/sqrt(n)
  c(lo = ci[1], hi = ci[2], hit = ci[1] <= mu & mu <= ci[2])
})
round(c(coverage = mean(cover["hit", ]), mean.width = mean(diff(cover[1:2, ]))), 4)

# 처음 40번의 구간을 그려 본다. 놓친 구간이 눈에 띈다.
i <- 1:40; hit <- cover["hit", i] == 1
plot(NA, xlim = range(cover[1:2, i]), ylim = c(0, 41), yaxt = "n", las = 1,
     bty = "l", xlab = expression(mu), ylab = "반복")
abline(v = mu, lty = 2)
segments(cover[1, i], i, cover[2, i], i, col = ifelse(hit, "gray55", "black"),
         lwd = ifelse(hit, 1, 2))
