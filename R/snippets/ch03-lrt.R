# 우도비 검정: 파라미터를 하나 더 준 모형의 -2logL 은 얼마나 줄어드는가.
# 귀무가설이 참일 때 그 감소량이 자유도 1의 카이제곱을 따른다는 것을 확인한다.
set.seed(20260828)
dev <- replicate(3000, {
  s  <- rnorm(20, 0, 1)                       # 참 평균은 0 (귀무가설이 참)
  m0 <- -2*sum(dnorm(s, 0,       sd = 1, log = TRUE))   # 축소 모형
  m1 <- -2*sum(dnorm(s, mean(s), sd = 1, log = TRUE))   # 전체 모형
  m0 - m1
})
round(c(mean = mean(dev), var = var(dev),      # chi2(1) 의 평균 1, 분산 2
        q95.sim = quantile(dev, 0.95), q95.chisq = qchisq(0.95, 1)), 4)

hist(dev, breaks = 40, freq = FALSE, las = 1, col = "gray92", border = "gray60",
     xlim = c(0, 12), main = "", xlab = expression(paste(Delta, (-2*log*L))))
curve(dchisq(x, 1), 0.05, 12, add = TRUE, lwd = 1.4)
abline(v = 3.84, lty = 2)
