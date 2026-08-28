# 중심극한정리: 치우친 분포에서 뽑아도 표본평균은 빠르게 정규에 가까워진다
set.seed(20260828)
ns  <- c(1, 4, 16)
mns <- sapply(ns, function(m) replicate(4000, mean(rlnorm(m, mu, sg))))
sd1 <- sqrt(exp(2*mu + sg^2)*(exp(sg^2) - 1))          # 로그정규 한 점의 SD
skew <- function(v) mean((v - mean(v))^3)/sd(v)^3
round(rbind(n = ns, mean = colMeans(mns), SD = apply(mns, 2, sd),
            SD.theory = sd1/sqrt(ns), skewness = apply(mns, 2, skew)), 4)

par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8))
for (j in seq_along(ns)) {
  hist(mns[, j], breaks = 40, freq = FALSE, las = 1, col = "gray92",
       border = "gray65", main = paste("n =", ns[j]), xlab = "표본평균",
       ylab = if (j == 1) "밀도" else "")
  curve(dnorm(x, mean(mns[, j]), sd(mns[, j])), add = TRUE, lwd = 1.4)
}
