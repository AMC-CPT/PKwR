# 판정에 앞서 자료부터 눈으로: 개체별 T/R 비를 크기순으로 그린다
rat <- sapply(c("AUClast", "Cmax"), function(v) {
  w <- tapply(d13[[v]], list(d13$SUBJ, d13$TRT), c)
  w[, "T"]/w[, "R"]
})
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
for (v in c("AUClast", "Cmax")) {
  plot(sort(rat[, v]), log = "y", las = 1, bty = "l", pch = 16, cex = 0.8,
       ylim = c(0.4, 2.5), xlab = "Subject (sorted)", ylab = "T/R ratio",
       main = v, panel.first = abline(h = c(0.8, 1, 1.25), lty = c(3, 2, 3)))
  points(n/2 + 0.5, exp(mean(log(rat[, v]))), pch = 5, cex = 1.4)
}
