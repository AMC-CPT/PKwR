# S5 (EBE): eta 의 분포. 제목에 shrinkage 를 함께 적는다.
par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8))
for (k in 1:3) {
  ek <- EBE[, k + 1]
  hist(ek, breaks = 6, freq = FALSE, las = 1, col = "gray92",
       xlab = bquote(eta[.(k)]), main = "")
  curve(dnorm(x, 0, sqrt(OM[k, k])), add = TRUE, lty = 2)
  title(sprintf("shrinkage %.0f%%", 100*shr[k]), font.main = 1, cex.main = 1.1)
}
