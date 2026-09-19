# S5 (EBE): eta 대 공변량. 공변량 후보 탐색과 채택 후 소실 확인에 쓴다.
wt <- DATA$BWT[match(unique(DATA$ID), DATA$ID)]     # 대상자별 체중
par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.4, 0.8), cex = 0.85)
for (k in 1:3) {
  ek <- EBE[, k + 1]
  plot(wt, ek, las = 1, bty = "l", type = "n",
       xlab = "Body weight (kg)", ylab = bquote(eta[.(k)]))
  abline(h = 0, lty = 3); lines(lowess(wt, ek), lty = 2)
  text(wt, ek, EBE[, "ID"], cex = 0.85)
  title(sprintf("r = %.2f", cor(wt, ek)), font.main = 1, cex.main = 1.1)
}
