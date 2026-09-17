# 세 모형의 곡선. V = 1 L, F = 1 이므로 약물량이 곧 농도이다.
# 곡선을 그리려면 촘촘한 격자가 필요하지만, 해석해는 각 격자점을 '직전 점에서
# 한 번 전진'시키는 것이므로 격자를 아무리 촘촘히 해도 정확도가 변하지 않는다.
tg  <- sort(unique(c(seq(0, 60, 0.05), 24, 27, 48)))
dhf <- data.frame(TIME = tg, AMT = NA, RATE = NA, CMT = NA)
dhf[dhf$TIME ==  0, 2:4] <- c(100,  0, 2)
dhf[dhf$TIME == 24, 2:4] <- c(150, 50, 2)
dhf[dhf$TIME == 48, 2:4] <- c(100,  0, 1)
dhf <- ExpandDH(dhf)

# 1구획은 n = 1 인 특수한 경우일 뿐이다. adj 가 1 이고 근이 하나이므로 C = 1 이다.
cc1 <- runN(dhf, 0.1,  list(matrix(1)))[, 2]
cc2 <- runN(dhf, lam,  Co )[, 2]
cc3 <- runN(dhf, lam3, Co3)[, 2]
c(one.comp.rule = max(abs(runN(dh2, 0.1, list(matrix(1))) - M1)))

ii <- dhf$TIME > 0                        # 투여 전인 0 시각은 세 곡선 모두 0 이다
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 0.8))
for (lg in c("", "y")) {
  matplot(dhf$TIME[ii], cbind(cc1, cc2, cc3)[ii, ], type = "l", lty = c(1, 2, 3), col = 1,
          log = lg, las = 1, bty = "l", xlab = "Time (hr)", ylab = "중심구획 농도 (mg/L)",
          ylim = if (lg == "") c(0, 145) else c(1, 300),
          main = if (lg == "") "(a) 선형 눈금" else "(b) 반로그 눈금", cex.main = 0.95)
  abline(v = c(0, 24, 48), lty = 3, col = "gray60")
  if (lg == "") legend("topleft", bty = "n", cex = 0.85, lty = 1:3,
                       legend = c("1구획", "2구획", "3구획"))
}
