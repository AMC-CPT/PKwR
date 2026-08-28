# S4 (Residuals): CWRES 를 시간과 예측값에 대해, 그리고 분포로 본다
cw <- TAB$CWRES
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.2, 0.8))
plot(TAB$TIME, cw, las = 1, bty = "l", pch = 1, cex = 0.8,
     xlab = "Time (hr)", ylab = "CWRES")
abline(h = 0, lty = 3); lines(lowess(TAB$TIME, cw), lty = 2)
plot(TAB$PRED, cw, las = 1, bty = "l", pch = 1, cex = 0.8,
     xlab = "PRED (mg/L)", ylab = "CWRES")
abline(h = 0, lty = 3); lines(lowess(TAB$PRED, cw), lty = 2)
plot(TAB$CIPREDI, abs(cw), las = 1, bty = "l", pch = 1, cex = 0.8,
     xlab = "IPRED (mg/L)", ylab = "|CWRES|")
lines(lowess(TAB$CIPREDI, abs(cw)), lty = 2)
qqnorm(cw, las = 1, bty = "l", pch = 1, cex = 0.8, main = "")
qqline(cw, lty = 2)
round(c(SD.CWRES = sd(cw), shapiro.p = shapiro.test(cw)$p.value), 3)
