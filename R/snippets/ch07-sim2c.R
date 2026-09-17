# 모의 자료: 250 mg 정맥 일시주입, 15회 채혈, 측정오차 8% (로그정규)
set.seed(20260827)
tobs <- c(5/60, 0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 16, 24, 36, 48)
dat2 <- data.frame(Time = tobs,
                   DV = round(C2iv(tobs)*exp(rnorm(length(tobs), 0, 0.08)), 3))
head(dat2, 3)

plot(dat2$Time, dat2$DV, log = "y", las = 1, bty = "l", pch = 16, cex = 0.8,
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
curve(C2iv(x), 0.02, 48, add = TRUE, lty = 3)
