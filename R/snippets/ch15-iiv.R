# 개인간 변이는 곡선과 곡선 사이의 거리, 개인내(잔차) 변이는 자기 곡선
# 둘레의 산포로 나타난다.  IV 일시주입, 8명, CL 의 로그정규 변이 CV 약 36%
TVCL <- 4; TVV <- 50; D <- 200                    # 모집단 대표값 (L/hr, L, mg)
CLi  <- TVCL*exp(rnorm(8, 0, 0.35))               # 개인의 청소율
t.ob <- c(0.5, 1, 2, 4, 8, 12, 24)                # 채혈시각

tt <- seq(0, 24, 0.1)
plot(NA, xlim = c(0, 24), ylim = c(0.05, 6), log = "y", las = 1, bty = "l",
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
for (i in 1:8) {
  ki <- CLi[i]/TVV
  lines(tt, D/TVV*exp(-ki*tt), col = "gray55")
  points(t.ob, D/TVV*exp(-ki*t.ob)*exp(rnorm(7, 0, 0.1)), pch = 16, cex = 0.55)
}
round(sort(CLi), 2)                               # 8명의 청소율 (L/hr)
