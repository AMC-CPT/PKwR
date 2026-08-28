# 개인별 농도-시간 곡선 (spaghetti plot): 개인차의 크기를 먼저 눈으로 본다
plot(DATA$TIME, DATA$DV, type = "n", las = 1, bty = "l",
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
for (id in unique(DATA$ID)) {
  d <- DATA[DATA$ID == id, ]
  lines(d$TIME, d$DV, col = "gray40")
  text(d$TIME[which.max(d$DV)], max(d$DV), id, pos = 3, cex = 0.7)
}
