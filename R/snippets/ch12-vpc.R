# VPC: 최종 추정치에서 500 회 모의 시험을 만들어 관측과 포갠다.
# 재현성: seed 명시, 재추출 단위 = 대상자(eta), 관측 설계는 원자료 그대로.
set.seed(20260827)
nsim <- 500
L <- t(chol(OM))                                   # eta ~ MVN(0, OM)
sim <- replicate(nsim, {
  do.call(c, lapply(unique(DATA$ID), function(id) {
    Di <- DATA[DATA$ID == id, ]
    Fi <- PRED(TH, as.numeric(L %*% rnorm(3)), Di)[, "F"]
    Fi*(1 + rnorm(length(Fi), 0, sqrt(SG[1, 1]))) +
      rnorm(length(Fi), 0, sqrt(SG[2, 2]))
  }))
})

tnom <- c(0, 0.25, 0.5, 1, 2, 3.5, 5, 7, 9, 12, 24)   # 명목 채혈 시각
bin  <- tnom[apply(abs(outer(DATA$TIME, tnom, "-")), 1, which.min)]
qs   <- apply(sim, 1, quantile, c(0.05, 0.5, 0.95))    # 관측점별 예측 분위수
band <- apply(qs, 1, tapply, bin, median)              # 시각 구간별로 요약

plot(DATA$TIME, DATA$DV, las = 1, bty = "l", pch = 1, cex = 0.7,
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
polygon(c(tnom, rev(tnom)), c(band[, "5%"], rev(band[, "95%"])),
        col = "gray90", border = NA)
points(DATA$TIME, DATA$DV, pch = 1, cex = 0.7)
lines(tnom, band[, "50%"], lwd = 1.2)                  # 예측 중앙값
lines(tnom, tapply(DATA$DV, bin, median), lty = 2)     # 관측 중앙값
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2, NA), pch = c(NA, NA, 22),
       pt.bg = "gray90", pt.cex = 1.8,
       legend = c("simulated median", "observed median", "simulated 90% PI"))
