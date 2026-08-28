# 말기 소실속도상수: 마지막 3점부터 점을 늘려 가며 수정 R^2 최대를 고른다
lambdaz <- function(t, C) {
  n <- length(t)
  fit <- sapply(3:(n - 1), function(k) {         # 마지막 k점 사용
    i <- (n - k + 1):n
    f <- lm(log(C[i]) ~ t[i])
    c(n = k, lambda.z = -coef(f)[[2]], R2adj = summary(f)$adj.r.squared)
  })
  fit[, which.max(fit["R2adj", ])]
}
lz <- lambdaz(dat2$Time, dat2$DV)
round(c(lz, t.half.z = log(2)/lz[["lambda.z"]]), 4)

# 사용한 점을 표시하고 회귀직선을 겹쳐 눈으로 확인한다
used <- seq(nrow(dat2) - lz[["n"]] + 1, nrow(dat2))
plot(dat2$Time, dat2$DV, log = "y", las = 1, bty = "l", pch = 1, cex = 0.9,
     xlab = "Time (hr)", ylab = "Concentration (mg/L)")
points(dat2$Time[used], dat2$DV[used], pch = 16, cex = 0.9)
C.at <- exp(predict(lm(log(DV) ~ Time, dat2[used, ]),
                    newdata = data.frame(Time = c(0, 48))))
lines(c(0, 48), C.at, lty = 2)
legend("topright", bty = "n", cex = 0.85, pch = c(16, 1),
       legend = c("used for lambda.z", "not used"))
