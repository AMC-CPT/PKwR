# 장간순환: 담즙으로 빠진 약물이 식사 때 장으로 쏟아져 재흡수된다 -> 이차 봉우리
library(deSolve)
ehc <- function(t, y, q) with(q, list(c(
  Ag = -kaE*y[["Ag"]],                            # 장관강
  A1 =  kaE*y[["Ag"]] - (CLE/VcE)*y[["A1"]] - kbE*y[["A1"]],   # 중심
  Ab =  kbE*y[["A1"]])))                          # 담낭(저장)
qE  <- list(kaE = 1.2, CLE = 5, VcE = 40, kbE = 0.15)
q0E <- qE; q0E$kbE <- 0                           # 담즙 배설이 아예 없는 경우
rel <- function(t, y, q) { y[["Ag"]] <- y[["Ag"]] + y[["Ab"]]   # 담낭 수축
                           y[["Ab"]] <- 0; y }

tE <- seq(0, 72, 0.02); meal <- c(4, 10, 24, 34, 48, 58)
y0E <- c(Ag = 100, A1 = 0, Ab = 0)
ev  <- list(func = rel, time = meal)
rA <- as.data.frame(lsoda(y0E, tE, ehc, q0E))         # 담즙 배설 없음
rB <- as.data.frame(lsoda(y0E, tE, ehc, qE, events = ev))   # 배설 + 재흡수
rC <- as.data.frame(lsoda(y0E, tE, ehc, qE))          # 배설만(항생제로 순환 차단)
ae <- function(r, upto) { i <- tE <= upto
  sum(diff(tE[i])*(head(r$A1[i], -1) + tail(r$A1[i], -1))/2)/qE$VcE }
round(rbind(AUC.0.24 = sapply(list(rA, rB, rC), ae, upto = 24),
            AUC.0.72 = sapply(list(rA, rB, rC), ae, upto = 72),
            AUC.exact = c(100/qE$CLE, 100/qE$CLE,
                          100/((qE$CLE/qE$VcE + qE$kbE)*qE$VcE))), 3)

matplot(tE, cbind(rA$A1, rB$A1, rC$A1)/qE$VcE, type = "l", lty = c(2, 1, 3),
        col = 1, las = 1, bty = "l", xlim = c(0, 40), ylim = c(0, 1.8),
        xlab = "Time (hr)", ylab = "Concentration (mg/L)")
abline(v = meal[1:3], lty = 3, col = "gray70")
legend("topright", bty = "n", cex = 0.8, lty = c(2, 1, 3),
       legend = c("담즙 배설 없음", "장간순환", "순환 차단(분변 배설)"))
