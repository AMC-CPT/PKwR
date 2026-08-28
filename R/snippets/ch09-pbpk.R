library(deSolve)
# 상태: 조직 농도 12개 + 동맥/정맥 농도 + 장관강 약물량(AGL) + 제거 누적량(AEL)
dydt <- function(t, y, p) with(p, {
  Cout <- y[tis]/Kp[tis]                         # 각 조직의 유출측 혈장농도
  dtis <- Q[tis]*(y[["ART"]] - Cout)/V[tis]      # 관류제한 기본형
  QLI  <- Q[["LI"]] + Q[["SP"]] + Q[["GU"]]      # 간 유출 = 간동맥 + 문맥
  dtis[["LI"]] <- (Q[["LI"]]*y[["ART"]] + Q[["SP"]]*Cout[["SP"]] +
                   Q[["GU"]]*Cout[["GU"]] - QLI*Cout[["LI"]] -
                   fu*CLint*Cout[["LI"]])/V[["LI"]]
  dtis[["KI"]] <- dtis[["KI"]] - fu*GFR*Cout[["KI"]]/V[["KI"]]     # 신배설
  dtis[["GU"]] <- dtis[["GU"]] + ka*y[["AGL"]]/V[["GU"]]           # 경구 흡수
  dLU  <- CO*(y[["VEN"]] - y[["LU"]]/Kp[["LU"]])/V[["LU"]]
  dART <- CO*(y[["LU"]]/Kp[["LU"]] - y[["ART"]])/V[["ART"]]
  dVEN <- (sum(Q[ven.in]*Cout[ven.in]) + QLI*Cout[["LI"]] -
           CO*y[["VEN"]])/V[["VEN"]]
  dAGL <- -ka*y[["AGL"]]
  dAEL <- fu*CLint*Cout[["LI"]] + fu*GFR*Cout[["KI"]]
  list(c(dLU, dtis, dART, dVEN, dAGL, dAEL))
})

p  <- list(V = V, Q = Q, Kp = Kp, CO = CO, fu = fu, CLint = CLint,
           GFR = GFR, ka = ka, tis = tis, ven.in = ven.in)
y0 <- setNames(numeric(15), c("LU", tis, "ART", "VEN", "AGL", "AEL"))

D  <- 100                                        # mg
yiv <- y0; yiv[["VEN"]] <- D/V[["VEN"]]          # 정맥 일시주사
tsim <- seq(0, 24, 0.01)
iv <- as.data.frame(lsoda(yiv, tsim, dydt, p))

i <- seq(2, nrow(iv), 2)                         # 그림은 성기게 그린다
matplot(iv$time[i], iv[i, c("VEN", "MU", "LI", "KI", "BR", "AD")],
        type = "l", log = "y", las = 1, bty = "l", col = 1, lty = 1:6,
        lwd = 1.1, ylim = c(0.008, 32), yaxt = "n",
        xlab = "Time (hr)", ylab = "Concentration (mg/L)")
axis(2, at = 10^(-2:1), labels = c("0.01", "0.1", "1", "10"), las = 1)
legend("topright", bty = "n", cex = 0.8, lty = 1:6, ncol = 2,
       legend = c("venous plasma", "muscle", "liver", "kidney",
                  "brain", "adipose"))
