# 시간 의존 비선형성: 자가유도. 약물이 자기 대사효소를 늘려 청소율이 시간에 따라 오른다.
library(deSolve)
tdep <- function(t, y, q) with(q, {
  Ci <- y[["A"]]/Vd
  list(c(A  = kaz*y[["Ag"]] - CL0*y[["E"]]*Ci,    # 청소율이 효소량 E 에 비례
         Ag = -kaz*y[["Ag"]],
         E  = kenz*(1 + Imax*Ci/(IC50 + Ci) - y[["E"]]))) })   # 효소의 회전
q1 <- list(Vd = 50, CL0 = 3, kaz = 1.2, kenz = 0.004, Imax = 2, IC50 = 4)
q0 <- q1; q0$Imax <- 0                            # 유도가 없는 대조

tt <- seq(0, 21*24, 0.5)
ev <- list(data = data.frame(var = "Ag", time = seq(0, 20*24, 24),
                             value = 400, method = "add"))
y0 <- c(A = 0, Ag = 0, E = 1)
r1 <- as.data.frame(lsoda(y0, tt, tdep, q1, events = ev))
r0 <- as.data.frame(lsoda(y0, tt, tdep, q0, events = ev))

dy <- c(1, 3, 7, 14, 21)                          # 각 날의 골농도와 효소량
i  <- match(dy*24 - 0.5, tt)
round(rbind(day = dy, Ctrough.induced = r1$A[i]/q1$Vd,
            Ctrough.control = r0$A[i]/q1$Vd, E = r1$E[i],
            CL = q1$CL0*r1$E[i]), 3)

plot(tt/24, r1$A/q1$Vd, type = "l", las = 1, bty = "l", xlab = "Day",
     ylab = "Concentration (mg/L)", ylim = c(0, 22))
lines(tt/24, r0$A/q1$Vd, lty = 2)
legend("topright", bty = "n", cex = 0.85, lty = c(1, 2),
       legend = c("자가유도 있음", "없음(대조)"))
