# 검산: 같은 투여 이력을 미분방정식으로 풀어 맞대어 본다
THv <- c(3.96, 33.1, 48.3, 6.99)                  # CL(CLcr 100 기준), V1, V2, Q
Dv  <- data.frame(TIME = c(0, 12, 24), AMT = c(1000, 1000, 0),
                  RATE = c(1000, 1000, 0), DV = NA)      # 1000 mg 를 1시간 주입 x 2
De2 <- addStop(Dv)
tg3 <- sort(unique(c(De2$TIME, seq(0, 36, 0.25))))
Dg  <- data.frame(TIME = tg3, AMT = 0,
                  RATE = approx(De2$TIME, De2$RATE, tg3, method = "constant",
                                rule = 2, f = 0)$y, DV = NA)
cc  <- pred2c(THv, c(0, 0, 0, 0), Dg, CLcr = 90)

library(deSolve)
pv  <- list(CL = THv[1]*0.9, V1 = THv[2], V2 = THv[3], Q = THv[4])
rateF <- approxfun(Dg$TIME, Dg$RATE, method = "constant", rule = 2, f = 0)
ode2c <- function(t, y, p) with(p, list(c(
  rateF(t) - CL/V1*y[1] - Q/V1*y[1] + Q/V2*y[2], Q/V1*y[1] - Q/V2*y[2])))
num <- lsoda(c(0, 0), tg3, ode2c, pv)
c(max.abs.diff = max(abs(cc - num[, 2]/pv$V1)), Cmax = max(cc),
  Ctrough.12h = cc[tg3 == 12], AUC24.pred = 2*1000/pv$CL)
