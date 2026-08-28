# 잔차법: (1) 말기 직선 회귀 -> (2) 뒤로 외삽하여 빼기 -> (3) 잔차 직선 회귀
ter <- dat2$Time >= 8                            # 말기(분포 완료 후) 6점
fb  <- lm(log(DV) ~ Time, dat2[ter, ])
B.hat <- exp(coef(fb)[[1]]); beta.hat <- -coef(fb)[[2]]

early <- dat2$Time <= 1.5                        # 분포기 5점
res   <- dat2$DV[early] - B.hat*exp(-beta.hat*dat2$Time[early])   # 잔차
fa  <- lm(log(res) ~ dat2$Time[early])
A.hat <- exp(coef(fa)[[1]]); alpha.hat <- -coef(fa)[[2]]

rbind(strip = c(A = A.hat, alpha = alpha.hat, B = B.hat, beta = beta.hat),
      true  = th)
