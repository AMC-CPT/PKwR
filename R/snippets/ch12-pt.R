# 가상의 새 환자: 참값을 알고 시작해야 방법을 평가할 수 있다 (7장의 전략).
# 청소율이 집단 대표값보다 훨씬 큰(빨리 없애는) 환자를 만든다.
eta.true <- c(-0.4, 0.20, 0.35)
D <- 320                                            # mg, 경구 단회
round(c(KA = TH[[1]]*exp(eta.true[1]), V = TH[[2]]*exp(eta.true[2]),
        K  = TH[[3]]*exp(eta.true[3]),
        CL = TH[[2]]*exp(eta.true[2])*TH[[3]]*exp(eta.true[3]),
        t.half = log(2)/(TH[[3]]*exp(eta.true[3]))), 3)

set.seed(20260827)                                  # 잔차오차 (비례 + 가법)
t.pt <- c(1, 2, 4, 8, 12)                           # 채혈 시각 (hr)
f.pt <- ipred(eta.true, t.pt, D)
dv.pt <- round(f.pt*(1 + rnorm(5, 0, sqrt(SG[["prop"]]))) +
               rnorm(5, 0, sqrt(SG[["add"]])), 2)
rbind(TIME = t.pt, DV = dv.pt)
