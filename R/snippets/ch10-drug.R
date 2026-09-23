# 약물 파라미터: 중등도 지용성 염기성 약물의 가상 예
fu    <- 0.3                                     # 혈장 유리분율
CLint <- 300                                     # 간 내적 청소율 (L/hr, 유리)
GFR   <- 7.5                                     # 사구체여과율 (L/hr)
ka    <- 1.5; Fa <- 0.9                          # 경구 흡수(10.6절)
Kp <- c(LU = 2, AD = 4, BO = 1.5, BR = 3, HT = 2.5, KI = 4,
        MU = 2.5, SK = 2, SP = 2, GU = 3.5, LI = 5)

# 조립 전에 종이 위에서 나오는 예측: well-stirred 간청소율과 Vss
QH <- sum(Q[c("SP", "GU", "LI")])                # 간 총혈류 90 L/hr
ER <- fu*CLint/(QH + fu*CLint)
round(c(ER = ER, CLh = QH*ER, CLr = fu*GFR, CL = QH*ER + fu*GFR,
        Vss = V[["ART"]] + V[["VEN"]] + sum(Kp*V[names(Kp)]),
        F.po = Fa*(1 - ER)), 4)
