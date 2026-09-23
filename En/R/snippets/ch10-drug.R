# Drug parameters: a hypothetical moderately lipophilic basic drug
fu    <- 0.3                                     # unbound fraction in plasma
CLint <- 300                                     # intrinsic hepatic CL (L/hr, unbound)
GFR   <- 7.5                                     # glomerular filtration rate (L/hr)
ka    <- 1.5; Fa <- 0.9                          # oral absorption (Section 10.6)
Kp <- c(LU = 2, AD = 4, BO = 1.5, BR = 3, HT = 2.5, KI = 4,
        MU = 2.5, SK = 2, SP = 2, GU = 3.5, LI = 5)

# Predictions on paper before assembly: well-stirred hepatic clearance and Vss
QH <- sum(Q[c("SP", "GU", "LI")])                # total hepatic blood flow 90 L/hr
ER <- fu*CLint/(QH + fu*CLint)
round(c(ER = ER, CLh = QH*ER, CLr = fu*GFR, CL = QH*ER + fu*GFR,
        Vss = V[["ART"]] + V[["VEN"]] + sum(Kp*V[names(Kp)]),
        F.po = Fa*(1 - ER)), 4)
