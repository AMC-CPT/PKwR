# 부하용량은 분포용적이, 유지용량은 청소율이 결정한다.
LD <- function(Vss, Cp.target) Vss * Cp.target          # mg
MD <- function(CLss, Cp.target) CLss * Cp.target        # mg/hr

# 디곡신: Vss 500 L, t(1/2) 40 hr -> CL = log(2)*Vss/t(1/2)
Vss <- 500; t.half <- 40; CL <- log(2)*Vss/t.half
Cp <- 0.0015                                           # mg/L (= 1.5 ng/mL)
c(CL.L.per.hr = CL, LD.mg = LD(Vss, Cp), MD.mg.per.day = MD(CL, Cp)*24)
