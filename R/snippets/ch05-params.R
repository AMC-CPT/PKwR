# 근본 파라미터에서 이차 파라미터로: ke = CL/V,  t(1/2) = log(2)/ke
sec.par <- function(CL, V) c(CL = CL, V = V, ke = CL/V, t.half = log(2)*V/CL)

# 반감기는 CL 과 V 가 함께 같은 방향으로 변하면 바뀌지 않는다.
rbind(basal   = sec.par(CL = 5,   V = 50),
      CL.half = sec.par(CL = 2.5, V = 50),   # 청소율만 반감 -> 반감기 2배
      V.double= sec.par(CL = 5,   V = 100),  # 분포용적만 2배 -> 반감기 2배
      both    = sec.par(CL = 2.5, V = 25))   # 둘 다 반감 -> 반감기 불변
