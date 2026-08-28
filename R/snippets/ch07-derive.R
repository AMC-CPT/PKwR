# 추정된 거시 상수 -> 미시 상수 -> 근본 파라미터
Ah <- e2[["A"]]; alh <- e2[["alpha"]]; Bh <- e2[["B"]]; beh <- e2[["beta"]]
k21h <- (Ah*beh + Bh*alh)/(Ah + Bh)
k10h <- alh*beh/k21h
k12h <- alh + beh - k21h - k10h
V1h  <- D/(Ah + Bh)

prm <- c(CL = k10h*V1h, V1 = V1h, Q = k12h*V1h, V2 = k12h*V1h/k21h,
         Vss = V1h*(1 + k12h/k21h), Vz = k10h*V1h/beh,
         t.half.z = log(2)/beh)
rbind(estimate = round(prm, 3),
      true     = c(CL, V1, Q, V2, V1 + V2, CL/th[["beta"]],
                   log(2)/th[["beta"]]))
