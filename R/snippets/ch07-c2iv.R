# 2구획 모형: 근본 파라미터(CL, V1, Q, V2) -> 거시 상수(A, alpha, B, beta)
macro2 <- function(D, CL, V1, Q, V2) {
  k10 <- CL/V1; k12 <- Q/V1; k21 <- Q/V2
  su  <- k10 + k12 + k21                       # = alpha + beta
  al  <- (su + sqrt(su^2 - 4*k21*k10))/2       # alpha*beta = k21*k10
  be  <- (su - sqrt(su^2 - 4*k21*k10))/2
  c(A = D*(al - k21)/(V1*(al - be)), alpha = al,
    B = D*(k21 - be)/(V1*(al - be)), beta  = be)
}

D <- 250; CL <- 4.5; V1 <- 12; Q <- 18; V2 <- 48    # mg, L/hr, L
th <- macro2(D, CL, V1, Q, V2)
round(th, 4)

C2iv <- function(t)                          # 2구획 정맥 일시주사 농도함수
  th[["A"]]*exp(-th[["alpha"]]*t) + th[["B"]]*exp(-th[["beta"]]*t)

# 속도상수와 분포용적의 비교: beta < k10, V1 < Vss < Vz
round(c(k10 = CL/V1, beta = th[["beta"]],
        V1 = V1, Vss = V1 + V2, Vz = CL/th[["beta"]]), 4)
