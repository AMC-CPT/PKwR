# 1구획 모형의 농도 함수 - 이 장에서 반복해 쓴다.
# IV bolus:  C(t) = (D/V) exp(-k t)
Civ <- function(t, D, V, k) D/V * exp(-k*t)

# 지속정주:  C(t) = (R0/CL) (1 - exp(-k t)),  CL = V k
Cinf <- function(t, R0, V, k) R0/(V*k) * (1 - exp(-k*t))

# 경구 일차 흡수: C(t) = (F D/V) ka/(ka-k) (exp(-k t) - exp(-ka t))
Cpo <- function(t, D, V, k, ka, F = 1)
  F*D/V * ka/(ka - k) * (exp(-k*t) - exp(-ka*t))

# 반감기와 제거속도상수의 상호 변환
k2half <- function(k) log(2)/k
half2k <- function(t.half) log(2)/t.half

c(k = half2k(12), t.half = k2half(half2k(12)))
