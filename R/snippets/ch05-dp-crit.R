# Smith 등(2000)의 임계영역: 용량비 r = Dmax/Dmin 에 대하여
#   1 + log(0.8)/log(r)  <=  b1  <=  1 + log(1.25)/log(r)
# 이면 용량 정규화 노출이 (0.8, 1.25) 안에 있다고 보아 비례성을 결론한다.
crit <- function(r, theta = 1.25)
  unname(c(1 - log(theta)/log(r), 1 + log(theta)/log(r)))

r <- max(pk$Dose)/min(pk$Dose)
cr <- crit(r)
c(dose.ratio = r, crit.lower = cr[1], crit.upper = cr[2],
  b1.lower = ci[1], b1.upper = ci[2],
  proportional = as.numeric(ci[1] >= cr[1] & ci[2] <= cr[2]))
