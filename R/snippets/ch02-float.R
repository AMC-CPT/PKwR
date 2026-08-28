# 컴퓨터의 실수는 유한한 비트에 담긴다. 십진 유한소수가 이진 무한소수일 수 있다.
c(0.1 + 0.2 == 0.3, 99/(72*1.3) == 99/72/1.3)  # Cockcroft-Gault: 순서만 다르다
signif(c(gap = 0.1 + 0.2 - 0.3), 4)
isTRUE(all.equal(99/(72*1.3), 99/72/1.3))     # 등호 대신 허용오차로 비교한다

# 기계 엡실론: 1 에 더했을 때 1 과 구별되는 가장 작은 수
macheps <- function(x = 1) { e <- 1; while (x + e > x) e <- e/2; 2*e }
signif(c(mine = macheps(), R = .Machine$double.eps, at100 = macheps(100)), 4)

# 넘침과 밑넘침은 오류가 아니라 Inf 와 0 으로 조용히 처리된다
signif(c(exp709 = exp(709), exp710 = exp(710), exp.m745 = exp(-745),
         exp.m746 = exp(-746)), 4)
signif(c(left = 1e200*1e300*1e-200, right = 1e200*(1e300*1e-200)), 4)

# 가까운 두 수의 뺄셈은 유효자릿수를 잃는다 (ka 와 k 가 비슷한 흡수 모형)
ka1 <- 0.2500001; ke1 <- 0.25
signif(c(difference = ka1 - ke1, rel.error = abs((ka1 - ke1)/1e-7 - 1)), 4)
