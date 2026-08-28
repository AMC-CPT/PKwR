# 모형 판별의 세 잣대. ch04-diag(c)의 자료(참으로 2지수)로 1지수와 겨룬다.
f2e <- function(p) sum((log(y2) - log(p[1]*exp(-p[2]*x2) + p[3]*exp(-p[4]*x2)))^2)
b2  <- optim(c(70, 1.2, 40, 0.12), f2e, method = "L-BFGS-B",
             lower = rep(1e-4, 4), upper = c(1e4, 20, 1e4, 20))
s1 <- sum((log(y2) - log(b1[1]*exp(-b1[2]*x2)))^2); s2 <- b2$value
nn <- length(y2); q1 <- 2; q2 <- 4

# (1) F 검정: 늘어난 파라미터가 줄인 SSE 가 남은 잔차분산에 비해 큰가
# (2) 우도비 검정: 같은 질문을 카이제곱으로 (대표본 근사)
Fs <- ((s1 - s2)/(q2 - q1))/(s2/(nn - q2)); LR <- nn*log(s1/s2)
round(c(SSE.1exp = s1, SSE.2exp = s2, F = Fs, dOFV = LR, df = q2 - q1), 4)
signif(c(p.F = pf(Fs, q2 - q1, nn - q2, lower.tail = FALSE),
         p.LRT = pchisq(LR, q2 - q1, lower.tail = FALSE)), 3)

# (3) 정보기준: 벌점의 크기만 다르다 (AIC 는 2k, BIC 는 k log n)
ic <- function(sse, k) { m <- nn*log(2*pi*sse/nn) + nn        # 정규·등분산 가정
  c(m2LL = m, AIC = m + 2*(k + 1), BIC = m + (k + 1)*log(nn)) }
round(rbind(exp1 = ic(s1, q1), exp2 = ic(s2, q2),
            difference = ic(s2, q2) - ic(s1, q1)), 3)
