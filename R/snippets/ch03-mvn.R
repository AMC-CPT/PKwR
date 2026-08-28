# 상관이 있는 다변량 정규난수: Cholesky 로 만든다. z ~ N(0,I) 이면 Lz ~ N(0, LL')
Om <- matrix(c(0.09, 0.036, 0.036, 0.04), nrow = 2)   # 10장의 OMEGA 와 같은 꼴
L  <- t(chol(Om))
set.seed(20260828)
eta <- t(L %*% matrix(rnorm(2*20000), nrow = 2))
round(rbind(target = c(var1 = Om[1, 1], var2 = Om[2, 2], corr = cov2cor(Om)[1, 2]),
            sample = c(var(eta[, 1]), var(eta[, 2]), cor(eta[, 1], eta[, 2]))), 4)

# CV 로 읽으면 개인간 변이가 30%, 20% 이고 둘의 상관이 0.6 이라는 뜻이다
round(c(CV1.pct = 100*sqrt(exp(Om[1, 1]) - 1), CV2.pct = 100*sqrt(exp(Om[2, 2]) - 1),
        corr = cov2cor(Om)[1, 2]), 2)
