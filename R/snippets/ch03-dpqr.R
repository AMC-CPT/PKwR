# R 의 분포 함수는 첫 글자로 구실이 정해진다: d 밀도, p 누적확률, q 분위수, r 난수
round(c(d = dnorm(1.96), p = pnorm(1.96), q = qnorm(0.975)), 4)
set.seed(20260828); round(rnorm(3), 4)

# 로그정규는 정규의 지수다. 두 경로가 같은 값을 준다.
round(c(via.lnorm = plnorm(120, mu, sg), via.norm = pnorm(log(120), mu, sg)), 6)

# 세 분포는 모두 정규에서 만들어진다: chi2 = 제곱합, t = Z/sqrt(V/k), F = 두 비
set.seed(20260828); B <- 20000; k1 <- 5; k2 <- 9
Z <- rnorm(B); V1 <- rchisq(B, k1); V2 <- rchisq(B, k2)
qq <- c(0.5, 0.95, 0.975)
round(rbind(t.sim  = quantile(Z/sqrt(V1/k1), qq), t.true = qt(qq, k1),
            F.sim  = quantile((V1/k1)/(V2/k2), qq), F.true = qf(qq, k1, k2)), 3)
